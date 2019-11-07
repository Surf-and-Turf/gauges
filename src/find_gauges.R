

library(tidyverse)
library(sf)
library(maps)
library(magrittr)
library(purrr)
library(data.table)
library(tmap)
library(readr)
library(lubridate)
library(mapview)
library(ggthemes)
library(vroom) #Fast csv reading
library(mapview)
library(nhdplusTools)
library(USAboundaries)
library(rmapshaper)

#download NHDplusv2 seamless continental USA to yoru computer here
#https://www.epa.gov/waterdata/nhdplus-national-data
#then can run the code below for staging and loading the network

# if you do not have nhdplusv2 network I can provide .RData file
# with all this data, and can drop in our google drive 

# load nhdplusv2 form my computer
nhdplus_path("D:/projects/TSS/data/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb")

#only need to run this staging once
nhd_paths <- stage_national_data()

#read in flowlines only
network <- readRDS(nhd_paths$flowline)

# clean network
network_prep <- network %>%
  filter(StreamOrde == StreamCalc) %>%
  filter(!FTYPE %in% c("CanalDitch" ,"Coastline", "Pipeline" )) %>%
  st_transform(2163)

# filter to reaches that are terminal outlets (into estuaries, coasts, terminal lakes)
terminal <- network_prep %>%
  filter(TerminalFl ==1) 

# buffer coastline. DONT run this "coast" code, it takes ~ 30 minutes
coast <- network %>%
  filter(FTYPE == "Coastline") %>%
  #slice(4000:5600) %>%
  st_transform(2163) %>%
  st_simplify(dTolerance=5000) %>%
  st_union() %>%
  st_buffer(5000) 

coast_terminal <- terminal[coast,]

# save this data
#rm(network)
#save.image("gauges.RData")

# check to see if only coast lines found. Looks good!
mapview(coast_terminal, lwd=5) + mapview(coast)

#############################################################
# get comid 100 km upstream into the whole network in all tributaries
comid <- coast_terminal$COMID

# DONT run this loop it takes a long time ~ few hours
# probably should furrr::future_map2 this

output <- list()
for (i in 1:length(comid)) {
#for(i in 1:10) {
  comids <- get_UT(network=network_prep, comid=comid[i], distance=100)
 
  comids <- c(comids, comid[i]) %>%
    as.data.frame() %>%
    mutate(terminal_comid = comid[i]) 

  output[[i]] <- comids
}

# dataframe with column of all COMID upstream of terminal COMID
out <- bind_rows(output) 
colnames(out) <- c("COMID", "terminal_COMID")

# write to csv for coastal comids
write_csv( out, "D:/Dropbox/projects/Surf_and_Turf/gauges/out/coastal_comid.csv")

# test what rivers look like
test_net <- network_prep %>%
  filter(COMID %in% out$COMID) %>%
  #filter out small streams so isn't a massive dataset to map
  filter(StreamOrde >2)

mapview(test_net) 


##################################################
# get usgs gauge names 

# load gage file that also has NHDplusv2 COMID. Download here
#https://www.sciencebase.gov/catalog/item/577445bee4b07657d1a991b6

# we should also thinmk about other sources of data for  coastal rivers
# i.e. NOAA NERR etc.
gages <- st_read("D:/Dropbox/projects/TSS/data/GageLoc/GageLoc.shp")

coast_gages <- gages %>%
  filter(FLComID %in% out$COMID) 

# make gauge ID searchable in dataRetrieval package
gage_ID <- coast_gages %>%
  st_set_geometry(NULL) %>%
  dplyr::select(SOURCE_FEA, FLComID) %>%
  mutate(SOURCE_FEA = as.character(SOURCE_FEA)) %>%
  rename(COMID = FLComID) %>%
  as.data.frame()

# see what gauges have different types of data
#https://help.waterdata.usgs.gov/parameter_cd?group_cd=PHY
# 00530 TSS mg/L
# 00060 discharge cfs

coast_gages_info <- readNWISsite(siteNumber = gage_ID$SOURCE_FEA)

# write file
write_csv(coast_gages_info,"D:/Dropbox/projects/Surf_and_Turf/gauges/out/coastal_gauges.csv" )


##########################################################
# now find what data is at these gauges

# example find what og these gauges have Q
gage_with_Q <- whatNWISdata(siteNumber = gage_ID$SOURCE_FEA, service = "dv",
                         parameterCd = c("00060"))






##########################################
### Munge

# load usa boundaries
# usa <- us_boundaries() %>%
#   filter(!state_name %in% c( "Puerto Rico", "Alaska", "Hawaii" )) %>%
#   filter(jurisdiction_type != "territory") %>%
#   st_union() %>%
#   st_transform(2163) 
#
# plot(st_geometry(usa))
# plot(st_geometry(usa3), add=T, border="red")

