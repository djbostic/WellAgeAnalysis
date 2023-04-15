# dry well analysis code
# ca
ca <- st_read("Boundaries/cb_2018_us_state_500k/cb_2018_us_state_500k.shp") %>% filter(STUSPS == "CA") %>% st_transform(., crs=merc)

# gsps
allgwbasins <- st_read("Boundaries/i08_B118_CA_GroundwaterBasins/i08_B118_CA_GroundwaterBasins.shp") %>% st_transform(., crs=merc)

sgmabasins <- st_read("Boundaries/GSP_submitted/GSA_Master.shp") %>% st_transform(., crs=merc)
gspcoda <- read_csv("Boundaries/gsp_coda.csv") %>% mutate(GSP.ID = as.character(GSP.ID))
gsps <- left_join(sgmabasins, gspcoda, by="GSP.ID")
gsps <- filter(gsps, is.na(GSP.NAME)==FALSE)
gsps$gsp_area <- st_area(gsps)
gsps_sp <- as_Spatial(gsps)

basins <- gsps %>% group_by(BASIN) %>%  st_buffer(100) %>% summarise(geometry = st_union(geometry))

# dacs
dacs <- st_read("Boundaries/census_data_disadvantaged_communities_2018/DAC_Pl18.shp") %>% filter(DAC18 == "Y") %>% st_transform(., merc) %>% mutate(area = st_area(.))

dcs <- st_intersection(dacs, gsps)
dcs$dcs_area <- st_area(dcs)
dcs$perc_overlap <- dcs$dcs_area / dcs$area
dcs <- filter(dcs, as.numeric(perc_overlap) > .5)

# MTs
mn <- read.csv("MTs/CentralValleyMTs.csv") %>% filter(is.na(Long)==FALSE & is.na(Lat)==FALSE & is.na(MT_dtw)==FALSE)
mts <- st_as_sf(mn, coords = c("Long","Lat"), crs=4326) %>% st_transform(., crs=merc)
mts <- st_intersection(mts, gsps)
mtjoindata <- mts %>% st_drop_geometry(.) %>% dplyr::select(GSP.ID, GSP.NAME) %>% unique(.)

# domestic wells
richsdws <- read_rds("DomesticWells/domcv6_mean_gw_with_beta_GF_CI.rds") %>% st_as_sf(.) %>% st_transform(., st_crs(gsps)) %>% filter(year >=1990)
dw <- st_intersection(richsdws, gsps)
hi <- dw %>% group_by(GSP.NAME) %>% summarise(`Number of Domestic Wells` = length(unique(WCRNumber)),`Average TCD` = mean(TotalCompletedDepth, na.rm=TRUE), `Average Pump Depth` = mean(pump_loc), `Fraction of All DWs`=100*length(unique(WCRNumber))/nrow(dw)) %>% st_drop_geometry(.)

# public supply wells
psws <- read.csv("DomesticWells/gama_location_construction_v2.csv") %>% filter(., GM_WELL_CATEGORY == "MUNICIPAL" | GM_WELL_CATEGORY == "WATER SUPPLY, OTHER")
psws <-  st_as_sf(psws, coords = c("GM_LONGITUDE","GM_LATITUDE"), crs=4326) %>% st_transform(., crs=merc)
psw <- psws %>% filter(is.na(GM_WELL_DEPTH_FT)==FALSE & GM_WELL_DEPTH_FT >0 & GM_WELL_DEPTH_FT < 5000) %>% st_intersection(., gsps)

# ALL DRINKING WATER WELLS
dws <- dw %>% mutate(type = "domestic") %>% dplyr::select(WCR = WCRNumber, type, BASIN, GSP.NAME=GSP.NAME, TCD = TotalCompletedDepth, pump_loc, top, bot)
psww <- psw %>% mutate(pump_loc = NA, type = "public supply") %>% 
  dplyr::select(WCR = GM_WELL_ID, type, BASIN, GSP.NAME=GSP.NAME, TCD = GM_WELL_DEPTH_FT, pump_loc, top = GM_TOP_DEPTH_OF_SCREEN_FT, bot = GM_BOTTOM_DEPTH_OF_SCREEN_FT)

dwws <- rbind(dws, psww) %>% filter(TCD > 0 & TCD < 5000)

# groundwater levels
cgwl_raster <- read_rds("InterpolationGWLevels/cgwl_raster.rds")
mt_raster <- read_rds("InterpolationGWLevels/mt_raster.rds")

wellanalysis <- function(x=dwws){
  cgwl_at_dw <- raster::extract(cgwl_raster, x) # intersect to get value of current water level at well points
  cad_dw <- cbind(x, cgwl_at_dw) 
  cad_dw <- cad_dw[!is.na(cad_dw$cgwl_at_dw), ] # remove wells where there is no current groundwater level value (wells are likely outside of interpolation area)
  cad_dw$tcd_dry <- ifelse(cad_dw$TCD <= cad_dw$cgwl_at_dw, "Failing", "Active")
  activedw <- cad_dw[cad_dw$tcd_dry == "Active", ]
  activedw <- activedw[!is.na(activedw$WCR), ]
  print(length(unique(activedw$WCR)) / length(unique(cad_dw$WCR))) # percent of wells whose TCD is below current groundwater levels (useable wells)
  
  # TCD
  mt_at_dw <- raster::extract(mt_raster, activedw) # intersect to get value of current water level at well points
  mad <- cbind(activedw, mt_at_dw) 
  mad <- mad[!is.na(mad$mt_at_dw), ] # remove wells where there is no current groundwater level value (wells are likely outside of interpolation area)
  mad$tcddry <- ifelse(mad$TCD <= mad$mt_at_dw, "Failing", "Active")
  print(table(mad$tcddry))
  print(table(mad$tcddry)/length(unique(mad$WCR)))
  
  # bottom of well screen
  bots <- filter(mad, is.na(mad$bot)==FALSE & mad$bot > 0 & mad$tcddry == "Active")
  bots$botdry <- ifelse(bots$bot <= bots$mt_at_dw, "Failing", "Active")
  print(table(bots$botdry))
  print(table(bots$botdry)/length(unique(bots$WCR)))
  
  # all 
  mad$top <- ifelse(is.na(mad$top)==TRUE, 0, mad$top)
  mad$dry <- ifelse((mad$mt_at_dw >= mad$TCD & mad$TCD > 0), "TCDdry",
                    ifelse((mad$top > 0 & mad$mt_at_dw >= mad$top & mad$mt_at_dw < mad$TCD), "topdry",
                           ifelse(mad$pump_loc > 0 & mad$mt_at_dw >= mad$pump_loc & mad$mt_at_dw < mad$TCD & mad$mt_at_dw < mad$top, "pumpdry", "active")))
  print(table(mad$dry)/length(unique(mad$WCR)))
  return(mad)
}
