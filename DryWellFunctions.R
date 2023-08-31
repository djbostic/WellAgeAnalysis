# dry well analysis code
# set coordinate reference system
merc <- crs("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0
            +k=1.0 +units=m +nadgrids=@null +no_defs")
# ca
ca <- st_read("Data/Boundaries/cb_2018_us_state_500k/cb_2018_us_state_500k.shp") %>% filter(STUSPS == "CA") %>% st_transform(., crs=merc)

# gsps
allgwbasins <- st_read("Data/Boundaries/i08_B118_CA_GroundwaterBasins/i08_B118_CA_GroundwaterBasins.shp") %>% st_transform(., crs=merc)

sgmabasins <- st_read("Data/Boundaries/GSP_submitted/GSA_Master.shp") %>% st_transform(., crs=merc)
gspcoda <- read_csv("Data/Boundaries/gsp_coda.csv") %>% mutate(GSP.ID = as.character(GSP.ID))
gsps <- left_join(sgmabasins, gspcoda, by="GSP.ID")
gsps <- filter(gsps, is.na(GSP.NAME)==FALSE)
gsps$gsp_area <- st_area(gsps)
gsps_sp <- as_Spatial(gsps)

basins <- gsps %>% group_by(BASIN) %>%  st_buffer(100) %>% summarise(geometry = st_union(geometry))

# dacs+
dacs <- st_read("Data/Boundaries/census_data_disadvantaged_communities_2018/DAC_Pl18.shp") %>% filter(DAC18 == "Y") %>% st_transform(., merc) %>% mutate(area = st_area(.))

dcs <- st_intersection(dacs, gsps)
dcs$dcs_area <- st_area(dcs)
dcs$perc_overlap <- dcs$dcs_area / dcs$area
dcs <- filter(dcs, as.numeric(perc_overlap) > .5)

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
