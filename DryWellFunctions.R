# dry well analysis code
#dacs <- st_read("Data/Boundaries/census_data_disadvantaged_communities_2018/DAC_Pl18.shp") %>% filter(DAC18 == "Y") %>% st_transform(., merc) %>% mutate(area = st_area(.))

dcs <- st_intersection(dacs, gsps)
dcs$dcs_area <- st_area(dcs)
dcs$perc_overlap <- dcs$dcs_area / dcs$area
dcs <- filter(dcs, as.numeric(perc_overlap) > .5)

# groundwater levels
library(readr)
library(raster)
cgwl_raster <- read_rds("Data/InterpolationGWLevels/cgwl2022_raster.rds")
projectRaster(cgwl_raster, crs=crs(setslist[[1]]))

mt_raster <- read_rds("Data/InterpolationGWLevels/mt_raster.rds")

wellanalysis <- function(x=dwws){
  cgwl_at_dw <- raster::extract(cgwl_raster, x) # intersect to get value of current water level at well points
  cad_dw <- cbind(x, cgwl_at_dw) 
  cad_dw <- cad_dw[!is.na(cad_dw$cgwl_at_dw), ] # remove wells where there is no current groundwater level value (wells are likely outside of interpolation area)
  cad_dw$tcd_dry <- ifelse(cad_dw$TOTALCOMPLETEDDEPTH <= cad_dw$cgwl_at_dw, "Failing", "Active")
  activedw <- cad_dw[cad_dw$tcd_dry == "Active", ]
  activedw <- activedw[!is.na(activedw$WCRNUMBER), ]
  print(length(unique(activedw$WCRNUMBER))) # percent of wells whose TCD is below current groundwater levels (useable wells)
  print(mean(activedw$TOTALCOMPLETEDDEPTH))
  print(mean(activedw$TOPOFPERFORATEDINTERVAL))
  
  # TCD
  mt_at_dw <- raster::extract(mt_raster, activedw) # intersect to get value of current water level at well points
  mad <- cbind(activedw, mt_at_dw) 
  mad <- mad[!is.na(mad$mt_at_dw), ] # remove wells where there is no current groundwater level value (wells are likely outside of interpolation area)
  mad$tcddry <- ifelse(mad$TOTALCOMPLETEDDEPTH <= mad$mt_at_dw, "Failing", "Active")
  #print(table(mad$tcddry))
  #print(table(mad$tcddry)/length(unique(mad$WCR)))
  
  # bottom of well screen
  #bots <- filter(mad, is.na(mad$BOTTOMOFPERFORATEDINTERVAL)==FALSE & mad$BOTTOMOFPERFORATEDINTERVAL > 0 & mad$tcddry == "Active")
  #bots$botdry <- ifelse(bots$BOTTOMOFPERFORATEDINTERVAL <= bots$mt_at_dw, "Failing", "Active")
  #print(table(bots$botdry))
  #print(table(bots$botdry)/length(unique(bots$WCR)))
  
  # all 
  mad$top <- ifelse(is.na(mad$TOPOFPERFORATEDINTERVAL)==TRUE, 0, mad$TOPOFPERFORATEDINTERVAL)
  mad$dry <- ifelse((mad$mt_at_dw >= mad$TOTALCOMPLETEDDEPTH & mad$TOTALCOMPLETEDDEPTH > 0), "TCDdry",
                    ifelse((mad$top > 0 & mad$mt_at_dw >= mad$top & mad$mt_at_dw < mad$TOTALCOMPLETEDDEPTH), "topdry", "active"))
  print(table(mad$dry))
  print(table(mad$dry)/length(unique(mad$WCR)))
  return(mad)
}

