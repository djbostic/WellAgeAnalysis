# load packages
library(tidyverse) # general purpose data science toolkit
library(sp)        # spatial objects
library(raster)    # for raster objects
library(sf)
library(readr)
library(gstat)
library(rgdal)
library(lubridate)

# set working directory
# load data
# shapefile names
measurements <- read.csv("Data/periodic_gwl_bulkdatadownload_08312023/measurements.csv", header = TRUE)
measurements$msmt_date <- as.Date(measurements$msmt_date)
measurements$msmt_year <- year(measurements$msmt_date)
measure2022 <- filter(measurements, msmt_year==2022)
stations <- read.csv("Data/periodic_gwl_bulkdatadownload_08312023/stations.csv")
gwl_m <- left_join(measurements, stations) %>% filter(., !is.na(latitude))
gwl_m_sf <- st_as_sf(gwl_m, coords=c("longitude", "latitude"), crs=4326)

gwl_m_sf$gse_gwe <- as.numeric(gwl_m_sf$gse_gwe) 
gwl_m_sf <- filter(gwl_m_sf, gse_gwe > 0)

fall <- filter(gwl_m_sf, msmt_date >= as.Date("2022-08-01") & msmt_date <= as.Date("2022-10-31"))
spring <- filter(gwl_m_sf, msmt_date >= as.Date("2022-01-01") & msmt_date <= as.Date("2022-03-31"))

# GSP outline
cv <- st_read("Data/Boundaries/i08_C2VSimFG_Boundary/i08_C2VSimFG_Boundary.shp")
cv <- st_transform(cv, crs=4326)
interp_boundary <- as_Spatial(cv) # transform central valley shapefile

#### interpolation ####
#### fall ####
# subset pts to the central valley polygon
subset_boundary <- function(x){x[interp_boundary,]}
f_cv <- subset_boundary(as_Spatial(fall))

# get sets of overlapping points
get_set <- function(x, y){zerodist(x)[, y]}
sfa1 <- get_set(f_cv, 1)      # index of set 1: wells wtih an overlapping observation
sfa2 <- get_set(f_cv, 2)      # index of set 2: wells wtih an overlapping observation


# get parallel minima of overlapping points
f_min_list = pmin(f_cv[sfa1,]$gse_gwe, f_cv[sfa2,]$gse_gwe)

# replace DGBS of set 2 wells wtih average of set 1 and 2
f_cv[sfa2, "gse_gwe"] <- f_min_list

# remove set 1 wells
f_cv <- f_cv[-sfa1, ]

# fix incorrect values: observations depth below groud surface > 0 
no_neg <- function(x){x[x$gse_gwe > 0, ]}
f_cv <- no_neg(f_cv)

# log transform Depth Below Ground Surface 
f_cv@data$gse_gwe <- log(f_cv@data$gse_gwe)

# plot to ensure all is working
title <- "2022 Groundwater Level Monitoring Wells"
st <- formatC(nrow(f_cv), big.mark = ",")

plot(interp_boundary, col="grey90", sub = paste0("Spatially Unique Observations: ", st))
plot(f_cv, add = T, pch = 16, cex = .2, col = "red")

#### set up interpolation boundary ####
r <- st_rasterize(cv)          # create a template raster to interpolate over
raster::res(r) <- 5000            # > township resolution: 6 miles = 9656.06 meters
g <- as(r, "SpatialGrid") # convert raster to spatial grid object


#### FALL 22 KRIGING #####  
library(gstat)
gs_f <- gstat(formula = gse_gwe ~ 1, # spatial data, so fitting xy as idp vars
              locations = f_cv)        # groundwater monitoring well points 

v_f <- variogram(gs_f,              # gstat object
                 width = 5000)    # lag distance

fve_f <- fit.variogram(v_f,         # takes `gstatVariogram` object
                       vgm(15,   # partial sill: semivariance at the range
                           "Exp",     # linear model type
                           100000,    # range: distance where model first flattens out
                           0.25))      # nugget

# plot variogram and fit
plot(v_f, fve_f, xlab = 'Distance (m)', main = "FA 2018 Variogram")

crs(g) <- crs(f_cv)

# ordinary kriging 
kp_f <- krige(gse_gwe ~ 1, f_cv, g, model = fve_f)

# backtransformed
bt_f <- exp(kp_f@data$var1.pred + (kp_f@data$var1.var / 2) )

# means of backtransformed values and the sampled values
mu_bt_f <- mean(bt_f)
mu_original_f <- mean(mean(exp(f_cv$DGBS)))

# these means differ by > 5%, thus we make another correction
btt_f <- bt_f * (mu_original_f/mu_bt_f)
kp_f@data$var1.pred <- bt_f                    # overwrite w/ correct vals 
kp_f@data$var1.var  <- exp(kp_f@data$var1.var)  # exponentiate the variance

# covert to raster brick and crop to CV
ok_f <- brick(kp_f)                          # spatialgrid df -> raster brick obj.
ok_f <- mask(ok_f, interp_boundary)                       # mask to cv extent
names(ok_f) <- c('Prediction', 'Variance') # name the raster layers in brick

plot(ok_f$Prediction)
ok_f$ci_upper <- ok_f$Prediction + (1.96 * sqrt(ok_f$Variance))
ok_f$ci_lower <- ok_f$Prediction - (1.96 * sqrt(ok_f$Variance))
readr::write_rds(ok_f, "InterpolationGWLevels/fall18_interp_allcobs.rds")

#### FALL AND SPRING 2019 KRIGING ####
d  <- read_csv("InterpolationGWLevels/drive-download-20220412T055017Z-001/Fall_Spring_2019_InterpolationPoints/measurements.csv")
d2 <- read_csv("InterpolationGWLevels/drive-download-20220412T055017Z-001/Fall_Spring_2019_InterpolationPoints/stations.csv")

# subset measurements to 2019
d <- filter(gwl_m_sf, lubridate::year(msmt_date) == 2022) %>% 
  left_join(d2, by = "STN_ID")

# subset for observation wells with a 2019 measurement
d3 <- gwl_m_sf %>% 
  filter(!is.na(gse_gwe) & well_use == "Observation") %>% 
  group_by(stn_id) %>% 
  summarise(mean_gwl = mean(gse_gwe)) %>% 
  left_join(d2, by = "STN_ID")

d3 <- d3 %>% filter(mean_gwl >= 1)

# convert d3 into spatialPointsDataFrame
# prepare the 3 components: coordinates, data, and proj4string
spdf <- as_Spatial(d3)

spdf_merc <- spTransform(spdf, crs(interp_boundary))

subset_cv <- function(x){x[interp_boundary, ]}
pcv <- subset_cv(spdf_merc) 

# plot to see where monitoring wells are
interp_boundary2 <- st_as_sf(interp_boundary)
pcv3 <- st_as_sf(pcv)
ggplot() +
  geom_sf(data = interp_boundary2)+
  geom_sf(data = pcv3, aes(color = mean_gwl)) + 
  scale_colour_gradientn(colours=rainbow(4))

# get sets of overlapping points
get_set <- function(x, y){zerodist(x)[, y]}
s1 <- get_set(pcv, 1)      # index of set 1: wells wtih an overlapping observation
s2 <- get_set(pcv, 2)      # index of set 2: wells wtih an overlapping observation

# get parallel minima of overlapping points
min_list = pmin(pcv[s1,]$mean_gwl, pcv[s2,]$mean_gwl)

# replace DGBS of set 2 wells wtih average of set 1 and 2
pcv[s2, "mean_gwl"] <- min_list

# remove set 1 wells
pcv <- pcv[-s1, ]

# log transform Depth Below Ground Surface 
pcv@data$mean_gwl_log <- log(pcv@data$mean_gwl)

library(automap)
plot(autofitVariogram(mean_gwl_log~1, pcv))

gs <- gstat(formula = mean_gwl_log ~ 1, # spatial data, so fitting xy as idp vars
            locations = pcv)        # groundwater monitoring well points 

v <- variogram(gs,              # gstat object
               width = 1)    # lag distance
plot(v)


fve <- fit.variogram(v,         # takes `gstatVariogram` object
                     vgm(1.5,   # partial sill: semivariance at the range
                         "Exp",     # linear model type
                         40,    # range: distance where model first flattens out
                         0.4))      # nugget

# plot variogram and fit
plot(v, fve, xlab = 'Distance (m)', main = "Mean 2022 Variogram")

# ordinary kriging 
kp <- krige(mean_gwl_log ~ 1, pcv, g, model = fve)

# backtransformed
bt <- exp(kp@data$var1.pred + (kp@data$var1.var / 2) )

# means of backtransformed values and the sampled values
mu_bt <- mean(bt)
mu_original <- mean(mean(exp(pcv$mean_gwl_log)))

# these means differ by > 5%, thus we make another correction
btt <- bt * (mu_original/mu_bt)
kp@data$var1.pred <- btt                    # overwrite w/ correct vals 
kp@data$var1.var  <- exp(kp@data$var1.var)  # exponentiate the variance

# covert to raster brick and crop to CV
ok_19 <- brick(kp)                          # spatialgrid df -> raster brick obj.
ok_19 <- mask(ok_19, interp_boundary)                       # mask to cv extent
names(ok_19) <- c('Prediction', 'Variance') # name the raster layers in brick

plot(ok_19$Prediction)
ok_19$ci_upper <- ok_19$Prediction + (1.96 * sqrt(ok_19$Variance))
ok_19$ci_lower <- ok_19$Prediction - (1.96 * sqrt(ok_19$Variance))
readr::write_rds(ok_19, "InterpolationGWLevels/FSP19_interpolation_allcobs.rds")

#### Average 2018 2019 groundwater level predictions ####
d_avg <- mean(ok_f$Prediction, ok_sp$Prediction, ok_19$Prediction)
readr::write_rds(d_avg, "InterpolationGWLevels/cgwl_raster.rds")

pal <- colorRampPalette(c("cornflowerblue","red"))
plot(d_avg, col = pal(6), main = "Mean WSE \n2018 - 2019", axes=FALSE, box=FALSE)
plot(gsps$geometry, add=T)

#writeRaster(d_avg, filename = "Output/1819avginterp_allcobs.tif", overwrite=TRUE)

#### AAAAA Interpolate USE THIS ONE PLEASE PLEASE ####
# subset pts to the central valley polygon
plot(interp_boundary)
plot(gwl, add=T)

gwl <- as_Spatial(gwl_cv)
spTransform(gwl, crs(interp_boundary))
gwl$gse_gwe <- as.numeric(gwl$gse_gwe) 
gwl@data <- filter(gwl@data, gse_gwe > 0)

#b <- as_Spatial(basins)
subset_gsp_outline <- function(x){x[interp_boundary,]}
gw_gsp_outline <- subset_gsp_outline(gwl) 

# get sets of overlapping points
get_set <- function(x, y){zerodist(x)[, y]}
s1_gw <- get_set(gw_gsp_outline, 1)      # index of set 1: wells wtih an overlapping observation
s2_gw <- get_set(gw_gsp_outline, 2)      # index of set 2: wells wtih an overlapping observation

# get parallel minima of overlapping points
min_list_gw = pmin(gw_gsp_outline[s1_gw,]$gse_gwe, gw_gsp_outline[s2_gw,]$gse_gwe)

# replace DGBS of set 2 wells wtih average of set 1 and 2
gw_gsp_outline[s2_gw, "gw_dtw"] <- min_list_gw

# remove set 1 wells
gw_gsp_outline <- gw_gsp_outline[-s1_gw, ]

# fix incorrect values: remove NAs
no_na <- function(x){x[!is.na(gw_gsp_outline$gw_dtw),]}
gw_gsp_outline <- no_na(gw_gsp_outline)

# log transform Depth Below Ground Surface 
mt_gsp_outline@data$MT_dtw <- log(mt_gsp_outline@data$MT_dtw)

# plot to ensure all is working
title <- "Township Coverage \nMinimum Threshold Wells"
st <- formatC(nrow(mt_gsp_outline), big.mark = ",")
plot(interp_boundary, main = title, sub = paste0("Monitoring Wells Used: ", st))
plot(mt_gsp_outline, add = T, pch = 16, cex = .2, col = "blue")


gs_mt <- gstat(formula = MT_dtw ~ 1, # spatial data, so fitting xy as idp vars
               locations = mt_gsp_outline)        # groundwater monitoring well points 

v_mt <- variogram(gs_mt,              # gstat object
                  width = 1000)    # lag distance

plot(v_mt)

fve_mt <- fit.variogram(v_mt,         # takes `gstatVariogram` object
                        vgm(.84,   # partial sill: semivariance at the range
                            "Exp",     # linear model type
                            38992,    # range: distance where model first flattens out
                            0.34))      # nugget

fve_mt <- autofitVariogram(MT_dtw~1, mt_gsp_outline, "Exp")
plot(autofitVariogram(MT_dtw~1, mt_gsp_outline, "Exp"))
# plot variogram and fit
plot(v_mt, fve_mt, main="Minimum Threshold Variogram")

# ordinary kriging 
kp_mt <- krige(MT_dtw ~ 1, mt_gsp_outline, g, model = fve_mt)

# backtransformed
bt_mt <- exp( kp_mt@data$var1.pred + (kp_mt@data$var1.var / 2) )

# means of backtransformed values and the sampled values
mu_bt_mt <- mean(bt_mt)
mu_original_mt <- mean(mean(exp(mt_gsp_outline$MT_dtw)))

# these means differ by > 5%, thus we make another correction
btt_mt <- bt_mt * (mu_original_mt/mu_bt_mt)
kp_mt@data$var1.pred <- bt_mt                    # overwrite w/ correct vals 
kp_mt@data$var1.var  <- exp(kp_mt@data$var1.var)  # exponentiate the variance

# covert to raster brick and crop to buff_ts
ok_mt <- brick(kp_mt)                          # spatialgrid df -> raster brick obj.
ok_mt <- mask(ok_mt, interp_boundary)                       # mask to gsp_outline extent
names(ok_mt) <- c('Prediction', 'Variance') # name the raster layers in brick

ok_mt$ci_upper <- ok_mt$Prediction + (1.96 * sqrt(ok_mt$Variance))
ok_mt$ci_lower <- ok_mt$Prediction - (1.96 * sqrt(ok_mt$Variance))

plot(ok_mt$Prediction)
write_rds(ok_mt, "InterpolationGWLevels/minthreshinterpolation_allcobs.rds")

k <- read_rds("InterpolationGWLevels/minthreshinterpolation_allcobs.rds")
plot(k$Prediction)

ba <- brick(d_avg$layer, ok_mt$Prediction)
names(ba) <- c("Current GWL", "MT GWL")
spplot(ba, sp.layout=gsps)

plot(gsp)
plot(ok_mt$Prediction, add=TRUE)

#### Remaining Questions ####
# Should I create a buffer based on monitoring well points for each kriging set? And then compare areas covered?
# Do I compare dist of log data to normal dist to see if smirnoff says its okay?

