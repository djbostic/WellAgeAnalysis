# load packages
library(tidyverse) # general purpose data science toolkit
library(sp)        # spatial objects
library(raster)    # for raster objects
library(sf)
library(readr)
library(gstat)
library(rgdal)
library(lubridate)
library(terra)
library(stars)

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

fall_and_spring22 <- rbind(fall, spring)
fs22 <- vect(fall_and_spring22)
fs22 <- spTransform(as_Spatial(st_as_sf(fs22)), CRS=CRS("+proj=merc +ellps=GRS80"))

#### BOUNDARIES ####
cv <- st_read("Data/Boundaries/i08_C2VSimFG_Boundary/i08_C2VSimFG_Boundary.shp")
cv <- st_transform(cv, crs=4326)
interp_boundary <- as_Spatial(cv) # transform central valley shapefile
CV.Shape <- vect(cv) 
#transforming the boundary
CV.Shape_merc <- spTransform(as_Spatial(st_as_sf(CV.Shape)), CRS=CRS("+proj=merc +ellps=GRS80"))
CV.Shape_merc #looking at summary output to check projection 

#### preparing an empty grid ####
G <- as.data.frame(spsample(fs22, "regular", n=5000)) #n = total number of grid cells
names(G) <- c("X", "Y")
coordinates(G) <- c("X", "Y")
gridded(G) <- TRUE  # create SpatialPixel object
fullgrid(G) <- TRUE  # create SpatialGrid object
proj4string(G) <- proj4string(fs22) # using the projection from F22_merc to project the grid G
proj4string(G) # checking that G is projected
plot(G)


#### FALL 2022 interpolation ####
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
title <- "FALL 2022 Groundwater Level Monitoring Wells"
st <- formatC(nrow(f_cv), big.mark = ",")

plot(interp_boundary, col="grey90", sub = paste0("Spatially Unique Observations: ", st))
plot(f_cv, add = T, pch = 16, cex = .2, col = "red")

### vectorize and raster
F22 <- vect(f_cv)

# transforming the filtered data so that it is a projected CRS
F22_merc <- spTransform(as_Spatial(st_as_sf(F22)), CRS=CRS("+proj=merc +ellps=GRS80")) 
F22_merc # looking at summary output to check projection 

#### FALL 22 KRIGING #####  
library(automap)
afv <- autofitVariogram(gse_gwe~1, F22_merc)
plot(afv)

library(gstat)
gs_f <- gstat(formula = gse_gwe ~ 1, # spatial data, so fitting xy as idp vars
              locations = F22_merc)        # groundwater monitoring well points 

v_f <- variogram(gs_f,              # gstat object
                 width = 50)    # lag distance

fve_f <- fit.variogram(v_f,         # takes `gstatVariogram` object
                       vgm(.83,   # partial sill: semivariance at the range
                           "Exp",     # linear model type
                           60000,    # range: distance where model first flattens out
                           .07))      # nugget

# plot variogram and fit
plot(v_f, fve_f, xlab = 'Distance (m)', main = "FA 2018 Variogram")

crs(G) <- crs(F22_merc)

# ordinary kriging 
kp_f <- krige(gse_gwe ~ 1, F22_merc, G, model = fve_f)

# backtransformed
bt_f <- exp(kp_f@data$var1.pred + (kp_f@data$var1.var / 2) )

# means of backtransformed values and the sampled values
mu_bt_f <- mean(bt_f)
mu_original_f <- mean(mean(exp(F22_merc$gse_gwe)))

# these means differ by > 5%, thus we make another correction
btt_f <- bt_f * (mu_original_f/mu_bt_f)
kp_f@data$var1.pred <- bt_f                    # overwrite w/ correct vals 
kp_f@data$var1.var  <- exp(kp_f@data$var1.var)  # exponentiate the variance

# covert to raster brick and crop to CV
ok_f <- brick(kp_f)                          # spatialgrid df -> raster brick obj.
ok_f <- mask(ok_f, CV.Shape_merc)                       # mask to cv extent
names(ok_f) <- c('Prediction', 'Variance') # name the raster layers in brick

plot(ok_f$Prediction)
ok_f$ci_upper <- ok_f$Prediction + (1.96 * sqrt(ok_f$Variance))
ok_f$ci_lower <- ok_f$Prediction - (1.96 * sqrt(ok_f$Variance))
readr::write_rds(ok_f, "Data/InterpolationGWLevels/fall22_interp.rds")

#### SPRING 2022 INTERPOLATION ####
# subset pts to the central valley polygon
s_cv <- subset_boundary(as_Spatial(spring))

# get sets of overlapping points
sfa1 <- get_set(s_cv, 1)      # index of set 1: wells wtih an overlapping observation
sfa2 <- get_set(s_cv, 2)      # index of set 2: wells wtih an overlapping observation

# get parallel minima of overlapping points
s_min_list = pmin(s_cv[sfa1,]$gse_gwe, s_cv[sfa2,]$gse_gwe)

# replace DGBS of set 2 wells wtih average of set 1 and 2
s_cv[sfa2, "gse_gwe"] <- s_min_list

# remove set 1 wells
s_cv <- s_cv[-sfa1, ]

# fix incorrect values: observations depth below groud surface > 0 
s_cv <- no_neg(s_cv)

# log transform Depth Below Ground Surface 
s_cv@data$gse_gwe <- log(s_cv@data$gse_gwe)

# plot to ensure all is working
title <- "SPRING 2022 Groundwater Level Monitoring Wells"
st <- formatC(nrow(s_cv), big.mark = ",")
plot(interp_boundary, col="grey90", sub = paste0("Spatially Unique Observations: ", st))
plot(s_cv, add = T, pch = 16, cex = .2, col = "red")

### vectorize and raster
S22 <- vect(s_cv)

# transforming the filtered data so that it is a projected CRS
S22_merc <- spTransform(as_Spatial(st_as_sf(S22)), CRS=CRS("+proj=merc +ellps=GRS80")) 
S22_merc # looking at summary output to check projection 

#### SPRING 22 KRIGING #####  
gs_f <- gstat(formula = gse_gwe ~ 1, # spatial data, so fitting xy as idp vars
              locations = S22_merc)        # groundwater monitoring well points 

v_f <- variogram(gs_f,              # gstat object
                 width = 50)    # lag distance

sve_f <- fit.variogram(v_f,         # takes `gstatVariogram` object
                       vgm(.83,   # partial sill: semivariance at the range
                           "Exp",     # linear model type
                           40000,    # range: distance where model first flattens out
                           .07))      # nugget

# plot variogram and fit
plot(v_f, sve_f, xlab = 'Distance (m)', main = "SP2022 Variogram")

crs(G) <- crs(S22_merc)

# ordinary kriging 
kp_f <- krige(gse_gwe ~ 1, S22_merc, G, model = sve_f)

# backtransformed
bt_f <- exp(kp_f@data$var1.pred + (kp_f@data$var1.var / 2) )

# means of backtransformed values and the sampled values
mu_bt_f <- mean(bt_f)
mu_original_f <- mean(mean(exp(S22_merc$gse_gwe)))

# these means differ by > 5%, thus we make another correction
btt_f <- bt_f * (mu_original_f/mu_bt_f)
kp_f@data$var1.pred <- bt_f                    # overwrite w/ correct vals 
kp_f@data$var1.var  <- exp(kp_f@data$var1.var)  # exponentiate the variance

# covert to raster brick and crop to CV
ok_s <- brick(kp_f)                          # spatialgrid df -> raster brick obj.
ok_s <- mask(ok_s, CV.Shape_merc)                       # mask to cv extent
names(ok_s) <- c('Prediction', 'Variance') # name the raster layers in brick

plot(ok_s$Prediction)
ok_s$ci_upper <- ok_s$Prediction + (1.96 * sqrt(ok_s$Variance))
ok_s$ci_lower <- ok_s$Prediction - (1.96 * sqrt(ok_s$Variance))
readr::write_rds(ok_s, "Data/InterpolationGWLevels/spring22_interp.rds")


#### Average 2022 groundwater level predictions ####
extent(ok_f$Prediction) <- extent(G)
ok_f <- resample(ok_f, ok_s)
d_avg <- mean(ok_f$Prediction, ok_s$Prediction)
readr::write_rds(d_avg, "Data/InterpolationGWLevels/cgwl2022_raster.rds")

pal <- colorRampPalette(c("cornflowerblue","red"))
plot(d_avg, col = pal(6), main = "Mean WSE \n2022", axes=FALSE, box=FALSE)
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

