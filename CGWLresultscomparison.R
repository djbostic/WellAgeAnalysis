# load 2022 data
cgwl2022 <- read_rds("Data/InterpolationGWLevels/cgwl2022_raster.rds")
wide2022 <- read_rds("drystats_by_bg_WIDE_2022cgwl.rds")
long2022 <- read_rds("drystats_by_bg_LONG_2022cgwl.rds")

# load 2019 data
cgwl2019 <- read_rds("Data/InterpolationGWLevels/cgwl_raster.rds")
wide2019 <- read_rds("drystats_by_bg_WIDE.rds")
long2019 <- read_rds("drystats_by_bg_LONG.rds")

# raster avg comp
cgwl2019 <- resample(cgwl2019,cgwl2022)
compcgwl <-cgwl2022-cgwl2019
plot(cgwl2022-cgwl2019, main="2022-2019 CGWL Raster")

compcgwl_spdf <- as(compcgwl, "SpatialPixelsDataFrame")
compcgwl_df <- as.data.frame(compcgwl_spdf)
colnames(compcgwl_df) <- c("value", "x", "y")

ggplot() +  
  geom_tile(data=compcgwl_df, aes(x=x, y=y, fill=value), alpha=1)+
  scale_fill_viridis() +
  coord_equal() +
  theme_map() +
  theme(legend.position="right") +
  ggtitle("2022 - 2019 CGWL")
  

cellStats(cgwl2019, mean)
cellStats(cgwl2022, mean)

# total number of wells
