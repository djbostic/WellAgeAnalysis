# load packages
library(tidyverse) # general purpose data science toolkit
library(sp)        # spatial objects
library(raster)    # for raster objects
library(here)
library(sf)
library(lubridate)
library(readr)
options(timeout=1000)

# load data
# set coordinate reference system
merc <- crs("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0
            +k=1.0 +units=m +nadgrids=@null +no_defs")
# ca
ca <- st_read("Data/Boundaries/cb_2018_us_state_500k/cb_2018_us_state_500k.shp") %>% filter(STUSPS == "CA") %>% st_transform(., crs=merc)

# groundwater levels
cgwl_raster <- read_rds("Data/InterpolationGWLevels/cgwl2022_raster.rds")
mt_raster <- read_rds("Data/InterpolationGWLevels/mt_raster.rds")

# gsps
allgwbasins <- st_read("Data/Boundaries/i08_B118_CA_GroundwaterBasins/i08_B118_CA_GroundwaterBasins.shp") %>% st_transform(., crs=merc)

sgmabasins <- st_read("Data/Boundaries/GSP_submitted/GSA_Master.shp") %>% st_transform(., crs=merc)
gspcoda <- read_csv("Data/Boundaries/gsp_coda.csv") %>% mutate(GSP.ID = as.character(GSP.ID))
gsps <- left_join(sgmabasins, gspcoda, by="GSP.ID")
gsps <- filter(gsps, is.na(GSP.NAME)==FALSE)
gsps <- st_transform(gsps, crs=merc)
gsps$gsp_area <- st_area(gsps) 
gsps_sp <- as_Spatial(gsps)
basins <- gsps %>% group_by(BASIN) %>%  st_buffer(100) %>% summarise(geometry = st_union(geometry))

# domestic wells - OSWCR
data <- read.csv("/Users/darcybostic/Library/CloudStorage/GoogleDrive-djbostic1@gmail.com/.shortcut-targets-by-id/1zxS7SNp6bWpJ9u6mnyUMJiNkb3JenKPJ/Well retirement age and data reliability note/Code/WellAgeAnalysis/Data/wellcompletionreports_04172023download.csv", na.strings=c("", "NA"))

#### descriptive stats on data ####
# filter out wells with no WCR ID
data1 <- data %>% dplyr::select(WCRNUMBER, PLANNEDUSEFORMERUSE, TOTALCOMPLETEDDEPTH, TOPOFPERFORATEDINTERVAL, BOTTOMOFPERFORATEDINTERVAL, DATEWORKENDED, Lat=DECIMALLATITUDE, Long=DECIMALLONGITUDE, GROUNDSURFACEELEVATION) %>% mutate(Lat = as.numeric(Lat), Long=as.numeric(Long))

tbl <- table(toupper(unlist(strsplit(as.character(data$PLANNEDUSEFORMERUSE), " "))))

# filter to wells constructed after 1950
data1$year <- year(as.Date(data1$DATEWORKENDED, format = "%m/%d/%Y"))
data1 <- filter(data1, is.na(year)==F & year >= 1900 & year <= 2022)

# filter to get only domestic and public supply wells
data1 <- data1 %>% filter(grepl("domestic",PLANNEDUSEFORMERUSE, ignore.case = T) & 
                            !grepl("destruction",PLANNEDUSEFORMERUSE, ignore.case = T) & 
                               !grepl("irrigation",PLANNEDUSEFORMERUSE, ignore.case = T) & 
                               !grepl("industrial",PLANNEDUSEFORMERUSE, ignore.case = T) & 
                               !grepl("stock",PLANNEDUSEFORMERUSE, ignore.case = T) &
                            is.na(PLANNEDUSEFORMERUSE)==FALSE &
                                        is.na(Lat)==FALSE &
                                        is.na(Long)==FALSE &
                                        Lat != "" &
                                        Long != "" &
                                        Lat != "NA" &
                                        Long != "NA")

data1 <- data1 %>% filter(., Long >-200 & Lat < 45) %>% st_as_sf(., coords = c("Long", "Lat"), crs=4326) %>% st_transform(., crs=merc)
st_crs(gsps) <- merc
data3 <- st_intersection(data1, gsps)


# run analysis that shows how many wells have1 missing data, by decade - end result should output a table that includes the decade, number of wells, number of wells with TCD, number of wells with TOS/BOS,

# number of missing TCD
years <- c(1900, 1952, 1977, 1994)
table <- list()

yearfilter <- function(x){
  data2 <- data3 %>% filter(year >= x)
  haveTCD <- data2 %>% filter(is.na(TOTALCOMPLETEDDEPTH)==FALSE) %>% summarise(nwells_tcd = length(unique(WCRNUMBER)))
  haveTOP <- data2 %>% filter(is.na(TOPOFPERFORATEDINTERVAL)==FALSE) %>% summarise(nwells_top = length(unique(WCRNUMBER)))
  haveBOT <- data2 %>% filter(is.na(BOTTOMOFPERFORATEDINTERVAL)==FALSE) %>% summarise(nwells_bot = length(unique(WCRNUMBER)))
  haveTCD_TOP <- data2 %>% filter(is.na(TOTALCOMPLETEDDEPTH)==FALSE & is.na(TOPOFPERFORATEDINTERVAL)==FALSE & TOTALCOMPLETEDDEPTH > 0 & TOPOFPERFORATEDINTERVAL > 0) %>% summarise(nwells_all = length(unique(WCRNUMBER)))
  combined50 <- cbind(haveTCD, haveTOP, haveBOT, haveTCD_TOP)
  return(combined50)
}

nabyyear <- data3 %>% st_drop_geometry(.) %>% group_by(year) %>% summarise(na_tcd=length(WCRNUMBER[is.na(TOTALCOMPLETEDDEPTH)]), na_top=length(WCRNUMBER[is.na(TOPOFPERFORATEDINTERVAL)]), y_tcd=length(WCRNUMBER[!is.na(TOTALCOMPLETEDDEPTH)]), y_top=length(WCRNUMBER[!is.na(TOPOFPERFORATEDINTERVAL)]))

nby <- nabyyear %>% 
  pivot_longer(
    cols = na_tcd:y_top, 
    names_to = "status",
    values_to = "value"
  )

ggplot(data=nby, aes(year,value))+
  geom_line(aes(color = status, linetype=status)) + 
  scale_color_manual(values = c("darkred", "darkorange", "steelblue", "cornflowerblue"))+
    scale_linetype_manual(values = c(1, 1, 2, 2))+
  ggtitle("Number of Missing and Available Data by Year")+
  theme_bw()

ttl <- list()
for (i in c(1:length(years))){
  yf <- yearfilter(years[i])
  yf$year <- years[i]
  ttl[[i]] <- yf
}
all <- do.call(rbind, ttl)

# this loop produces a list of dataframes with the domestic well depths
setslist <- list()
for (i in c(1:length(years))){
  setslist[[i]] <- data3 %>% filter(year>=years[i] & 
                          is.na(TOTALCOMPLETEDDEPTH)==FALSE & 
                          is.na(TOPOFPERFORATEDINTERVAL)==FALSE &
                            TOTALCOMPLETEDDEPTH > 0 &
                            TOPOFPERFORATEDINTERVAL > 0)
}

# now we do the dry well analysis on the data frames
# remove wells whose TCD are above CGWL
activewells <- list()
for (i in c(1:length(setslist))){
  print(years[i])
  activewells[[i]] <- wellanalysis(setslist[[i]])
}

activewells[[1]]$scenario <- "All Time - 1900"
activewells[[2]]$scenario <- "70 Years - 1952"
activewells[[3]]$scenario <- "45 Years - 1977"
activewells[[4]]$scenario <- "28 Years - 1994"

ac <- do.call(rbind,activewells)
#ac$TOTALCOMPLETEDDEPTH <- -1*ac$TOTALCOMPLETEDDEPTH
# box plot of depths for each scenario
ggplot()+
  geom_boxplot(data=ac, aes(ac$TOTALCOMPLETEDDEPTH, group=ac$scenario, color=scenario), outlier.alpha = .1)+
  coord_flip(xlim=c(1000, 0))+
  theme_bw()

alp <- c(1, .5, .2, .1)
red3 <- "#9b2226"
red2 <- "#ae2012"
red1 <- "#bb3e03"
orange3 <- "#ca6702"
orange2 <- "#ee9b00"
orange1 <- "#e9d8a6"
blue1 <- "#94d2bd"
blue2 <- "#0a9396"
blue3 <- "#005f73"
black <- "#001219"
cols <- rev(c("#005f99", blue3, blue2, blue1))
ggplot()+
  geom_jitter(data=ac, aes(ac$TOTALCOMPLETEDDEPTH, ac$year, color=scenario), alpha=.8)+
  coord_flip(xlim=c(1000, 0))+
  scale_color_manual(values=cols)+
  theme_bw()
# now we intersect with census block groups?
cbg <- st_read("https://gis.water.ca.gov/arcgis/rest/services/Society/i16_Census_BlockGroup_DisadvantagedCommunities_2020/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
cbg <- st_read("Data/i16_Census_BlockGroup_DisadvantagedCommunities_2020/i16_Census_BlockGroup_DisadvantagedCommunities_2020.shp") %>% st_make_valid(.)
cv_cbg <- st_intersection(cbg, st_make_valid(gsps))

wells_bg <- list()
bg_dry <- list()
wide_bg <- list()
#fcv <- cv_cbg
for (i in c(1:length(activewells))){
  print(years[i])
  wells_bg[[i]] <- st_intersection(cv_cbg, activewells[[i]])
  bg_dry[[i]] <- wells_bg[[i]] %>% st_drop_geometry(.) %>% group_by(GEOID20,DAC20, dry) %>% summarise(WellCounts=length(unique(WCRNUMBER)), MHI=mean(MHI20))
  wide_bg[[i]] <- bg_dry[[i]] %>% spread(dry, WellCounts)
  wide_bg[[i]][is.na(wide_bg[[i]])] <- 0
  wide_bg[[i]]$perc_impacted <- (wide_bg[[i]]$TCDdry+wide_bg[[i]]$topdry)/(wide_bg[[i]]$TCDdry+wide_bg[[i]]$topdry+wide_bg[[i]]$active)
  wide_bg[[i]]$perc_fullydew <- (wide_bg[[i]]$TCDdry)/(wide_bg[[i]]$TCDdry+wide_bg[[i]]$topdry+wide_bg[[i]]$active)
  wide_bg[[i]]$perc_partidew <- (wide_bg[[i]]$topdry)/(wide_bg[[i]]$TCDdry+wide_bg[[i]]$topdry+wide_bg[[i]]$active)
  wide_bg[[i]]$YEAR <- years[i]
}

albg <- do.call(rbind, wide_bg)

albg_wide <- albg %>%
  tidyr::pivot_wider(
    names_from  = c(YEAR), # Can accommodate more variables, if needed.
    values_from = c(TCDdry, topdry, active, perc_impacted, perc_fullydew, perc_partidew)
  )

#all in one with block group, year, percent impacted to calculate percent change 
write_rds(albg_wide,"/Users/darcybostic/Downloads/drystats_bg_WIDE_2022cgwl_updated.rds")
write_rds(albg, "/Users/darcybostic/Downloads/drystats_bg_LONG_2022cgwl_updated.rds")

# by well 
bw <- list()
wide_bw <- list()
for (i in c(1:length(activewells))){
  print(years[i])
  bw[[i]] <- st_intersection(activewells[[i]], cv_cbg)
  bw[[i]]$YEAR <- years[i]
  bw[[i]]<-bw[[i]][,-c(42:51)]
}

long_bw <- do.call(rbind, bw)

wide_bw <- long_bw %>%
  tidyr::pivot_wider(.,
    names_from  = c(year), # Can accommodate more variables, if needed.
    values_from = c(GEOID20)
  )

#all in one with block group, year, percent impacted to calculate percent change 
write_rds(wide_bw,"/Users/darcybostic/Library/CloudStorage/GoogleDrive-djbostic1@gmail.com/.shortcut-targets-by-id/1zxS7SNp6bWpJ9u6mnyUMJiNkb3JenKPJ/Well retirement age and data reliability note/Code/WellAgeAnalysis/dryWells_wide_2022cgwl_updated.rds")
write_rds(long_bw, "/Users/darcybostic/Library/CloudStorage/GoogleDrive-djbostic1@gmail.com/.shortcut-targets-by-id/1zxS7SNp6bWpJ9u6mnyUMJiNkb3JenKPJ/Well retirement age and data reliability note/Code/WellAgeAnalysis/dryWells_long_2022cgwl_updated.rds")

bg_geom <- left_join(cbg, albg, by=GEOID20)
ggplot()+geom_sf(data=cbg)+theme_void()


 