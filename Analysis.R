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
cgwl_raster <- read_rds("Data/InterpolationGWLevels/cgwl_raster.rds")
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
data <- read.csv(here("Data", "wellcompletionreports_04172023download.csv"), na.strings=c("", "NA"))

#### descriptive stats on data ####
# filter out wells with no WCR ID
data1 <- data %>% dplyr::select(WCRNUMBER, PLANNEDUSEFORMERUSE, TOTALCOMPLETEDDEPTH, TOPOFPERFORATEDINTERVAL, BOTTOMOFPERFORATEDINTERVAL, DATEWORKENDED, Lat=DECIMALLATITUDE, Long=DECIMALLONGITUDE, GROUNDSURFACEELEVATION) %>% mutate(Lat = as.numeric(Lat), Long=as.numeric(Long))

tbl <- table(toupper(unlist(strsplit(as.character(data$PLANNEDUSEFORMERUSE), " "))))

# filter to wells constructed after 1950
data1$year <- year(as.Date(data1$DATEWORKENDED, format = "%m/%d/%Y"))
data1 <- filter(data1, is.na(year)==F & year>=1952 & year <= 2022)

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
years <- c(1952, 1962, 1972, 1989, 1994, 2000)
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
  activewells[[i]] <- wellanalysis(setslist[[i]])
}

 