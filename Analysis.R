# load packages
library(tidyverse) # general purpose data science toolkit
library(sp)        # spatial objects
library(raster)    # for raster objects
library(here)
library(sf)
library(lubridate)

options(timeout=1000)

# load data
# domestic wells - OSWCR
data <- read.csv("C://Users/dbostic/Downloads/wellcompletionreports_04172023download.csv")

#### descriptive stats on data ####
# filter out wells with no WCR ID
data1 <- data %>% dplyr::select(WCRNUMBER, PLANNEDUSEFORMERUSE, TOTALCOMPLETEDDEPTH, TOPOFPERFORATEDINTERVAL, BOTTOMOFPERFORATEDINTERVAL, DATEWORKENDED, Lat=DECIMALLATITUDE, Long=DECIMALLONGITUDE, GROUNDSURFACEELEVATION)

# filter to wells constructed after 1950
data1$year <- year(as.Date(data1$DATEWORKENDED, format = "%m/%d/%Y"))
data1 <- filter(data1, is.na(year)==F & year>=1950 & year <= 2022)

# filter to get only domestic and public supply wells
data1 <- data1 %>% filter((grepl("water supply",PLANNEDUSEFORMERUSE, ignore.case = T)|grepl("domestic",PLANNEDUSEFORMERUSE, ignore.case = T)) & (!grepl("destruction",PLANNEDUSEFORMERUSE, ignore.case = T)& !grepl("irrigation",PLANNEDUSEFORMERUSE, ignore.case = T)& !grepl("industrial",PLANNEDUSEFORMERUSE, ignore.case = T)& !grepl("stock",PLANNEDUSEFORMERUSE, ignore.case = T)))

#View(as.data.frame(table(data1$PLANNEDUSEFORMERUSE)))

# run analysis that shows how many wells have missing data, by decade - end result should output a table that includes the decade, number of wells, number of wells with TCD, number of wells with TOS/BOS,

# number of missing TCD
years <- c(1950, 1960, 1975, 1990, 2005)
table <- list()
for (j in c(1:5)){
  for (i in years){
  data2 <- data1 %>% filter(year >= i)

  haveTCD <- data2 %>% filter(is.na(TOTALCOMPLETEDDEPTH)==FALSE) %>% summarise(nwells_tcd = length(unique(WCRNUMBER)))
  haveTOP <- data2 %>% filter(is.na(TOPOFPERFORATEDINTERVAL)==FALSE) %>% summarise(nwells_top = length(unique(WCRNUMBER)))
  haveBOT <- data2 %>% filter(is.na(BOTTOMOFPERFORATEDINTERVAL)==FALSE) %>% summarise(nwells_bot = length(unique(WCRNUMBER)))
  haveALL <- data2 %>% filter(is.na(TOTALCOMPLETEDDEPTH)==FALSE & is.na(TOPOFPERFORATEDINTERVAL)==FALSE & is.na(BOTTOMOFPERFORATEDINTERVAL)==FALSE) %>% summarise(nwells_all = length(unique(WCRNUMBER)))

  combined50 <- cbind(haveTCD, haveTOP, haveBOT, haveALL)
  table[[j]] <- combined50
  }
}

combined60<- cbind(haveTCD, haveTOP, haveBOT, haveALL)
combined75<- cbind(haveTCD, haveTOP, haveBOT, haveALL)
combined90<- cbind(haveTCD, haveTOP, haveBOT, haveALL)
combined05<- cbind(haveTCD, haveTOP, haveBOT, haveALL)

combined <- rbind(combined50, combined60, combined75, combined90, combined05)

#percent of wells without a type

# once DryWellFunctions.R is run, come back to this code:

dw1960 <- 

dw1975 <- 

dw1990 <- 
  
dw2005 <- 
  
 