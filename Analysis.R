# load packages
library(tidyverse) # general purpose data science toolkit
library(sp)        # spatial objects
library(raster)    # for raster objects
library(here)
library(sf)

options(timeout=1000)



# load data
# domestic wells - OSWCR
data <- read.table("https://data.cnra.ca.gov/dataset/647afc02-8954-426d-aabd-eff418d2652c/resource/8da7b93b-4e69-495d-9caa-335691a1896b/download/wellcompletionreports.csv", header = T, sep = '\t')


# once DryWellFunctions.R is run, come back to this code:

dw1960 <- 

dw1975 <- 

dw1990 <- 
  
dw2005 <- 
  
# run analysis that shows how many wells have missing data, by decade - end result should output a table that includes the decade, number of wells, number of wells with TCD, number of wells with TOS/BOS, 