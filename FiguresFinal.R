# SCRIPT FOR FIGURES #
library(cowplot)
library(tidyverse)
library(lubridate)
#### LOAD ANALYSIS IMAGE ####
load("analysis_session_05212024.RData")

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

#### FIGURE 1 - LOAN DATA ####
loans <- readxl::read_xlsx("WellLoanData.xlsx", sheet="California")
loans$year_loan_closed <- year(as.Date(loans$`Year Loan Closed`, format="%Y-%m-%d"), origin = "1904-01-01")
totalloancost <- loans %>% group_by(`Loan Number`, City, County, State, `Date Loan Closed`) %>% summarise(yearloanclosed=mean(`Year Loan Closed`), totalprojectcost = sum(`Total Project Cost`), totalloancost = sum(`Loan Amount`), loanterm = mean(Term, na.rm=T))

totalloancost$proj_cost_thousands <- totalloancost$totalprojectcost/1000

totalloancost <- filter(totalloancost, yearloanclosed >2012)
avgcost <- totalloancost %>%
  group_by(yearloanclosed) %>%
  summarise(mean_score = mean(proj_cost_thousands)) 

ggplot()+
  geom_point(data=totalloancost, aes(x=yearloanclosed, y=proj_cost_thousands), color="#AEC1A1")+
  geom_line(data = avgcost, aes(x = (yearloanclosed), y = mean_score),color="#286309", lwd=1, alpha=.7)+  
  xlim(2012, 2023)+
  labs(x="Year Loan Closed", y="Total Project Cost (Thousands)")+
  theme_bw()+ 
  theme(axis.text=element_text(size=20, color = "black"),axis.title=element_text(size=20))
  
ggsave("Figure1_WellLoanData.eps", dpi=1200, width = 12, height = 10)

jhi <- totalloancost %>%
  group_by(yearloanclosed) %>%
  summarise(mean_score = mean(proj_cost_thousands)) %>%
  ggplot() +
  geom_line(data = avgcost, aes(x = (yearloanclosed), y = mean_score),color="#ADC4A1") +
  labs(x = "Year", y = "Mean Project Cost Per Year (Thousands)")+
  theme_bw()

#### FIGURE 2 - Year Well Completed by TCD ####
mt_raster <- readRDS("rasters/Copy of mt_raster.rds")
cgwl_raster <- readRDS("rasters/Copy of cgwl_raster.rds")

ac$scenario2 <- ifelse(ac$scenario == "All Time - 1900", "All Time: 1900-2022", ifelse(ac$scenario =="70 Years - 1952", "70 Years: 1952-2022", ifelse(ac$scenario == "45 Years - 1977", "45 Years: 1977-2022", ifelse(ac$scenario == "28 Years - 1994", "28 Years: 1994-2022", "ERROR"))))

alp <- c(1, .5, .2, .1)

cols <- rev(c("#005f99", blue3, blue2, blue1))

df <- as.data.frame(cgwl_raster) %>% drop_na() %>% add_column(scenario = "Current Groundwater Levels")
mtdf <- as.data.frame(mt_raster) %>% drop_na() %>% add_column(scenario = "Minimum Threshold Groundwater Levels")

p <- ggplot()+
  geom_boxplot(data=ac, aes(x=str_wrap(scenario2,9), y=TOTALCOMPLETEDDEPTH, color=scenario2), lwd=1.5)+
  geom_boxplot(data=df, aes(x=str_wrap(scenario, 9), y=layer), lwd=1.5, color=orange1)+
  geom_boxplot(data=mtdf, aes(x=str_wrap(scenario, 9), y=Prediction), lwd=1.5, color=orange2)+
  ylim(550,0)+
  scale_color_manual(values=cols)+
  #stat_summary(fun.data=mean_sdl, size=0.6, geom="pointrange", color="black")+
  #geom_smooth(method="lm", color="#286309")+
  #stat_summary(fun="mean", geom="line")+
  theme_bw()+
  ylab("Total Completed Depth (ft bgs) and Depth to Groundwater (ft bgs)")+
  xlab("Scenario")+
  #labs(color="Model Scenarios")
  #labs(caption = "Note: All wells constructed after the model scenario year are included in each respective scenario.\nFor example, the All Time scenario includes wells from 1900 to 2022.")
  theme(legend.position="none", axis.text=element_text(size=18, color = "black", angle=0), axis.title=element_text(size=20))
p

ggsave("Fig2_Boxplots.png", dpi=1200, width = 12, height = 10)


ggplot()+
  geom_tile(data=ac, aes(y=TOTALCOMPLETEDDEPTH, x=year), alpha=.9)+
  #coord_flip(ylim=c(1000, 0))+
  scale_color_manual(values=cols)+
  #stat_summary(fun.data=mean_sdl, size=0.6, geom="pointrange", color="black")+
  #geom_smooth(method="lm", color="#286309")+
  #stat_summary(fun="mean", geom="line")+
  theme_bw()+
  ylab("Year Well Completed")+
  xlab("Total Completed Depth (ft bgs)")+
  #labs(color="Model Scenarios")
  #labs(caption = "Note: All wells constructed after the model scenario year are included in each respective scenario.\nFor example, the All Time scenario includes wells from 1900 to 2022.")
  theme(legend.position="none", plot.caption = element_text(hjust = 0))


ggplot()+
  geom_boxplot(data=ac, aes(ac$TOTALCOMPLETEDDEPTH, ac$year, color=scenario), alpha=.9)+
  coord_flip(xlim=c(1000, 0))+
  scale_color_manual(values=cols)+
  #stat_summary(fun.data=mean_sdl, size=0.6, geom="pointrange", color="black")+
  geom_smooth(method="lm", color="#286309")+
  stat_summary(fun="mean", geom="line")+
  theme_bw()+
  ylab("Year Well Completed")+
  xlab("Total Completed Depth (ft bgs)")+
  labs(color="Model Scenarios")+
  labs(caption = "Note: All wells constructed after the model scenario year are included in each respective scenario.\nFor example, the All Time scenario includes wells from 1900 to 2022.")+
  theme(plot.caption = element_text(hjust = 0))

#### FIGURE 3 - Percent Full Dewatered by Block Group ####

#### FIGURE 4 - Difference Dewatered Wells % ####
bg_geom <- left_join(cbg, albg, by="GEOID20")
bg_geom <- st_intersection(bg_geom, cv_cbg)
ggplot()+geom_sf(data=bg_geom, lwd=.1, aes(fill=active))+theme_void()

# we want a plot that shows the CHANGE in number of dewatered wells between 28 years and 70 AND 28 years and 45
# I need a table of BGs with FullyDewatered(70-28), FullyDewatered(45-28)
# I also want to show where Fully Dewatered #s are the same

albg_nozero <- filter(albg_wide, TCDdry_1994 >0)

cleant <- albg_nozero %>% group_by(GEOID20, MHI) %>% 
  summarise(NumWells_1900=TCDdry_1900+topdry_1900+active_1900,
            NumWells_1952=TCDdry_1952+topdry_1952+active_1952,
            NumWells_1977=TCDdry_1977+topdry_1977+active_1977, 
            NumWells_1994=TCDdry_1994+topdry_1994+active_1994,
            TCDdry_1900,
            TCDdry_1952,
            TCDdry_1977,
            TCDdry_1994,
            FullyDe7028 = 100*(TCDdry_1952-TCDdry_1994)/TCDdry_1994,
            FullyDe4528 = 100*(TCDdry_1977-TCDdry_1994)/TCDdry_1994)

cleant$perc_categories_4528 <- ifelse(cleant$FullyDe4528 <= 50, "0 - 50%",
                                      ifelse((cleant$FullyDe4528 > 50 & cleant$FullyDe4528) <= 100, "51 - 100%",
                                             ifelse((cleant$FullyDe4528 > 100 & cleant$FullyDe4528) <= 2000, "101 - 2,000",
                                                    ifelse(cleant$FullyDe4528 > 2000, "2,001 - 8,000 %", "Other"))))

# lets try for relative change % (new-old)/old*100
cbg <- filter(cbg, GEOID20 %in% albg_wide$GEOID20)
cleanbg <- left_join(cbg, cleant, by="GEOID20")
cleanbg <- st_intersection(cleanbg, basins)

BGs <- Demographics_blockg %>% dplyr::select(GEOID20 = GEOID)
Data <- left_join(BGs, Data, by = c("GEOID20" = "GEOID20"))
Data <- filter(Data, is.na(perc_impacted_1900)==FALSE)
Data <- st_transform(Data, st_crs(basins))
plotdata <- st_intersection(Data, basins)

plotdata$cat.change.fully.1994.1900 <- ifelse(plotdata$change.fullydew.1994.1900 < -50, "-100 to -51%",
                                              ifelse(plotdata$change.fullydew.1994.1900 >= -50 & plotdata$change.fullydew.1994.1900 < -1, "-50 to -1%",
                                                     ifelse(plotdata$change.fullydew.1994.1900 == 0, "0",
                                                            ifelse(plotdata$change.fullydew.1994.1900 > 0 & plotdata$change.fullydew.1994.1900 <=50, "1 to 50%",
                                                                   ifelse(plotdata$change.fullydew.1994.1900 > 50, "51 to 100%", "NA")))))

plotdata$cat.change.fully.1994.1952 <- ifelse(plotdata$change.fullydew.1994.1952 < -50, "-100 to -51%",
                                              ifelse(plotdata$change.fullydew.1994.1952 >= -50 & plotdata$change.fullydew.1994.1952 < -1, "-50 to -1%",
                                                     ifelse(plotdata$change.fullydew.1994.1952 == 0, "0",
                                                            ifelse(plotdata$change.fullydew.1994.1952 > 0 & plotdata$change.fullydew.1994.1952 <=50, "1 to 50%",
                                                                   ifelse(plotdata$change.fullydew.1994.1952 > 50, "51 to 100%", "NA")))))

plotdata$cat.change.fully.1994.1952 <- ifelse(plotdata$change.fullydew.1994.1952 < -50, "-100 to -51%",
                                              ifelse(plotdata$change.fullydew.1994.1952 >= -50 & plotdata$change.fullydew.1994.1952 < -1, "-50 to -1%",
                                                     ifelse(plotdata$change.fullydew.1994.1952 == 0, "0",
                                                            ifelse(plotdata$change.fullydew.1994.1952 > 0 & plotdata$change.fullydew.1994.1952 <=50, "1 to 50%",
                                                                   ifelse(plotdata$change.fullydew.1994.1952 > 50, "51 to 100%", "NA")))))

plotdata$cat.change.fully.1994.1977 <- ifelse(plotdata$change.fullydew.1994.1977 < -50, "-100 to -51%",
                                              ifelse(plotdata$change.fullydew.1994.1977 >= -50 & plotdata$change.fullydew.1994.1977 < -1, "-50 to -1%",
                                                     ifelse(plotdata$change.fullydew.1994.1977 == 0, "0",
                                                            ifelse(plotdata$change.fullydew.1994.1977 > 0 & plotdata$change.fullydew.1994.1977 <=50, "1 to 50%",
                                                                   ifelse(plotdata$change.fullydew.1994.1977 > 50, "51 to 100%", "NA")))))

library(RColorBrewer)
myColors <- brewer.pal(6,"Spectral")
names(myColors) <- levels(plotdata$cat.change.fully.1994.1977)
#colScale <- 
pd <- filter(plotdata, !is.na(cat.change.fully.1994.1900) & cat.change.fully.1994.1900 != "NA")
f9400 <- ggplot()+
  geom_sf(data=basins, fill=NA, col="black")+
  geom_sf(data=filter(pd, is.na(cat.change.fully.1994.1900)==F), lwd=.01, aes(fill=cat.change.fully.1994.1900), col=NA)+
  scale_fill_manual(name = "Change Between 1994 and 1900",values = myColors)+
  ggtitle("Change Between 1994 and 1900")+
  theme_void()+ 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=16), title = element_text(size=20))

pd <- filter(plotdata, !is.na(cat.change.fully.1994.1952) & cat.change.fully.1994.1952 != "NA")
f9452 <- ggplot()+
  geom_sf(data=basins, fill=NA, col="black", lwd=.5)+
  geom_sf(data=filter(pd, is.na(cat.change.fully.1994.1952)==F), lwd=.01, aes(fill=cat.change.fully.1994.1952), col=NA)+
  scale_fill_manual(name = "Change Between 1994 and 1952",values = myColors)+
  ggtitle("Change Between 1994 and 1952")+
  theme_void()+ 
  theme(legend.text=element_text(size=14), legend.title=element_text(size=16), title = element_text(size=20))

pd <- filter(plotdata, !is.na(cat.change.fully.1994.1977) & cat.change.fully.1994.1977 != "NA")
f9477 <- ggplot()+
  geom_sf(data=basins, fill=NA, col="black", lwd=.5)+
  geom_sf(data=pd, lwd=.01, aes(fill=cat.change.fully.1994.1977))+
  scale_fill_manual(name = "Percent Difference\nBetween Scenarios",values = myColors, na.translate=F)+
  theme_void()+
  ggtitle("Change Between 1994 and 1977")+
  theme(legend.text=element_text(size=18), legend.title=element_text(size=18), title=element_text(size=20))

a <- plot_grid(f9400, f9452,f9477, labels = c("","",""), ncol=3)
a
ggsave("Figure3_1977.png", dpi=1200)

save.image("FinalFigRDataSession_12092024.RData")
