# SCRIPT FOR FIGURES #
library(cowplot)
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
loans <- readxl::read_xlsx("Data/WellLoanData.xlsx")
loans$year_loan_closed <- year(loans$`Date Loan Closed`)
loans$proj_cost_thousands <- loans$`Total Project Cost`/1000
ggplot(data=loans, aes(x=`Date Loan Closed`, y=proj_cost_thousands))+
  geom_point(color="#ADC4A1")+
  geom_smooth(method="lm", color="#286309")+
  stat_summary(fun="mean", geom="line")+
  labs(x="Date Loan Closed", y="Total Project Cost (Thousands)")+
  theme_bw()

hi <- loans %>%
    group_by(year_loan_closed) %>%
    summarise(mean_score = mean(proj_cost_thousands)) %>%
    ggplot(aes(x = (year_loan_closed), y = mean_score)) +
    geom_line() +
    labs(x = "Year", y = "Mean Project Cost")

#### FIGURE 2 - Year Well Completed by TCD ####
alp <- c(1, .5, .2, .1)

cols <- rev(c("#005f99", blue3, blue2, blue1))
p <- ggplot()+
  geom_jitter(data=ac, aes(ac$TOTALCOMPLETEDDEPTH, ac$year, color=scenario), alpha=.9)+
  coord_flip(xlim=c(1000, 0))+
  scale_color_manual(values=cols)+
  stat_summary(fun.data=mean_sdl, size=0.6, geom="pointrange", color="black")+
  theme_bw()+
  ylab("Year Well Completed")+
  xlab("Total Completed Depth (ft bgs)")+
  labs(color="Model Scenarios")+
  labs(caption = "Note: All wells constructed after the model scenario year are included in each respective scenario.\nFor example, the All Time scenario includes wells from 1900 to 2022.")+
  theme(plot.caption = element_text(hjust = 0))
p

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

BGs <- Demographics_blockg %>% select(GEOID20 = GEOID)
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
names(myColors) <- levels(plotdata$change.fullydew.1994.1900)
#colScale <- 

f9400 <- ggplot()+
  geom_sf(data=basins, fill=NA, col="black")+
  geom_sf(data=filter(plotdata, is.na(cat.change.fully.1994.1900)==F), lwd=.01, aes(fill=cat.change.fully.1994.1900), col=NA)+
  scale_fill_manual(name = "Change Between 1994 and 1900",values = myColors)+
  theme_void()


f9452 <- ggplot()+
  geom_sf(data=basins, fill=NA, col="black")+
  geom_sf(data=filter(plotdata, is.na(cat.change.fully.1994.1952)==F), lwd=.01, aes(fill=cat.change.fully.1994.1952), col=NA)+
  scale_fill_manual(name = "Change Between 1994 and 1952",values = myColors)+
  theme_void()

f9477 <- ggplot()+
  geom_sf(data=basins, fill=NA, col="black")+
  geom_sf(data=filter(plotdata, is.na(cat.change.fully.1994.1952)==F), lwd=.01, aes(fill=cat.change.fully.1994.1977), col=NA)+
  scale_fill_manual(name = "Change Between 1994 and 1977",values = myColors)+
  theme_void()

a <- plot_grid(f9400, f9452,f9477, labels = c("","",""), ncol=3)
#ggsave(a, "Figure3.png")
a
