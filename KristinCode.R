#Block group analysis
  
  #Load data and reduce dataset. NAs indicate there is no wells for that Block Group for that scenario
library(tidyverse)
Data <- readRDS("/Users/darcybostic/Downloads/new_BG_results_05072024.rds")
Data <- Data[ ,c(1,16,17,18,19,20,21,22,23)]


#make DVs for change in percent impacted and change in percent fully dewatered for 50, 70 and all year scenarios comnpared to 28. This represents change in percent of wells impacted rather than percent change. This is an important difference to be careful about in writing up results. 
Data$change.impacted.1994.1900 <- (Data$perc_impacted_1900- Data$perc_impacted_1994)*100
summary(Data$change.impacted.1994.1900)

Data$change.impacted.1994.1952 <- (Data$perc_impacted_1952- Data$perc_impacted_1994)*100
summary(Data$change.impacted.1994.1952)

Data$change.impacted.1994.1977 <- (Data$perc_impacted_1977- Data$perc_impacted_1994)*100
summary(Data$change.impacted.1994.1977)

Data$change.fullydew.1994.1900 <- (Data$perc_fullydew_1900 - Data$perc_fullydew_1994)*100
summary(Data$change.fullydew.1994.1900)

Data$change.fullydew.1994.1952 <- (Data$perc_fullydew_1952 - Data$perc_fullydew_1994)*100
summary(Data$change.fullydew.1994.1952)

Data$change.fullydew.1994.1977 <- (Data$perc_fullydew_1977 - Data$perc_fullydew_1994)*100
summary(Data$change.fullydew.1994.1977)


#add census data, using 2022 acs 5-year estimates
library(tidycensus)

#get census data
Demographics_blockg <- get_acs(geography = "block group", variables = c("B19013_001E", "B03002_001E", "B03002_003E", "B03002_004E", "B03002_005E", "B03002_006E", "B03002_007E", "B03002_012E", "B19013_001E", "B25003_001E", "B25003_002E", "B25003_003E", "B11001_001E"), state = "CA", year = 2022, output = "wide", survey = "acs5", geometry = TRUE)

#rename variables
Demographics_blockg <- Demographics_blockg %>% rename(Median.hh.income = B19013_001E, Race.estimate.total = B03002_001E, Race.white.alone = B03002_003E, Race.black.alone = B03002_004E, Race.native.alone = B03002_005E, Race.asian.alone = B03002_006E, Race.PI.alone = B03002_007E, Race.hispanicorlatino = B03002_012E, Tenure.estimate.total = B25003_001E, Tenure.owner = B25003_002E, Tenure.renter = B25003_003E, Households.total = B11001_001E)

#Get rid of MOE columns
Demographics_blockg <- Demographics_blockg[,-c(4,6,8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 27)]

#make percentages for block group SES variables
Demographics_blockg$pct.whitealone <- (Demographics_blockg$Race.white.alone)/(Demographics_blockg$Race.estimate.total)

Demographics_blockg$pct.hispanicorlatino <- (Demographics_blockg$Race.hispanicorlatino)/(Demographics_blockg$Race.estimate.total)

Demographics_blockg$pct.blackalone <- (Demographics_blockg$Race.black.alone)/(Demographics_blockg$Race.estimate.total)

Demographics_blockg$pct.nativealone <- (Demographics_blockg$Race.native.alone)/(Demographics_blockg$Race.estimate.total)

Demographics_blockg$pct.asianalone <- (Demographics_blockg$Race.asian.alone)/ (Demographics_blockg$Race.estimate.total)

Demographics_blockg$pct.PIalone <- (Demographics_blockg$Race.PI.alone)/(Demographics_blockg$Race.estimate.total)

Demographics_blockg$pct.owner <- (Demographics_blockg$Tenure.owner)/(Demographics_blockg$Tenure.estimate.total)

Demographics_blockg$pct.renter <- (Demographics_blockg$Tenure.renter)/(Demographics_blockg$Tenure.estimate.total)

#create education variable (requires aggregating multiple census variables)
##Get percent with HS diploma over age 25
ca_ed <- get_acs(
  geography = "block group",
  variables = paste0("B15003_0", 17:25),  # hs diploma and above variables
  summary_var = "B15003_001",  # pop 25 years and older - denominator
  year = 2022,
  state = "CA"
)

ed_sum <- ca_ed %>% 
  group_by(GEOID, NAME) %>% 
  summarize(
    n_hs_above = sum(estimate),
    n_hs_above_moe = moe_sum(moe, estimate),
    n_pop_over_25 = summary_est[1],
    n_pop_over_25_moe = summary_moe[1]
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_hs_above = n_hs_above / n_pop_over_25,
    pct_hs_above_moe = moe_prop(n_pop_over_25, n_pop_over_25,
                                n_hs_above_moe, n_pop_over_25_moe)
  )

#calculate percent without HS diploma over 25
ed_sum$pct_no_hs <- ((1) - (ed_sum$pct_hs_above))

#Join all data together
Data <- left_join(Data, Demographics_blockg, by = c("GEOID20" = "GEOID"))
ed_sum <- ed_sum[,-c(2:6,8)]
Data <- left_join(Data, ed_sum, by = c("GEOID20" = "GEOID"))

#exploratory T tests

#What types of groupings are most intuitive and meaningful?

#Try BGs with > 75% pop white versus < 25%
Greaterthan75POC <- Data %>% filter(pct.whitealone<.25)
Lessthan75POC <- Data %>% filter(pct.whitealone>.75)
t.test(Greaterthan75POC$change.fullydew.1994.1952, Lessthan75POC$change.fullydew.1994.1952)

#Try >50% pop white versus <50% 
Greaterthan50POC <- Data %>% filter(pct.whitealone<.5)
Lessthan50POC <-  Data %>% filter(pct.whitealone>.5)
t.test(Greaterthan50POC$change.fullydew.1994.1952, Lessthan50POC$change.fullydew.1994.1952)


#How does this compare to using the quartiles/medians for the variable which would give bigger samples to compare
summary(Data$pct.whitealone) #1st quartile is 0.2029, median is 0.3760 and 3rd quartile is 0.5729
TopQPOC <- Data %>% filter(pct.whitealone<.2029)
BottomQPOC <- Data %>% filter(pct.whitealone>.5729)
t.test(TopQPOC$change.fullydew.1994.1952, BottomQPOC$change.fullydew.1994.1952)

TopHalfPOC <- Data %>% filter(pct.whitealone<.3760)
BottomHalfPOC <-  Data %>% filter(pct.whitealone>.3760)
t.test(TopHalfPOC$change.fullydew.1994.1952, BottomHalfPOC$change.fullydew.1994.1952) # They are all pretty similar/consistent which is good

# Is there a difference with the 45 year assumption?
t.test(TopQPOC$change.fullydew.1994.1977, BottomQPOC$change.fullydew.1994.1977) #Impacts smaller but still significantly more impacts among POC dominated BGs

t.test(TopHalfPOC$change.fullydew.1994.1977, BottomHalfPOC$change.fullydew.1994.1977) #Same


##RESULTS FOR PAPER WRITE UP

#Paragraph one of results
#Understanding the degree of impacts for each scenario (Paragraph 1 of results). NOTE THAT THE TOTAL NUMBER OF BGS FOR EACH SCENARIO DIFFERS SO PERCENT OF BGS NEEDS TO BE CALCULATED WITH UNIQUE DENOMINATOR
summary(Data$perc_fullydew_1900)
summary(Data$perc_fullydew_1952)
summary(Data$perc_fullydew_1977)


#How many BGs have more than 50% wells dry?
sum(na.omit(Data$perc_fullydew_1994 >0.5))
sum(na.omit(Data$perc_fullydew_1977 >0.5))
sum(na.omit(Data$perc_fullydew_1952 >0.5))
sum(na.omit(Data$perc_fullydew_1900 >0.5))

#How many have any well failure predicted?
sum(na.omit(Data$perc_fullydew_1994 >0))
sum(na.omit(Data$perc_fullydew_1977 >0))
sum(na.omit(Data$perc_fullydew_1952 >0))
sum(na.omit(Data$perc_fullydew_1900 >0))

#How many see no well impacts?
sum(na.omit(Data$perc_fullydew_1994==0))
sum(na.omit(Data$perc_fullydew_1977==0))
sum(na.omit(Data$perc_fullydew_1952==0))
sum(na.omit(Data$perc_fullydew_1900==0))

#Hww many see more than 10% well impacts?
sum(na.omit(Data$perc_fullydew_1994 >0.10))
sum(na.omit(Data$perc_fullydew_1977 >0.10))
sum(na.omit(Data$perc_fullydew_1952 >0.10))
sum(na.omit(Data$perc_fullydew_1900 >0.10))

#Paragraph 2 of results
#change in estimated wells dewatered. NOTE that total number of BGs in analysis is now 944 for all scenarios since we loose BGs without wells in 1994 when we move towards calculating change from the other scenarios to that one (which has the smallest number of wells analyzed)
summary(Data$change.fullydew.1994.1952)
summary(Data$change.fullydew.1994.1977)
summary(Data$change.fullydew.1994.1900)

#How often does the extended retirement age make things look better versus worse?

#Better - number of dry wells in 1994 scenario is larger than the older scenarios
sum(na.omit(Data$change.fullydew.1994.1952 <0))
sum(na.omit(Data$change.fullydew.1994.1977 <0))
sum(na.omit(Data$change.fullydew.1994.1900 <0))

#Worse - Number of dry wells in 1994 is less than older scenarios
sum(na.omit(Data$change.fullydew.1994.1952 >0))
sum(na.omit(Data$change.fullydew.1994.1977 >0))
sum(na.omit(Data$change.fullydew.1994.1900 >0))

#No change in well impacts
sum(na.omit(Data$change.fullydew.1994.1952 ==0))
sum(na.omit(Data$change.fullydew.1994.1977 ==0))
sum(na.omit(Data$change.fullydew.1994.1900 ==0))


#Mean demographics for three groups of block groups (those with positive change,negative change and no change)
Data$Group45 <- NA
Data$Group45 <- ifelse(Data$change.fullydew.1994.1977>0, "Increased impacts", Data$Group45)
Data$Group45 <- ifelse(Data$change.fullydew.1994.1977==0, "No change", Data$Group45)
Data$Group45 <- ifelse(Data$change.fullydew.1994.1977<0, "Decreased impacts", Data$Group45)
Data$Group45 <- as.factor(Data$Group45)

Data$Group70 <- NA
Data$Group70 <- ifelse(Data$change.fullydew.1994.1952>0, "Increased impacts", Data$Group70)
Data$Group70 <- ifelse(Data$change.fullydew.1994.1952==0, "No change", Data$Group70)
Data$Group70 <- ifelse(Data$change.fullydew.1994.1952<0, "Decreased impacts", Data$Group70)
Data$Group70 <- as.factor(Data$Group70)

Data$GroupALL <- NA
Data$GroupALL <- ifelse(Data$change.fullydew.1994.1900>0, "Increased impacts", Data$GroupALL)
Data$GroupALL <- ifelse(Data$change.fullydew.1994.1900==0, "No change", Data$GroupALL)
Data$GroupALL <- ifelse(Data$change.fullydew.1994.1900<0, "Decreased impacts", Data$GroupALL)
Data$GroupALL <- as.factor(Data$GroupALL)

#Data_small <- Data[,c(13,26:31,33,35:37)]
Data_small <- Data[,c(17,30:35,37,39:42)]

Dem70 <- Data_small %>%
  group_by(Group70) %>%
  summarise(across(
    .cols = where(is.numeric), 
    .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

Dem45 <- Data_small %>%
  group_by(Group45) %>%
  summarise(across(
    .cols = where(is.numeric), 
    .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

DemALL <- Data_small %>%
  group_by(GroupALL) %>%
  summarise(across(
    .cols = where(is.numeric), 
    .fns = list(Mean = mean, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))
#Interestingly just taking the averages for the three groups the demographics looks about the same. Magnitude of impacts must be important to account for rather than this overly coarse look at things

#Paragraph three of results

#Comparing Q1 and Q4
#Percent white alone
summary(Data$pct.whitealone)
Q1white <- Data %>% filter(pct.whitealone<=.2029)
Q4white <- Data %>% filter(pct.whitealone>.5729)

t.test(Q1white$change.fullydew.1994.1952, Q4white$change.fullydew.1994.1952) 

Data$Qwhite <- "Q4"
Data$Qwhite <- ifelse(Data$pct.whitealone<=.5729, "Q3", Data$Qwhite)
Data$Qwhite <- ifelse(Data$pct.whitealone<=.3760, "Q2", Data$Qwhite)
Data$Qwhite <- ifelse(Data$pct.whitealone<=.2029, "Q1", Data$Qwhite)

Data$Qwhite <- as.factor(Data$Qwhite)
summary(Data$Qwhite)

Bplot_Whitedata <- Data %>% filter(Qwhite == "Q1" | Qwhite == "Q4")
Bplot_Whitedata$Qwhite <- droplevels(Bplot_Whitedata$Qwhite)
Bplot_White <- ggplot(Bplot_Whitedata, aes(x=Qwhite, y = change.fullydew.1994.1952, group=Qwhite)) +
  geom_boxplot() + labs(x= "Percent population white non-hispanic", y = "Change in percent of dewatered wells")


#median household income
summary(Data$Median.hh.income)
Q1MHI <- Data %>% filter(Median.hh.income<=58256)
Q4MHI <- Data %>% filter(Median.hh.income>106544)

t.test(Q1MHI$change.fullydew.1994.1952, Q4MHI$change.fullydew.1994.1952) 

Data$QMHI <- "Q4"
Data$QMHI <- ifelse(Data$Median.hh.income<=106544, "Q3", Data$QMHI)
Data$QMHI <- ifelse(Data$Median.hh.income<=7904, "Q2", Data$QMHI)
Data$QMHI <- ifelse(Data$Median.hh.income<=58256, "Q1", Data$QMHI)

Data$QMHI <- as.factor(Data$QMHI)
summary(Data$QMHI)

Bplot_MHIdata <- Data %>% filter(QMHI == "Q1" | QMHI == "Q4")
Bplot_MHIdata$QMHI <- droplevels(Bplot_MHIdata$QMHI)
Bplot_MHI <- ggplot(Bplot_MHIdata, aes(x=QMHI, y = change.fullydew.1994.1952, group=QMHI)) +
  geom_boxplot() + labs(x= "Median Household Income (MHI)", y = "Change in percent of dewatered wells")

#homeownership
summary(Data$pct.owner)
Q1own <- Data %>% filter(pct.owner<=0.4822)
Q4own <- Data %>% filter(pct.owner>0.8074)

t.test(Q1own$change.fullydew.1994.1952, Q4own$change.fullydew.1994.1952) 

Data$Qown <- "Q4"
Data$Qown <- ifelse(Data$pct.owner<=0.8074, "Q3", Data$Qown)
Data$Qown <- ifelse(Data$pct.owner<=0.6631, "Q2", Data$Qown)
Data$Qown <- ifelse(Data$pct.owner<=0.4822, "Q1", Data$Qown)

Data$Qown <- as.factor(Data$Qown)
summary(Data$Qown)

Bplot_Owndata <- Data %>% filter(Qown == "Q1" | Qown == "Q4")
Bplot_Owndata$Qown <- droplevels(Bplot_Owndata$Qown)
Bplot_Own <- ggplot(Bplot_Owndata, aes(x=Qown, y = change.fullydew.1994.1952, group=Qown)) +
  geom_boxplot() + labs(x= "Percent of households that are owner-occupied", y = "Change in percent of dewatered wells")

#high school educaiton
summary(Data$pct_hs_above)
Q1ed <- Data %>% filter(pct_hs_above<=0.6942)
Q4ed <- Data %>% filter(pct_hs_above>0.9203)

t.test(Q1ed$change.fullydew.1994.1952, Q4ed$change.fullydew.1994.1952) 

Data$Qed <- "Q4"
Data$Qed <- ifelse(Data$pct_hs_above<=0.9203, "Q3", Data$Qed)
Data$Qed <- ifelse(Data$pct_hs_above<=0.8283, "Q2", Data$Qed)
Data$Qed <- ifelse(Data$pct_hs_above<=0.6942, "Q1", Data$Qed)

Data$Qed <- as.factor(Data$Qed)
summary(Data$Qed)

Bplot_Eddata <- Data %>% filter(Qed == "Q1" | Qed == "Q4")
Bplot_Eddata$Qed <- droplevels(Bplot_Eddata$Qed)
Bplot_Ed <- ggplot(Bplot_Eddata, aes(x=Qed, y = change.fullydew.1994.1952, group=Qed)) +
  geom_boxplot() + labs(x= "Percent of population ≥ 25 years with HS diploma", y = "Change in percent of dewatered wells")


#Final paragraph
#Q1s or Q4s across all four variables
Q1all4 <- Data %>% filter(Qwhite == "Q1" & QMHI == "Q1" & Qown == "Q1" & Qed == "Q1")
Q4all4 <- Data %>% filter(Qwhite == "Q4" & QMHI == "Q4" & Qown == "Q4" & Qed == "Q4")

t.test(Q1all4$change.fullydew.1994.1952, Q4all4$change.fullydew.1994.1952)

summary(Q1all4$perc_fullydew_1994)
summary(Q1all4$perc_fullydew_1952)

summary(Q4all4$perc_fullydew_1994)
summary(Q4all4$perc_fullydew_1952)

Bplot_Owndata <- Data %>% filter(Qown == "Q1" | Qown == "Q4")
Bplot_Owndata$Qown <- droplevels(Bplot_Owndata$Qown)
Bplot_Own <- ggplot(Bplot_Owndata, aes(x=Qown, y = change.fullydew.1994.1952, group=Qown)) +
  geom_boxplot() + labs(x= "Percent of households that are owner-occupied", y = "Change in percent of dewatered wells")

my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")[c(1, 3, 7, 2)]
theme_flip <-
  theme(
    axis.text.x = element_text(face = "plain", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_line(color = "grey90", size = .6),
    panel.grid.major.y = element_blank(),
    legend.position = "top", 
    legend.text = element_text( size = 18),
    legend.title = element_text(face = "bold", size = 18, margin = margin(b = 25))
  )

g_white <- 
  Data %>%
  drop_na(Qwhite) %>%
  filter(., Qwhite == "Q1" | Qwhite == "Q4")%>%
  ggplot(., aes(change.fullydew.1994.1952, fct_rev(Qwhite), color = Qwhite, fill = Qwhite)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07)) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none") +
  labs(x="Change in Percent of Fully Dewatered Wells", y="Percent of White People")+
  xlim(-25, 30)+
  theme_bw()+
  ggridges::geom_density_ridges(na.rm=T,
    alpha = .7
  )


g_ed <- 
  Data %>%
  filter(., Qed == "Q1" | Qed == "Q4")%>%
  ggplot(., aes(change.fullydew.1994.1952, fct_rev(Qed), color = Qed, fill = Qed)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07)) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none") +
  labs(x="Change in Percent of Fully Dewatered Wells", y="Percent of People with HS Diploma")+
  xlim(-25, 30)+
  theme_bw()+
  ggridges::geom_density_ridges(na.rm=T,
                                alpha = .7
  )

g_mhi <- 
  Data %>%
  drop_na(QMHI) %>%
  filter(., QMHI == "Q1" | QMHI == "Q4")%>%
  ggplot(., aes(change.fullydew.1994.1952, fct_rev(QMHI), color = QMHI, fill = QMHI)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07)) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none") +
  labs(x="Change in Percent of Fully Dewatered Wells", y="Percent of People with Income Above MHI")+
  xlim(-25, 30)+
  theme_bw()+
  ggridges::geom_density_ridges(na.rm=T,
                                alpha = .7
  )

g_own <- 
  Data %>%
  drop_na(Qown) %>%
  filter(., Qown == "Q1" | Qown == "Q4")%>%
  ggplot(., aes(change.fullydew.1994.1952, fct_rev(Qown), color = Qown, fill = Qown)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07)) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none") +
  labs(x="Change in Percent of Fully Dewatered Wells", y="Percent of People who Own Their Home")+
  xlim(-25, 30)+
  theme_bw()+
  ggridges::geom_density_ridges(na.rm=T,
                                alpha = .7
  )
a <- plot_grid(g_own, g_mhi,g_white,g_ed, labels = c("","","", ""))
