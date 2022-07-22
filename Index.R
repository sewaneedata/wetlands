library(tidyverse) 
library(gsheet)
library(lubridate)
library(data.table)
library(readxl)
library(ggpubr)
library(broom)
library(AICcmodavg)
library(readr)
library(ggplot2)

url<-"https://docs.google.com/spreadsheets/d/14nn7NWMBatbzcz9nqcTFzQghmzMUE2o0/edit?usp=sharing&ouid=104854259661631892531&rtpof=true&sd=true"
sud <-gsheet2tbl(url)
url2 <- 'https://docs.google.com/spreadsheets/d/1WdrZuZP9J6Im4KQdo6MudPQC4Fwf13rD/edit?usp=sharing&ouid=104854259661631892531&rtpof=true&sd=true'
sond<-gsheet2tbl(url2)
url3 <- "https://docs.google.com/spreadsheets/d/1eRF3RnsWTt6ObsY6hlgi6jOjvliS1Gn5/edit#gid=200414766"
lagoonC<-gsheet2tbl(url3)
lagoonC

######## CLEAN THE DATA #######################

names(sud)
sond<-sond%>%
  rename(Date = `Date (MM/DD/YYYY)`)

sud<-sud%>%
  rename(Date = `Date Values Reflect`)

sud<-sud %>%
  filter(Date != "MISSING DATA" | Date != "NO POWER - AERATOR INSTALL")

library(lubridate)
sud2 <- sud
sud2$Date <- mdy(sud$Date)

lagoonC<-lagoonC%>%
  rename(Date = `Date (MM/DD/YYYY)`)

lagoonC<-lagoonC %>%
  filter(Date != "MISSING DATA - POWER ISSUE" | Date != "SONDES AT WATSON RESEARCH SITE" | Date != "DAMAGED COND/TEMP SENSOR CAUSED FAULT THAT HAULTED LOGGING")

############FIX THE DATES #####################

sond$Date <- mdy(sond$Date)

sud<-sud2%>%
  filter(Date >= "2020-10-19")

lagoonC$Date <- mdy(lagoonC$Date)

lagoonC<-lagoonC%>%
  filter(year(as_date(Date)) != 2022)

############ MELT THE DATA ######################

lagoonC2<-melt(data = as.data.table(lagoonC),
               id.vars= 1:4,
               measure.vars= 5:ncol(lagoonC))

sond2<-melt(data = as.data.table(sond),
            id.vars= 1:4,
            measure.vars= 5:ncol(sond))

############# ADD THE MONTH AND YEAR COLUMNS ###############

sud<-sud2%>%
  mutate(month = month(Date))%>%
  mutate(month = month.name[month])%>%
  mutate(year = year(Date))

sond2<-sond2%>%
  mutate(month = month(Date))%>%
  mutate(month = month.name[month])%>%
  mutate(year = year(Date))

lagoonC2<-lagoonC2%>%
  mutate(month = month(Date))%>%
  mutate(month = month.name[month])%>%
  mutate(year = year(Date))

########## combine the data sets ##################
all_data<-rbind(lagoonC2, sond2)

###################################################
#######################################################
################### Anova Test ########################

# pH anova test

anv_ph <- all_data %>%
  filter(  variable == "pH" ) %>%
  filter(year == 2021) %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

anv_ph <- aov( value ~ Site + month, data = anv_ph)
summary(anv_ph)
TukeyHSD(anv_ph)

# turbidity anova test  

anv_turb <- all_data %>%
  filter(year == 2021) %>% 
  filter(  variable == 'Turbidity NTU' ) %>%
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

anv_turb <- aov( value ~ Site + month, data = anv_turb )
summary(anv_turb)

# conductivity anova test

anv_cond <- all_data %>%
  filter( variable == 'Cond µS/cm') %>%
  filter(value != 'SENSOR FAILURE') %>% 
  filter(year == 2021) %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit

anv_cond <- aov( value ~ Site + month, data = anv_cond)
summary(anv_cond)

# ODO anova test

anv_odo <- all_data %>%
  filter( variable == 'ODO mg/L') %>%
  filter(year == 2021) %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

anv_odo <- aov(value ~ Site + month, data = anv_odo)
summary(anv_odo)

# ORP mv anova test

anv_orp <- all_data %>%
  filter( variable == 'ORP mV') %>%
  filter(year == 2021) %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

anv_orp <- aov(value ~ Site + month, data = anv_orp)  
summary(anv_orp)  

# SpCond µS/cm anova test

anv_spcond <- all_data %>%
  filter( variable == 'ORP mV') %>%
  filter(year == 2021) %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

anv_spcond <- aov(value ~ Site + month, data = anv_spcond)
summary(anv_spcond)

# NitraLED mg/L anova test

anv_nitra <- all_data %>%
  filter( variable == 'ORP mV') %>%
  filter(year == 2021) %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

anv_nitra <- aov(value ~ Site + month, data = anv_nitra)
summary(anv_nitra)

# Water temp anova test 

anv_temp_c <- all_data %>%
  filter( variable == 'Temp °C') %>%
  filter(year == 2021) %>% 
  filter(value != 'SENSOR FAILURE') %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

anv_temp_c <- aov(value ~ Site + month, data = anv_temp_c)
summary(anv_temp_c)

# NH4+ -N mg/L Anova Test

anv_nh4 <- all_data %>%
  filter( variable == 'NH4+ -N mg/L') %>%
  filter(year == 2021) %>% 
  filter(value != 'SENSOR FAILURE') %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

anv_nh4 <- aov(value ~ month, data = anv_nh4)
summary(anv_nh4)

# NH3 mg/L anova test

anv_nh3 <- all_data %>%
  filter( variable == 'NH3 mg/L') %>%
  filter(year == 2021) %>% 
  filter(value != 'SENSOR FAILURE') %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

anv_nh3 <- aov(value ~ month, data =anv_nh3)
summary(anv_nh3)


