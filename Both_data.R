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

avg_boxplot <- all_data %>%
  filter(year == year)%>%
  filter(year == 2021) %>% 
  filter(variable == 'Turbidity NTU') %>% 
  mutate(month= factor(month,
                        levels = c('January', 'February', 
                                   'March', 
                                   'April', 
                                   'May', 'June', 
                                   'July', 'August', 'September', 
                                   'October', 'November', 'December'))) %>% 
  na.omit()

# boxplot of turbidity
#substr(month,1,1),
ggplot(data = avg_boxplot, aes(x = month, y = as.numeric(value)))+
  geom_boxplot()+
  labs(title = 'Variance of Turbidity (2021)',
       subtitle = 'Turbidity (NTU) in Lagoon C and Basin 3', 
       y = 'Turbidity NTU', 
       x = 'Months')+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~`Site Name`)

# code for for predictive model for turbidity
avg_turb <- avg_boxplot %>% 
  filter(year == 2021) %>% 
  group_by(month) %>% 
filter(variable =='Turbidity NTU') %>% 
  #filter(`Site Name` == 'Wetland Basin 3') %>% 
  summarise(avgturb = mean(as.numeric(value)))

# predictive model for turbidity plot
ggplot(data = avg_boxplot, aes( x= (month), y = as.numeric(value)))+
  geom_point()+
   theme(axis.text.x = element_text(angle = 90))+
  labs(title = 'Predicted Turbidity Using 2021',
       subtitle = 'Turbidity (NTU) Predicted per Month',
       y = 'Turbidity NTU',
       x = 'Months')+
  geom_point(data = avg_turb, aes(x = month, y = avgturb), 
             size = 2, color = 'red')+
  geom_line(data = avg_turb, aes(x = month, y = avgturb), 
            size = .5, color = 'blue', group =1)
  #ylim(0,125)

#######################################################
################### Anova Test ########################

# pH anova test
ph_df <- all_data %>%
  filter(  variable == "pH" ) %>%
  filter(year == 2021) %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

pH_df <- aov( value ~ Site + month, data = ph_df )
  summary(pH_df)
TukeyHSD(pH_df)

# turbidity anova test  
turb_df <- all_data %>%
  filter(year == 2021) %>% 
  filter(  variable == 'Turbidity NTU' ) %>%
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

turb_df <- aov( value ~ Site + month, data = turb_df )
summary(turb_df)

# conductivity anova test
cond_df <- all_data %>%
  filter( variable == 'Cond µS/cm') %>%
  filter(value != 'SENSOR FAILURE') %>% 
  filter(year == 2021) %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit

cond_df <- aov( value ~ Site + month, data = cond_df)
summary(cond_df)

# ODO anova test
odo_df <- all_data %>%
filter( variable == 'ODO mg/L') %>%
  filter(year == 2021) %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

odo_df <- aov(value ~ Site + month, data = odo_df)
summary(odo_df)

# ORP mv anova test
orp_df <- all_data %>%
  filter( variable == 'ORP mV') %>%
  filter(year == 2021) %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

orp_df <- aov(value ~ Site + month, data = orp_df)  
summary(orp_df)  

# SpCond µS/cm anova test
spcond_df <- all_data %>%
  filter( variable == 'ORP mV') %>%
  filter(year == 2021) %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

spcond_df <- aov(value ~ Site + month, data = spcond_df)
summary(spcond_df)

# NitraLED mg/L anova test
nitra_df <- all_data %>%
  filter( variable == 'ORP mV') %>%
  filter(year == 2021) %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

nitra_df <- aov(value ~ Site + month, data = nitra_df)
summary(nitra_df)

# Water temp anova test 
temp_c_df <- all_data %>%
  filter( variable == 'Temp °C') %>%
  filter(year == 2021) %>% 
  filter(value != 'SENSOR FAILURE') %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()
temp_c_df <- aov(value ~ Site + month, data = temp_c_df)
summary(temp_c_df)

# NH4+ -N mg/L Anova Test
nh4_df <- all_data %>%
  filter( variable == 'NH4+ -N mg/L') %>%
  filter(year == 2021) %>% 
  filter(value != 'SENSOR FAILURE') %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()

nh4_df <- aov(value ~ month, data = nh4_df)
summary(nh4_df)

# NH3 mg/L anova test
nh3_df <- all_data %>%
  filter( variable == 'NH3 mg/L') %>%
  filter(year == 2021) %>% 
  filter(value != 'SENSOR FAILURE') %>% 
  rename( "Site" = `Site Name`) %>%
  mutate( Site = factor(Site)) %>% 
  na.omit()
nh3_df <- aov(value ~ month, data = nh3_df)
summary(nh3_df)

###################################################3
url5 <- "https://docs.google.com/spreadsheets/d/14nn7NWMBatbzcz9nqcTFzQghmzMUE2o0/edit#gid=571749034"
sud_hourly<-gsheet2tbl(url5)

# number of missing data
errors <- all_data %>% 
  filter(value == 'SENSOR FAILURE') %>% 
  filter(year == 2021) %>% 
  group_by(variable, month) %>% 
tally()
# plot of missing data
ggplot(data = errors, aes( x = variable, y = n))+
  geom_col()+
  facet_wrap(~month)
  
#####################################################
url5 <- "https://docs.google.com/spreadsheets/d/14nn7NWMBatbzcz9nqcTFzQghmzMUE2o0/edit#gid=571749034"
sud_hourly<-gsheet2tbl(url5)

# select what ya need
sudhour<- sud_hourly %>% 
  select(Timestamp, `VPD Avg (Kpa)`, `Rain (mm)`, `Solar Total (MJ/m²)`)

# make a year column
sudhour2 <- sudhour %>% 
  mutate(yyyy = year(mdy_hm(Timestamp))) 
bads <- which(is.na(sudhour2$yyyy))

sudhour2$yyyy[bads]<-year(ymd_hms(sudhour2$Timestamp[bads]))        
sudhour2[bads,]

# month column
sudhour2 <- sudhour2 %>% 
mutate(mm = month(mdy_hm(Timestamp)))
bads2 <- which(is.na(sudhour2$mm))
sudhour2$mm[bads2]<-month(ymd_hms(sudhour2$Timestamp[bads2]))
sudhour2[bads2,]

# sud VPD avg
vpd_avg<- sudhour2 %>% 
  mutate(Month = factor(mm,
  levels = c('Jan', 'Feb', 
              'Mar', 
             'Apr', 
           'May', 'Jun', 
          'Jul', 'Aug', 'Sep', 
     'Oct', 'Nov', 'Dec'))) %>% 
  filter(yyyy == 2021) %>% 
  group_by(mm) %>% 
summarise(vpdavg = mean(`VPD Avg (Kpa)`)) 
# plot of average VPD
ggplot(data = vpd_avg, aes( x = mm, y = vpdavg))+
  geom_col(fill = 'aquamarine3')+
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.name)+
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,.8)+
  labs(title = "VPD Average per Month (2021)",
       subtitle = 'At Sewanee Utility District',
       y = 'Average VPD (Kpa)',
       x = 'Months')

# solar total 
solar_avg <- sudhour2 %>% 
  filter(yyyy == 2021) %>% 
  group_by(mm) %>% 
  summarise(solar_avg = mean(`Solar Total (MJ/m²)`))
# graph of average solar total 
ggplot(data = solar_avg, aes(x = mm, y = solar_avg)) +
  geom_col(fill = 'yellow3')+
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.name)+
  theme(axis.text.x = element_text(angle = 90))+
  
  labs(title = "Solar Total Average per Month (2021)",
       subtitle = 'At Sewanee Utility District',
       y = 'Solar Total (MJ/m2)',
       x = 'Months')

# SUD rain avg
rain_avg <-  sudhour2 %>% 
  filter(yyyy == 2021) %>% 
  group_by(mm) %>% 
  summarise(rain_avg = mean(`Rain (mm)`))
#  SUD average rainfall
ggplot(data = rain_avg, aes(x = mm, y = rain_avg))+
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.name)+
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,.15)+
  geom_col(fill = 'darkolivegreen3')+
labs(title = "Rainfall per Month (2021)",
     subtitle = 'Average Rainfall at Sewanee Utility District',
     y = 'Average Rainfall (mm)',
     x = 'Months')

oessrain <- mean_sdrain %>% 
  select(mean, Month)

# rainfall in SUD and OESS
ggplot()+
  geom_col(data = oessrain, aes(x = Month, y = mean), fill = 'red')+
  geom_col(data = rain_avg, aes( x = mm, y = rain_avg), fill = 'blue')+
  labs(title = 'Average Rainfall per Month (2021)',
       subtitle = 'Sewanee Utility District vs. OESS',
       y = 'Average Rain (mm)', 
       x = 'Month')+
  theme(axis.text.x = element_text(angle = 90))
 
#################################################    

####################################################
############ Delta between each site ###############

all_data %>% 
  filter(`Site Name` == 'Lagoon C') %>% 
  filter(variable == 'Cond µS/cm') %>% 
tally()
all_data %>% 
  filter(`Site Name` == 'Wetland Basin 3') %>% 
  filter(variable == 'Cond µS/cm' ) %>%
  tally()

# Basin 3: NitraLed: 7306, NH4: 7306, NH3: 7306, Cond: 7306
# Lagoon C: Nitraled: 7686, NH4: 0, NH3: 0, Cond: 7686
# missing variables: 17,272: lagoon C has 380 more rows per variable

dfdelta <- all_data %>% 
  select(month, `Time (HH:mm:ss)`, variable, `Site Name`, value, year, Date) %>%
  group_by( `Time (HH:mm:ss)` ) %>%
  mutate(condbasin = 
           
           dfdelta[dfdelta$`Time (HH:mm:ss)` == `Time (HH:mm:ss)`
                   &dfdelta$variable == 'Cond µS/cm'
                   &dfdelta$`Site Name` == 'Wetland Basin 3'
                   &dfdelta$Date == Date]$value[1])

