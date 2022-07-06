library(tidyverse) 
library(gsheet)
library(lubridate)
library(data.table)
library(readxl)
library(ggpubr)
library(broom)
library(AICcmodavg)
library(dplyr)

url<-"https://docs.google.com/spreadsheets/d/14nn7NWMBatbzcz9nqcTFzQghmzMUE2o0/edit?usp=sharing&ouid=104854259661631892531&rtpof=true&sd=true"
sud <-gsheet2tbl(url)
url4 <-"https://docs.google.com/spreadsheets/d/1A_ljZAZmiRBsW5iL40EGRlVG1LkMa_w_7rqCnwAFWKA/edit#gid=537305485"
rainfalldf<-gsheet2tbl(url4)

# rename 
sud<-sud%>%
  rename(Date = `Date Values Reflect`)

# filter missing data
sud<-sud %>%
  filter(Date != "MISSING DATA" | Date != "NO POWER - AERATOR INSTALL")

# change date 
library(lubridate)
sud2 <- sud
sud2$Date <- mdy(sud$Date)

# month column 
sud<-sud2%>%
  mutate(month = month(Date))%>%
  mutate(month = month.name[month])%>%
  mutate(year = year(Date))

# select the needed columns in Sud data 
sud3 <- sud2 %>% 
  select(Date, `Timestamp*`, `Air temp Avg (C)`, `Air Temp Max (C)`,`Air Temp Min`, `Solar Total (MJ/m²)`)

# rename variables
sud3<-sud3 %>% rename(Dates=Date) %>% rename(Time=`Timestamp*`) %>% 
  rename(air_temp_avg_C = `Air temp Avg (C)`) %>% rename(air_temp_max_C = `Air Temp Max (C)`) %>% 
  rename(air_temp_min_C = `Air Temp Min`)
# year and month columnns
sud3 <- sud3 %>% 
  mutate(year = year(Dates))
sud3 <- sud3 %>% 
  mutate(months = month(Dates))

#######################################

# avg air temp           ###### NO
avgC <- sud3 %>% 
  filter(year == 2021) %>% 
  filter(air_temp_avg_C >= 0) %>% 
  group_by(months) %>% 
  summarise(avgC = mean(air_temp_avg_C)) %>% 
  mutate(months = recode_factor(.x = months,
                        "1" = 'January', 
                        "2" = 'February', 
                        "3" = 'March', 
                        "4" = 'April', 
                        "5" = 'May', 
                        "6" = 'June', 
                        "7" = 'July',
                        "8" = 'August', 
                        "9" = 'September', 
                        "10" = 'October', 
                        "11" = 'November', 
                        "12" = 'December'))
# average air temperature graph ######### NO
ggplot(data = avgC, aes(x = months, y = avgC))+
  geom_point(color = 'blue')+
  geom_line(group=1, color = 'blue')+
  theme(axis.text.x  = element_text(angle = 90))+
  labs(title = 'Average Air Temperature 2021',
       subtitle= 'Air Temperature in Celsius by Month',
       y = 'Average Air Temperature (C)',
       x = 'Months')

# average Solar Total per month  ######## YES
avgSolar <- sud3 %>% 
  filter(year == 2021) %>% 
  filter(`Solar Total (MJ/m²)` >= 0) %>% 
  group_by(months) %>% 
  summarise(avgSolar = mean(`Solar Total (MJ/m²)`)) %>% 
  mutate(months = recode_factor(.x = months,
                                "1" = 'January', 
                                "2" = 'February', 
                                "3" = 'March', 
                                "4" = 'April', 
                                "5" = 'May', 
                                "6" = 'June', 
                                "7" = 'July',
                                "8" = 'August', 
                                "9" = 'September', 
                                "10" = 'October', 
                                "11" = 'November', 
                                "12" = 'December'))
# average solar total ##### YES
ggplot()+
  geom_col(data = avgSolar, aes(x = months, y = avgSolar), fill = 'gold')+
  theme(axis.text.x  = element_text(angle = 90))+
  labs(title = 'Average Solar Total 2021',
       subtitle= 'In (MJ/m2) per Month',
       y = 'Average Solar Total (MJ/m2)',
       x = 'Months')


###########################################################
####################

# OESS Precipitation and Temp data  ####3 YES ALL OF THIS?
rainfalldf1 <- rainfalldf

# remove titles within data set
      #rainfalldf1 <- rainfalldf1 %>% 
      #filter(Date != 'Date') %>%
      #drop_na( Year )

# month collumn
rainfalldf1 <- rainfalldf %>% 
  filter(!is.na(Date)) %>%
  unite( col=Date, Year:Date, sep="-")
  #mutate( Month = month.abb[month( as_date(Date)) ] )

#reformat datasets because date was not matche up with the year
 rainfalldf1 <-rainfalldf1 %>%
   mutate(dates = c(rainfalldf1$Date[1:369] %>% ydm(), rainfalldf1$Date[370:376] %>% ymd())) %>% 
 mutate( Month = month.abb[month( as_date(dates)) ] )

 # select the columns needed
rainfalldf1 <- rainfalldf1 %>% 
  select(dates, Date, Month, `High temp (F)`, `Low temp (F)`, `rainfall (inches)`)
####################################################


# avg max temp per month          # YES
tempmax <- rainfalldf1 %>% 
  mutate(Month = factor(Month,
                        levels = c('Jan', 'Feb', 
                                   'Mar', 
                                   'Apr', 
                                   'May', 'Jun', 
                                   'Jul', 'Aug', 'Sep', 
                                   'Oct', 'Nov', 'Dec'))) %>% 
  group_by(Month) %>% 
  filter(year(dates) == 2021) %>% 
  summarise(tempmax = mean(`High temp (F)`))

# avg min temperature per month  # YES
tempmin <- rainfalldf1 %>%
  mutate(Month = factor(Month,
                        levels = c('Jan', 'Feb', 
                                   'Mar', 
                                   'Apr', 
                                   'May', 'Jun', 
                                   'Jul', 'Aug', 'Sep', 
                                   'Oct', 'Nov', 'Dec'))) %>% 
  group_by(Month) %>% 
  filter(year(dates) == 2021) %>% 
  summarise(tempmin = mean(`Low temp (F)`)) 

# average temperature plot  # YES
ggplot()+
  geom_col(data = tempmax, aes(x = Month, y = tempmax), fill= 'blue')+
  geom_col(data = tempmin, aes(x = Month, y = tempmin), fill = 'red')+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = 'Highest and Lowest Temperatures (2021)',
       subtitle = 'Average Temperature (C) per Month ',
       y = 'Average Temperature',
       x = 'Months')

# total rainfall per month     # YES
totalrain <- rainfalldf1 %>% 
  mutate(Month = factor(Month,
                        levels = c('Jan', 'Feb', 
                                   'Mar', 
                                   'Apr', 
                                   'May', 'Jun', 
                                   'Jul', 'Aug', 'Sep', 
                                   'Oct', 'Nov', 'Dec'))) %>% 
  group_by(Month) %>% 
  filter(year(dates)==2021) %>% 
  summarise(totalrain = sum(na.rm = TRUE,(`rainfall (inches)`)))

# total rainfall plot      # YES
ggplot()+
  geom_col(data = totalrain, aes( x= Month, y = totalrain),fill  = 'skyblue1')+
  labs(title = 'TotalRainfall (2021)',
       subtitle = 'Total Rainfall (in) per Month ',
       y = 'Total Rainfall (in)',
       x = 'Months')
rainfalldf1 <- rainfalldf1 %>% rename(rainfall = `rainfall (inches)`)

# avg rainfall per month       YES
avgrain <- rainfalldf1 %>% 
  mutate(Month = factor(Month,
                        levels = c('Jan', 'Feb', 
                                   'Mar', 
                                   'Apr', 
                                   'May', 'Jun', 
                                   'Jul', 'Aug', 'Sep', 
                                   'Oct', 'Nov', 'Dec'))) %>% 
  group_by(Month) %>% 
  filter(year(dates)==2021) %>% 
  summarise(avgrain = mean(na.rm = TRUE,(`rainfall (inches)`)))

# average rainfall per month plot
ggplot()+  
  geom_col(data =mean_sdrain, aes(x = Month, y = mean),
           fill = 'blue', group = 1)+
  geom_errorbar(data = mean_sdrain, aes(x = Month, ymax = mean+sd, width = .1, ymin = mean))+
  labs(title = 'Average Rainfall (2021)',
       subtitle = 'Average Rainfall (in) per Month ',
       y = 'Average Rainfall (in)',
       x = 'Months')

#################################################
##############

# mean and standard deviation for temp and rainfall

# mean/sd/upper/lower for average rainfall
mean_sdrain <- rainfalldf1 %>% 
  mutate(Month = factor(Month,
                        levels = c('Jan', 'Feb', 
                                   'Mar', 
                                   'Apr', 
                                   'May', 'Jun', 
                                   'Jul', 'Aug', 'Sep', 
                                   'Oct', 'Nov', 'Dec'))) %>% 
  group_by(Month) %>% 
  summarise(mean = mean(rainfall, na.rm = TRUE), sd = sd(rainfall, na.rm = TRUE)) %>%           
  mutate(lower = mean-sd, upper = mean+sd)



# mean/sd plot for rainfall
ggplot()+  
  geom_col(data =mean_sdrain, aes(x = Month, y = mean),
           fill = 'blue', group = 1)+
  geom_errorbar(data = mean_sdrain, aes(x = Month, ymax = mean+sd, width = .1, ymin = mean))+
  labs(title = 'Average Rainfall (2021)',
     subtitle = 'Average Rainfall (in) per Month ',
     y = 'Average Rainfall (in)',
     x = 'Months')

# mean/sd/upper/lower for temp max

meansd_tempmax <- rainfalldf1 %>% 
  mutate(Month = factor(Month,
                        levels = c('Jan', 'Feb', 
                                   'Mar', 
                                   'Apr', 
                                   'May', 'Jun', 
                                   'Jul', 'Aug', 'Sep', 
                                   'Oct', 'Nov', 'Dec'))) %>% 
  group_by(Month) %>% 
  summarise(mean = mean(`High temp (F)`, na.rm = TRUE), sd = sd(`High temp (F)`, na.rm = TRUE)) %>%           
  mutate(lower = mean-sd, upper = mean+sd)

# plot of temp max 
ggplot()+  
  geom_col(data =meansd_tempmax, aes(x = Month, y = mean),
           fill = 'blue', group = 1)+
  geom_errorbar(data = meansd_tempmax, aes(x = Month, ymax = mean+sd, width = .1, ymin = mean))+
  labs(title = 'Temperature Max (2021)',
       subtitle = 'Temp Max (C) per Month ',
       y = 'Average Temperature (C)',
       x = 'Months')

# mean/sd/upper/lower for temp min

meansd_tempmin <- rainfalldf1 %>% 
  mutate(Month = factor(Month,
                        levels = c('Jan', 'Feb', 
                                   'Mar', 
                                   'Apr', 
                                   'May', 'Jun', 
                                   'Jul', 'Aug', 'Sep', 
                                   'Oct', 'Nov', 'Dec'))) %>% 
  group_by(Month) %>% 
  summarise(mean = mean(`Low temp (F)`, na.rm = TRUE), sd = sd(`Low temp (F)`, na.rm = TRUE)) %>%           
  mutate(lower = mean-sd, upper = mean+sd)

# plot of temp min
ggplot()+  
  geom_col(data =meansd_tempmin, aes(x = Month, y = mean),
           fill = 'blue', group = 1)+
  geom_errorbar(data = meansd_tempmin, aes(x = Month, ymax = mean+sd, width = .1, ymin = mean))+
  labs(title = 'Temperature Min (2021)',
       subtitle = 'Temp Min (C) per Month ',
       y = 'Average Min Temperature (C)',
       x = 'Months')



