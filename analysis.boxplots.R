library(tidyverse)
library(gsheet)
library(lubridate)
library(ggplot2)

url2 <- 'https://docs.google.com/spreadsheets/d/1WdrZuZP9J6Im4KQdo6MudPQC4Fwf13rD/edit?usp=sharing&ouid=104854259661631892531&rtpof=true&sd=true'
sond<-gsheet2tbl(url2)
###############################################
names(sond)
max(sond$`Date (MM/DD/YYYY)`)
min(sond$`Date (MM/DD/YYYY)`)

# Rename Variables

sond<-sond%>%
  rename(Date = `Date (MM/DD/YYYY)`)

# filter missing data from SUD data 

# Reormat the dates 

library(lubridate)

sond_new <- sond
sond_new$Date <- mdy(sond$Date)

# Make a month column 
# sond_new <- sond_new %>% 
# mutate(Month = month(Date))

# sud2 <- sud2 %>% 
# mutate(Month = month(Date))

# Name of the Months column 

sond_new <-sond_new %>%
  mutate(month = month(Date))%>%
  mutate(month = month.name[month])

sond_new <- sond_new %>% 
  mutate(year = year(Date))
  rename(sitename = `Site Name`)
avgConduc <- 
  sond_new %>% 
  rename(sitename = `Site Name`) %>% 
  filter(year == 2021) %>% 
  group_by(month) %>% 
  group_by(`Cond µS/cm`) %>% 
  mutate(month = factor(month,
                        levels = c('January', 'February', 
                                   'March', 
                                   'April', 
                                   'May', 'June', 
                                   'July', 'August', 'September', 
                                   'October', 'November', 'December'))) %>% 
  na.omit()
w
ggplot(data = avgConduc, aes(x = sitename, y =`Cond µS/cm` ))+
  geom_boxplot()+
  facet_wrap(~month)

