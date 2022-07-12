library(tidyverse)
library(gsheet)
library(lubridate)
library(ggplot2)
library(gsheet)

###############################################
url3 <- "https://docs.google.com/spreadsheets/d/1eRF3RnsWTt6ObsY6hlgi6jOjvliS1Gn5/edit#gid=200414766"
lagoonC<-gsheet2tbl(url3)
lagoonC

lagoonC<-lagoonC%>%
  filter(year != 2022)

lagoonC <- lagoonC%>%
  rename(Date = `Date (MM/DD/YYYY)`)
lagoonC<-lagoonC %>%
  filter(Date != "MISSING DATA - POWER ISSUE" | Date != "SONDES AT WATSON RESEARCH SITE" | Date != "DAMAGED COND/TEMP SENSOR CAUSED FAULT THAT HAULTED LOGGING")

lagoonC$Date <- mdy(lagoonC$Date)

lagoonC <- lagoonC%>%
  mutate(month = month(Date))%>%
  mutate(month = month.name[month])%>%
  mutate(year = year(Date))

# avg conductance 
lagcond <- lagoonC %>% 
  group_by(month) %>% 
  #summarise(avgtemp = head(`Temp °C`, 1)) %>% 
  summarise(avgcond = mean(as.numeric(`Cond µS/cm`), na.rm = TRUE)) %>% 
  mutate(month = factor(month,
                        levels = c('January', 'February', 
                                   'March', 
                                   'April', 
                                   'May', 'June', 
                                   'July', 'August', 'September', 
                                   'October', 'November', 'December'))) %>% 
  na.omit()

# plot of avg conductance lagoon C
ggplot(data = lagcond, aes(x = month, y = avgcond, group = 1))+
  geom_point()+
  geom_line(color = 'deepskyblue1')+
  theme(axis.text = element_text(angle = 90))+
  theme(legend.position = 'none')+
  labs(title = "Average Conductance in Lagoon C (2021)",
       subtitle = 'Average Conductance (uS/cm) per Month',
       y = 'Average Conductance (uS/cm)',
       x = 'Month')


# avg turbidity
lagturb <- lagoonC %>% 
  filter(year == 2021) %>% 
  group_by(month) %>% 
  summarise(avgturb = mean(`Turbidity NTU`)) %>% 
    mutate(month = factor(month,
                          levels = c('January', 'February', 
                                     'March', 
                                     'April', 
                                     'May', 'June', 
                                     'July', 'August', 'September', 
                                     'October', 'November', 'December'))) %>% 
    na.omit()

# plot of avg turb

ggplot(data = lagturb, aes(x = month, y = avgturb, group = 1))+
  geom_point()+
  geom_line(color= 'brown')+
  theme(axis.text = element_text(angle = 90))+
  theme(legend.position = 'none')+
  labs(title = "Average Turbidity in Lagoon C (2021)",
       subtitle = 'Average Turbidity NTU per Month',
       y = 'Average Turbidity NTU',
       x = 'Month')

# avg ORP
lagorp <- lagoonC %>% 
  filter(year == 2021) %>% 
  group_by(month) %>% 
  summarise(avgorp = mean(`ORP mV`)) %>% 
  mutate(month = factor(month,
                        levels = c('January', 'February', 
                                   'March', 
                                   'April', 
                                   'May', 'June', 
                                   'July', 'August', 'September', 
                                   'October', 'November', 'December'))) %>% 
  na.omit()

# plot of avg ORP
ggplot(data = lagorp, aes(x = month, y = avgorp, group = 1))+
  geom_point()+
  geom_line(color = 'blue')+
  theme(axis.text = element_text(angle = 90))+
  theme(legend.position = 'none')+
  labs(title = "Average Turbidity in Lagoon C (2021)",
       subtitle = 'Average Turbidity NTU per Month',
       y = 'Average Turbidity NTU',
       x = 'Month')

# avg Specific Conductance
lagspcond <- lagoonC %>% 
  group_by(month) %>% 
  #summarise(avgtemp = head(`Temp °C`, 1)) %>% 
  summarise(avgspcond = mean(as.numeric(`SpCond µS/cm`), na.rm = TRUE)) %>% 
  mutate(month = factor(month,
                        levels = c('January', 'February', 
                                   'March', 
                                   'April', 
                                   'May', 'June', 
                                   'July', 'August', 'September', 
                                   'October', 'November', 'December'))) %>% 
  na.omit()

# plot of avg specific conductance Lagoon C

ggplot(data = lagspcond, aes(x = month, y = avgspcond, group = 1))+
  geom_point()+
  geom_line(color = 'darkolivegreen')+
  theme(axis.text = element_text(angle = 90))+
  theme(legend.position = 'none')+
  labs(title = "Average Specific Conductance in Lagoon C (2021)",
       subtitle = 'Average SP Conductance (uS/cm) per Month',
       y = 'Average SP Conductance (uS/cm)',
       x = 'Month')

# Avg Nitraled
lagnitra <- lagoonC %>% 
  filter(year == 2021) %>% 
  group_by(month) %>% 
  summarise(avgnitra = mean(`NitraLED mg/L`)) %>% 
  mutate(month = factor(month,
                        levels = c('January', 'February', 
                                   'March', 
                                   'April', 
                                   'May', 'June', 
                                   'July', 'August', 'September', 
                                   'October', 'November', 'December'))) %>% 
  na.omit()

# plot of average NitraLED
ggplot(data = lagnitra, aes(x = month, y = avgnitra, group = 1))+
  geom_point()+
  geom_line(color = 'yellow')+
  theme(axis.text = element_text(angle = 90))+
  theme(legend.position = 'none')+
  labs(title = "Average NitraLED in Lagoon C (2021)",
       subtitle = 'Average NitraLED (mg/L) NTU per Month',
       y = 'Average NitraLED (mg/L)',
       x = 'Month')

# avg temp lagoon C
lagtemp <- lagoonC %>% 
 group_by(month) %>% 
  #summarise(avgtemp = head(`Temp °C`, 1)) %>% 
  summarise(avgtemp = mean(as.numeric(`Temp °C`), na.rm = TRUE)) %>% 
  mutate(month = factor(month,
                        levels = c('January', 'February', 
                                   'March', 
                                   'April', 
                                   'May', 'June', 
                                   'July', 'August', 'September', 
                                   'October', 'November', 'December'))) %>% 
  na.omit()
# graph of avg temp (C) lagoon C

ggplot(data = lagtemp, aes(x = month, y = avgtemp, group = 1))+
  geom_point()+
  geom_line(color = 'orange')+
  theme(axis.text = element_text(angle = 90))+
  theme(legend.position = 'none')+
  labs(title = "Average Temperature in Lagoon C (2021)",
       subtitle = 'Average Temperature (C) per Month',
       y = 'Average Temperature (C)',
       x = 'Month')

# avg pH lagoon C

lagpH <- lagoonC %>% 
  filter(year == 2021) %>% 
  group_by(month) %>% 
  summarise(avgpH = mean(pH)) %>% 
  mutate(month = factor(month,
                        levels = c('January', 'February', 
                                   'March', 
                                   'April', 
                                   'May', 'June', 
                                   'July', 'August', 'September', 
                                   'October', 'November', 'December'))) %>% 
  na.omit()

# plot of avg ph
ggplot(data = lagpH, aes(x = month, y = avgpH, group = 1))+
  geom_point()+
  geom_line(color = 'aquamarine3')+
  theme(axis.text = element_text(angle = 90))+
  theme(legend.position = 'none')+
  labs(title = "Average pH in Lagoon C (2021)",
       subtitle = 'Average pH per Month',
       y = 'Average pH',
       x = 'Month')
