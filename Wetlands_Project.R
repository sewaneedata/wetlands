library(tidyverse)
library(gsheet)
library(lubridate)
library(ggplot2)

url<-"https://docs.google.com/spreadsheets/d/14nn7NWMBatbzcz9nqcTFzQghmzMUE2o0/edit?usp=sharing&ouid=104854259661631892531&rtpof=true&sd=true"
sud <-gsheet2tbl(url)

###############################################
names(sond)
max(sond$`Date (MM/DD/YYYY)`)
min(sond$`Date (MM/DD/YYYY)`)

# Rename Variables
names(sud)
sond<-sond%>%
  rename(Date = `Date (MM/DD/YYYY)`)

sud<-sud%>%
  rename(Date = `Date Values Reflect`)

ggplot(data = sond, aes(Date, `pH mV`,na.rm))+
  geom_jitter()

# filter missing data from SUD data 
sud<-sud %>%
  filter(Date != "MISSING DATA" | Date != "NO POWER - AERATOR INSTALL")

# Reormat the dates 

library(lubridate)
sud2 <- sud
sud2$Date <- mdy(sud$Date)

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

sud2 <- sud2 %>%
  mutate(month = month(Date))%>%
  mutate(month = month.name[month])

sond_new <- sond_new %>% 
  mutate(year = year(Date))

sud2 <- sud2 %>% 
  mutate(year = year(Date))

#### avg conductance

sond_cond <- sond_new %>% 
  filter(year == 2021) %>% 
  group_by(month) %>% 
  summarise(avgConduc = mean(`Cond µS/cm`)) %>% 
  mutate(month = factor(month,
                        levels = c('January', 'February', 
                                   'March', 
                                   'April', 
                                   'May', 'June', 
                                   'July', 'August', 'September', 
                                   'October', 'November', 'December'))) %>% 
  na.omit()
 
# Plot of average Conductance 

ggplot(data = sond_cond, aes(x = month, y = avgConduc, group = 1))+
  geom_point()+
  geom_line(color = 'yellow')+
theme(axis.text = element_text(angle = 90))+
  theme(legend.position = 'none')+
  labs(title = "Average Conductance in Basin 3 (2021)",
       subtitle = 'Average Conductance (uS/cm) per Month',
       y = 'Average Conductance (uS/cm)',
       x = 'Month')

#### avg turbidity 

avgturb <- sond_new %>% 
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

# Plot of avg Turbidity 

ggplot(data = avgturb, aes(x = month, y = avgturb, group = 1) ) +
  geom_point()+
  geom_line(color = 'deeppink1')+
  theme(axis.text = element_text(angle = 90))+
  theme(legend.position = 'none')+
  labs(title = 'Average Turbidity 2021',
       subtitle = 'Average Turbidity per Month',
       y = 'Average Turbidity (NTU)',
       x = 'Month')

sond_new <- sond_new %>% 
  rename(Turbidity_NTU = 'Turbidity NTU')

# turbidity v PH 
avgph <- sond_new %>% 
  filter(year == 2021) %>% 
 group_by(month) %>% 
  summarise(avgph = mean(pH)) %>% 
mutate(month = factor(month,
                      levels = c('January', 'February', 
                                 'March', 
                                 'April', 
                                 'May', 'June', 
                                 'July', 'August', 'September', 
                                 'October', 'November', 'December'))) %>% 
  na.omit()
# plot of avg pH
ggplot(data = avgph, aes(x = month, y = avgph, group = 1))+
  geom_point()+
  geom_line(color = 'blue')+
  theme(axis.text = element_text(angle = 90))+
  theme(legend.position = 'none')+
  labs(title = "Average pH in Lagoon C (2021)",
       subtitle = 'Average pH per Month',
       y = 'Average pH',
       x = 'Month')

# Average Turbidity and pH per Month (2021)

ggplot()+
  geom_line(data = avgph, aes(x = month, y = avgph, group = 1),color = "blue")+ 
       geom_line(data = avgturb, aes(x = month, y = avgturb, group = 1), color = "hot pink")+
        theme(axis.text = element_text(angle = 90))+
  labs(title = 'Average Turbidity by pH in Basin 3 (2021)',
       subtitle = 'pH and Turbidity (NTUs)',
       y = 'Average Turbidity NTU and pH',
       x = 'Month')

# Average ORP code (-50 to 350)

avgorp <- sond_new %>% 
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

# Average ORP graph 

ggplot(data = avgorp, aes(x = month, y = avgorp, group = 1))+
  geom_point()+
  geom_line(color = 'brown')+
  theme(legend.position = 'none')+
  theme(axis.text= element_text(angle = 90))+
  labs(title = 'Average ORP in Basin 3 (2021)', 
       subtitle = 'Average ORP per Month', 
       y = 'Average ORP (mV)', 
       x = 'Month')

# Avg Specific Conductance
avgspcond <- sond_new %>% 
  filter(year == 2021) %>% 
  group_by(month) %>% 
  summarise(avgspcond = mean(`SpCond µS/cm`)) %>% 
  mutate(month = factor(month,
                        levels = c('January', 'February', 
                                   'March', 
                                   'April', 
                                   'May', 'June', 
                                   'July', 'August', 'September', 
                                   'October', 'November', 'December'))) %>% 
  na.omit()

# Graph of specific conductance (uS/cm) (0-200-low)(200-1000 mid)(1000-10000 high)

ggplot(data = avgspcond, aes(x = month, y = avgspcond, group = 1))+
  geom_line(color  = 'bisque2')+
  geom_point()+
  theme(legend.position = 'none')+
  theme(axis.text = element_text(angle = 90))+
  labs(title = 'Average Specific Conductance in Basin 3 (2021)', 
       subtitle = 'Average SP Conductance per Month', 
       y = 'Average SP Conductance (uS/cm)', 
       x = 'Month')

# average NitraLED mg/L per month (good range (<= 1 mg/L))
avgnitra <- sond_new %>% 
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

# Plot of average NitraLED mg/L per month 

ggplot(data = avgnitra, aes(x = month, y = avgnitra, group= 1))+
  geom_line(color  = 'aquamarine3')+
  geom_point()+
  theme(legend.position = 'none')+
  theme(axis.text = element_text(angle = 90))+
  labs(title = 'Average NitraLED in Basin 3 (2021)', 
       subtitle = 'Average NitraLED per Month', 
       y = 'Average NitraLED mg/L', 
       x = 'Month')

# Average Temperature (good range 20-25 C for mesophilic)
avgtemp <- sond_new %>% 
  filter(year == 2021) %>% 
  group_by(month) %>% 
  summarise(avgtemp = mean(`Temp °C`)) %>% 
  mutate(month = factor(month,
                        levels = c('January', 'February', 
                                   'March', 
                                   'April', 
                                   'May', 'June', 
                                   'July', 'August', 'September', 
                                   'October', 'November', 'December'))) %>% 
  na.omit()

# Plot of average temperature (basin 3)

ggplot(data = avgtemp, aes(x = month, y = avgtemp, group = 1))+
  geom_line(color = 'darkorchid1')+
  geom_point()+
 theme(legend.position = 'none')+
  theme(axis.text = element_text(angle = 90))+
  labs(title = 'Average Temperature in Basin 3 (2021)', 
       subtitle = 'Average Temperature per Month', 
       y = 'Average Temperature (C)', 
       x = 'Month')
# done now

################################# rainfall doc














