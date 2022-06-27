library(tidyverse) 
library(gsheet)
library(lubridate)
library(data.table)
library(readxl)

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
  mutate(month = factor(month,
                        levels = c('January', 'February', 
                                   'March', 
                                   'April', 
                                   'May', 'June', 
                                   'July', 'August', 'September', 
                                   'October', 'November', 'December'))) %>% 
  na.omit()

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
  filter(`Site Name` == 'Wetland Basin 3') %>% 
  summarise(avgturb = mean(as.numeric(value)))



# predictive model for turbidity
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




 
  


      
  
       
