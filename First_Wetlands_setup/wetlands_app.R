library(shinydashboard)
library(shiny)
library(tidyverse)
library(gsheet)
library(lubridate)
library(readxl)
#install.packages("shinyWidgets")
library(shinyWidgets)

#Valid colors are: red, yellow, aqua, blue, light-blue, 
#green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black

library(tidyverse) 
library(gsheet)
library(lubridate)
library(data.table)

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


######### make a vector
possible_dates<-seq(min(ymd(all_data$Date), na.rm =TRUE), max(ymd(all_data$Date), na.rm =TRUE), by = "day")

dates_we_have<- all_data$Date%>%
  unique()%>%
  ymd()%>%
  na.omit()

keeps<-which(!possible_dates%in%dates_we_have)

dates_to_disable<-possible_dates[keeps]

######## CHANGING THE COLOR BASED ON EPA STANDARDS
head(all_data)

all_data<-all_data %>% 
  mutate(color = NA) %>% 
  mutate(color = 
           case_when(
             variable == 'Cond µS/cm' & value > 200 & value < 0 ~ 'red',
             variable == 'pH' & value > 8.5 & value < 6.5  ~ 'red',
             variable == 'ODO mg/L' & value > 253 & value < 260 ~ 'red',
             variable == 'NH3 mg/L' & value > 253 & value < 260 ~ 'red',
             variable == 'Turbidity NTU' & value > .3 ~ 'red',
             variable == 'NitraLED mg/L' & value > 10 ~ 'red',
             variable == 'Temp °C' & value > 253 & value < 260 ~ 'red',
             variable == 'NH4+ -N mg/L' & value > 253 & value < 260 ~ 'red',
             variable == 'ORP mV' & value < -50 ~ 'red',
             TRUE ~ 'green'
           ))
#################################### CHANGING THE FONT


############################## UI
ui <- dashboardPage(skin = 'black',
  dashboardHeader(title = h4("Sewanee Wetlands")),
  ## Sidebar content
  sidebar <-dashboardSidebar( 
    sidebarMenu(
      menuItem("About the Project", tabName = "overview", icon = icon("water")),
      menuItem("Water Quality Comparison", tabName = "dashboard", icon = icon("th")),
      menuItem("Trends", tabName = "trends", icon = icon("list-alt")),
      menuItem("Boxplots", tabName = 'boxplots', icon = icon("bar-chart-o")),
      menuItem('Predictive Models', tabName = "models", icon = icon("table"))
      
    )
  ),
 ##################### ## Body content
  body <-dashboardBody(
    tabItems(
      
      # First tab content
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Overview",
                    solidHeader = TRUE,
                    width = 12,
                    background = "orange")
              ),
              fluidRow(
                tabBox(title = "DataLab Team",
                       id = "tabset1",
                       tabPanel("tab1", "Tessa Shackleford"),
                       tabPanel("tab2", "Lauren Hutchsion"),
                       tabPanel("tab3", "Harrison Lowery")
                      
                )
                
              )),
      
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Variable Averages by Month",
                    solidHeader = TRUE,
                    width = 12, 
                    plotOutput("sond_cond"),
                    background = "navy")),
              fluidRow( 
                 box(title = "Specified Variable Average by Month",
                     solidHeader = TRUE,
                   plotOutput("avg_vari_site"),
                    background = "navy"),
                
              
                box( 
                  title = "Select:",
                  background = "navy",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  selectInput("month", "Month",
                              choices = c("January", "February", "March",
                                          "April", "May", "June", "July", "August", "September", "October",
                                          "November", "December")),
                  
                  selectInput("year", "Year" ,
                              choices = c("2020", "2021"),
                              selected = "2021"),
                  selectInput("variable", "Variable",
                              choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                          "Temp °C", "NH4+ -N mg/L", "NH3 mg/L"))
                )
                )
             ),
      # TRENDS TAB
      tabItem(tabName = "trends",
              fluidRow(
                box(title = "Monthly Trends in Variables",
                    solidHeader = TRUE,
                  plotOutput("trend_data"),
                    background = "blue"),
                
                box(
                  title = "Select Monthly:",
                  background = "blue",
                  collapsible = TRUE,
                  
                  solidHeader = TRUE,
                  
                  selectInput("year4", "Year",
                              choices = c("2020", "2021")),
                  selectInput("site4", "Site" ,
                              choices = c("Wetland Basin 3", "Lagoon C")),
                  selectInput("variable4", "Variable",
                              choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                          "Temp °C", "NH4+ -N mg/L", "NH3 mg/L"))
                  
                  )),
                fluidRow(
                box(title = "Daily Trends in Variables",
                    solidHeader = TRUE,
                  plotOutput("trend_data2"),
                    background = "maroon"),
                
                box(
                  title = "Select Daily:",
                  background = "maroon",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  
                  selectInput("year5", "Year",
                              choices = c("2020", "2021"),
                              selected = "2021"),
                  selectInput("site5", "Site" ,
                              choices = c("Wetland Basin 3", "Lagoon C")),
                  selectInput("variable5", "Variable",
                              choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                          "Temp °C", "NH4+ -N mg/L", "NH3 mg/L")),
                  selectInput( "month5", "Month",
                               multiple = TRUE,
                               choices = c("January", "February", "March",
                                           "April", "May", "June", "July", "August", "September", "October",
                                           "November", "December"),
                               selected = "January"
                              
                     ))
                  
                ),
                
               fluidRow(
                box(title = "Hourly Trends in Variables",
                    solidHeader = TRUE,
                  plotOutput("trend_data3"),
                    background = "olive"),
                box(
                  title = "Select Hourly:",
                  background = "olive",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  
                  selectInput("site6", "Site" ,
                              choices = c("Wetland Basin 3", "Lagoon C")),
                  selectInput("variable6", "Variable",
                              choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                          "Temp °C", "NH4+ -N mg/L", "NH3 mg/L")),
                  airDatepickerInput("date6", "Date", 
                            minDate  = (min(ymd(all_data$Date), na.rm =TRUE)),
                            maxDate = (max(ymd(all_data$Date), na.rm =TRUE)),
                            disabledDates = dates_to_disable,
                            multiple = TRUE,
                            value = "2021-10-19")
                                   
                  )
                  
                
                
                
                )),
                ################################## BOXPLOTS TAB #############################
      tabItem(tabName = "boxplots",
              fluidRow(
                box(title = "Varience of Variables by Month",
                    solidHeader = TRUE,
                  plotOutput("avg_boxplot"),
                  background = "teal"),
                
                box(
                  title = "Select:",
                  background = "teal",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  selectInput("year2", "Year",
                              choices = c("2020", "2021")),
                  selectInput("site2", "Site" ,
                              choices = c("Wetland Basin 3", "Lagoon C")),
                  selectInput("variable2", "Variable",
                              choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                          "Temp °C", "NH4+ -N mg/L", "NH3 mg/L"))
      )) ),
      ############################ MODELS TAB ############################################
      tabItem(tabName = "models",
              fluidRow(
                box(title = "Predictive Models Using 2021 Data",
                    solidHeader = TRUE,
                  plotOutput("predic_model"),
                    background = "purple"),
                box(
                  title = "Select:",
                  background = "purple",
                  collapsible = TRUE,
                  
                  solidHeader = TRUE,
                  
                  selectInput("sitename3", "Site" ,
                              choices = c("Wetland Basin 3", "Lagoon C")),
                  selectInput("variable3", "Variable",
                              choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                          "Temp °C", "NH4+ -N mg/L", "NH3 mg/L"))
                  
                )))
      
      
      )))
      
    # Boxes need to be put in a row (or column)
      
####### SERVER ###########################
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  rv <- reactiveValues()
  
  
  observe({
    
  
   })
  
  output$sond_cond <- renderPlot({
    
    new_df<-all_data%>%
      group_by(`Site Name`, month, variable)%>%
      filter(year == input$year)%>%
      filter(month == input$month)%>%
      filter(variable %in% c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
             "Temp °C", "NH4+ -N mg/L", "NH3 mg/L"))%>%
      summarise(avg = mean(as.numeric(value), na.rm = TRUE), color = color)
    
    ggplot(data = new_df)+
      geom_col(aes(variable, avg, fill = color), position = "dodge")+
      theme(axis.text = element_text(angle = 90))+
      labs(x = "Variable",
           y = "Units")+
      scale_fill_manual(values = c(green = "darkgreen", red = "red"))+
      facet_wrap(~`Site Name`)
   
  })
  
  
  output$avg_vari_site <- renderPlot({
    avg_vari_site<-all_data%>%
      group_by(month, year, `Site Name`)%>%
      filter(year == input$year)%>%
      filter(month == input$month)%>%
      filter(variable == input$variable)%>%
      summarise(avg = mean(as.numeric(value), na.rm = TRUE), color = color)
    ggplot(data = avg_vari_site)+
      geom_col(aes(month, avg, fill = color), position = "dodge")+
      labs(x = "Month",
           y = "Unit")+
      scale_fill_manual(values = c(green = "darkgreen", red = "red"))+
      facet_wrap(~`Site Name`)
  })
  
  output$trend_data <- renderPlot({  
 
    ############################## month trends
    month_trend<-all_data%>%
      group_by(month)%>%
      filter(year == input$year4)%>%
      filter(variable == input$variable4)%>%
      summarise(avg_month = mean(as.numeric(value)))%>%
      mutate(month = factor(month,
                            levels = c("January", "February", "March",
                                       "April", "May", "June", "July", "August", "September", "October",
                                       "November", "December")))
    
    ggplot(data = month_trend, aes(month, avg_month))+
      geom_point()+
      geom_line(group = 1)+
      theme(axis.text.x = element_text(angle = 90))+
      labs(x = "Month",
           y = "Units")
  })

    output$trend_data2 <- renderPlot({    
  
       ################################# daily trends
    daily_all_data <- all_data %>%
      mutate(days = day(Date))
    avgDay <- daily_all_data %>%
      group_by(month, days) %>%
      filter(year == input$year5) %>%
      filter(month %in% input$month5) %>%
      filter(`Site Name` == input$site5)%>%
      filter(variable == input$variable5) %>%
      summarise(meanVar = mean(as.numeric(value)))
    ggplot(data = avgDay, aes(x = days, y =meanVar, color = month))+
      geom_point()+
      geom_line()+
      scale_y_continuous(breaks = seq(0,40,2))+
      scale_x_continuous(breaks = seq(0,30,2))+
      labs(x = "Day of the Month",
           y = "Units")
    
    
    
  })
    output$trend_data3 <- renderPlot({    
     
       ######################################## hourly trends
      time_attempt<-all_data %>%
        mutate( hour = hour(`Time (HH:mm:ss)`)) %>%
       # filter(year == input$year6)%>%
        filter(variable == input$variable6)%>%
        filter(Date %in% input$date6)%>% 
        filter(`Site Name` == input$site6) 
      
      ggplot(data = time_attempt, aes(x=hour, y=as.numeric(value), color = factor(Date)))+
        geom_point()+
        geom_line()+
        labs(x = "Hour of the Day",
             y = "Units",
             color = "Date")
      
      
    
    })
  
    ##################################################################### 
  output$avg_boxplot <- renderPlot({  
    avg_boxplot <- all_data %>%
      filter(year == input$year2) %>%
      filter(`Site Name` == input$site2)%>%
      filter(variable == input$variable2)
    
    ggplot(data = avg_boxplot, 
           aes(x = month, y = as.numeric(value)))+
      geom_boxplot()+
      theme(axis.text = element_text(angle = 90))+
      labs(x = "Month",
           y = "Units")
  })
    #####################################################################  
  output$predic_model <- renderPlot({
    
    avg_boxplot <- all_data %>%
      filter(`Site Name` == input$sitename3)%>%
      filter(variable == input$variable3)
    # code for for predictive model for turbidity
    avg_predict <- avg_boxplot %>%
      filter( year == 2021 ) %>% 
      group_by( month ) %>% 
      summarize(avg = mean(as.numeric(value)))
    
    #####################################################################   
    # predictive model using year 2021
    ggplot( data = avg_boxplot, aes( x= (month), y = as.numeric(value)))+
      geom_point()+
      theme(axis.text.x = element_text(angle = 90))+
      labs(
           y = 'Units',
           x = 'Months')+
      geom_point(data = avg_predict, aes(x = month, y = avg), 
                 size = 2, color = 'red')+
      geom_line(data = avg_predict, aes(x = month, y = avg), 
                size = 0.5, color = 'blue', group =1)
    
  })
  
  
}

shinyApp(ui, server)
