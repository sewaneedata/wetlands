library(shinydashboard)
library(shiny)
library(tidyverse)
library(gsheet)
library(lubridate)

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


############################## UI
ui <- dashboardPage(skin = 'black',
  dashboardHeader(title = h4("Sewanee Wetlands")),
  ## Sidebar content
  sidebar <-dashboardSidebar( 
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("th")),
      menuItem("Plots", tabName = "plots1", icon = icon("th"))
      
    )
  ),
 ##################### ## Body content
  body <-dashboardBody(
    tabItems(
      
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("sond_cond")),
                box(plotOutput("avg_vari_site")),
                
                
                box(
                  title = "Select:",
                  background = "black",
                  collapsible = TRUE,
                  select = "success",
                  solidHeader = TRUE,
                  selectInput("month", "Month",
                              choices = c("January", "February", "March",
                                          "April", "May", "June", "July", "August", "September", "October",
                                          "November", "December")),
                  
                  selectInput("year", "Year" ,
                              choices = c("2020", "2021")),
                  selectInput("variable", "Variable",
                              choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                          "Temp °C", "NH4+ -N mg/L", "NH3 mg/L"))
                )
                )
             ),
      tabItem(tabName = "plots1",
              fluidRow(
                box(plotOutput("avg_boxplot")),
                
                box(
                  title = "Select:",
                  background = "black",
                  collapsible = TRUE,
                
                  solidHeader = TRUE,
                  selectInput("year2", "Year",
                              choices = c("2020", "2021")),
                  selectInput("site2", "Site" ,
                              choices = c("Wetland Basin 3", "Lagoon C")),
                  selectInput("variable2", "Variable",
                              choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                          "Temp °C", "NH4+ -N mg/L", "NH3 mg/L"))
      )) )
      
      
      )))
      
    # Boxes need to be put in a row (or column)
      

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
      summarise(avg = mean(as.numeric(value), na.rm = TRUE))
    
    ggplot(data = new_df)+
      geom_col(aes(variable, avg, fill = `Site Name`), position = "dodge")+
      theme(axis.text = element_text(angle = 90))
   
  })
  
  
  output$avg_vari_site <- renderPlot({
    avg_vari_site<-all_data%>%
      group_by(month, year, `Site Name`)%>%
      filter(year == input$year)%>%
      filter(month == input$month)%>%
      filter(variable == input$variable)%>%
      summarise(avg = mean(as.numeric(value), na.rm = TRUE))
    ggplot(data = avg_vari_site)+
      geom_col(aes(month, avg, fill = `Site Name`), position = "dodge")
  })
  
  
  output$avg_boxplot <- renderPlot({  
    avg_boxplot <- all_data %>%
      filter(year == input$year2) %>%
      filter(`Site Name` == input$site2)%>%
      filter(variable == input$variable2)
    
    ggplot(data = avg_boxplot, 
           aes(x = month, y = as.numeric(value)))+
      geom_boxplot()+
      theme(axis.text = element_text(angle = 90))
  })
}

shinyApp(ui, server)