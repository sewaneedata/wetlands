library(shinydashboard)
library(shiny)
library(tidyverse)
library(gsheet)
library(lubridate)
#Valid colors are: red, yellow, aqua, blue, light-blue,
#green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black
url<-"https://docs.google.com/spreadsheets/d/14nn7NWMBatbzcz9nqcTFzQghmzMUE2o0/edit?usp=sharing&ouid=104854259661631892531&rtpof=true&sd=true"
sud <-gsheet2tbl(url)
url2 <- 'https://docs.google.com/spreadsheets/d/1WdrZuZP9J6Im4KQdo6MudPQC4Fwf13rD/edit?usp=sharing&ouid=104854259661631892531&rtpof=true&sd=true'
sond<-gsheet2tbl(url2)
###
ui <- dashboardPage(
  dashboardHeader(title = h4("Sewanee Wetlands")),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("sond_cond")),
                box(
                  title = "Select:",
                  background = "lime",
                  collapsible = TRUE,
                  status = "success",
                  solidHeader = TRUE,
                  selectInput("month", "Month",
                              choices = c("January", "February", "March",
                                          "April", "May", "June", "July", "August", "September", "October",
                                          "November", "December")),
                  selectInput("year", "Year" ,
                              choices = c("2020", "2021"))
                )
              )
      )) )
)
# Boxes need to be put in a row (or column)
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  rv <- reactiveValues()
  observe({
  })
  output$sond_cond <- renderPlot({
    library(tidyverse)
    library(gsheet)
    library(lubridate)
    url<-"https://docs.google.com/spreadsheets/d/14nn7NWMBatbzcz9nqcTFzQghmzMUE2o0/edit?usp=sharing&ouid=104854259661631892531&rtpof=true&sd=true"
    sud <-gsheet2tbl(url)
    url2 <- 'https://docs.google.com/spreadsheets/d/1WdrZuZP9J6Im4KQdo6MudPQC4Fwf13rD/edit?usp=sharing&ouid=104854259661631892531&rtpof=true&sd=true'
    sond<-gsheet2tbl(url2)
    ###############################################
    names(sond)
    ## fixing the dates -----
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
    ### matching the dates in datasets and adding month
    sud<-sud2%>%
      filter(Date >= "2020-10-19")%>%
      mutate(month = month(Date))%>%
      mutate(month = month.name[month])%>%
      mutate(year = year(Date))
    sond$Date <- mdy(sond$Date)
    sond<-sond%>%
      mutate(month = month(Date))%>%
      mutate(month = month.name[month])%>%
      mutate(year = year(Date))
    ####
    rv$sc_sond%>%
      summarise(avg = mean(pH))
    ### avg cond
    sond_cond1 <- sond %>%
      group_by(month, year) %>%
      summarise(avgConduc = mean(`Cond µS/cm`))%>%
      mutate(month = factor(month,
                            levels = c("January", "February", "March",
                                       "April", "May", "June", "July", "August", "September", "October",
                                       "November", "December")))%>%
      na.omit()
    sond_cond<-ggplot(data = sond_cond, aes(x = month, y = avgConduc, fill = year))+
      geom_col()+
      theme(axis.text = element_text(angle = 90))
  })
}
shinyApp(ui, server)
















