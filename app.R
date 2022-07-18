library(shinydashboard)
library(shiny)
library(tidyverse)
library(gsheet)
library(lubridate)
library(readxl)
#install.packages("shinyWidgets")
library(shinyWidgets)
library(plotly)
#install.packages("slickR")
library(slickR)
#install.packages("wesanderson")
library(wesanderson)


#Valid colors are: red, yellow, aqua, blue, light-blue, 
#green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black

library(tidyverse) 
library(gsheet)
library(lubridate)
library(data.table)

url<-"https://docs.google.com/spreadsheets/d/14nn7NWMBatbzcz9nqcTFzQghmzMUE2o0/edit?usp=sharing&ouid=104854259661631892531&rtpof=true&sd=true"
sud <-gsheet2tbl(url)
url2 <- 'https://docs.google.com/spreadsheets/d/1gM_2BuFFZjyv59s3UMuZl1NEmv6eMCxJ/edit#gid=153164412'
sond<-gsheet2tbl(url2)
url3 <- "https://docs.google.com/spreadsheets/d/1gM_2BuFFZjyv59s3UMuZl1NEmv6eMCxJ/edit#gid=200414766"
lagoonC<-gsheet2tbl(url3)
lagoonC

############## removing the serial numbers #########

sond <- read.csv(text = gsheet2text(url2, format='csv'), skip = 2, check.names = FALSE)
lagoonC <- read.csv(text = gsheet2text(url3, format='csv'), skip = 1, check.names = FALSE)

lagoonC <- lagoonC[-20]
sond <- sond[-22]
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




########## combine the data sets ################## ----
all_data<-rbind(lagoonC2, sond2)

all_data%>%
  mutate(month = factor(month,
                        levels = c("January", "February", "March",
                                   "April", "May", "June", "July", "August", "September", "October",
                                   "November", "December")))
############## making an hour column on combined sond data ----

#all_data<- all_data%>%
 # mutate(hour = hour(lubridate::as_datetime(`Time (HH:mm:ss)`, origin = "1970-1-1")), na.rm = TRUE)

######### make a vector
possible_dates<-seq(min(ymd(all_data$Date), na.rm =TRUE), max(ymd(all_data$Date), na.rm =TRUE), by = "day")

dates_we_have<- all_data$Date%>%
  unique()%>%
  ymd()%>%
  na.omit()

keeps<-which(!possible_dates%in%dates_we_have)

dates_to_disable<-possible_dates[keeps]

######## CHANGING THE COLOR BASED ON EPA STANDARDS ------
head(all_data)

all_data<-all_data %>% 
  mutate(color = NA) %>% 
  mutate(color = 
           case_when(
             variable == 'Cond µS/cm' & value > 50 & value < 1500 ~ 'meets standards',
             variable == 'pH' & value > 6 & value < 9  ~ 'meets standards',
             variable == 'ODO mg/L' & value > 3 ~ 'meets standards',
             variable == 'NH3 mg/L' & value < 17 ~ 'meets standards',
             variable == 'Turbidity NTU' & value > 0 & value < 10 ~ 'meets standards',
             variable == 'NitraLED mg/L' & value < 10 ~ 'meets standards',
             variable == 'Temp °C' & value > 20 & value < 35 ~ 'meets standards',
             variable == 'NH4+ -N mg/L' & value < 17 ~ 'meets standards',
             variable == 'ORP mV' & value > 0 ~ 'meets standards',
             TRUE ~ 'does not meet standards'
           )) %>% 
  mutate(standard_min = 
           case_when(
             variable == 'Cond µS/cm' ~  50,
           variable == 'pH' ~ 6,
           variable == 'ODO mg/L' ~ 3, 
           variable == 'NH3 mg/L' ~ 0,
           variable == 'Turbidity NTU' ~ 0, 
           variable == 'NitraLED mg/L' ~ 0, 
            variable == 'Temp °C' ~ 20, 
            variable == 'NH4+ -N mg/L' ~ 0, 
             variable == 'ORP mV' ~ 1,
             TRUE ~ -Inf
           )) %>% 
  mutate(standard_max =
           case_when(
             variable == 'Cond µS/cm' ~  1500,
             variable == 'pH' ~ 9,
             variable == 'ODO mg/L' ~ Inf,
             variable == 'NH3 mg/L' ~ 17,
             variable == 'Turbidity NTU' ~ 10,
             variable == 'NitraLED mg/L' ~ 10,
             variable == 'Temp °C' ~ 35,
             variable == 'NH4+ -N mg/L' ~ 17,
             variable == 'ORP mV' ~ Inf,
             TRUE ~ -Inf
           ))

head(all_data)

names(all_data)
all_data %>% 
  group_by(variable, color) %>% 
  summarize(n = n(),
            standard_min = unique(standard_min),
            standard_max = unique(standard_max),
            min = min(as.numeric(value), na.rm=TRUE),
            max = max(as.numeric(value), na.rm=TRUE))

#################################### SUD DATA BEGINS -- CLEANING AND MODIFICATIONS ------

url5 <- "https://docs.google.com/spreadsheets/d/14nn7NWMBatbzcz9nqcTFzQghmzMUE2o0/edit#gid=571749034"
sud_hourly<-gsheet2tbl(url5)
url4 <-"https://docs.google.com/spreadsheets/d/1A_ljZAZmiRBsW5iL40EGRlVG1LkMa_w_7rqCnwAFWKA/edit#gid=537305485"
oess_data<-gsheet2tbl(url4)

# select what ya need
sud_hourly<- sud_hourly %>% 
  select(Timestamp, `VPD Avg (Kpa)`, `Rain (mm)`, `Solar Total (MJ/m²)`)

# make a year column
sudhour<- sud_hourly %>% 
  mutate(yyyy = year(mdy_hm(Timestamp))) 
bads <- which(is.na(sudhour$yyyy))

sudhour$yyyy[bads]<-year(ymd_hms(sudhour$Timestamp[bads]))        
sudhour[bads,]

# month column
sudhour <- sudhour %>% 
  mutate(mm = month(mdy_hm(Timestamp)))
bads2 <- which(is.na(sudhour$mm))
sudhour$mm[bads2]<-month(ymd_hms(sudhour$Timestamp[bads2]))
sudhour[bads2,]

# OESS Precipitation and Temp data  ####3 YES ALL OF THIS?
oess_data2 <- oess_data

# remove titles within data set
#rainfalldf1 <- rainfalldf1 %>% 
#filter(Date != 'Date') %>%
#drop_na( Year )

# month collumn
oess_data2 <- oess_data %>% 
  filter(!is.na(Date)) %>%
  unite( col=Date, Year:Date, sep="-")
#mutate( Month = month.abb[month( as_date(Date)) ] )

#reformat datasets because date was not matche up with the year
oess_data2<-oess_data2 %>%
  mutate(dates = c(oess_data2$Date[1:369] %>% ydm(), oess_data2$Date[370:376] %>% ymd())) %>% 
  mutate( Month = month.abb[month( as_date(dates)) ] )

# select the columns needed
oess_data2 <- oess_data2 %>% 
  select(dates, Date, Month, `High temp (F)`, `Low temp (F)`, `rainfall (inches)`)
####################################################

##################################################################################################################



############################## UI
ui <- dashboardPage(skin = 'black',
  dashboardHeader(title = h4("Sewanee Wetlands")),
  ## Sidebar content
  sidebar <-dashboardSidebar( 
    sidebarMenu(
      menuItem("About the Project", tabName = "overview", icon = icon("home")),
      menuItem("Water Quality Comparison", tabName = "dashboard", icon = icon("water")),
      menuItem("Trends", tabName = "trends", icon = icon("list-alt")),
      menuItem("Boxplots", tabName = 'boxplots', icon = icon("bar-chart-o")),
      menuItem('Descriptive Models', tabName = "models", icon = icon("table")),
      menuItem("Weather Data", tabName = "weather", icon = icon("sun")),
      menuItem("Aerator", tabName = "aer", icon = icon("code"))
      
    )
  ),
 ##################### ## Body content
  body <-dashboardBody(
    tabItems(
      
      # First tab content
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Wetlands Project Overview",
                    solidHeader = TRUE,
                    width = 12,
                    background = "orange")
              ),
              fluidRow(
                tabBox(title = "Our Project",
                  
                  id = "tabset2",
                  width = 10,
                  #Project Summary
                  tabPanel(title = "Project Summary",
                  fluidRow(column(6, 
                                   h3("Wetland Project Summary"))),
                  fluidRow(column(12,
                                   br(),
                                   p("Constructed wetlands are a more sustainable and 
                                   cost-effective alternative to wastewater treatment
                                     in comparison to conventional municipal treatment plants. 
                                     Through the microbial activity,
                                     wetlands naturally treat contaminants and pollutants,
                                     providing a more biologically efficient method of wastewater treatment.
                                     Wetland treatment has worked successfully on a
                                     large-scale such as the systems in Clayton County,
                                     GA, and Orange County, CA. However, wetlands have not been used 
                                     for wastewater treatment
                                     on a small-rural scale."),
                                    br(),
                                    br(),
                                    p("In 2016, Dr. McGrath, Professor of Biology at Sewanee, 
                                      lobbied for the construction of three wetland basins at the Sewanee
                                      Utility District (SUD) in order to research the efficacy of wetlands
                                      for wastewater treatment for a small community setting.
                                      For the past few years, water quality measurements have been
                                      taken at the SUD's wetlands to better determine the outcomes of wetland treatment
                                      in rural communities like Sewanee. Our team's goal was to set up visualizations
                                      that allow our community partner, Dr. McGrath, to understand the water quality
                                      trends of the wetlands over time in order to push for sustainable development
                                      in the community."),
                                      br(),
                                      br(),
                                      p("Our dashboard is set up to show the trends of water quality
                                      parameters for two treatment sites: the conventional lagoon treatment
                                      and the experimental wetland treatment. We’ve used EPA standards
                                      on surface water quality to determine whether the wetlands meet the
                                      criteria for each parameter. Additionally, the water quality trends
                                      are shown throughout years, months, days, and hours, so that Dr. McGrath
                                      can have a better insight on when wetland treatment performs best.
                                      Furthermore, we show trends in weather data, such as air temperature
                                      and precipitation in order to assess the impacts that climate change
                                      has had on the wetlands."),
                                      br(),
                                      br(), 
                                      p("Our most essential tab on the dashboard 
                                        is the aerator tab. The aerator was added 
                                        to wetland treatment at the beginning of 2022 
                                        as a way to increase dissolved oxygen levels in the wetlands.
                                        In our data from 2020-2021, dissolved oxygen has been much 
                                        lower than we would want because of invasive species taking 
                                        over the small wetland basin. However, Dr. McGrath predicts
                                        that the implementation of the aerator will increase oxygen
                                        levels in the water; therefore, improving the overall quality
                                        of water in the wetlands. From our time at DataLab, all of the
                                        data we worked with was pre-aeration; however, we’ve set up our
                                        dashboard in a way that allows Dr. McGrath to continue inputting
                                        data, so that she can see the impacts that the aerator has on the
                                        quality of wetland treatment. ")))),
                  #WQ Variable & Definition
                  tabPanel(title = "Water Quality Variables",
                           fluidRow(column(12, h3("Water Quality Variables and Criteria"), br())),
                           fluidRow(column(4,
                                                 box(
                                                   title = "Select Variable:",
                                                   background = "light-blue",
                                                   collapsible = TRUE,
                                                   width = 8,
                                                   
                                                   solidHeader = TRUE,
                                                   
                                                   selectInput("variable10", "Variables",
                                                               choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                                                           "Temp °C", "NH4+ -N mg/L", "NH3 mg/L" )))
                                           
                                                  
                             
                           ), column(8, textOutput("variable10")))),
                  #photos of wetlands
                  tabPanel(title = "Wetland Photos",
                        
                                           fluidRow(
                                            column(3, 
                                                   tags$img(src = "watertank.png", width = "100%", alt = "watertank"),
                                                   br(),
                                                   br(),
                                                   p("Water supply tank"),
                                                   tags$img(src = "lagoonc.png", width = "100%", alt = "lagoonc"),
                                                   br(),
                                                   br(),
                                                   p("Lagoon C")),
                                          column(3,
                                                   tags$img(src = "wetlandbasin3.png", width = "100%", alt = "wetlandbasin3"),
                                                   br(),
                                                   br(),
                                                   p("Wetland Basin 3 & SONDE Probe"),
                                                   tags$img(src = "Team.png", width = "100%", alt = "teams"),
                                                   br(),
                                                   br(),
                                                   p("Our team")),
                                        column(3,
                                                   tags$img(src = "sprayfields.png", width = "100%", alt = "sprayfields"),
                                                   br(),
                                                   br(),
                                                   p("Spray Field"),
                                                   tags$img(src = "pickerelweed.png", width = "100%", alt = "pickerelweed"),
                                                   br(),
                                                   br(),
                                                   p("Pickerel weed")),
                                            column(3,
                                                   tags$img(src = "mcgrath.png", width = "100%", alt = "mcgrath"),
                                                   br(),
                                                   br(),
                                                   p("Dr. McGrath"),
                                                   tags$img(src = "rosemallow.png", width = "100%", alt = "rosemallow"),
                                                   br(),
                                                   br(),
                                                   p("Rose mallow"))
                
              )))),
              fluidRow(
                #About us 
                tabBox(title = "Who We Are",
                       id = "tabset1",
                       width = 8,
                       tabPanel(title = "DataLab Fellows",
                                fluidRow(column(3, 
                                                fluidRow(tags$img(src = "Lauren.png", width="100%", alt="Lauren")),
                                                fluidRow(tags$img(src = "Harrison.png", width="100%", alt="Harrison")),
                                                fluidRow(tags$img(src = "Tessa.png", width="100%", alt="Tessa"))),
                                         column(8, h3("Lauren Hutchison C '22"),
                                                p("Lauren Hutchison is from Aldie, VA and is an Environment & Sustainability major at Sewanee: The University of the South. She is a member of the Order of the Gown and played for the women's soccer team."),
                                                br(),
                                                br(),
                                                br(),
                                                br(),
                                                h3("Harrison Lowery C '24"),
                                                p("Harrison Lowery is from Homewood, AL and is an English major at Sewanee: The University of the South. Harrison is a member of the Sewanee-Monteagle Rotary Club and a member of Phi Gamma Delta."),
                                                br(),
                                                br(),
                                                br(),
                                                br(),
                                                h3("Tessa Shackelford C '24"),
                                                p("Tessa Shackelford is from Sewanee, TN and is an Environment & Sustainability major at Sewanee: The University of the South. 
                                                  She is a member of the Order of the Gown, a site leader for the Bonner/Canale program at the university farm, a member of Theta Kappa Phi,
                                                  and plays for the women's tennis team."
                                                ))
                                )), 
                       tabPanel(title = "Project Mentor",
                                fluidRow(
                                  column(3,
                                         fluidRow(tags$img(src = "Catherine.png", width = "90%", alt = "Catherine"))),
                                  column(5, h3("Dr. Catherine Cavagnaro", align = "left")),
                                  br(),
                                  br(),
                                  br(),
                                  p("Dr. Catherine Cavagnaro is a Professor of Mathematics at Sewanee: 
                                                  The University of the South, and the project mentor for the Sewanee Wetlands Project at DataLab. 
                                                  She holds a B.S from Santa Clara University and a Ph.D. from University of Illinois 
                                                  at Urbana-Champaign.", align = "left"))),
                       tabPanel(title = "Community Partners",
                                fluidRow(
                                  column(3, 
                                         fluidRow(tags$img(src = "Deb.png", width = "100%", alt = "Deb")),
                                         fluidRow(tags$img(src = "SUD .png", width = "100%", alt = "SUD"))),
                                  column(8,
                                         h3("Dr. Deborah McGrath"),
                                         p("Dr. Deborah McGrath is a Professor of Biology and the Head Department Chair of Environment & Sustainability at Sewanee: 
                                                  The University of the South. She is also the Assistant Dean for the Sewanee Integrated Program in the Environment. 
                                                  Dr. McGrath is the project director for the Sewanee Wetland Research Station, and the community partner 
                                                  for the Sewanee Wetlands Project at DataLab.",
                                           br(),
                                           br(),
                                           br(),
                                           br(),
                                           
                                           h3("Sewanee Utility District (SUD)"),
                                           p("The Sewanee Utility District of Franklin and Marion Counties treats and manages wastewater through a lagoon and spray field system. 
                                                    They provide potable water to Sewanee, Midway, Deepwoods, and Jumpoff communities. 
                                                    The SUD has collected all data used in the Wetlands Projects' graphs and visuals.")))))
                       
                )
                
              )),
      ###################################### water quality comparison tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Variable Averages by Month",
                    solidHeader = TRUE,
                    width = 12, 
                    plotlyOutput("sond_cond"),
                    background = "navy")),
              fluidRow( 
                 box(title = "Specified Variable Average by Month",
                     solidHeader = TRUE,
                   plotlyOutput("avg_vari_site"),
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
                ))
             ,
      # TRENDS TAB -----
      tabItem(tabName = "trends",
              fluidRow(
                tabBox(
                  title = "Trends",
                  id= "trendstab",
                  width = 12,
                  tabPanel( "Monthly",
                    box(
                      title = "Monthly Trends in Variables",
                      solidHeader = TRUE,
                      width = 6,
                      plotlyOutput("trend_data"),
                      background = "blue"),
                    
                    box(
                      title = "Select Monthly:",
                      background = "blue",
                      collapsible = TRUE,
                      width = 6,
                      
                      solidHeader = TRUE,
                      
                      selectInput("year4", "Year",
                                  choices = c("2020", "2021")),
                      selectInput("site4", "Site" ,
                                  choices = c("Wetland Basin 3", "Lagoon C"),
                                  multiple = TRUE,
                                  selected = "Wetland Basin 3"),
                      selectInput("variable4", "Variable",
                                  choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                              "Temp °C", "NH4+ -N mg/L", "NH3 mg/L")),
                      # checkboxInput("average_temp", "Average Temperature"),
                      checkboxInput("avg_temp", "Max Temperature"),
                      checkboxInput("avg_temp2", "Min Temperature"),
                      checkboxInput("total_rain", "Total Rainfall"),
                      checkboxInput("solar", "Solar Measure")

                      
                    )),
                    tabPanel( "Daily",
                      box(title = "Daily Trends in Variables",
                          solidHeader = TRUE,
                          width = 6,
                          plotlyOutput("trend_data2"),
                          background = "maroon"),
                      
                      box(
                        title = "Select Daily:",
                        background = "maroon",
                        collapsible = TRUE,
                        width = 6,
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
                    
                    tabPanel( "Hourly",
                      box(title = "Hourly Trends in Variables",
                          solidHeader = TRUE,
                          width = 6,
                          plotlyOutput("trend_data3"),
                          background = "olive"),
                      box(
                        title = "Select Hourly:",
                        background = "olive",
                        width = 6,
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
                      
                      
                      
                      
                    )))),
                ################################## BOXPLOTS TAB #############################
      tabItem(tabName = "boxplots",
              fluidRow(
                box(title = "Varience of Variables by Month",
                    solidHeader = TRUE,
                    width = 12,
                  plotlyOutput("avg_boxplot"),
                  background = "teal")),
                fluidRow(
                box(
                  title = "Select:",
                  background = "teal",
                  collapsible = TRUE,
                  width = 12,
                  solidHeader = TRUE,
                  selectInput("year2", "Year",
                              choices = c("2020", "2021")),
                  selectInput("site2", "Site" ,
                              choices = c("Wetland Basin 3", "Lagoon C"), 
                              multiple = TRUE,
                              selected = "Wetland Basin 3"),
                  selectInput("variable2", "Variable",
                              choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                          "Temp °C", "NH4+ -N mg/L", "NH3 mg/L"))
      )) ),
      ############################ MODELS TAB ############################################
      tabItem(tabName = "models",
              fluidRow(
                box(title = "Descriptive Models Using 2021 Data",
                    solidHeader = TRUE,
                    width = 12,
                  plotOutput("descrip_model"),
                    background = "purple")),
              
              fluidRow(
                box(
                  title = "Select:",
                  background = "purple",
                  collapsible = TRUE,
                  width = 12,
                  
                  solidHeader = TRUE,
                  
                  selectInput("sitename3", "Site" ,
                              choices = c("Wetland Basin 3", "Lagoon C")),
                  selectInput("variable3", "Variable",
                              choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                          "Temp °C", "NH4+ -N mg/L", "NH3 mg/L"))
                ))),
      #### weather tab -----
      
      tabItem(tabName = "weather",
              fluidRow(
                tabBox(title = "Weather Data",
                       width = 12,
                       tabPanel(
                         title = "Average Rainfall",
                         plotlyOutput("weather_data1")),
                       tabPanel(
                         title = "Maximum Temperature",
                         plotlyOutput("weather_data2")
                       ),
                       tabPanel(
                         title = "Minimum Temperature",
                         plotlyOutput("weather_data3")
                       ),
                       tabPanel(
                         title = "Average Vapor Pressure Deficit",
                         plotlyOutput("weather_data4")
                       ),
                       tabPanel(
                         title = "Total Solar",
                         plotlyOutput("weather_data5")
                       ),
                       tabPanel(
                         title = "Rainfall Data Comparison",
                         plotlyOutput("rainfall_data")
                       )
                       )
  
                )),
      ###### aerator tab -----
      tabItem(
        tabName = "aer",
        fluidRow(
          box(
            title = "Aerator Installation",
            width = 12,
            plotlyOutput("aer_plot"),
            selectInput("variable7", "Variable",
                        choices = c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
                                          "Temp °C", "NH4+ -N mg/L", "NH3 mg/L")),
            selectInput("site7", "Site",
                        choices = c("Wetland Basin 3", "Lagoon C")),
            selectInput("year7", "Year",
                        choices = c(2020, 2021, 2022, 2023),
                        multiple = TRUE)
              
            )
          )
        )
      )
      
      
      ))
      
    # Boxes need to be put in a row (or column)
      
####### SERVER ###########################
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  rv <- reactiveValues()
  
  
  observe({
    
  
   })
  

  
  #Water Quality Definitions ##################################################
  output$variable10 <- renderText({
    if( input$variable10 == "Cond µS/cm"){
      "Conductivity is a measure of the ability of water to pass an electrical current. 
    Conductivity in water is affected by the presence of inorganic dissolved solids
Conductivity is also affected by temperature:
the warmer the water, the higher the conductivity.
The conductivity of rivers in the United States generally ranges from 50 to 1500 [µs/cm]."
    } else if( input$variable10 == "ORP mV" ){
      "Oxidation-reduction (redox) reactions control many chemical and biochemical processes
    in both nature and engineered systems, such as water treatment processes.
    ORP readings can provide water utility operators with beneficial water quality 
    information such as the effectiveness of disinfectant and microorganism kill rates. 
ORP values would ideally be positive. 
    Here, the solution can undergo nitrogen transformation processes and become more aerobic."
    } else if( input$variable10 == "pH"){
      "pH is an expression of hydrogen ion concentration in water. The term is used to indicate basicity or acidity 
    of a solution on a scale of 0 to 14, with pH 7 being neutral. U.S. EPA water quality criteria 
    for pH in freshwater suggest a range of 6.5 to 9."
    } else if(input$variable10 == "Turbidity NTU"){
      "Turbidity is a measure of water clarity and
    how much the material suspended in water decreases the passage of light through the water.
    Turbidity can be useful as an indicator of the effects of runoff from construction, agricultural practices, logging activity, discharges, and other sources.
Turbidity is closely related to stream flow and velocity and should be correlated with these factors."
    } else if(input$variable10 == "NitraLED mg/L"){
      "Nitrates are a form of nitrogen, which is found in several different forms in terrestrial and aquatic ecosystems.
    Nitrates are essential plant nutrients, but in excess amounts they can cause significant water quality problems. 
Together with phosphorus, nitrates in excess amounts can accelerate eutrophication, 
causing dramatic increases in aquatic plant growth and changes in the types of plants and animals that live in the stream.
Excess nitrates can cause hypoxia (low levels of dissolved oxygen) 
and can become toxic to warm-blooded animals at higher concentrations (10 mg/L) or higher) under certain conditions. In the effluent of wastewater
treatment plants, it can range up to 30 mg/L."
    } else if (input$variable10 == "ODO mg/L"){
      "Dissolved oxygen (DO) is the
    amount of oxygen that is present in water. Low levels of oxygen (hypoxia) or no oxygen levels (anoxia) 
    can occur when excess organic materials, such as large algal blooms, are decomposed by microorganisms. DO is considered an important measure of water quality 
    as it is a direct indicator of an aquatic resource’s ability to support aquatic life.
While each organism has its own DO tolerance range, generally, DO levels below 3 milligrams 
per liter (mg/L) are of concern and waters with levels below 1 mg/L are considered hypoxic and usually devoid of life."
    } else if (input$variable10 == "Temp °C"){
      "The rates of biological and chemical processes depend on temperature. Aquatic organisms from microbes to fish are dependent 
    on certain temperature ranges for their optimal health.
Temperature affects the oxygen content of the water (oxygen levels become lower as temperature increases). Like humans 
and many other living organisms, bacteria in wastewater treatment systems function best within a certain temperature range
– typically between 68 and 95 F (20 – 35 C)."
    } else if (input$variable10 == "NH4+ -N mg/L"){
      "Ammonia nitrogen includes the ionized form (ammonium, NH4+). A decrease in pH favors the ionized (NH4+) form."
    } else if(input$variable10 == "NH3 mg/L" ){
      "Ammonia (NH3) is a common toxicant derived from wastes, fertilizers, and natural processes. Ammonia nitrogen includes the unionized form (ammonia, NH3).
    An increase in pH favors formation of the more toxic unionized form (NH3). Ammonia can lead to heavy plant growth
    (eutrophication) due to its nutrient properties. EPA recommends an acute criterion magnitude of 17 mg Total Ammonia Nitrogen 
    (TAN) per liter at pH 7 and 20°C for a one-hour average duration, not to be exceeded more than once every three years on average."
    }
  })
##########################################################################
  
  
  
  output$sond_cond <- renderPlotly({
    
    new_df<-all_data%>%
      group_by(`Site Name`, month, variable)%>%
      filter(year == input$year)%>%
      filter(month == input$month)%>%
      filter(variable %in% c("Cond µS/cm", "ORP mV", "pH", "Turbidity NTU", "NitraLED mg/L", "ODO mg/L",
             "Temp °C", "NH4+ -N mg/L", "NH3 mg/L"))%>%
      summarise(avg = mean(as.numeric(value), na.rm = TRUE), 
                color = color,
                standard_min = unique(standard_min),
                standard_max = unique(standard_max)) %>% 
      mutate(color = ifelse(avg > standard_min & avg < standard_max, 'meets standards', 'does not meet standards'))
    
    ggplot(data = new_df)+
      geom_col(aes(variable, avg, fill = color), position = "dodge")+
      theme(axis.text = element_text(angle = 90))+
      labs(x = "Variable",
           y = "Measurement") + 
      scale_fill_manual(values = c(`meets standards` = "slategray2", `does not meet standards` = "yellowgreen"))+
      facet_wrap(~`Site Name`)
   
  })
  
  
  output$avg_vari_site <- renderPlotly({
    avg_vari_site<-all_data%>%
      group_by(month, year, `Site Name`)%>%
      filter(year == input$year)%>%
      filter(month == input$month)%>%
      filter(variable == input$variable)%>%
      summarise(avg = mean(as.numeric(value), na.rm = TRUE), 
                color = color,
                standard_min = unique(standard_min),
                standard_max = unique(standard_max)) %>% 
      mutate(color = ifelse(avg > standard_min & avg < standard_max, 'meets standards', 'does not meet standards'))
    
    
    ggplot(data = avg_vari_site)+
      geom_col(aes(month, avg, fill = color), position = "dodge")+
      labs(x = "Month",
           y = input$variable)+
      scale_fill_manual(values = c(`meets standards` = "slategray2", `does not meet standards` = "yellowgreen"))+
      facet_wrap(~`Site Name`)
  })
  
  output$trend_data <- renderPlotly({  
 
    ############################## month trends in server -----
    month_trend<-all_data%>%
      group_by(month, `Site Name`)%>%
      filter(year == input$year4)%>%
      filter(`Site Name` %in% input$site4)%>%
      filter(variable == input$variable4)%>%
      summarise(avg_month = mean(as.numeric(value), na.rm = TRUE), site = `Site Name`) # %>%
      # mutate(month = factor(month,
                            # levels = c("January", "February", "March",
                                       # "April", "May", "June", "July", "August", "September", "October",
                                       # "November", "December")))
    month_trend$month <- factor(month_trend$month, levels = c("January",
                                                              "February",
                                                              "March",
                                                              "April",
                                                              "May",
                                                              "June",
                                                              "July",
                                                              "August",
                                                              "September",
                                                              "October",
                                                              "November",
                                                              "December"),
                                labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    
    #opt_plots <- 
   #   if(input$average_rain){geom_bar(data = mtcars, x = mpg)}
     # else{NA}
    
    g <- ggplot()+
      geom_point(data = month_trend, aes(as.numeric(month), avg_month, color = site))+
      geom_line(data = month_trend, aes(as.numeric(month), avg_month, color = site))+
      theme(axis.text.x = element_text(angle = 90))+
      labs(x = "Month",
           y = input$variable4) +
      xlim(1, 12) +
      scale_x_continuous(breaks=seq(1, 12, 1),
                         labels = c("January", "February", "March",
                                    "April", "May", "June", "July", "August", "September", "October",
                                    "November", "December"))
    
    # Capture the show average rain checkbox
    show_total_rain <- input$total_rain 
    if(show_total_rain){
     # save(g, file = '~/Desktop/temp.RData')
      # create some fake data
      oess_totalrain2 <-oess_data2 %>% 
        mutate(month = month(dates))%>%
        group_by(month) %>% 
        filter(year(dates)==2021) %>% 
        summarise(total.rain = sum(na.rm = TRUE,(`rainfall (inches)`)))
      g <- g +
        geom_line(color = 'purple',
                  data = oess_totalrain2,
                  aes(x = month,
                      y = total.rain))
    }
    getwd()
    ### check input solar -----
    show_solar <- input$solar 
    if(show_solar){
      #save(g, file = '~/Desktop/wetlands project folder/wetlands')
      # create some fake data
      avg_solar <- sudhour%>% 
        filter(yyyy == 2021) %>% 
        group_by(mm) %>% 
        summarise(avgsolar = mean(`Solar Total (MJ/m²)`))
      g <- g +
        geom_line(color = 'blue',
                  data = avg_solar,
                  aes(x = mm,
                      y = avgsolar))
    }
    ### check input max temperature -----
    show_avg_temp <- input$avg_temp 
    if(show_avg_temp){
      #save(g, file = '~/Desktop/wetlands project folder/wetlands')
      # create some fake data
      tempmax <- oess_data2 %>% 
        mutate(month = month(dates))%>%
        group_by(month) %>% 
        filter(year(dates) == 2021) %>% 
        summarise(max.temp = mean(`High temp (F)`))
      g <- g +
        geom_line(data = tempmax, aes(x = month, y = max.temp), color= 'red')
    }
    ### check input min temperature -----
    show_avg_temp2 <- input$avg_temp2
    if(show_avg_temp2){
      #save(g, file = '~/Desktop/wetlands project folder/wetlands')
      # create some data
      tempmin <- oess_data2 %>%
        mutate(month = month(dates))%>% 
        group_by(month) %>% 
        filter(year(dates) == 2021) %>% 
        summarise(min.temp = mean(`Low temp (F)`))
      g <- g +
        geom_line(data = tempmin, aes(x = month, y = min.temp), color= 'orange')
    }

  })
  
  output$trend_data2 <- renderPlotly({
  
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
         y = input$variable5)
})
  
    output$trend_data3 <- renderPlotly({    
     
       ######################################## hourly trends
      time_attempt<-all_data %>%
        #mutate( hour = hour(`Time (HH:mm:ss)`)) %>%
       # filter(year == input$year6)%>%
        filter(variable == input$variable6)%>%
        filter(Date %in% input$date6)%>% 
        filter(`Site Name` == input$site6)
      
      ggplot(data = time_attempt, aes(x= `Time (HH:mm:ss)`, y=as.numeric(value), color = factor(Date)))+
        geom_point()+
        geom_line(aes(group = factor(Date)))+
        labs(x = "Hour of the Day",
             y = input$variable6,
             color = "Date")+
        theme(axis.text.x = element_text(angle = 90))
      
      
    
    })
  
    ##################################################################### 
  output$avg_boxplot <- renderPlotly({  
    avg_boxplot <- all_data %>%
      filter(year == input$year2) %>%
      filter(`Site Name` %in% input$site2)%>%
      filter(variable == input$variable2)%>%
      mutate(month = factor(month,
                            levels = c("January", "February", "March",
                                       "April", "May", "June", "July", "August", "September", "October",
                                       "November", "December")))
    
  
    ggplot(data = avg_boxplot, 
           aes(x = month, y = as.numeric(value), color = `Site Name`))+
      geom_boxplot()+
      theme(axis.text = element_text(angle = 90))+
      labs(x = "Month",
           y = input$variable2)
  })
    #####################################################################  
  output$descrip_model <- renderPlot({
    
    avg_boxplot <- all_data %>%
      filter(`Site Name` == input$sitename3)%>%
      filter(variable == input$variable3)%>%
      mutate(month = factor(month,
                            levels = c("January", "February", "March",
                                       "April", "May", "June", "July", "August", "September", "October",
                                       "November", "December")))
    # code for for descriptive model for turbidity
    avg_predict<- avg_boxplot %>%
      filter( year == 2021 ) %>% 
      group_by( month ) %>% 
      summarize(avg = mean(as.numeric(value)))%>%
      mutate(month = factor(month,
                            levels = c("January", "February", "March",
                                       "April", "May", "June", "July", "August", "September", "October",
                                       "November", "December")))
    
    #####################################################################   
    # descriptive model using year 2021
    ggplot( data = avg_boxplot, aes( x= (month), y = as.numeric(value)))+
      geom_jitter(alpha = .3)+
      theme(axis.text.x = element_text(angle = 90))+
      labs(
           y = input$variable3,
           x = 'Months')+
      geom_point(data = avg_predict, aes(x = month, y = avg), 
                 size = 2, color = 'red')+
      geom_line(data = avg_predict, aes(x = month, y = avg), 
                size = 0.5, color = 'blue', group =1)
    
  })
  
  output$weather_data1 <- renderPlotly({
    
    # mean and standard deviation for temp and rainfall
    
    # mean/sd/upper/lower for average rainfall
    mean_sdrain <- oess_data2 %>% 
      mutate(Month = factor(Month,
                            levels = c('Jan', 'Feb', 
                                       'Mar', 
                                       'Apr', 
                                       'May', 'Jun', 
                                       'Jul', 'Aug', 'Sep', 
                                       'Oct', 'Nov', 'Dec'))) %>% 
      group_by(Month) %>% 
      summarise(mean = mean(`rainfall (inches)`, na.rm = TRUE), sd = sd(`rainfall (inches)`, na.rm = TRUE))%>%           
      mutate(lower = mean-sd, upper = mean+sd)
    
    
    # mean/sd plot for rainfall
    ggplot()+  
      geom_col(data =mean_sdrain, aes(x = Month, y = mean),
               fill = 'orange')+
      geom_errorbar(data = mean_sdrain, aes(x = Month, ymax = mean+sd, width = .1, ymin = mean))+
      labs(title = 'Average Rainfall (2021)',
           subtitle = 'Average Rainfall (in) per Month ',
           y = 'Average Rainfall (in)',
           x = 'Months')
    
    
  })
  
  output$weather_data2 <- renderPlotly({
    
    # mean/sd/upper/lower for temp max
    
    meansd_tempmax <- oess_data2 %>% 
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
               fill = 'aquamarine2', group = 1)+
      geom_errorbar(data = meansd_tempmax, aes(x = Month, ymax = mean+sd, width = .1, ymin = mean))+
      labs(title = 'Temperature Max (2021)',
           subtitle = 'Temp Max (C) per Month ',
           y = 'Average Temperature (C)',
           x = 'Months')
    
  })
  
  output$weather_data3 <- renderPlotly({
    
    # mean/sd/upper/lower for temp min
    
    meansd_tempmin <- oess_data2 %>% 
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
               fill = 'aquamarine3', group = 1)+
      geom_errorbar(data = meansd_tempmin, aes(x = Month, ymax = mean+sd, width = .1, ymin = mean))+
      labs(title = 'Temperature Min (2021)',
           subtitle = 'Temp Min (C) per Month ',
           y = 'Average Min Temperature (C)',
           x = 'Months')
  })
    
  output$weather_data4 <- renderPlotly({
    
    # sud VPD avg
    avg_vpd<- sudhour %>% 
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
    ggplot(data = avg_vpd, aes( x = mm, y = vpdavg))+
      geom_col(fill = 'purple')+
      scale_x_continuous(
        breaks = seq_along(month.name), 
        labels = month.name)+
      theme(axis.text.x = element_text(angle = 90))+
      ylim(0,.8)+
      labs(title = "VPD Average per Month (2021)",
           subtitle = 'At Sewanee Utility District',
           y = 'Average VPD (Kpa)',
           x = 'Months')
    
 
    
    
  })
  
  output$weather_data5 <- renderPlotly({
    
    # solar total 
    avg_solar <- sudhour%>% 
      filter(yyyy == 2021) %>% 
      group_by(mm) %>% 
      summarise(avgsolar = mean(`Solar Total (MJ/m²)`))
    
    # graph of average solar total 
    ggplot(data = avg_solar, aes(x = mm, y = avgsolar)) +
      geom_col(fill = 'yellow3')+
      scale_x_continuous(
        breaks = seq_along(month.name), 
        labels = month.name)+
      theme(axis.text.x = element_text(angle = 90))+
      
      labs(title = "Solar Total Average per Month (2021)",
           subtitle = 'At Sewanee Utility District',
           y = 'Solar Total (MJ/m2)',
           x = 'Months')
  })
  
  output$rainfall_data <- renderPlotly({
    oess_totalrain <-oess_data2 %>% 
      mutate(Month = factor(Month,
                            levels = c('Jan', 'Feb', 
                                       'Mar', 
                                       'Apr', 
                                       'May', 'Jun', 
                                       'Jul', 'Aug', 'Sep', 
                                       'Oct', 'Nov', 'Dec'))) %>% 
      group_by(Month) %>% 
      filter(year(dates)==2021) %>% 
      summarise(OESS = sum(na.rm = TRUE,(`rainfall (inches)`)))
    
    # total rainfall at SUD
    sud_totalrain <- sudhour %>% 
      filter(yyyy == 2021) %>% 
      group_by(mm) %>% 
      summarise(SUD = sum(`Rain (mm)`))
    
    # code for comparison of total rainfall at sud and oess
    comparedrain <- cbind(sud_totalrain, oess_totalrain)
    
    
    comparedrain <- comparedrain %>% select(-mm)
    
    # plot of total rain at oess and sud 
    ggplot( comparedrain %>% pivot_longer(!Month), aes(x=Month, y=value, fill=name)) + 
      geom_col(position="dodge")+
      #View(comparedrain %>% pivot_longer(!Month))+
      labs(title = 'Total Rainfall per Month (2021)',
           subtitle = 'Sewanee Utility District vs. OESS',
           y = 'Total Rain (mm)', 
           x = 'Month')+
      theme(axis.text.x = element_text(angle = 90))+
      scale_fill_manual(values = c("darkolivegreen4", "lightblue3"))
  })
  
  output$aer_plot <- renderPlotly({
    ### aerator plot -----
    aerator <- all_data%>%
      group_by(month, year)%>%
      mutate(month = factor(month,
                            levels = c("January", "February", "March",
                                       "April", "May", "June", "July", "August", "September", "October",
                                       "November", "December")))%>%
      filter(`Site Name` == input$site7)%>%
      filter(variable == input$variable7)%>%
      filter(year %in% input$year7)%>% 
      summarise(avg = mean(as.numeric(value), na.rm = TRUE),
                standard_min = unique(standard_min),
                standard_max = unique(standard_max))
    
    aerator<-aerator%>%
      mutate(aerator_status = ifelse(year >= 2022, "post", "pre"))%>%
      na.omit()
      #mutate(aerator_status = ifelse(month <= "March", "post", "pre"))
    
    ggplot(data = aerator, aes(month, avg, color = year, fill= aerator_status))+
      geom_col()+
      theme(axis.text.x = element_text(angle = 90))
     # scale_fill_manual(values = c(pre = "purple", 
      #                             post = "aquamarine3"))+
      #scale_color_manual(values = c(2020 = "orange",
       #                             2021 = "dark green",
        #                            2022 = "pink", 
         #                           2023 = "red"))
      
  })
    
}

shinyApp(ui, server)
