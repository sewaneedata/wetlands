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




########## combine the data sets ################## ----
all_data<-rbind(lagoonC2, sond2)

all_data%>%
  mutate(month = factor(month,
                        levels = c("January", "February", "March",
                                   "April", "May", "June", "July", "August", "September", "October",
                                   "November", "December")))
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


# OESS Precipitation and Temp data   ------
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
      menuItem("Weather Data", tabName = "weather", icon = icon("sun"))
      
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
                  #Project Sumary
                  tabPanel(title = "Project Summary"),
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
                           fluidRow(column(3,
                                           fluidRow(tags$img(src = "watertank.png", width = "100%", alt = "watertank")),
                                           fluidRow(tags$img(src = "lagoonc.png", width = "100%", alt = "lagoonc")),
                                           fluidRow(tags$img(src = "wetlandbasin3.png", width = "100%", alt = "wetlandbasin3")),
                                           fluidRow(tags$img(src = "sprayfields.png", width = "100%", alt = "sprayfields")),
                                           fluidRow(tags$img(src = "pickerelweed.png", width = "100%", alt = "pickerelweed"))))
                )
              )),
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
      # TRENDS TAB
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
                      checkboxInput("average_rain", "Average Rainfall"),
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
                  plotOutput("predic_model"),
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
 
    ############################## month trends
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
    
    ggplot()+
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
  })

    ############################## SUD DATA 
  aver_temp <- renderPlotly({
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
  })

    # total rainfall per month     # YES
  total_rain  <- renderPlotly({
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
  })
    
    # avg rainfall per month   YES
    avg_rain <- renderPlotly({
      
   
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
      summarise( avgrain = mean(na.rm = TRUE,(`rainfall (inches)`)))
  
    
    # average rainfall per month plot
  average_rain  <-ggplot()+  
      geom_col(data =mean_sdrain, aes(x = Month, y = mean),
               fill = 'blue', group = 1)+
      geom_errorbar(data = mean_sdrain, aes(x = Month, ymax = mean+sd, width = .1, ymin = mean))+
      labs(title = 'Average Rainfall (2021)',
           subtitle = 'Average Rainfall (in) per Month ',
           y = 'Average Rainfall (in)',
           x = 'Months')
    
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
        mutate( hour = hour(`Time (HH:mm:ss)`)) %>%
       # filter(year == input$year6)%>%
        filter(variable == input$variable6)%>%
        filter(Date %in% input$date6)%>% 
        filter(`Site Name` == input$site6)
      
      ggplot(data = time_attempt, aes(x=hour, y=as.numeric(value), color = factor(Date)))+
        geom_point()+
        geom_line()+
        labs(x = "Hour of the Day",
             y = input$variable6,
             color = "Date")
      
      
    
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
    avg_predict <- avg_boxplot %>%
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
  
  
}

shinyApp(ui, server)
