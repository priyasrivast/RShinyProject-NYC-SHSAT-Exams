#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(rgdal)
library(leaflet)
library(googleVis)


#Loading school district file for 2016 school data
schoolDF = read.csv('./data/2016 School Explorer.csv',
                    header = TRUE,
                    stringsAsFactors = FALSE)

# loading shsat by school data
shsatDF = read.csv(
  './data/D5 SHSAT Registrations and Testers.csv',
  header = TRUE,
  stringsAsFactors = FALSE
)

#loading test offers csv
offersDF = read.csv(
  './data/2016-2017_SHSAT_Admissions_Test_Offers_By_Sending_School.csv',
  header = TRUE,
  stringsAsFactors = FALSE
)

#Loading school district breakdown, this provides data for gender and ethnicity
brkdwnDF = read.csv(
  './data/school-district-breakdowns.csv',
  header = TRUE,
  stringsAsFactors = FALSE
)



#Cleaning up SHSAT dataframe

# Rename to short meaningful column names

shsatDF = shsatDF %>% rename(Num_enrol = 'Enrollment.on.10.31',
                             Num_reg = 'Number.of.students.who.registered.for.the.SHSAT' ,
                             Num_took = 'Number.of.students.who.took.the.SHSAT')

#Number of students registered can not be greater than enrolled, this step cleans out three dirty observations
shsatDF = shsatDF %>% filter(Num_reg <= Num_enrol)

# Cleaning unwanted observations from schoolDF

schoolDF = schoolDF %>%  filter(
  Average.ELA.Proficiency != 'N/A' &
    Average.Math.Proficiency != 'N/A' &
    (Grade.High %in% c('08', '09', '10', '12'))
)


#Selecting and Cleaning column names from school explorer data frame

viewSchoolDF = schoolDF %>%
  select(
    "School Name" = School.Name,
    "Average ELA" = Average.ELA.Proficiency,
    "Average Math" = Average.Math.Proficiency,
    "Location Code" = Location.Code,
    Address = Address..Full.,
    City,
    Grades,
    "Community School" = Community.School.,
    "Economic Need Index" = Economic.Need.Index,
    "School Income Estimate" = School.Income.Estimate,
    "Student Attendance" = Student.Attendance.Rate ,
    "Students Chronically Absent" = Percent.of.Students.Chronically.Absent
  )


#Cleaning Average ELA and Average Math variables, converting character type to Numeric, NAs introduced are legitimate
# corresponding to 'N/A' entries. those NAs can be ignored while calculating mean

SchoolSmryDF = viewSchoolDF %>% mutate(Avg_ELA = as.numeric(`Average ELA`),
                                       Avg_Math = as.numeric(`Average Math`))

# Deriving the min max and avg grades to use in Info Box
avg_math_ELADF = SchoolSmryDF %>% group_by(`Community School`) %>%
  summarise(
    'Average English' = round(mean(Avg_ELA, na.rm = TRUE), 2),
    'Average Math' = round(mean(Avg_Math, na.rm = TRUE), 2),
    'Max English' = round(max(Avg_ELA, na.rm = TRUE), 2),
    'Max Math' = round(max(Avg_Math, na.rm = TRUE), 2),
    'Min English' = round(min(Avg_ELA, na.rm = TRUE), 2),
    'Min Math' = round(min(Avg_Math, na.rm = TRUE), 2)
  )

# data load . cleaning and manipulation steps for Percentage took leaflet map


offersDF = offersDF %>% rename (School.Name = Feeder.School.Name) %>% filter(Count.of.Testers != 'N/A')

schlOfrDF =  left_join(offersDF, schoolDF, by = 'School.Name')

schlOfrDF = schlOfrDF %>% mutate(Count_Testers = as.numeric(Count.of.Testers)) %>% filter(!is.na(Count_Testers))

schlOfrDF = schlOfrDF %>% mutate(PerTook = round((
  Count_Testers / Count.of.Students.in.HS.Admissions
) * 100))


mapCtyDF = schoolDF %>% select(School.Name, City)
schlOfrDF = left_join(schlOfrDF, mapCtyDF, by = 'School.Name')

# Following steps create data to populate in Leaflet tool tip on Percent takers map

bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
pal <-
  colorBin(
    c(
      "maroon",
      "red",
      "orange",
      "gray",
      "yellow",
      "purple" ,
      "navy",
      "blue" ,
      "green",
      "yellowgreen"
    ),
    domain = schlOfrDF$PerTook,
    bins = bins
  )


districtNumber <- paste(
  "<strong>City: </strong>",
  schlOfrDF$City.x,
  "<br><strong>School Name: </strong>",
  schlOfrDF$School.Name,
  "<br><strong>Total Students in HS Admissions</strong>",
  schlOfrDF$Count.of.Students.in.HS.Admissions,
  "<br><strong>Total SHSAT takers: </strong>",
  schlOfrDF$Count_Testers,
  "<br><strong>Total Offers: </strong>",
  schlOfrDF$Count.of.Offers,
  "<br><strong>Percent Takers: </strong>",
  schlOfrDF$PerTook
) %>% lapply(htmltools::HTML)

# Following steps create data to populate in Leaflet tool tip on percent of offers map

# success percent-------

offerMap = schlOfrDF

offerMap$Count.of.Offers = as.numeric(gsub(
  x = offerMap$Count.of.Offers,
  pattern = '0-5',
  replacement = 5
))

offerMap = offerMap %>% mutate(PerOffer = round((Count.of.Offers / Count_Testers) *
                                                  100))

binsOfer <- c(0, 7, 14, 21, 28, 35, 42, 70, 100)

palOfer <-
  colorBin(
    c(
      "maroon",
      "red",
      "orange",
      "yellow",
      "green",
      "yellowgreen",
      "purple" ,
      "blue",
      "navy"
    ),
    domain = offerMap$PerOffer,
    bins = binsOfer
  )

offerTooltip <- paste(
  "<strong>City: </strong>",
  offerMap$City.x,
  "<br><strong>School Name: </strong>",
  offerMap$School.Name,
  "<br><strong>Total Students in HS Admissions</strong>",
  offerMap$Count.of.Students.in.HS.Admissions,
  "<br><strong>Total SHSAT takers: </strong>",
  offerMap$Count_Testers,
  "<br><strong>Total Offers: </strong>",
  ifelse(offerMap$Count.of.Offers == 5, '0-5', offerMap$Count.of.Offers),
  "<br><strong>Percent Offers: </strong>",
  offerMap$PerOffer
) %>% lapply(htmltools::HTML)

# Following steps create data to populate in Leaflet tool tip on school details map
schoolTooltip <- paste(
  "<strong>City: </strong>",
  schlOfrDF$City.x,
  "<br><strong>School Name: </strong>",
  schlOfrDF$School.Name,
  "<br><strong>Average ELA Proficiency</strong>",
  schlOfrDF$Average.ELA.Proficiency,
  "<br><strong>Average Math Proficiency: </strong>",
  schlOfrDF$Average.Math.Proficiency,
  "<br><strong>Economic Need Index: </strong>",
  schlOfrDF$Economic.Need.Index,
  "<br><strong>Community School: </strong>",
  schlOfrDF$Community.School.
) %>% lapply(htmltools::HTML)



# Read the shape file
nydistricts <-
  readOGR(dsn = './School Districts', layer = "geo_export_96cb9211-c0fc-4b99-975e-94c20e7c39c0")

#-------------------------------------------------------------------------------#

#Following steps clean and manipulate data for Ethnicty analysis

#Deriving County from Jurisdiction

countyDF = brkdwnDF %>% mutate(County = substr(JURISDICTION.NAME, 8 , 25))

#Rename to readable colnames and ignore other and not disclosed ethnicity because we can take no action on them
EthnctyDF = countyDF %>% rename(
  Hispanic = COUNT.HISPANIC.LATINO ,
  'American Indian' = COUNT.AMERICAN.INDIAN,
  Asian = COUNT.ASIAN.NON.HISPANIC ,
  White = COUNT.WHITE.NON.HISPANIC,
  Black = COUNT.BLACK.NON.HISPANIC ,
  'Ethnicity Total' = COUNT.ETHNICITY.TOTAL ,
  PacIslander = COUNT.PACIFIC.ISLANDER
)

#Richmond has no data so filter it out
#Calculate percentage ethnicity by county

ethCountDF =      EthnctyDF %>%  filter(County != "Richmond") %>%  group_by(County) %>%
  summarise(
    Hispanic = sum(Hispanic),
    `American Indian` = sum(`American Indian`)	,
    Asian = sum(Asian),
    White = sum(White)	,
    Black = sum(Black),
    PacIslander = sum(PacIslander),
    `Ethnicity Total` = sum(`Ethnicity Total`)
  )

ethGthrDF =     gather(
  ethCountDF ,
  key = 'Ethnicity',
  value = 'Percent',
  c(Hispanic , `American Indian`, Asian, White , Black , PacIslander),
  na.rm = TRUE
)

# Following steps are for ethnicity by school
#----------------------------------------------------------------------------------#
schoolEth =  schoolDF %>% select(
  School.Name,
  City,
  Asian = Percent.Asian,
  Black = Percent.Black,
  White = Percent.White,
  Hispanic = Percent.Hispanic
)

gatherbar = gather(
  data = schoolEth,
  key = Ethnicity,
  value = Percentage,
  c('Asian', 'Black', 'White', 'Hispanic')
)

gatherbar$Percentage = as.numeric(strsplit(gatherbar$Percentage, '%'))

# List of City
schoolCity = schoolEth %>% select(City) %>% distinct()

#Following are steps for plots of economic need index
#---------------------------------------------------------------------------------#

#---------------For Bubble chart---------

schlBubDF = schlOfrDF %>% rename(
  City = City.x,
  Asian = Percent.Asian,
  Black = Percent.Black,
  White = Percent.White,
  Hispanic = Percent.Hispanic
) %>% filter(Economic.Need.Index != 'NA')


schlBubDF$Economic.Need.Index = as.numeric(schlBubDF$Economic.Need.Index, na.rm = TRUE)

gatherSchoolOfr = gather(
  data = schlBubDF,
  key = Ethnicity,
  value = Percentage,
  c('Asian', 'Black', 'White', 'Hispanic')
)

gatherSchoolOfr$Percentage = as.numeric(strsplit(gatherSchoolOfr$Percentage, '%'))

BubDF = gatherSchoolOfr %>% filter (
  City %in% c(
    "NEW YORK",
    "BRONX",
    "BROOKLYN",
    "STATEN ISLAND",
    "QUEENS VILLAGE",
    "LONG ISLAND CITY"
  )
) %>%
  select(PerTook, Economic.Need.Index, City, Ethnicity, Percentage) %>% top_n(500)



#-------------------------------------------------------------------------------#

#ui

ui <-  dashboardPage(
  dashboardHeader(title = "NYC School Stats"),
  
  
  dashboardSidebar(
    sidebarMenu(
      img (
        src = "students.jpg",
        width = "200" ,
        height = "200"
      ),
      
      menuItem(
        "Introduction",
        tabName = "intro",
        icon =  icon("list-alt")
      ),
      
      menuItem(
        "Schools Data Explorer",
        tabName = "schools",
        icon =  icon("table")
      ),
      radioButtons(
        "radio",
        label = ("Community Schools"),
        choices = list("Yes" = "Yes", "No" = "No"),
        selected = "Yes"
      ),
      
      menuItem("Geographic", tabName = "map", icon =  icon("map")),
      menuItem(
        "Ethnicity",
        tabName = "ethnicity",
        icon =  icon("flag-checkered")
      ),
      menuItem(
        "Economic Need Index",
        tabName = "economic",
        icon =  icon("hand-holding-usd")
      ),
      menuItem(
        "References",
        tabName = "references",
        icon =  icon("asterisk")
      )
      
    )
  ),
  
  
  dashboardBody(tabItems(
    tabItem(tabName = "intro",
            
            fluidRow(
              box(
                h3("NYC SHSAT Exam: Which Schools need help?"),
                tags$p(
                  "The Specialized High School Admissions Test (SHSAT) is the only criterion for admissions to 8 of the 9 New York City Specialized High Schools. The only exception is the Fiorello H. LaGuardia High School of Music & Art and Performing Arts, which requires an audition or portfolio for admission."
                ),
                tags$p(
                  "The SHSAT is administered by the New York City Department of Education and is only available to New York City residents in the 8th/9th grade. In 2016, approximately 28,000+ students took the SHSAT, and less than 20% of those students were accepted to a New York City Specialized High School."
                ),
                tags$p("More details on SHSAT exam can be found"),
                tags$a(href = "https://www.kaptest.com/shsat/what-is-the-shsat", "HERE!"),
                width = 12
              )
            ),
            
            fluidRow(
              box(
                h3("What are we trying to achieve from this app?	"),
                tags$p(
                  "Our agenda is to figure out NYC schools that need help so that their 8th graders can perform better in SHSAT. We would analyze what are the contributing factors that could lead to success/failure. Through this app user can gain insights based on data and derive conclusions to which schools need help to perform better."
                ),
                tags$p(
                  "This app could specifically benefit Nonprofit organizations like PASSNYC, HELICON ,Inc. who strive to identify talented underserved students within New York City's underperforming school districts in order to increase the diversity of students taking the SHSAT. These organizations provide free educational opportunities to underserved students to help them prepare better and score a spot in Elite High School"
                ),
                tags$br(),
                tags$br(),
                width = 12
              )
              
            ),
            fluidRow (
              box(plotOutput("PlotSHSATstats"), width = 6),
              
              box(plotOutput("PlotSchoolstats"), width = 6)
              
            )),
    tabItem(tabName = "references",
            
            fluidRow(
              box(
                strong("SHSAT References:"),
                tags$br(),
                tags$a(
                  href = "https://www.schools.nyc.gov/school-life/learning/testing/specialized-high-school-admissions-test",
                  " -> https://www.schools.nyc.gov/school-life/learning/testing/specialized-high-school-admissions-test"
                ),
                tags$br(),
                tags$a(href = "https://www.kaptest.com/shsat/what-is-the-shsat", " -> https://www.kaptest.com/shsat/what-is-the-shsat"),
                tags$br(),
                tags$br(),
                strong(
                  "Non-profit organizations who work towards providing aid to underserved students:"
                ),
                tags$br(),
                tags$a(href = "http://www.passnyc.org/", " -> http://www.passnyc.org/"),
                tags$br(),
                tags$a(href = "http://www.heliconinc.org/shsat-prep-course.html", " -> http://www.heliconinc.org/shsat-prep-course.html"),
                tags$br(),
                tags$br(),
                strong("Dataset Links:"),
                tags$br(),
                tags$a(href = "https://www.kaggle.com/passnyc/data-science-for-good#2016%20School%20Explorer.csv", " 1)	School Explorer dataset"),
                tags$br(),
                tags$a(href = "https://www.kaggle.com/passnyc/data-science-for-good#D5%20SHSAT%20Registrations%20and%20Testers.csv", " 2)	SHSAT Registration dataset"),
                tags$br(),
                tags$a(href = "https://www.kaggle.com/new-york-city/nyc-school-district-breakdowns", " 3) School District Breakdown of gender and ethnicity"),
                tags$br(),
                tags$a(href = "https://data.cityofnewyork.us/Education/2016-2017-SHSAT-Admissions-Test-Offers-By-Sending-/8ws3-956v", " 4)	Test Offers details by School"),
                tags$br(),
                strong("Racial Demographics:"),
                tags$a(href = "https://www.vox.com/2018/6/14/17458710/new-york-shsat-test-asian-protest", " -> Link to Racial Demographics"),
                tags$br(),
                tags$br(),
                strong("Future Work:"),
                tags$br(),
                tags$br(),
                tags$p("1)	Style the UI using style.css and modify the HTML look"),
                tags$p("2)	Create a custom School icon for leaflet map"),
                tags$p("3)	Analyze Gender criteria to create diversity	"),
                tags$p("4)	Apply ML techniques and revise analysis")
                
              )
            )),
    
    tabItem(
      tabName = "schools",
      
      fluidRow(
        infoBoxOutput("maxMathBox"),
        infoBoxOutput("avgMathBox"),
        infoBoxOutput("minMathBox")
      ),
      
      fluidRow(
        infoBoxOutput("maxEngBox"),
        infoBoxOutput("avgEngBox"),
        infoBoxOutput("minEngBox")
      ),
      
      fluidRow(box(DT::dataTableOutput("table"), width = 12))
    ),
    
    
    tabItem(tabName = "map",
            fluidRow(
              column(width = 9,
                     uiOutput("plotBoxUI")),
              
              column(
                width = 3,
                box(
                  title = "Select to Plot",
                  solidHeader = T,
                  width = NULL,
                  status = "info",
                  selectizeInput(
                    "geoin",
                    label = NULL,
                    choices = list(
                      "Percent test takers" = 'test_takers',
                      "Percent SHSAT Success" = 'test_offers',
                      "School Details" = 'school'
                    ),
                    selected = 'test_takers'
                  )
                )
              )
            )),
    
    
    tabItem(
      tabName = "ethnicity",
      fluidRow(
        column(
          width = 9,
          box(
            title = "Known fact: Racial Demographics by Year",
            solidHeader = T,
            status = "info",
            tags$img(
              src = "Racial.png",
              width = 400,
              height = 300
            ),
            width = NULL,
            height = "auto"
          )
          
        ),
        
        column(
          width = 9,
          box(
            title = "Analyze: Ethnicity Breakdown by County",
            solidHeader = T,
            status = "info",
            htmlOutput("PlotEthnicity"),
            width = NULL,
            height = "auto"
          )
        ),
        
        column(
          width = 3,
          box(
            title = "Select County",
            solidHeader = T,
            width = NULL,
            status = "info",
            selectizeInput(
              "countyin",
              label = NULL,
              choices = list(
                "Bronx - Bronx" = 'Bronx',
                "Brooklyn - Kings" = 'Brooklyn',
                "Manhattan - New York" = 'Manhattan',
                "Queens - Queens" = 'Queens'
              ),
              selected = 'Bronx'
            )
          )
        ),
        
        column(
          width = 9,
          box(
            title = "Analyze: Ethnicity Breakdown by City",
            solidHeader = T,
            status = "info",
            plotOutput("PlotEthnAllSchool"),
            width = NULL,
            height = "auto"
          )
        ),
        
        
        column(
          width = 3,
          box(
            title = "Select City",
            solidHeader = T,
            width = NULL,
            status = "info",
            selectizeInput(
              "cityin",
              label = NULL,
              choices = unique(schoolEth$City),
              selected = unique(schoolEth$City)[1]
            )
          )
        ),
        
        column(
          width = 9,
          box(
            title = "Analyze: Ethnicity Breakdown by City and School",
            solidHeader = T,
            status = "info",
            htmlOutput("gvisBarPlot"),
            width = NULL,
            height = "auto"
          )
        ),
        
        column(width = 3,
               uiOutput("schoolNmBoxUI"))
        
        
        
      )
    ),
    
    tabItem(tabName = "economic",
            fluidRow(
              column(
                width = 6,
                box(
                  title = "Economic Need Index Density by Ethnicity",
                  solidHeader = T,
                  status = "info",
                  plotOutput("Plotindex"),
                  width = NULL,
                  height = "auto"
                )
              ),
              column(
                width = 6,
                box(
                  title = "Percentage Attempted SHSAT by Economic Need Index and Ethnicity",
                  solidHeader = T,
                  status = "info",
                  plotOutput("PlotindexPerTook"),
                  width = NULL,
                  height = "auto"
                )
              ),
              column(
                width = 12,
                box(
                  title = "4D Relationship between Economic Need Index, Percentage Attempted SHSAT, City and Ethnicity",
                  solidHeader = T,
                  status = "info",
                  htmlOutput("gvisBubbleCorr"),
                  width = NULL,
                  height = "auto"
                )
              )
              
              
            ))
    
  ))
)



# Define server logic
server <- function(input, output, session) {
  output$PlotSHSATstats <- renderPlot({
    shsatPerc = shsatDF %>% group_by(Year.of.SHST) %>% summarise(
      Num_enrol = sum(Num_enrol),
      Num_reg = sum(Num_reg),
      Num_took = sum(Num_took)
    ) %>%
      mutate(
        Perc_reg = (Num_reg / Num_enrol) * 100,
        Perc_took = (Num_took / Num_enrol) * 100
      )
    
    SHPercDF = gather(
      data = shsatPerc,
      key = Reg_took,
      value = Percentage,
      Perc_reg:Perc_took
    )
    
    ggplot(data = SHPercDF, aes(x = Year.of.SHST , y = Percentage)) + geom_bar(aes(fill =
                                                                                     Reg_took), stat = 'identity', position = 'dodge') +
      labs(x = "Year of SHSAT", y = "Percentage of students" , title = "Statistics of Students appearing in SHSAT exams") +
      geom_label(label = paste0(round(SHPercDF$Percentage), '%'), nudge_x = 0.15) +
      theme_economist() + scale_fill_discrete(name = '', labels = c('Registered', 'Took'))
    
  })
  
  output$PlotSchoolstats <- renderPlot({
    schoolSHSAT = shsatDF %>% filter(Year.of.SHST == 2016 &
                                       Grade.level == 8) %>%
      mutate(per_reg = round((Num_reg / Num_enrol) * 100),
             PercentageTook = round((Num_took / Num_enrol) * 100))
    
    
    ggplot(data = schoolSHSAT, aes(x = reorder(School.name, PercentageTook), y = PercentageTook)) + geom_bar(aes(fill = PercentageTook) , stat =
                                                                                                               'identity') +
      coord_flip(ylim = c(0, 60)) + labs(x = "School Name", y = "% of students who took SHSAT in 2016 " , title = "You can get it only if you TOOK it") +
      geom_label(label = paste0(schoolSHSAT$PercentageTook, '%'))
    
    
  })
  
  output$maxMathBox <- renderInfoBox({
    max_math_value = avg_math_ELADF %>% filter(`Community School` == input$radio) %>% select(`Max Math`)
    infoBox('Max Math',
            max_math_value,
            icon = icon("hand-o-up"),
            color = "green")
  })
  
  output$avgMathBox <- renderInfoBox({
    avg_math_value = avg_math_ELADF %>% filter(`Community School` == input$radio) %>% select(`Average Math`)
    infoBox('Avg Math',  avg_math_value,  icon = icon("arrows-alt-h"))
  })
  
  output$minMathBox <- renderInfoBox({
    min_math_value = avg_math_ELADF %>% filter(`Community School` == input$radio) %>% select(`Min Math`)
    infoBox('Min Math',
            min_math_value,
            icon = icon("hand-o-down") ,
            color = "red")
  })
  
  output$maxEngBox <- renderInfoBox({
    max_eng_value = avg_math_ELADF %>% filter(`Community School` == input$radio) %>% select(`Max English`)
    infoBox('Max ELA',
            max_eng_value,
            icon = icon("hand-o-up"),
            color = "green")
  })
  
  output$avgEngBox <- renderInfoBox({
    avg_eng_value = avg_math_ELADF %>% filter(`Community School` == input$radio) %>% select(`Average English`)
    infoBox('Avg ELA',  avg_eng_value,  icon = icon("arrows-alt-h"))
  })
  
  output$minEngBox <- renderInfoBox({
    min_eng_value = avg_math_ELADF %>% filter(`Community School` == input$radio) %>% select(`Min English`)
    infoBox('Min ELA',
            min_eng_value,
            icon = icon("hand-o-down") ,
            color = "red")
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(
      data = viewSchoolDF %>% filter(`Community School` == input$radio),
      options = list(scrollX = TRUE)
    )
  })
  
  
  
  
  
  # Switching maps based on input, creating run time headers and updating lists based on user input
  observe({
    if (input$geoin == "test_takers") {
      output$percent_map <- renderLeaflet({
        leaflet(data = nydistricts) %>%
          addProviderTiles("Esri.WorldStreetMap") %>%
          addPolygons(
            fillColor = ~ pal(schlOfrDF$PerTook),
            fillOpacity = 0.7,
            weight = 1,
            label = districtNumber,
            highlight = highlightOptions(
              weight = 5,
              fillOpacity = 0.7,
              bringToFront = TRUE
            )
          ) %>%
          addLegend(
            pal = pal,
            values = ~ schlOfrDF$PerTook,
            opacity = 0.7,
            title = "Percent takers",
            position = "bottomright"
          )
        
      })
      
      output$plotBoxUI <- renderUI({
        box(
          title = "Percent of student population taking the SHSAT",
          solidHeader = T,
          status = "info",
          leafletOutput("percent_map", height = 500),
          width = NULL,
          height = "auto"
        )
      })
      
    } else if (input$geoin == "test_offers") {
      output$percent_map <- renderLeaflet({
        leaflet(data = nydistricts) %>%
          addProviderTiles("Esri.WorldStreetMap") %>%
          addPolygons(
            fillColor = ~ palOfer(offerMap$PerOffer),
            fillOpacity = 0.7,
            weight = 1,
            label = offerTooltip,
            highlight = highlightOptions(
              weight = 5,
              fillOpacity = 0.7,
              bringToFront = TRUE
            )
          ) %>%
          addLegend(
            pal = palOfer,
            values = ~ offerMap$PerOffer,
            opacity = 0.7,
            title = "SHSAT Success Percentage",
            position = "bottomright"
          )
        
        
      })
      
      output$plotBoxUI <- renderUI({
        box(
          title = "Percent of SHSAT offers by school",
          solidHeader = T,
          status = "info",
          leafletOutput("percent_map", height = 500),
          width = NULL,
          height = "auto"
        )
      })
      
    } else if (input$geoin == "school") {
      output$percent_map <- renderLeaflet({
        leaflet(schlOfrDF) %>%
          addProviderTiles("Esri.WorldStreetMap") %>%
          addAwesomeMarkers(
            ~ Longitude,
            ~ Latitude,
            label = schoolTooltip,
            popup = ~ Address..Full.,
            clusterOptions = markerClusterOptions()
          )
      })
      
      output$plotBoxUI <- renderUI({
        box(
          title = "School Clusters: Click to zoom, mouse over for detailed school info",
          solidHeader = T,
          status = "info",
          leafletOutput("percent_map", height = 500),
          width = NULL,
          height = "auto"
        )
      })
      
    }
    
    output$PlotEthnicity <- renderGvis({
      Pie_DF = ethGthrDF %>% select (Ethnicity, Percent, County)  %>% filter(County ==  input$countyin)
      
      gvisPieChart(Pie_DF,
                   options = list(title = input$countyin, is3D = TRUE))
      
      
    })
    
    output$schoolNmBoxUI <- renderUI({
      box(
        title = "Select School",
        solidHeader = T,
        width = NULL,
        status = "info",
        selectizeInput(
          "schoolin",
          label = NULL,
          choices = unique(
            schoolEth %>%
              filter(schoolEth$City == input$cityin) %>%
              select(School.Name)
          ),
          selected = unique(
            schoolEth %>%
              filter(schoolEth$City == input$cityin) %>%
              select(School.Name)
          )[1]
          
        )
      )
    })
    
    
    output$gvisBarPlot <- renderGvis({
      schoolbar = gatherbar %>% filter(School.Name == input$schoolin)
      
      gvisBarChart(
        schoolbar ,
        xvar = 'Ethnicity',
        options = list(
          bar = "{groupWidth: '80%'}",
          hAxis = "{title:'Ethnicity Percentage (by School)', format: 'short', scaleType: 'log'}",
          animation = "{startup: true}",
          legend = 'None'
        )
      )
      
      
    })
    
    output$PlotEthnAllSchool <- renderPlot({
      Eth_by_City = gatherbar %>% filter(City == input$cityin)
      
      set.seed(1)
      
      ggplot(Eth_by_City %>% filter (School.Name %in% c(
        sample(Eth_by_City$School.Name, 20, replace = TRUE)
      )))  +
        geom_col(aes(x = reorder(School.Name,Percentage), y = Percentage, fill = Ethnicity),
                 position = 'dodge') +
        coord_flip() + labs(x = 'School Name')
      
    })
    
    
    output$Plotindex <- renderPlot({
      ggplot(BubDF) + geom_density(aes(x = Economic.Need.Index, color = Ethnicity)) +
        labs(x = 'Economic Need Index')
      
    })
    
    output$PlotindexPerTook <- renderPlot({
      ggplot(BubDF) + geom_col(aes(x = Economic.Need.Index, y = PerTook, fill = Ethnicity),
                               position = 'dodge') +
        labs(x = 'Economic Need Index', y = 'Percentage Attempted SHSAT')
      
    })
    
    output$gvisBubbleCorr <- renderGvis({
      BubDF[BubDF$City == 'NEW YORK', ] = BubDF %>% filter(City == 'NEW YORK') %>% mutate(City = 'NY')
      BubDF[BubDF$City == 'BROOKLYN', ] = BubDF %>% filter(City == 'BROOKLYN') %>% mutate(City = 'BRK')
      BubDF[BubDF$City == 'BRONX', ] = BubDF %>% filter(City == 'BRONX') %>% mutate(City = 'BX')
      BubDF[BubDF$City == 'STATEN ISLAND', ] = BubDF %>% filter(City == 'STATEN ISLAND') %>% mutate(City = 'SI')
      
      gvisBubbleChart(
        BubDF,
        idvar = "City",
        xvar =  "Economic.Need.Index",
        yvar = "PerTook",
        colorvar = "Ethnicity",
        sizevar = "Percentage",
        options = list(
          title = "Economic Need Index vs Percentage Attempted SHSAT",
          hAxis = '{minValue:0,
          maxValue:1}',
          width =  1000,
          height = 1000,
          colorAxis =
            "{colors: ['lightblue', 'blue']}"
        ),
        chartid = "Bubble_Chart_colour_Axis"
      )
      
    })
    
  })
  
}

shinyApp(ui, server)

