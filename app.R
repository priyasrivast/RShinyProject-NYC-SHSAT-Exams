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
library(plotly)

source("load_files.R")



#Loading school district file for 2016 school data

schoolDF = load_file_to_DF("2016 School Explorer.csv")

# loading shsat by school data

shsatDF = load_file_to_DF("D5 SHSAT Registrations and Testers.csv")

#loading test offers csv

offersDF = load_file_to_DF("2016-2017_SHSAT_Admissions_Test_Offers_By_Sending_School.csv")

#Loading school district breakdown, this provides data for gender and ethnicity

brkdwnDF = load_file_to_DF("school-district-breakdowns.csv")

#Cleaning up SHSAT dataframe

# Rename to short meaningful column names
#Number of students registered can not be greater than enrolled, this step cleans out three dirty observations

shsatDF = shsatDF %>% rename(Num_enrol = 'Enrollment.on.10.31',
                             Num_reg = 'Number.of.students.who.registered.for.the.SHSAT' ,
                             Num_took = 'Number.of.students.who.took.the.SHSAT') %>% filter(Num_reg <= Num_enrol)


# Cleaning unwanted observations from schoolDF

schoolDF = schoolDF %>%  filter(
  Average.ELA.Proficiency != 'N/A' &
    Average.Math.Proficiency != 'N/A' &
    (Grade.High %in% c('08', '09', '10', '12'))
) %>% mutate(Economic.Need.Index = as.numeric(Economic.Need.Index))



# data load, cleaning and manipulation steps for Percentage took leaflet map

offersDF = offersDF %>% rename (School.Name = Feeder.School.Name) %>% filter(Count.of.Testers != 'N/A')

schlOfrDF =  left_join(offersDF, schoolDF, by = 'School.Name')

schlOfrDF = schlOfrDF %>% mutate(
  Count_Testers = as.numeric(Count.of.Testers),
  Average.Math.Proficiency = as.numeric(Average.Math.Proficiency),
  Average.ELA.Proficiency = as.numeric(Average.ELA.Proficiency)
) %>%
  filter(!is.na(Count_Testers))

schlOfrDF = schlOfrDF %>% mutate(PerTook = round((
  Count_Testers / Count.of.Students.in.HS.Admissions
) * 100))

# Following steps create data to populate in Leaflet tool tip on percent takers and offers map
# schlOfrDF is being saved in offerMap for data wrangling, schlOfrDF is left isolated of this process

offerMap = schlOfrDF

offerMap$Count.of.Offers = as.numeric(gsub(
  x = offerMap$Count.of.Offers,
  pattern = '0-5',
  replacement = 5
))

offerMap = offerMap %>% mutate(PerOffer = round((Count.of.Offers / Count_Testers) *
                                                  100))

mapDataDF = offerMap %>% filter(!is.na(City)) %>% group_by(City) %>%
  summarise(
    avg_ENI = round(mean(Economic.Need.Index), 4),
    avg_math = round(mean(Average.Math.Proficiency), 2),
    avg_ELA = round(mean(Average.ELA.Proficiency), 2),
    avg_perTook = round(mean(PerTook), 2),
    avg_perOffer = round(mean(PerOffer), 2)
  )


bins = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)
pal =
  colorBin(
    c(
      "maroon",
      "red",
      "orange",
      "yellow",
      "purple" ,
      "navy",
      "blue" ,
      "green",
      "yellowgreen"
    ),
    domain = mapDataDF$avg_perTook,
    bins = bins
  )

labelPerTook = paste(
  "<strong>City: </strong>",
  mapDataDF$City,
  "<br><strong>Percent Takers: </strong>",
  mapDataDF$avg_perTook,
  "<br><strong>Economic Need Index: </strong>",
  mapDataDF$avg_ENI,
  "<br><strong>Average Math Proficiency: </strong>",
  mapDataDF$avg_math,
  "<br><strong>Average English Proficiency: </strong>",
  mapDataDF$avg_ELA
) %>% lapply(htmltools::HTML)


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
    domain = mapDataDF$avg_perOffer,
    bins = binsOfer
  )

offerTooltip <- paste(
  "<strong>City: </strong>",
  mapDataDF$City,
  "<br><strong>Percent SHSAT Success: </strong>",
  mapDataDF$avg_perOffer,
  "<br><strong>Economic Need Index: </strong>",
  mapDataDF$avg_ENI,
  "<br><strong>Average Math Proficiency: </strong>",
  mapDataDF$avg_math,
  "<br><strong>Average English Proficiency: </strong>",
  mapDataDF$avg_ELA
) %>% lapply(htmltools::HTML)


# Following steps create data to populate in Leaflet tool tip on school details map


schoolTooltip <- paste(
  "<strong>City: </strong>",
  offerMap$City,
  "<br><strong>School Name: </strong>",
  offerMap$School.Name,
  "<br><strong>Average ELA Proficiency</strong>",
  offerMap$Average.ELA.Proficiency,
  "<br><strong>Average Math Proficiency: </strong>",
  offerMap$Average.Math.Proficiency,
  "<br><strong>Percent Takers: </strong>",
  offerMap$PerTook,
  "<br><strong>Percent SHSAT Success: </strong>",
  offerMap$PerOffer,
  "<br><strong>Economic Need Index: </strong>",
  offerMap$Economic.Need.Index,
  "<br><strong>Community School: </strong>",
  offerMap$Community.School.
) %>% lapply(htmltools::HTML)



# Read the shape file
nydistricts <-
  readOGR(dsn = './School Districts', layer = "geo_export_96cb9211-c0fc-4b99-975e-94c20e7c39c0")

#-------------------------------------------------------------------------------#


#Selecting and Cleaning column names from school explorer data table

viewSchoolDF = offerMap %>%  filter(
  Average.ELA.Proficiency != 'N/A' &
    Average.Math.Proficiency != 'N/A' &
    (Grade.High %in% c('08', '09', '10', '12'))
) %>%
  select(
    "School Name" = School.Name,
    "Average ELA" = Average.ELA.Proficiency,
    "Average Math" = Average.Math.Proficiency,
     City,
     "Community School" = Community.School.,
    "Total HS Admissions"  = Count.of.Students.in.HS.Admissions,
    "Total SHSAT Takers" = Count_Testers,
    "Total Offers" = Count.of.Offers,
    "Percentage Took SHSAT" = PerTook,
    "SHSAT Success Rate" = PerOffer,
    "Economic Need Index" = Economic.Need.Index,
    Grades,
    "Location Code" = Location.Code,
     Address = Address..Full.,
    "School Income Estimate" = School.Income.Estimate,
    "Student Attendance" = Student.Attendance.Rate ,
    "Students Chronically Absent" = Percent.of.Students.Chronically.Absent
  ) 


viewSchoolDF = viewSchoolDF %>% mutate( `Total Offers`  =  ifelse(`Total Offers` == 5, '0-5', `Total Offers`))

#Cleaning Average ELA and Average Math variables, converting character type to Numeric, NAs introduced are legitimate
# corresponding to 'N/A' entries. those NAs can be ignored while calculating mean


# Deriving the min max and avg grades to use in Info Box
avg_math_ELADF = viewSchoolDF %>% group_by(`Community School`) %>%
  summarise(
    'Average English' = round(mean(`Average ELA`, na.rm = TRUE), 2),
    'Average Math' = round(mean(`Average Math`, na.rm = TRUE), 2),
    'Max English' = round(max(`Average ELA`, na.rm = TRUE), 2),
    'Max Math' = round(max(`Average Math`, na.rm = TRUE), 2),
    'Min English' = round(min(`Average ELA`, na.rm = TRUE), 2),
    'Min Math' = round(min(`Average Math`, na.rm = TRUE), 2)
  )

#--------------------------------------------------------------------------------------------------------#

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

#---------------Data Prep For Bubble chart---------

schlBubDF = schlOfrDF %>% rename(
  City = City,
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


# Data frame to be used for analysing Community school vs Economic need Index wrt Percentage too and Percentage Offers

CommDF = offerMap %>% filter(!is.na(Community.School.)) 
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
      
      menuItem(
        "Community",
        tabName = "community",
        icon =  icon("church")
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
        "So Which Schools?",
        tabName = "findSchools",
        icon =  icon("graduation-cap")
      ),
      menuItem(
        "References",
        tabName = "references",
        icon =  icon("asterisk")
      )
      
    )
  ),
  
  
  dashboardBody(
    tabItems(
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
                  strong("My Shiny Project code and blog:"),
                  tags$br(),
                  tags$a(
                    href = "https://github.com/priyasrivast/RShinyProject-NYC-SHSAT-Exams","-> Link to GitHub"),
                  tags$br(),
                  tags$a(
                    href = "https://nycdatascience.com/blog/student-works/data-science-for-good-lets-identify-which-schools-needs-help/","-> Link to my Blog"),
                  tags$br(),
                  tags$br(),
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
                  tags$br(),
                  strong("Racial Demographics:"),
                  tags$br(),
                  tags$a(href = "https://www.vox.com/2018/6/14/17458710/new-york-shsat-test-asian-protest", " -> Link to Racial Demographics"),
                  tags$br(),
                  tags$br(),
                  strong("Future Work:"),
                  tags$br(),
                  tags$p("1)	Style the UI using style.css and modify the HTML look"),
                  tags$p("2)	Create a custom School icon for leaflet map"),
                  tags$p("3)	Analyze Gender criteria to create diversity	"),
                  tags$p("4)	Apply ML techniques and revise analysis")
                  
                ),
                
                box( 
                  tags$h4(strong("Appendix:")),
                  tags$br(),
                  strong("Economic Need Index:"),
                  tags$br(),
                  tags$br(),
                  tags$p("Economic Need Index is calculated as:"),
                  tags$p("(%temp housing) + (% HRA eligible *0.5) + (% free lunch eligible *0.5)"),
                  tags$p("The higher the index, the higher the need."),
                  tags$br(),
                  strong("Community School:"),
                  tags$br(),
                  tags$br(),
                  tags$p("The term 'community school' refers to a type of publicly funded school in the United States that serves as both an educational institution and a center of community life. A community school is both a place and a set of partnerships between the school and other community resources."),
                  tags$p("Any school can be a community school. This includes regular public schools, charter schools, magnet schools, parochial schools and private schools. However, most existing community schools are public schools."),
                  tags$img(src = 'comm.png')
                  )
              )
              
   ),
      
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
              plotlyOutput("PlotEthnAllSchool"),
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
                choices = schoolEth %>% select(City) %>% distinct() %>% arrange(City),
                selected = 'BRONX'
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
                    plotlyOutput("PlotindexPerTook"),
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
                
                
              )),
      
      tabItem(
        tabName = "community",
        fluidRow(column(
          width = 6,
          box(
            title = "Do we have as many Community Schools as Non Community?",
            solidHeader = T,
            status = "info",
            plotOutput("PlotComunCnt"),
            width = NULL,
            height = "auto"
          )
        ),
        column(
          width = 6,
          box(
            title = "Which kind of School has more needy students?",
            solidHeader = T,
            status = "info",
            plotOutput("PlotComunBox"),
            width = NULL,
            height = "auto"
          )
        )),
        fluidRow(column(
          width = 6,
          box(
            title = "Who is making an attempt to SHSAT and who is not?",
            solidHeader = T,
            status = "info",
            plotOutput("PlotComTakers"),
            width = NULL,
            height = "auto"
          )
        ),
        column(
          width = 6,
          box(
            title = "Who has higher Success rate?",
            solidHeader = T,
            status = "info",
            plotOutput("PlotComOffers"),
            width = NULL,
            height = "auto"
          )
        ))
        
        
      ),
      tabItem(tabName = "findSchools",
              fluidRow(
                column(
                  width = 3,
                  box(
                    title = "Select City",
                    solidHeader = T,
                    width = NULL,
                    status = "info",
                    selectizeInput(
                      "schoolcityin",
                      label = NULL,
                      choices = schoolEth %>% select(City) %>% distinct() %>% arrange(City),
                      selected = 'BRONX'
                    )
                  )
                ),
                
                #*****************************************************************
                column(
                  width = 3,
                  box(
                    title = "Economic Need Index Range",
                    solidHeader = T,
                    width = NULL,
                    status = "info",
                    sliderInput(
                      "sliderEconomic",
                      label = '',
                      min = 0,
                      max = 1,
                      value = c(0.7, 1)
                    )
                    
                  )
                ),
                column(
                  width = 3,
                  box(
                    title = "Percent Takers",
                    solidHeader = T,
                    width = NULL,
                    status = "info",
                    sliderInput(
                      "sliderTakers",
                      label = '',
                      min = 0,
                      max = 100,
                      value = c(0, 40)
                    )
                    
                  )
                ),
                column(
                  width = 3,
                  box(
                    title = "Percent SHSAT Success",
                    solidHeader = T,
                    width = NULL,
                    status = "info",
                    sliderInput(
                      "sliderSuccess",
                      label = '',
                      min = 0,
                      max = 100,
                      value = c(0, 20)
                    )
                    
                  )
                )
                
              ),
              
              #fluidRow(box(DT::dataTableOutput("tableNeedySchools"), width = 12))
              
              fluidRow( uiOutput("needySchoolsBoxUI"), width = 12)
      
              
              
              )
    )
  ))




# Define server logic
server <- function(input, output, session) {
  
  #************************************************************************
  # You can access the value of the widget with input$slider1, e.g.
  output$value <- renderPrint({ input$slider1 })
  
  # You can access the values of the second widget with input$slider2, e.g.
  output$range <- renderPrint({ input$slider2[1] })
  
  #************************************************************************
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
  
  
  output$Plotindex <- renderPlot({
    ggplot(BubDF) + geom_density(aes(x = Economic.Need.Index, color = Ethnicity)) +
      labs(x = 'Economic Need Index')
    
  })
  
  output$PlotindexPerTook <- renderPlotly({
    ggplot(BubDF) + geom_col(aes(x = Economic.Need.Index, y = PerTook, fill = Ethnicity),
                             position = 'dodge') +
      labs(x = 'Economic Need Index', y = 'Percentage Attempted SHSAT')
    
  })
  
  output$gvisBubbleCorr <- renderGvis({
    BubDF[BubDF$City == 'NEW YORK', ] = BubDF %>% filter(City == 'NEW YORK') %>% mutate(City = 'NY')
    BubDF[BubDF$City == 'BROOKLYN', ] = BubDF %>% filter(City == 'BROOKLYN') %>% mutate(City = 'BRK')
    BubDF[BubDF$City == 'BRONX', ] = BubDF %>% filter(City == 'BRONX') %>% mutate(City = 'BX')
    BubDF[BubDF$City == 'STATEN ISLAND', ] = BubDF %>% filter(City == 'STATEN ISLAND') %>% mutate(City = 'SI')
    BubDF[BubDF$City == 'QUEENS VILLAGE', ] = BubDF %>% filter(City == 'QUEENS VILLAGE') %>% mutate(City = 'QNS')
    BubDF[BubDF$City == 'LONG ISLAND CITY', ] = BubDF %>% filter(City == 'LONG ISLAND CITY') %>% mutate(City = 'LI')
    
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
  
  output$PlotComunBox <- renderPlot({
    #shows economic need index distribution for community and non community
    CommDF %>%
      ggplot(aes(x = Community.School. , y = Economic.Need.Index)) + 
      geom_boxplot(aes(fill = Community.School. ))+
      labs(x= 'Community School',y = 'Economic Need Index'  )+
      scale_fill_discrete(name = 'Community School')
    
  })
  
  output$PlotComunCnt <- renderPlot({
    # School Volume by community and non community
    CommDF %>%
       ggplot(aes(x = Community.School.)) +
      geom_bar(aes(fill = Community.School. )) + labs(x= 'Community School' )+
      scale_fill_discrete(name = 'Community School')
    
  })
  
  output$PlotComTakers <- renderPlot({
    # Takers by Economic Need Index for Coomunity/non community
    CommDF %>%
      ggplot(aes(x= Economic.Need.Index, y=PerTook))+
      geom_point(aes(color = Community.School. ))+
      labs(x = 'Economic Need Index' , y= 'Percentage SHSAT Takers' ) 
    
  })
  
  output$PlotComOffers <- renderPlot({
    # Offer by Economic Need Index for Community/non community
    CommDF %>%
      ggplot(aes(x= Economic.Need.Index, y= PerOffer))+
      geom_point(aes(color = Community.School. ))+
      labs(x = 'Economic Need Index' , y= 'Percentage SHSAT Offers' ) 
    
  })
  
  
  # Switching maps based on input, creating run time headers and updating lists based on user input
  observe({
    if (input$geoin == "test_takers") {
      output$percent_map <- renderLeaflet({
      
        leaflet(data = nydistricts) %>%
          addProviderTiles("Esri.WorldStreetMap") %>%
          addPolygons(
            fillColor = ~ pal(mapDataDF$avg_perTook),
            fillOpacity = 0.7,
            weight = 1,
            label = labelPerTook,
            highlight = highlightOptions(
              weight = 5,
              fillOpacity = 0.7,
              bringToFront = TRUE
            )
          ) %>%
          addLegend(
            pal = pal,
            values = ~ mapDataDF$avg_perTook,
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
            fillColor = ~ palOfer(mapDataDF$avg_perOffer),
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
            values = ~ mapDataDF$avg_perOffer,
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
        leaflet(offerMap) %>%
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
    
    output$PlotEthnAllSchool <- renderPlotly({
      Eth_by_City = gatherbar %>% filter(City == input$cityin)
      
      set.seed(1)
      
      ggplot(Eth_by_City %>% filter (School.Name %in% c(
        sample(Eth_by_City$School.Name, 20, replace = TRUE)
      )))  +
        geom_col(aes(x = reorder(School.Name,Percentage), y = Percentage, fill = Ethnicity),
                 position = 'dodge') +
        coord_flip() + labs(x = 'School Name')
      
    })
    
    output$tableNeedySchools <- DT::renderDataTable({
      DT::datatable(
        data = offerMap %>% filter((City == input$schoolcityin) &
                                          (PerTook >= input$sliderTakers[1] & PerTook <= input$sliderTakers[2])&
                                          (Economic.Need.Index >= input$sliderEconomic[1] &
                                             Economic.Need.Index <= input$sliderEconomic[2])&
                                          (PerOffer >= input$sliderSuccess[1] &
                                             PerOffer <= input$sliderSuccess[2])) %>%
          arrange(PerTook) %>% 
          select(
            "School Name" = School.Name,
            Address = Address..Full.,
            "Percent Takers" = PerTook ,
            "Percent SHSAT Success" = PerOffer,
            "Economic Need Index" = Economic.Need.Index,
            Asian = Percent.Asian,
            Black = Percent.Black,
            Hispanic = Percent.Hispanic,
            White = Percent.White
          ) ,
        options = list(scrollX = TRUE)
      )
    })
    
    
    output$needySchoolsBoxUI <- renderUI({
      box(
        title = "Here are the schools which needs help with SHSAT prep (sorted by Percentage of SHSAT takers in 2016)",
        solidHeader = T,
        status = "info",
        DT::dataTableOutput("tableNeedySchools"),
        width = NULL,
        height = "auto"
      )
    })

    
  })
  
}

shinyApp(ui, server)

