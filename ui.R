suppressPackageStartupMessages(library(leaflet))             # Allows display spatial data
suppressPackageStartupMessages(library(RColorBrewer))        # For additional color samples
suppressPackageStartupMessages(library(scales))              # For additonal scales   
suppressPackageStartupMessages(library(lattice))             # Plotting 
suppressPackageStartupMessages(library(dplyr))               # Easier data table format changes
suppressPackageStartupMessages(library(shiny))               # Allows creating web based reactive interfaces
suppressPackageStartupMessages(library(raster))              # Allows for working with raster data sets
suppressPackageStartupMessages(library(rgdal))               # Allows for working with spatial data such as shape files
suppressPackageStartupMessages(library(shinyjs))             # Additional functionality for web based interfaces
suppressPackageStartupMessages(library(shinyBS))             # Additional functionality for web based interfaces
suppressPackageStartupMessages(library(ggplot2))             # For Plotting
suppressPackageStartupMessages(library(rgdal))               # Geospatial data abstraction library. Depends R (≥ 2.14.0)
suppressPackageStartupMessages(library(lubridate))           # Make dealing with dates a little easier. Depends R (≥ 3.0.0)
suppressPackageStartupMessages(library(CircStats))           # Circular Statistics - Von Mises distribution
suppressPackageStartupMessages(library(Rcpp))                # Seamless R and C++ Integration. Depends R (≥ 3.0.0)
suppressPackageStartupMessages(library(ncdf4))               # work with NetCDF datasets
suppressPackageStartupMessages(library(dismo))               # Regression for ecological datasets
suppressPackageStartupMessages(library(sp))                  # Classes and methods for spatial data
suppressPackageStartupMessages(library(shinydashboard))      # Dashboard layout for webbased UI
suppressPackageStartupMessages(library(shinydashboardPlus))  # Additional functionallity for webbased dashboard UIs
source("helpers.R")
source("Generalizablepestandpathogenmodel.R")
source("weather_coeff.R")
source("InfoLabelInput.R")
source("getUnit.R")
source("zipcreator.R")
sourceCpp("scripts/myCppFunctions2.cpp") # load custom functions dispersal that use C++ (Faster)
dataForPlot <<- data.frame(Year = 0,  Area = 0,  Count =0, Host =0)
rUnit <<- ''

dashboardPage(
  dashboardHeader(
    titleWidth = 350,
    title = tags$a(href = 'https://github.com/ChrisJones687/APHIS-Modeling-Project/milestones', tags$img(src = "PoPSS_Logo.png", height = '80', width ='100%'))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PoPSS Overview", tabName = "overview", icon = icon("question")),
      menuItem("Species Parameters", tabName = "species", icon = icon("bug")),
      menuItem("Hosts Data", tabName = "hosts", icon = icon("tree")),
      menuItem("Environmental Data", tabName = "environmental", icon = icon("sun-o")),
      menuItem("Weather Coeffient", tabName = "weather", icon = icon("cloud")),
      menuItem("Estimate Dispersal", tabName = "dispersal", icon = icon("plane")),
      menuItem("Map", tabName = "map", icon = icon("globe")),
      downloadButton("userManual.pdf", "User Manual ", icon("cloud-download"), style = "color: green; display: center-align"),
      br(),
      actionButton("zip", "Download Output", icon = icon("cloud-download")),
      br(),
      withBusyIndicatorUI(actionButton("run", " Run Model", icon = icon("play"))),
      br(),
      br(),
      #img(src='PoPSS_Logo.png'),
      br(),
      socialButton(url = "https://github.com/ChrisJones687/APHIS-Modeling-Project/issues", type="github")
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css")),
    tags$head(tags$style(HTML('background-color: "black'))),
    
    tabItems(
      tabItem(
        tabName = "overview",
        tags$video(src='modelexplanation.mp4', type = "video/mp4", width = "560px", height = "315px", controls = "controls", autoplay = NA)
      ),
      
      tabItem(
        tabName = "species",
        wellPanel(h3("Species Parameters", icon("bug")),
                  style = "background-color: #54ACC1; border: #ADBD60; color: black; padding: 1px 10px 1px 10px",
                  textInput(inputId = "pest", label = infoLabelInputUI("pest", label = "Species Name", title = "Pest or Pathogen that is being simulated!")),
                  numericInput("start", label = infoLabelInputUI(id = "start", label = "Start Year", title = "Year to start simulation"), value = 2000, min = 1960, max = 2020),
                  numericInput("end", label = infoLabelInputUI(id = "end", label = "End Year", title = "Year to end simulation"), value = 2010, min = 1960, max = 2020),
                  selectInput(inputId = "seasonQ", label = infoLabelInputUI(id = "SeasonQ", label = "Does seasonality affect spread?", title = "Limits spread to only months selected to limit computational time"), choices = c("YES","NO")),
                  conditionalPanel(
                    condition = "input.seasonQ == 'YES'", 
                    sliderInput("seasonMonths", label = infoLabelInputUI(id = "seasonMonths", label = "Month Range", title = "Months that contribute to the spread of pest/pathogen."), value = c(1,9), min =1, max =12, step =1)
                  ),
                  numericInput(inputId ="sporeRate", label = infoLabelInputUI(id = "sporeRate", label = "Spread Rate", title = "Determines the average number of individuals that infect another cell during a time step."), value = "4.4", min=0, max = 100, step = 0.1),
                  fileInput(inputId = "initialInfection", label = infoLabelInputUI(id = "initialInfection", label = "Initial Infection Data:", title = "Input a raster or shapefile of the location of infections at the start of simulation."), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                  bsAlert("initialInfectionID"),
                  selectInput(inputId = "kernelType", label = infoLabelInputUI(id = "kernelType", label = "Select the best dispersal kernel.", title = "Choose the dispersal kernel that heuristically fits the dispersal pattern of your pest/pathogen."), choices = c('Cauchy', 'Cauchy Mixture', 'Exponential', 'Gauss')),
                  numericInput(inputId ="scale_1", label = infoLabelInputUI(id = "scale_1", label = "Short distance dispersal scale parameter", title = "Short distance scale parameter for dispersal kernel"), value = "20.57", min=0, max = 1000, step = 0.01),
                  numericInput(inputId ="scale_2", label = infoLabelInputUI(id = "scale_2", label = "Long distance dispersal scale parameter", title = "Long distance scale parameter for dispersal kernel"), value = "8557", min=0, max = 50000, step = 11),
                  numericInput(inputId ="gamma", label = infoLabelInputUI(id = "gamma", label = "Gamma", title = "Sets the percent of short distance dispersal. If only short distance set to 1"), value = "1", min=0, max = 1, step = 0.01),
                  numericInput(inputId ="seed", label = infoLabelInputUI(id = "seed", label = "Random Seed Number", title = "Random Seed Number: Use to duplicate a single run"), value = "42", min=0, max = 5000, step = 1),
                  selectInput(inputId = "time_step", label = infoLabelInputUI(id = "time_step", label = "Time Step", title = "Time step: Monthly, Weekly, or Daily"), choices = c("days","weeks","months"))
        )
      ),
      
      tabItem(
        tabName = "hosts",
        wellPanel(h3("Hosts", icon("tree")),
                  style = "background-color: #ADBD60; border: #ADBD60; color: black; padding: 1px 10px 1px 10px",
                  numericInput("hostMulti", label = infoLabelInputUI(id = "hostMulti", label = "Number of host species", title = "Select the number of host species that affect spread."), value = 1, min =1, max =10, step =1),
                  fileInput("hostDataM1", label = infoLabelInputUI(id = "hostData1", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                  conditionalPanel(
                    condition = "input.hostMulti >= 2",
                    numericInput(inputId = "hostIndexScore1", label = infoLabelInputUI(id = "hostIndexScore1", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1),
                    fileInput("hostDataM2",label = infoLabelInputUI(id = "hostData2", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                    numericInput(inputId = "hostIndexScore2", label = infoLabelInputUI(id = "hostIndexScore2", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                  ),
                  conditionalPanel(
                    condition = "input.hostMulti >= 3",
                    fileInput("hostDataM3",label = infoLabelInputUI(id = "hostData3", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                    numericInput(inputId = "hostIndexScore3", label = infoLabelInputUI(id = "hostIndexScore3", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                  ),
                  conditionalPanel(
                    condition = "input.hostMulti >= 4",
                    fileInput("hostDataM4",label = infoLabelInputUI(id = "hostData4", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                    numericInput(inputId = "hostIndexScore4", label = infoLabelInputUI(id = "hostIndexScore4", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                  ),
                  conditionalPanel(
                    condition = "input.hostMulti >= 5",
                    fileInput("hostDataM5",label = infoLabelInputUI(id = "hostData5", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                    numericInput(inputId = "hostIndexScore5", label = infoLabelInputUI(id = "hostIndexScore5", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                  ),
                  conditionalPanel(
                    condition = "input.hostMulti >= 6",
                    fileInput("hostDataM6",label = infoLabelInputUI(id = "hostData6", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                    numericInput(inputId = "hostIndexScore6", label = infoLabelInputUI(id = "hostIndexScore6", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                  ),
                  conditionalPanel(
                    condition = "input.hostMulti >= 7",
                    fileInput("hostDataM7",label = infoLabelInputUI(id = "hostData7", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                    numericInput(inputId = "hostIndexScore7", label = infoLabelInputUI(id = "hostIndexScore7", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                  ),
                  conditionalPanel(
                    condition = "input.hostMulti >= 8",
                    fileInput("hostDataM8",label = infoLabelInputUI(id = "hostData8", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                    numericInput(inputId = "hostIndexScore8", label = infoLabelInputUI(id = "hostIndexScore8", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                  ),
                  conditionalPanel(
                    condition = "input.hostMulti >= 9",
                    fileInput("hostDataM9",label = infoLabelInputUI(id = "hostData9", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                    numericInput(inputId = "hostIndexScore9", label = infoLabelInputUI(id = "hostIndexScore9", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                  ),
                  conditionalPanel(
                    condition = "input.hostMulti >= 10",
                    fileInput("hostDataM10",label = infoLabelInputUI(id = "hostData10", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                    numericInput(inputId = "hostIndexScore10", label = infoLabelInputUI(id = "hostIndexScore10", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                  ),
                  fileInput("totalSpeciesData", label = infoLabelInputUI(id = "totalSpeciesData", label = "Total Species Data:?", title = "Select the raster of all tree species. Used to determine percent of area occuppied by host species."), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img"))
        )
      ),
      
      tabItem(
        tabName = "environmental",
        wellPanel(h3("Environmental Effects", icon("sun-o"), style = "color: black"), 
                  style = "color: black;background-color: #E7B15F; border: #E7B15F; padding: 1px 10px 1px 10px",
                  # Create box asking if wind affects spread and if wind data is available
                  selectInput(inputId = "windQ", label = infoLabelInputUI(id = "WindQ", label = "Does wind affect spread?", title = "Select yes if you have wind data for your study area and wind affects the spread of your pest/pathogen."), choices = c("NO","YES")),
                  conditionalPanel(
                    condition = "input.windQ == 'YES'", 
                    selectInput("windType",label = infoLabelInputUI(id = "windType", label = "Wind data type:", title = "Do you have detailed raster wind data or only predominate wind direction?"), choices = c("Direction","Raster"))
                  ),
                  # add a data input box if yes is selected
                  conditionalPanel(
                    condition = "input.windQ == 'YES' && input.windType == 'Raster'", 
                    fileInput("windData", label = infoLabelInputUI(id = "windData", label = "Wind data:", title = "Select wind wind data raster or netcdf file"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img"))
                  ),
                  # add a data type option for wind in case detailed wind data isn't available but predominate wind direction
                  conditionalPanel(
                    condition = "input.windQ == 'YES' && input.windType == 'Direction'", 
                    selectInput("windDir", label = infoLabelInputUI(id = "windDir", label = "Predominate wind direction:", title = "Select the predominate wind direction for your study area."), choices = c("NE","E","SE","S","SW","W","NW","N")),
                    numericInput("kappa", label = infoLabelInputUI(id = "kappa", label = "Kappa:", title = "Kappa controls the dispersal direction dominance (i.e. kappa = 0 all directions equally likely, kappa = 8 much more likely to disperse in predominate direction)"), value = 2, min = 0, max = 100)
                  ),
                  # Create box asking if temperature affects spread and if Temperature data is available
                  selectInput(inputId = "temp", label = infoLabelInputUI(id = "temp", label = "Does temperature affect spread?", title = "Select yes if you have temperature data for your study area and temperature affects the ability of your species to disperse."), choices = c("NO","YES")),
                  # add a data input box if yes is selected
                  conditionalPanel(
                    condition = "input.temp == 'YES'", 
                    fileInput("tempData", label = infoLabelInputUI(id = "tempData", label = "Temperature Data:", title = "Select the temperature raster or netcdf file."), accept = c(".tif", ".nc"))
                  ),
                  # Create box asking if precipitation affects spread and if precipitation data is available
                  selectInput(inputId = "precip", label = infoLabelInputUI(id = "precip", label = "Does precipitation affect spread?", title = "Select yes if you have precipitation data for your study area and precipitation affects the ability of your species to disperse."), choices = c("NO","YES")),
                  # add a data input box if yes is selected
                  conditionalPanel(
                    condition = "input.precip == 'YES'", 
                    fileInput("precipData", label = infoLabelInputUI(id = "precipData", label = "Precipitation Data:?", title = "Select the precipitation raster or netcdf file"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img"))
                  )
        )
      ),
      
      tabItem(
        tabName = "weather",
        wellPanel(h3("Weather Indices", icon("tint"), style = "color: black"), 
                  style = "color: black;background-color: #AED6F1; border: #AED6F1; padding: 1px 10px 1px 10px",
                  # Structure of precipitation index inputs
                  selectInput(inputId = "prcp", label = infoLabelInputUI(id = "prcp", label = "Create moisture index?", title = "Select yes if you want to create a moisture index for your study area and pest species."), choices = c("NO","YES")),
                  conditionalPanel(
                    condition = "input.prcp == 'YES'", 
                    selectInput("prcp_method", label = infoLabelInputUI(id = "prcp_method", label = "Select function type:", title = "Select the type of function to use to create your moisture index."), choices = c("threshold", "polynomial"))
                  ),
                  conditionalPanel(
                    condition = "input.prcp == 'YES' && input.prcp_method == 'threshold'",
                    selectInput("prcp_operator", label = infoLabelInputUI(id = "prcp_operator", label = "Choose operator:", title = "Select the correct operator for your threshold"), choices = c("<", "<=", ">", ">=")),
                    numericInput(inputId = "prcp_thresh", label = infoLabelInputUI(id = "prcp_thresh", label = "Precipitation threshold:", title = "Input the threshold at which precipitation affects your pests."), value = 0, min=0, max=1000, step = .1)
                  ),
                  conditionalPanel( 
                    condition = "input.prcp == 'YES' && input.prcp_method == 'polynomial'",
                    numericInput("prcp_degree", label = infoLabelInputUI(id = "prcp_degree", label = "Select the degree of the polynomial", title = "What degree of polynomial is the function you are trying to fit? (up to 3rd degree)"), value = 1, min = 1, max = 3, step = 1),
                    numericInput(inputId = "prcp_a0", label = infoLabelInputUI(id = "prcp_a0", label = "Value of a0", title = "a0 is the constant value that sets the y-intercept"), value = 0, min=-100, max=100, step = .0001),
                    numericInput(inputId = "prcp_a1", label = infoLabelInputUI(id = "prcp_a1", label = "Value of a1", title = "a1 is the constant that is mulitplied by x. (x is the precipitation value)"), value = 0, min=-100, max=100, step = .0001),
                    conditionalPanel(
                      condition = "input.prcp_degree > 1",
                      numericInput(inputId = "prcp_a2", label = infoLabelInputUI(id = "prcp_a2", label = "Value of a2", title = "a2 is the constant that is mulitplied by x^2. (x is the precipitation value)"), value = 0, min=-100, max=100, step = .0001)   
                    ),
                    conditionalPanel(
                      condition = "input.prcp_degree > 2",
                      numericInput(inputId = "prcp_a3", label = infoLabelInputUI(id = "prcp_a3", label = "Value of a3", title = "a3 is the constant that is mulitplied by x^3. (x is the precipitation value)"), value = 0, min=-100, max=100, step = .0001)
                    )
                  )
        ),
        wellPanel(h3("Temperature Index", icon("thermometer-half"), style = "color: black"), 
                  style = "color: black;background-color: #EDBB99; border: #EDBB99; padding: 1px 10px 1px 10px",
                  # Structure of temperature index inputs
                  selectInput(inputId = "temp_index", label = infoLabelInputUI(id = "temp_index", label = "Create moisture index?", title = "Select yes if you want to create a moisture index for your study area and pest species."), choices = c("NO","YES")),
                  conditionalPanel(
                    condition = "input.temp_index == 'YES'", 
                    selectInput("temp_method", label = infoLabelInputUI(id = "temp_method", label = "Select function type:", title = "Select the type of function to use to create your moisture index."), choices = c("threshold", "polynomial"))
                  ),
                  conditionalPanel(
                    condition = "input.temp_index == 'YES' && input.temp_method == 'threshold'",
                    selectInput("temp_operator", label = infoLabelInputUI(id = "temp_operator", label = "Choose operator:", title = "Select the correct operator for your threshold"), choices = c("<", "<=", ">", ">=")),
                    numericInput(inputId = "temp_thresh", label = infoLabelInputUI(id = "temp_thresh", label = "Precipitation threshold:", title = "Input the threshold at which precipitation affects your pests."), value = 0, min=0, max=1000, step = .1)
                  ),
                  conditionalPanel( 
                    condition = "input.temp_index == 'YES' && input.temp_method == 'polynomial'",
                    numericInput("temp_degree", label = infoLabelInputUI(id = "temp_degree", label = "Select the degree of the polynomial", title = "What degree of polynomial is the function you are trying to fit? (up to 3rd degree)"), value = 1, min = 1, max = 3, step = 1),
                    numericInput(inputId = "temp_a0", label = infoLabelInputUI(id = "temp_a0", label = "Value of a0", title = "a0 is the constant value that sets the y-intercept"), value = 0, min=-100, max=100, step = .0001),
                    numericInput(inputId = "temp_a1", label = infoLabelInputUI(id = "temp_a1", label = "Value of a1", title = "a1 is the constant that is mulitplied by x. (x is the precipitation value)"), value = 0, min=-100, max=100, step = .0001),
                    conditionalPanel(
                      condition = "input.temp_degree > 1",
                      numericInput(inputId = "temp_a2", label = infoLabelInputUI(id = "temp_a2", label = "Value of a2", title = "a2 is the constant that is mulitplied by x^2. (x is the precipitation value)"), value = 0, min=-100, max=100, step = .0001)   
                    ),
                    conditionalPanel(
                      condition = "input.temp_degree > 2",
                      numericInput(inputId = "temp_a3", label = infoLabelInputUI(id = "temp_a3", label = "Value of a3", title = "a3 is the constant that is mulitplied by x^3. (x is the precipitation value)"), value = 0, min=-100, max=100, step = .0001)
                    )
                  )
        ),
        wellPanel(h3("Directories and Study Area", icon("files-o"), style = "color: black"), 
                  style = "color: black;background-color: #EDBB99; border: #EDBB99; padding: 1px 10px 1px 10px",
                  # Directory Inputs
                  textInput(inputId = "input_directory", label = infoLabelInputUI(id = "input_directory", label = "Daymet Directory", title = "Include complete path to daymet data (e.g. C://Users/Chris/Desktop/DaymetUS) "), value = "C://Users/Chris/Desktop/DaymetUS"),
                  textInput(inputId = "output_directory", label = infoLabelInputUI(id = "output_directory", label = "Daymet Directory", title = "Include complete path to output location (e.g. C://Users/Chris/Desktop/DaymetUS/pest)"), value = "C://Users/Chris/Desktop/DaymetUS/pest"),
                  textInput(inputId = "study_area", label = infoLabelInputUI(id = "study_area", label = "Study Area", title = "List all states that you want in your study area"), value = "Maryland")
        ),
        withBusyIndicatorUI(actionButton("weather_coeff_run", " Create Weather Coefficients", icon = icon("thermometer-full")))
      ),
      
      tabItem(tabName = "dispersal"
              
      ),
      
      tabItem(
        tabName = "map",
        column(width = 12,
               uiOutput("rSlider"),
               leafletOutput("mapData", height = "600px"),
               absolutePanel(top=80, left=25,
                             sliderInput("dec", "Year", min=1, max=11, value=1, step=1, animate= animationOptions(interval=1000)),
                             numericInput("host", label = "Choose species", min = 1, max = 3, value = 1),
                             checkboxInput("legend", "Show legend", TRUE) 
               ),
               br()
               # plotOutput("plotData", height = "600px"),
               # selectInput(inputId = "plotDataSelect", label = "Select data to display", choices = names(dataForPlot)[2:(length(names(dataForPlot))-1)])
        )
      )
    )
  )
)