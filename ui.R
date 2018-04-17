suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(rgdal))     # Geospatial data abstraction library. Depends R (≥ 2.14.0)
suppressPackageStartupMessages(library(lubridate)) # Make dealing with dates a little easier. Depends R (≥ 3.0.0)
suppressPackageStartupMessages(library(CircStats)) # Circular Statistics - Von Mises distribution
suppressPackageStartupMessages(library(Rcpp))      # Seamless R and C++ Integration. Depends R (≥ 3.0.0)
suppressPackageStartupMessages(library(ncdf4))     # work with NetCDF datasets
suppressPackageStartupMessages(library(dismo))     # Regression for ecological datasets
suppressPackageStartupMessages(library(sp))        # Classes and methods for spatial data
source("helpers.R")
source("Generalizablepestandpathogenmodel.R")
source("InfoLabelInput.R")
source("getUnit.R")
sourceCpp("scripts/myCppFunctions2.cpp") # load custom functions dispersal that use C++ (Faster)
dataForPlot <<- data.frame(Year = 0,  Area = 0,  Count =0, Host =0)
rUnit <<- ''

fluidPage(theme = "shiny.css",
  useShinyjs(),
  tags$head(tags$style(type="text/css","a{color: green;}")),
  ## Add Title to App
  h1("Generalizable Pest and Pathogen Model", icon("envira"), style = "color: green;"),
  ## Create a sidebar for variable inputs that react to user changes
  fluidRow(
    column(width = 4, style = "padding: 10px",
           # Create panel for Species paramaters
           wellPanel(h3("Species Parameters", icon("bug")),
                     style = "background-color: #54ACC1; border: #ADBD60; color: black; padding: 1px 10px 1px 10px",
                     # Create text box for pest or pathogen being simulated
                     textInput(inputId = "pest", label = infoLabelInputUI("pest", label = "Species Name", title = "Pest or Pathogen that is being simulated!")),
                     # Create date range box for simulation
                     #dateRangeInput("date", "Start date to end date"),
                     numericInput("start", label = infoLabelInputUI(id = "start", label = "Start Year", title = "Year to start simulation"), value = 2000, min = 1960, max = 2020),
                     numericInput("end", label = infoLabelInputUI(id = "end", label = "End Year", title = "Year to end simulation"), value = 2010, min = 1960, max = 2020),
                     # Create seasonality box
                     selectInput(inputId = "seasonQ", label = infoLabelInputUI(id = "SeasonQ", label = "Does seasonality affect spread?", title = "Limits spread to only months selected to limit computational time"), choices = c("YES","NO")),
                     conditionalPanel(
                       condition = "input.seasonQ == 'YES'", sliderInput("seasonMonths", label = infoLabelInputUI(id = "seasonMonths", label = "Month Range", title = "Months that contribute to the spread of pest/pathogen."), value = c(1,9), min =1, max =12, step =1)
                       ),
                     numericInput(inputId ="sporeRate", label = infoLabelInputUI(id = "sporeRate", label = "Spread Rate", title = "Determines the average number of individuals that infect another cell during a time step."), value = "4.4", min=0, max = 100, step = 0.1),
                     fileInput(inputId = "initialInfection", label = infoLabelInputUI(id = "initialInfection", label = "Initial Infection Data:", title = "Input a raster or shapefile of the location of infections at the start of simulation."), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                     bsAlert("initialInfectionID"),
                     selectInput(inputId = "kernelType", label = infoLabelInputUI(id = "kernelType", label = "Select the best dispersal kernel.", title = "Choose the dispersal kernel that heuristically fits the dispersal pattern of your pest/pathogen."), choices = c('Cauchy', 'Cauchy Mixture', 'Exponential', 'Gauss'))
                     ),
           # Create panel for Host variables
           wellPanel(h3("Hosts", icon("tree")),
                     style = "background-color: #ADBD60; border: #ADBD60; color: black; padding: 1px 10px 1px 10px",
                     # Create input for host data
                     selectInput(inputId = "hostQ", label = infoLabelInputUI(id = "hostQ", label = "Is the system single- or multi-host?", title = "Select multi-host if the pest/pathogen can use multiple species for feeding/reproduction."), choices = c("single-host","multi-host")),
                     conditionalPanel(
                       condition = "input.hostQ == 'multi-host'",
                       numericInput("hostMulti", label = infoLabelInputUI(id = "hostMulti", label = "Number of host species", title = "Select the number of host species that affect spread."), value = 2, min =2, max =10, step =1)
                       ),
                     fileInput("hostDataM1", label = infoLabelInputUI(id = "hostData2", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                     conditionalPanel(
                       condition = "input.hostQ == 'multi-host' && input.hostMulti >= 2",
                       numericInput(inputId = "hostIndexScore1", label = infoLabelInputUI(id = "hostIndexScore1", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1),
                       fileInput("hostDataM2",label = infoLabelInputUI(id = "hostData2", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                       numericInput(inputId = "hostIndexScore2", label = infoLabelInputUI(id = "hostIndexScore2", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                       ),
                     conditionalPanel(
                       condition = "input.hostQ == 'multi-host' && input.hostMulti >= 3",
                       fileInput("hostDataM3",label = infoLabelInputUI(id = "hostData3", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                       numericInput(inputId = "hostIndexScore3", label = infoLabelInputUI(id = "hostIndexScore3", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                       ),
                     conditionalPanel(
                       condition = "input.hostQ == 'multi-host' && input.hostMulti >= 4",
                       fileInput("hostDataM4",label = infoLabelInputUI(id = "hostData4", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                       numericInput(inputId = "hostIndexScore4", label = infoLabelInputUI(id = "hostIndexScore4", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                     ),
                     conditionalPanel(
                       condition = "input.hostQ == 'multi-host' && input.hostMulti >= 5",
                       fileInput("hostDataM5",label = infoLabelInputUI(id = "hostData5", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                       numericInput(inputId = "hostIndexScore5", label = infoLabelInputUI(id = "hostIndexScore5", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                     ),
                     conditionalPanel(
                       condition = "input.hostQ == 'multi-host' && input.hostMulti >= 6",
                       fileInput("hostDataM6",label = infoLabelInputUI(id = "hostData6", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                       numericInput(inputId = "hostIndexScore6", label = infoLabelInputUI(id = "hostIndexScore6", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                     ),
                     conditionalPanel(
                       condition = "input.hostQ == 'multi-host' && input.hostMulti >= 7",
                       fileInput("hostDataM7",label = infoLabelInputUI(id = "hostData7", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                       numericInput(inputId = "hostIndexScore7", label = infoLabelInputUI(id = "hostIndexScore7", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                     ),
                     conditionalPanel(
                       condition = "input.hostQ == 'multi-host' && input.hostMulti >= 8",
                       fileInput("hostDataM8",label = infoLabelInputUI(id = "hostData8", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                       numericInput(inputId = "hostIndexScore8", label = infoLabelInputUI(id = "hostIndexScore8", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                     ),
                     conditionalPanel(
                       condition = "input.hostQ == 'multi-host' && input.hostMulti >= 9",
                       fileInput("hostDataM9",label = infoLabelInputUI(id = "hostData9", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                       numericInput(inputId = "hostIndexScore9", label = infoLabelInputUI(id = "hostIndexScore9", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                     ),
                     conditionalPanel(
                       condition = "input.hostQ == 'multi-host' && input.hostMulti >= 10",
                       fileInput("hostDataM10",label = infoLabelInputUI(id = "hostData10", label = "Host Data:", title = "Select the raster data of host species density"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")),
                       numericInput(inputId = "hostIndexScore10", label = infoLabelInputUI(id = "hostIndexScore10", label = "Host Index Score:", title = "This indicates the host preference or compotency with 10 being most competent and 1 being least."), value = 10, min=1, max=10, step = 1)
                     ),
                     fileInput("totalSpeciesData", label = infoLabelInputUI(id = "totalSpeciesData", label = "Total Species Data:?", title = "Select the raster of all tree species. Used to determine percent of area occuppied by host species."), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img"))
                     ),
           # Create panel for Environmental variables
           wellPanel(h3("Environmental Effects", icon("sun-o"), style = "color: black"), 
                     style = "color: black;background-color: #E7B15F; border: #E7B15F; padding: 1px 10px 1px 10px",
                     # Create box asking if wind affects spread and if wind data is available
                     selectInput(inputId = "windQ", label = infoLabelInputUI(id = "WindQ", label = "Does wind affect spread?", title = "Select yes if you have wind data for your study area and wind affects the spread of your pest/pathogen."), choices = c("NO","YES")),
                     conditionalPanel(
                       condition = "input.windQ == 'YES'", selectInput("windType",label = infoLabelInputUI(id = "windType", label = "Wind data type:", title = "Do you have detailed raster wind data or only predominate wind direction?"), choices = c("Direction","Raster"))
                       ),
                     # add a data input box if yes is selected
                     conditionalPanel(condition = "input.windQ == 'YES' && input.windType == 'Raster'", 
                                      fileInput("windData", label = infoLabelInputUI(id = "windData", label = "Wind data:", title = "Select wind wind data raster or netcdf file"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img"))
                                      ),
                     # add a data type option for wind in case detailed wind data isn't available but predominate wind direction
                     conditionalPanel(condition = "input.windQ == 'YES' && input.windType == 'Direction'", 
                       selectInput("windDir", label = infoLabelInputUI(id = "windDir", label = "Predominate wind direction:", title = "Select the predominate wind direction for your study area."), choices = c("NE","E","SE","S","SW","W","NW","N")),
                       numericInput("kappa", label = infoLabelInputUI(id = "kappa", label = "Kappa:", title = "Kappa controls the dispersal direction dominance (i.e. kappa = 0 all directions equally likely, kappa = 8 much more likely to disperse in predominate direction)"), value = 2, min = 0, max = 100)
                       ),
                     # Create box asking if temperature affects spread and if Temperature data is available
                     selectInput(inputId = "temp", label = infoLabelInputUI(id = "temp", label = "Does temperature affect spread?", title = "Select yes if you have temperature data for your study area and temperature affects the ability of your species to disperse."), choices = c("NO","YES")),
                     # add a data input box if yes is selected
                     conditionalPanel(
                       condition = "input.temp == 'YES'", fileInput("tempData", label = infoLabelInputUI(id = "tempData", label = "Temperature Data:", title = "Select the temperature raster or netcdf file."), accept = c(".tif", ".nc"))
                       ),
                     # Create box asking if precipitation affects spread and if precipitation data is available
                     selectInput(inputId = "precip", label = infoLabelInputUI(id = "precip", label = "Does precipitation affect spread?", title = "Select yes if you have precipitation data for your study area and precipitation affects the ability of your species to disperse."), choices = c("NO","YES")),
                     # add a data input box if yes is selected
                     conditionalPanel(
                       condition = "input.precip == 'YES'", fileInput("precipData", label = infoLabelInputUI(id = "precipData", label = "Precipitation Data:?", title = "Select the precipitation raster or netcdf file"), accept = c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img"))
                       )
                         ),
           # Create an Action button that will run the model when pressed
           withBusyIndicatorUI(
             actionButton("run", " Run Model", icon = icon("play")))
           ),
    column(width = 8,
           # Let the user know if their extents match
           #textOutput("extentMatch")
           # Create a download link for the user Manual
           downloadLink("pdf", "Download User Manual ", icon("cloud-download"), style = "color: green"),
           #sliderInput(inputId = 'yearSlider', label = 'Year', value = "input.start", step = 1, min = 2000, max = 2010, sep = "", animate = TRUE),
           uiOutput("rSlider"),
           leafletOutput("mapData", height = "600px"),
           br(),
           tabsetPanel(id = "tabsPanel",
               tabPanel(title = "Plot", 
                      plotOutput("plotData", height = "600px"),
                      selectInput(inputId = "plotDataSelect", label = "Select data to display", choices = names(dataForPlot)[2:(length(names(dataForPlot))-1)])
                      # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      #               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      #               width = 330, height = "auto",
                      #               selectInput(inputId = "plotDataSelect", label = "Select data to display", choices = names(dataForPlot)[2:(length(names(dataForPlot))-1)])
                      # )
                      )
             # tabPanel(title = "State Summary", leafletOutput("stateData", height = "600px")),
             # tabPanel(title = "County Summary", leafletOutput("countyData", height = "600px"))
           )
           )
    ))