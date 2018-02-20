source("helpers.r", local = TRUE)
source("Generalizablepestandpathogenmodel.r")

function(input, output) {
  options(shiny.maxRequestSize=70000*1024^2) 
  # Creates the text file that is downloaded upon model completion
  #output$model <- renderPrint({c("Model inputs are:",input$wind, input$windData, input$temp, input$tempData, input$precip, input$precipData)})
  
  # Used to set the initial zoom of the map and color of the rasters
  r <- raster("C:\\Users\\cmjone25\\Dropbox\\Projects\\APHIS\\BayOakCode/layers/UMCA_den_100m.img")
  pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(r), na.color = "transparent")
  
  #Creates the text saying the model is running when the action button is pressed
  modeltext <- eventReactive(input$run, {"Model has finished"})
  output$modelText <- renderText({modeltext()})
  
  #inTotalSpeciesData <- reactive(input$totalSpeciesData)
  #inTSD <- raster(inTotalSpeciesData())
  
  modelRun <- observeEvent(input$run, 
                           {withBusyIndicatorServer("run",{pest("./layers/UMCA_den_100m.img","./layers/OAKS_den_100m.img","./layers/TPH_den_100m.img", "./layers/init_2000_cnt.img",
                                 input$start, input$end, input$seasonQ, input$seasonMonths[1],input$seasonMonths[2], 
                                 input$sporeRate, input$windQ, input$windDir, './layers/weather/weatherCoeff_2000_2014.nc')})
                           })
  
  inTempData <- renderText(input$tempData$datapath)
  # Plot the data
  output$plotData <- renderLeaflet({
    if (is.null(input$windData)==TRUE && is.null(input$tempData)==TRUE && is.null(input$totalSpeciesData)==TRUE){
      leaflet(height = "300px") %>%
        addProviderTiles("Esri.WorldImagery", group="background 1") %>%
        addRasterImage(r, opacity=0.0)
    } else if (is.null(input$windData)==TRUE && is.null(input$tempData)==TRUE && is.null(input$totalSpeciesData)==FALSE) {
      inTotalSpeciesData <- input$totalSpeciesData
      rastTSD <- raster(inTotalSpeciesData$datapath)
      pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTSD), na.color = "transparent")
      leaflet(height = "300px") %>%
        addProviderTiles("Esri.WorldImagery", group="background 1") %>%
        addRasterImage({raster(inTotalSpeciesData$datapath)}, opacity=0.5, colors = pal, group = "Total Host") %>%
        addLayersControl(
          overlayGroups ="Total Host",
          options = layersControlOptions(collapsed = FALSE, opacity =0.6)) %>%
        addLegend("bottomright", pal = pal, values = values(rastTSD),
                  title = "Host species",
                  #labFormat = labelFormat(prefix = "$"),
                  opacity = 1)
    } else if (is.null(input$windData)==FALSE && is.null(input$tempData)==TRUE && is.null(input$totalSpeciesData)==TRUE) {
      inWind <- input$windData
      leaflet(height = "300px") %>%
        addProviderTiles("Esri.WorldImagery", group="background 1") %>%
        addRasterImage({raster(inWind$datapath)}, opacity=0.6)
    } else if (is.null(input$windData)==TRUE && is.null(input$tempData)==FALSE && is.null(input$totalSpeciesData)==TRUE){
      inTemp <- input$tempData
      leaflet(height = "300px") %>%
        addProviderTiles("Esri.WorldImagery", group="background 1") 
      addRasterImage({raster(inTemp$datapath)}, opacity=0.6)
    } else if (is.null(input$windData)==FALSE && is.null(input$tempData)==FALSE && is.null(input$totalSpeciesData)==TRUE) {
      inTemp <- input$tempData
      inWind <- input$windData
      leaflet(height = "300px") %>%
        addProviderTiles("Esri.WorldImagery", group="background 1") %>%
        addRasterImage({raster(inTemp$datapath)}, opacity=0.6) %>%
        addRasterImage({raster(inWind$datapath)}, opacity=0.4)
    }
    
  })
  
  output$extentMatch <- renderText({
    if (is.null(input$windData)==FALSE && is.null(input$tempData)==FALSE) {
      inTemp <- input$tempData
      inWind <- input$windData
      exMatch <- extent(inTemp)==extent(inWind)
      if (exMatch==TRUE){
        print("Your extents match")
      } else if (exMatch == FALSE) {
        print("Your extents don't match would you like to resample")
      }
    }
  })
  
  # Allows for the downloading of the user manual when the download link is pressed
  output$pdf <- downloadHandler("generalizablepestandpathogenmodel.pdf", content = function(file){
    file.copy("C:\\Users\\Chris\\Desktop\\Generalizable Pest and Pathogen Model User.pdf",file)
  })
  
}