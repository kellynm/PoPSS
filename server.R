function(input, output) {
  options(shiny.maxRequestSize=70000*1024^2) 
  # Creates the text file that is downloaded upon model completion
  #output$model <- renderPrint({c("Model inputs are:",input$wind, input$windData, input$temp, input$tempData, input$precip, input$precipData)})
  
  # Used to set the initial zoom of the map and color of the rasters
  r <- raster("./layers/UMCA_den_100m.img")
  pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(r), na.color = "transparent")
  # Create Raster Stack for holding output from model run
  modelRastOut <- r
  olg <- c()
  
  # Creates the text saying the model is running when the action button is pressed
  modeltext <<- eventReactive(input$run, {"Model has finished"})
  output$modelText <- renderText({modeltext()})

  modelRun <- observeEvent(input$run, {
    #inTotalSpeciesData <- input$totalSpeciesData
    #rastTotalSpeciesData <<- raster(inTotalSpeciesData$datapath)
    #inInitialInfection <- input$initialInfection
    #rastInitialInfection <- raster(inInitialInfection$datapath)
    inHostDataM1 <- input$hostDataM1
    rastHostDataM1 <- raster(inHostDataM1$datapath)
    inHostDataM2 <- input$hostDataM2
    rastHostDataM2 <- raster(inHostDataM2$datapath)
    years = seq(input$start, input$end, 1)
                           withBusyIndicatorServer("run",{modelRastOut <- pest(rastHostDataM1,rastHostDataM2,rastTotalSpeciesData, rastInitialInfection,
                                 input$start, input$end, input$seasonQ, input$seasonMonths[1],input$seasonMonths[2], 
                                 input$sporeRate, input$windQ, input$windDir, './layers/weather/weatherCoeff_2000_2014.nc')})  
                           proxy <- leafletProxy("mapData")
                           if (nlayers(modelRastOut)>1) {
                             olg = c("All Trees", "Initial Infection","Host","Host 2")
                             olg <- list(olg)
                             for (i in 1:(nlayers(modelRastOut)-1)){
                              pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(modelRastOut[[i]]), na.color = "transparent")
                              olg[[i+1]] <- c(olg[[i]], paste("year", (years[nlayers(modelRastOut)-i])))
                              proxy <- proxy %>% 
                               addRasterImage(modelRastOut[[i]], opacity= 0.8, group = paste("year", (years[nlayers(modelRastOut)-i]))) %>%
                               addLayersControl(
                                 overlayGroups = olg[[i+1]],
                                 options = layersControlOptions(collapsed = FALSE, opacity =0.6))
                           }}
                           })
  
  ## Set up GUI maps to be flexible
  observeEvent(input$initialInfection, {
    inInitialInfection <- input$initialInfection
    rastInitialInfection <<- raster(inInitialInfection$datapath)
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInitialInfection), na.color = "transparent")
    olg <<- c(olg, "Initial Infection")
    proxy <- proxy %>%
      addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal, group = "Initial Infection") %>%
      addLayersControl(
        overlayGroups = olg,
        options = layersControlOptions(collapsed = FALSE, opacity =0.6)) %>%
      addLegend("bottomright", pal = pal, values = values(rastInitialInfection),
                title = "Host species",
                opacity = 1)
  })
  observeEvent(input$totalSpeciesData, {
    inTotalSpeciesData <- input$totalSpeciesData
    rastTotalSpeciesData <<- raster(inTotalSpeciesData$datapath)
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
    olg <<- c(olg, "All Trees")
    proxy <- proxy %>%
        addProviderTiles("Esri.WorldImagery", group="background 1") %>%
        addRasterImage({rastTotalSpeciesData}, opacity=0.5, colors = pal, group = "All Trees") %>%
        addLayersControl(
          overlayGroups = olg,
          options = layersControlOptions(collapsed = FALSE, opacity =0.6)) %>%
        addLegend("bottomright", pal = pal, values = values(rastTotalSpeciesData),
                  title = "Host species",
                  opacity = 1)
  })
  observeEvent(input$hostDataM1, {
    inHostDataM1 <- input$hostDataM1
    rastHostDataM1 <- raster(inHostDataM1$datapath)
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM1), na.color = "transparent")
    olg <<- c(olg, "Host 1")
    proxy <- proxy %>%
      addProviderTiles("Esri.WorldImagery", group="background 1") %>%
      addRasterImage({rastHostDataM1}, opacity=0.5, colors = pal, group = "All Trees") %>%
      addLayersControl(
        overlayGroups = olg,
        options = layersControlOptions(collapsed = FALSE, opacity =0.6)) %>%
      addLegend("bottomright", pal = pal, values = values(rastHostDataM1),
                title = "Host species",
                opacity = 1)
  })
  observeEvent(input$hostDataM2, {
    inHostDataM2 <- input$hostDataM2
    rastHostDataM2 <- raster(inHostDataM2$datapath)
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM2), na.color = "transparent")
    olg <<- c(olg, "Host 2")
    proxy <- proxy %>%
      addProviderTiles("Esri.WorldImagery", group="background 1") %>%
      addRasterImage({rastHostDataM2}, opacity=0.5, colors = pal, group = "Host 2") %>%
      addLayersControl(
        overlayGroups = olg,
        options = layersControlOptions(collapsed = FALSE, opacity =0.6)) %>%
      addLegend("bottomright", pal = pal, values = values(rastHostDataM2),
                title = "Host species",
                opacity = 1)
  })

  output$mapData <- renderLeaflet({
      leaflet(height = "300px") %>%
        addProviderTiles("Esri.WorldImagery", group="background 1") %>%
        addRasterImage(r, opacity=0.0)
  })
  proxy <- leafletProxy("mapData")
  
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
  
  # Plot the data
  # output$mapData <- renderLeaflet({
  #   if (is.null(input$initialInfection)==TRUE && is.null(input$totalSpeciesData)==TRUE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==TRUE && is.null(input$hostDataM2)==TRUE){
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage(r, opacity=0.0)
  #   } else if (is.null(input$initialInfection)==FALSE && is.null(input$totalSpeciesData)==TRUE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==TRUE && is.null(input$hostDataM2)==TRUE) {
  #     inInitialInfection <- input$initialInfection
  #     rastInitialInfection <- raster(inInitialInfection$datapath)
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInitialInfection), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal, group = "Initial Infection") %>%
  #       addLayersControl(
  #         overlayGroups = c("Initial Infection"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6)) %>%
  #       addLegend("bottomright", pal = pal, values = values(rastInitialInfection),
  #                 title = "Host species",
  #                 opacity = 1)
  #   } else if (is.null(input$initialInfection)==TRUE && is.null(input$totalSpeciesData)==FALSE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==TRUE && is.null(input$hostDataM2)==TRUE) {
  #     inTotalSpeciesData <- input$totalSpeciesData
  #     rastTotalSpeciesData <- raster(inTotalSpeciesData$datapath)
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastTotalSpeciesData}, opacity=0.5, colors = pal, group = "All Trees") %>%
  #       addLayersControl(
  #         overlayGroups = c("All Trees"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6)) %>%
  #       addLegend("bottomright", pal = pal, values = values(rastTotalSpeciesData),
  #                 title = "Host species",
  #                 opacity = 1)
  #   } else if (is.null(input$initialInfection)==TRUE && is.null(input$totalSpeciesData)==TRUE && is.null(input$hostDataSingle)==FALSE && is.null(input$hostDataM1)==TRUE && is.null(input$hostDataM2)==TRUE) {
  #     inHostDataSingle <- input$hostDataSingle
  #     rastHostDataSingle <- raster(inHostDataSingle$datapath)
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataSingle), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataSingle}, opacity=0.5, colors = pal, group = "Host") %>%
  #       addLayersControl(
  #         overlayGroups = c("Host"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6)) %>%
  #       addLegend("bottomright", pal = pal, values = values(rastHostDataSingle),
  #                 title = "Host species",
  #                 opacity = 1)
  #   } else if (is.null(input$initialInfection)==TRUE && is.null(input$totalSpeciesData)==TRUE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==FALSE && is.null(input$hostDataM2)==TRUE) {
  #     inHostDataM1 <- input$hostDataM1
  #     rastHostDataM1 <- raster(inHostDataM1$datapath)
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM1), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataM1}, opacity=0.5, colors = pal, group = "Host") %>%
  #       addLayersControl(
  #         overlayGroups ="Host",
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6)) %>%
  #       addLegend("bottomright", pal = pal, values = values(rastHostDataM1),
  #                 title = "Host species",
  #                 opacity = 1)
  #   } else if (is.null(input$initialInfection)==TRUE && is.null(input$totalSpeciesData)==TRUE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==TRUE && is.null(input$hostDataM2)==FALSE) {
  #     inHostDataM2 <- input$hostDataM2
  #     rastHostDataM2 <- raster(inHostDataM2$datapath)
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM2), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataM2}, opacity=0.5, colors = pal, group = "Host") %>%
  #       addLayersControl(
  #         overlayGroups ="Host",
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6)) %>%
  #       addLegend("bottomright", pal = pal, values = values(rastHostDataM2),
  #                 title = "Host species",
  #                 opacity = 1)
  #   } else if (is.null(input$initialInfection)==TRUE && is.null(input$totalSpeciesData)==TRUE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==FALSE && is.null(input$hostDataM2)==FALSE) {
  #     inHostDataM1 <- input$hostDataM1
  #     rastHostDataM1 <- raster(inHostDataM1$datapath)
  #     inHostDataM2 <- input$hostDataM2
  #     rastHostDataM2 <- raster(inHostDataM2$datapath)
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM1), na.color = "transparent")
  #     pal2 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM2), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataM1}, opacity=0.5, colors = pal, group = "Host") %>%
  #       addRasterImage({rastHostDataM2}, opacity=0.5, colors = pal2, group = "Host 2") %>%
  #       addLayersControl(
  #         overlayGroups =c("Host","Host 2"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==TRUE && is.null(input$totalSpeciesData)==FALSE && is.null(input$hostDataSingle)==FALSE && is.null(input$hostDataM1)==TRUE && is.null(input$hostDataM2)==TRUE) {
  #     inTotalSpeciesData <- input$totalSpeciesData
  #     rastTotalSpeciesData <- raster(inTotalSpeciesData$datapath)
  #     inHostDataSingle <- input$hostDataSingle
  #     rastHostDataSingle <- raster(inHostDataSingle$datapath)
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataSingle), na.color = "transparent")
  #     pal2 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastTotalSpeciesData}, opacity=0.5, colors = pal2, group = "All Trees") %>%
  #       addRasterImage({rastHostDataSingle}, opacity=0.5, colors = pal, group = "Host") %>%
  #       addLayersControl(
  #         overlayGroups =c("Host", "All Trees"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==FALSE && is.null(input$totalSpeciesData)==TRUE && is.null(input$hostDataSingle)==FALSE && is.null(input$hostDataM1)==TRUE && is.null(input$hostDataM2)==TRUE) {
  #     inInitialInfection <- input$initialInfection
  #     rastInitialInfection <- raster(inInitialInfection$datapath)
  #     inHostDataSingle <- input$hostDataSingle
  #     rastHostDataSingle <- raster(inHostDataSingle$datapath)
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataSingle), na.color = "transparent")
  #     pal2 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInitialInfection), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal2, group = "Initial Infection") %>%
  #       addRasterImage({rastHostDataSingle}, opacity=0.5, colors = pal, group = "Host") %>%
  #       addLayersControl(
  #         overlayGroups =c("Host", "All Trees"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==FALSE && is.null(input$totalSpeciesData)==FALSE && is.null(input$hostDataSingle)==FALSE && is.null(input$hostDataM1)==TRUE && is.null(input$hostDataM2)==TRUE) {
  #     inInitialInfection <- input$initialInfection
  #     rastInitialInfection <- raster(inInitialInfection$datapath)
  #     inTotalSpeciesData <- input$totalSpeciesData
  #     rastTotalSpeciesData <- raster(inTotalSpeciesData$datapath)
  #     inHostDataSingle <- input$hostDataSingle
  #     rastHostDataSingle <- raster(inHostDataSingle$datapath)
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataSingle), na.color = "transparent")
  #     pal2 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInitialInfection), na.color = "transparent")
  #     pal3 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal2, group = "Initial Infection") %>%
  #       addRasterImage({rastTotalSpeciesData}, opacity=0.5, colors = pal3, group = "All Trees") %>%
  #       addRasterImage({rastHostDataSingle}, opacity=0.5, colors = pal, group = "Host") %>%
  #       addLayersControl(
  #         overlayGroups =c("Host", "All Trees", "Initial Infection"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==FALSE && is.null(input$totalSpeciesData)==FALSE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==TRUE && is.null(input$hostDataM2)==TRUE) {
  #     inInitialInfection <- input$initialInfection
  #     rastInitialInfection <- raster(inInitialInfection$datapath)
  #     inTotalSpeciesData <- input$totalSpeciesData
  #     rastTotalSpeciesData <- raster(inTotalSpeciesData$datapath)
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
  #     pal2 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInitialInfection), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal2, group = "Initial Infection") %>%
  #       addRasterImage({rastTotalSpeciesData}, opacity=0.5, colors = pal, group = "All Trees") %>%
  #       addLayersControl(
  #         overlayGroups =c("All Trees", "Initial Infection"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==FALSE && is.null(input$totalSpeciesData)==FALSE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==FALSE && is.null(input$hostDataM2)==FALSE) {
  #     inInitialInfection <- input$initialInfection
  #     rastInitialInfection <- raster(inInitialInfection$datapath)
  #     inTotalSpeciesData <- input$totalSpeciesData
  #     rastTotalSpeciesData <- raster(inTotalSpeciesData$datapath)
  #     inHostDataM1 <- input$hostDataM1
  #     rastHostDataM1 <- raster(inHostDataM1$datapath)
  #     inHostDataM2 <- input$hostDataM2
  #     rastHostDataM2 <- raster(inHostDataM2$datapath)
  #     pal3 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM1), na.color = "transparent")
  #     pal4 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM2), na.color = "transparent")
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
  #     pal2 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInitialInfection), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataM1}, opacity=0.5, colors = pal3, group = "Host") %>%
  #       addRasterImage({rastHostDataM2}, opacity=0.5, colors = pal4, group = "Host 2") %>%
  #       addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal2, group = "Initial Infection") %>%
  #       addRasterImage({rastTotalSpeciesData}, opacity=0.5, colors = pal, group = "All Trees") %>%
  #       addLayersControl(
  #         overlayGroups = c("All Trees", "Initial Infection","Host","Host 2"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==TRUE && is.null(input$totalSpeciesData)==FALSE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==FALSE && is.null(input$hostDataM2)==FALSE) {
  #     inTotalSpeciesData <- input$totalSpeciesData
  #     rastTotalSpeciesData <- raster(inTotalSpeciesData$datapath)
  #     inHostDataM1 <- input$hostDataM1
  #     rastHostDataM1 <- raster(inHostDataM1$datapath)
  #     inHostDataM2 <- input$hostDataM2
  #     rastHostDataM2 <- raster(inHostDataM2$datapath)
  #     pal3 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM1), na.color = "transparent")
  #     pal4 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM2), na.color = "transparent")
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataM1}, opacity=0.5, colors = pal3, group = "Host") %>%
  #       addRasterImage({rastHostDataM2}, opacity=0.5, colors = pal4, group = "Host 2") %>%
  #       addRasterImage({rastTotalSpeciesData}, opacity=0.5, colors = pal, group = "All Trees") %>%
  #       addLayersControl(
  #         overlayGroups =c("All Trees", "Host","Host 2"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==FALSE && is.null(input$totalSpeciesData)==TRUE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==FALSE && is.null(input$hostDataM2)==FALSE) {
  #     inInitialInfection <- input$initialInfection
  #     rastInitialInfection <- raster(inInitialInfection$datapath)
  #     inHostDataM1 <- input$hostDataM1
  #     rastHostDataM1 <- raster(inHostDataM1$datapath)
  #     inHostDataM2 <- input$hostDataM2
  #     rastHostDataM2 <- raster(inHostDataM2$datapath)
  #     pal3 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM1), na.color = "transparent")
  #     pal4 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM2), na.color = "transparent")
  #     pal2 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInitialInfection), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataM1}, opacity=0.5, colors = pal3, group = "Host") %>%
  #       addRasterImage({rastHostDataM2}, opacity=0.5, colors = pal4, group = "Host 2") %>%
  #       addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal2, group = "Initial Infection") %>%
  #       addLayersControl(
  #         overlayGroups =c("Initial Infection","Host","Host 2"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==FALSE && is.null(input$totalSpeciesData)==FALSE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==FALSE && is.null(input$hostDataM2)==TRUE) {
  #     inInitialInfection <- input$initialInfection
  #     rastInitialInfection <- raster(inInitialInfection$datapath)
  #     inTotalSpeciesData <- input$totalSpeciesData
  #     rastTotalSpeciesData <- raster(inTotalSpeciesData$datapath)
  #     inHostDataM1 <- input$hostDataM1
  #     rastHostDataM1 <- raster(inHostDataM1$datapath)
  #     pal3 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM1), na.color = "transparent")
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
  #     pal2 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInitialInfection), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataM1}, opacity=0.5, colors = pal3, group = "Host") %>%
  #       addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal2, group = "Initial Infection") %>%
  #       addRasterImage({rastTotalSpeciesData}, opacity=0.5, colors = pal, group = "All Trees") %>%
  #       addLayersControl(
  #         overlayGroups =c("All Trees", "Initial Infection","Host"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==FALSE && is.null(input$totalSpeciesData)==FALSE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==TRUE && is.null(input$hostDataM2)==FALSE) {
  #     inInitialInfection <- input$initialInfection
  #     rastInitialInfection <- raster(inInitialInfection$datapath)
  #     inTotalSpeciesData <- input$totalSpeciesData
  #     rastTotalSpeciesData <- raster(inTotalSpeciesData$datapath)
  #     inHostDataM2 <- input$hostDataM2
  #     rastHostDataM2 <- raster(inHostDataM2$datapath)
  #     pal4 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM2), na.color = "transparent")
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
  #     pal2 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInitialInfection), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataM2}, opacity=0.5, colors = pal4, group = "Host 2") %>%
  #       addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal2, group = "Initial Infection") %>%
  #       addRasterImage({rastTotalSpeciesData}, opacity=0.5, colors = pal, group = "All Trees") %>%
  #       addLayersControl(
  #         overlayGroups =c("All Trees", "Initial Infection","Host 2"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==FALSE && is.null(input$totalSpeciesData)==TRUE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==TRUE && is.null(input$hostDataM2)==FALSE) {
  #     inInitialInfection <- input$initialInfection
  #     rastInitialInfection <- raster(inInitialInfection$datapath)
  #     inHostDataM2 <- input$hostDataM2
  #     rastHostDataM2 <- raster(inHostDataM2$datapath)
  #     pal4 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM2), na.color = "transparent")
  #     pal2 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInitialInfection), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataM2}, opacity=0.5, colors = pal4, group = "Host 2") %>%
  #       addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal2, group = "Initial Infection") %>%
  #       addLayersControl(
  #         overlayGroups =c("Initial Infection","Host 2"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==TRUE && is.null(input$totalSpeciesData)==FALSE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==TRUE && is.null(input$hostDataM2)==FALSE) {
  #     inTotalSpeciesData <- input$totalSpeciesData
  #     rastTotalSpeciesData <- raster(inTotalSpeciesData$datapath)
  #     inHostDataM2 <- input$hostDataM2
  #     rastHostDataM2 <- raster(inHostDataM2$datapath)
  #     pal4 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM2), na.color = "transparent")
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataM2}, opacity=0.5, colors = pal4, group = "Host 2") %>%
  #       addRasterImage({rastTotalSpeciesData}, opacity=0.5, colors = pal, group = "All Trees") %>%
  #       addLayersControl(
  #         overlayGroups =c("All Trees","Host 2"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==FALSE && is.null(input$totalSpeciesData)==TRUE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==FALSE && is.null(input$hostDataM2)==TRUE) {
  #     inInitialInfection <- input$initialInfection
  #     rastInitialInfection <- raster(inInitialInfection$datapath)
  #     inHostDataM1 <- input$hostDataM1
  #     rastHostDataM1 <- raster(inHostDataM1$datapath)
  #     pal3 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM1), na.color = "transparent")
  #     pal2 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInitialInfection), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataM1}, opacity=0.5, colors = pal3, group = "Host") %>%
  #       addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal2, group = "Initial Infection") %>%
  #       addLayersControl(
  #         overlayGroups =c("Initial Infection","Host"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   } else if (is.null(input$initialInfection)==TRUE && is.null(input$totalSpeciesData)==FALSE && is.null(input$hostDataSingle)==TRUE && is.null(input$hostDataM1)==FALSE && is.null(input$hostDataM2)==TRUE) {
  #     inTotalSpeciesData <- input$totalSpeciesData
  #     rastTotalSpeciesData <- raster(inTotalSpeciesData$datapath)
  #     inHostDataM1 <- input$hostDataM1
  #     rastHostDataM1 <- raster(inHostDataM1$datapath)
  #     pal3 <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM1), na.color = "transparent")
  #     pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
  #     leaflet(height = "300px") %>%
  #       addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  #       addRasterImage({rastHostDataM1}, opacity=0.5, colors = pal3, group = "Host") %>%
  #       addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal2, group = "Initial Infection") %>%
  #       addLayersControl(
  #         overlayGroups =c("All Trees","Host"),
  #         options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #   }
  # })
  
  
  # Allows for the downloading of the user manual when the download link is pressed
  output$pdf <- downloadHandler("generalizablepestandpathogenmodel.pdf", content = function(file){
    file.copy("C:\\Users\\Chris\\Desktop\\Generalizable Pest and Pathogen Model User.pdf",file)
  })
  
}