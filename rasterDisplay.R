rasterDisplayUI <- function(id, fileID, textID){
  ns <- NS(id)
  leafletOutput("ns(mapData)", height = "600px")
  fileInput(fileID, textID, accept = c(".tif"))
}


rasterDisplay <- function(input,output, seession, data){
  output$mapdata <- renderLeaflet({
    inFile <- input$fileID
    rastInFile <- raster(inFile$datapath)
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInFile), na.color = "transparent")
    leaflet(height = "300px") %>%
      addProviderTiles("Esri.WorldImagery", group="background 1") %>%
      addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal, group = "Initial Infection") %>%
      addLayersControl(
        overlayGroups ="Initial Infection",
        options = layersControlOptions(collapsed = FALSE, opacity =0.6)) %>%
      addLegend("bottomright", pal = pal, values = values(rastInitialInfection),
                title = "Host species",
                opacity = 1)
  })
}