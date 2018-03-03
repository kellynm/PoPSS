rasterDisplayUI <- function(id, fileID, textID){
  ns <- NS(id)
  tagList(
    fileInput(ns(fileID), ns(textID), accept = c(".tif")),
    leafletOutput(ns("mapData"), height = "600px")
  )
}


rasterDisplay <- function(input,output, session){
  output$mapData <- renderLeaflet({
    inFile <- input$fileID
    rastInFile <- raster(inFile$datapath)
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInFile), na.color = "transparent")
    olg = c(olg, input$id)
    leafletProxy("mapData") %>%
      addProviderTiles("Esri.WorldImagery", group="background 1") %>%
      addRasterImage({rastInFile}, opacity=0.5, colors = pal, group = "Initial Infection") %>%
      addLayersControl(
        overlayGroups = olg,
        options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  })
}


ui <- fluidPage(
  rasterDisplayUI('two', 'rock', 'insert roll')
)

server <- function(input, output){
  r <- raster("C:\\Users\\chris\\Dropbox\\Projects\\APHIS\\BayOakCode/layers/UMCA_den_100m.img")
  pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(r), na.color = "transparent")
  
  output$mapData <- renderLeaflet(
    leaflet(height = "300px") %>%
      addProviderTiles("Esri.WorldImagery", group="background 1") %>%
      addRasterImage(r, opacity=0.0)
  )
  #callModule(rasterDisplay, "two")
}
shinyApp(ui, server)