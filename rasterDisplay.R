rasterDisplayUI <- function(id, fileID, textID){
  ns <- NS(id)
  tagList(
    fileInput(ns(fileID), ns(textID), accept = c(".tif")),
    bsAlert("initialInfectionID"),
    leafletOutput("mapData", height = "600px")
  )
}


rasterDisplay <- function(input,output, session){
  output$mapData <- renderLeaflet({
    if (extension(input$initialInfection$datapath) %in% c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")) {
      inFile <- input$fileID
      rastInFile <- raster(inFile$datapath)
      pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInFile), na.color = "transparent")
      olg <<- c(olg, ns(fileID))
      proxy %>%
        addRasterImage({rastInFile}, opacity=0.5, colors = pal, group = "Initial Infection") %>%
        addLayersControl(
          overlayGroups = olg,
          options = layersControlOptions(collapsed = FALSE, opacity =0.6))
    } else {
      createAlert(session, "initialInfectionID", content = "Please")
    }
  })
}

## Test the module with simple example


ui <- fluidPage(
  rasterDisplayUI('two', 'rock', 'insert roll')
)

server <- function(input, output){
  r <- raster("C:\\Users\\chris\\Dropbox\\Projects\\APHIS\\BayOakCode/layers/UMCA_den_100m.img")
  pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(r), na.color = "transparent")
  olg <- c()
  
  
  output$mapData <- renderLeaflet(
    leaflet(height = "300px") %>%
      addProviderTiles("Esri.WorldImagery", group="background 1") %>%
      addRasterImage(r, opacity=0.0)
  )
  proxy <- leafletProxy("mapData")
  callModule(rasterDisplay, "two")
}
shinyApp(ui, server)