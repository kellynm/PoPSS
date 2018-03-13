# Used to set the initial zoom of the map and color of the rasters
r <<- raster("./layers/UMCA_den_100m.img")
pal <<- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(r), na.color = "transparent")
usCounties <<- readOGR("./layers/usLower48Counties.shp")
usStates <<- readOGR("./layers/usLower48States.shp")
data2 <- data.frame(Year = 0,  Area = 0,  Count =0, Host =0)

function(input, output) {
  options(shiny.maxRequestSize=70000*1024^2) 
  # Creates the text file that is downloaded upon model completion
  #output$model <- renderPrint({c("Model inputs are:",input$wind, input$windData, input$temp, input$tempData, input$precip, input$precipData)})
  
  # Create Raster Stack for holding output from model run
  modelRastOut <- r
  olg <- c()
  dataForPlot <<- data2

  observeEvent(input$run, {
    years = seq(input$start, input$end, 1)
                           withBusyIndicatorServer("run",{dataList <- pest(rastHostDataM1,rastHostDataM2,rastTotalSpeciesData, rastInitialInfection,
                                 input$start, input$end, input$seasonQ, input$seasonMonths[1],input$seasonMonths[2], 
                                 input$sporeRate, input$windQ, input$windDir, './layers/weather/weatherCoeff_2000_2014.nc')}) 
                             proxy <- leafletProxy("mapData")
                             modelRastOut <<- dataList[[2]]
                             dataReturn <<- dataList[[1]]
                             if (nrow(dataForPlot)<=1 && nrow(dataReturn)>1){
                               make1 <- dataReturn[,1:3]
                               make2 <- dataReturn[,c(1,4:5)]
                               names(make1) <- c('Year','Area','Count')
                               names(make2) <- c('Year','Area','Count')
                               make1$Host <- 'Tanoak'
                               make2$Host <- 'Oaks'
                               dataForPlot <<- rbind(make1,make2) 
                             }
                           if (nlayers(modelRastOut)>1) {
                             #olg <- list(olg)
                             for (i in 1:(nlayers(modelRastOut)-1)){
                              pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(modelRastOut[[i]]), na.color = "transparent")
                              #olg[[i+1]] <- c(olg[[i]], paste("year", (years[nlayers(modelRastOut)-i])))
                              olg <<- c(olg, paste("year", (years[nlayers(modelRastOut)-i])))
                              proxy <- proxy %>% 
                               addRasterImage(modelRastOut[[i]], opacity= 0.8, group = paste("year", (years[nlayers(modelRastOut)-i]))) %>%
                               addLayersControl(
                                 #overlayGroups = olg[[i+1]],
                                 overlayGroups = olg,
                                 options = layersControlOptions(collapsed = FALSE, opacity =0.6))
                           }}
                           })
  
  ## Create information file of model input parameters
  observeEvent(input$run,{
    parameterTable <<- data.frame(Species = input$pest, Start = input$start, End = input$end, Seasonality = input$seasonQ, monthStart = input$seasonMonths[1],monthEnd = input$seasonMonths[2], ReproductionRate = input$sporeRate)
  })
  
  ## Set up plot and tab GUI state and county maps
  output$plotData <- renderPlot({plot(dataReturn$years, dataReturn$infectedHost2Individuals)})
  output$stateData <- renderLeaflet({
    leaflet(usStates) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
  })
  output$countyData <- renderLeaflet({
    leaflet(usCounties) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
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
        options = layersControlOptions(collapsed = FALSE, opacity =0.6))
      # addLegend("bottomright", pal = pal, values = values(rastInitialInfection),
      #           title = "Host species",
      #           opacity = 1)
  })
  observeEvent(input$totalSpeciesData, {
    inTotalSpeciesData <- input$totalSpeciesData
    rastTotalSpeciesData <<- raster(inTotalSpeciesData$datapath)
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
    olg <<- c(olg, "All Trees")
    proxy <- proxy %>%
        addRasterImage({rastTotalSpeciesData}, opacity=0.5, colors = pal, group = "All Trees") %>%
        addLayersControl(
          overlayGroups = olg,
          options = layersControlOptions(collapsed = FALSE, opacity =0.6))
        # addLegend("bottomright", pal = pal, values = values(rastTotalSpeciesData),
        #           title = "Host species",
        #           opacity = 1)
  })
  observeEvent(input$hostDataM1, {
    inHostDataM1 <- input$hostDataM1
    rastHostDataM1 <<- raster(inHostDataM1$datapath)
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM1), na.color = "transparent")
    olg <<- c(olg, "Host 1")
    proxy <- proxy %>%
      addRasterImage({rastHostDataM1}, opacity=0.5, colors = pal, group = "Host 1") %>%
      addLayersControl(
        overlayGroups = olg,
        options = layersControlOptions(collapsed = FALSE, opacity =0.6))
      # addLegend("bottomright", pal = pal, values = values(rastHostDataM1),
      #           title = "Host species",
      #           opacity = 1)
  })
  observeEvent(input$hostDataM2, {
    inHostDataM2 <- input$hostDataM2
    rastHostDataM2 <<- raster(inHostDataM2$datapath)
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM2), na.color = "transparent")
    olg <<- c(olg, "Host 2")
    proxy <- proxy %>%
      addRasterImage({rastHostDataM2}, opacity=0.5, colors = pal, group = "Host 2") %>%
      addLayersControl(
        overlayGroups = olg,
        options = layersControlOptions(collapsed = FALSE, opacity =0.6))
      # addLegend("bottomright", pal = pal, values = values(rastHostDataM2),
      #           title = "Host species",
      #           opacity = 1)
  })
  observeEvent(input$hostDataS1, {
    inHostDataS1 <- input$hostDataS1
    rastHostDataS1 <<- raster(inHostDataS1$datapath)
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataS1), na.color = "transparent")
    olg <<- c(olg, "Host 2")
    proxy <- proxy %>%
      addRasterImage({rastHostDataS1}, opacity=0.5, colors = pal, group = "Host") %>%
      addLayersControl(
        overlayGroups = olg,
        options = layersControlOptions(collapsed = FALSE, opacity =0.6))
    # addLegend("bottomright", pal = pal, values = values(rastHostDataS1),
    #           title = "Host species",
    #           opacity = 1)
  })
  
  output$mapData <- renderLeaflet({
      leaflet(height = "300px") %>%
        addProviderTiles("Esri.WorldImagery", group="background 1") %>%
        addRasterImage(r, opacity=0.0)
  })
  proxy <- leafletProxy("mapData")
  
  output$extentMatch <- renderText({
      exMatch <- extent(inTemp)==extent(inWind)
      if (exMatch==TRUE){
        print("Your extents match")
      } else if (exMatch == FALSE) {
        print("Your extents don't match would you like to resample")
      }
  })
  
  output$plotData <- renderPlot({
    data <- data.frame(Year = dataForPlot$Year, Variable = dataForPlot[[input$plotDataSelect]], Host = dataForPlot$Host)
    if (input$plotDataSelect == "Area"){
      yName <- expression("Infected Area "*~(m^2))
    } else {
      yName <- expression("Number of Infected Trees")
    }
    title = "Model Output"
    theme = theme_set(theme_classic())
    theme = theme_update(legend.position="top", legend.title=element_blank(),legend.spacing=unit(-0.5,"lines"), plot.background = element_rect(fill = "#3F3E3E", colour = "#3F3E3E"), panel.background = element_rect(fill = "#3F3E3E", colour = "#3F3E3E"), legend.background = element_rect(fill = "#3F3E3E"))#,panel.margin =unit(c(0,0,0,0),"null"), plot.margin =unit(c(0,0,0,0),"null"))
    theme = theme_update(axis.text = element_text(size = 12, colour="white"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5,colour="white", size =18), axis.line = element_line(colour="white"),axis.title=element_text(size=16, vjust=0,35,colour="white"),legend.text=element_text(size=12,colour="white"))
    ggplot(data, aes(x=Year, y=Variable, color=factor(Host)))+geom_line(aes(Year, Variable), size = 1.5)+scale_color_manual(values=c("#54ACC1", "#ADBD60"))+scale_fill_manual(values=c("#54ACC1", "#ADBD60"))+
      ggtitle(title)+
      scale_x_continuous(name="Year", breaks=seq(input$start, input$end, 2))+
      scale_y_continuous(name=yName)+guides(col=guide_legend(ncol=3),shape=guide_legend(ncol = 1))
  })
  
  # Allows for the downloading of the user manual when the download link is pressed
  output$pdf <- downloadHandler("generalizablepestandpathogenmodel.pdf", content = function(file){
    file.copy("C:\\Users\\Chris\\Desktop\\Generalizable Pest and Pathogen Model User.pdf",file)
  })
  
}