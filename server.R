# Used to set the initial zoom of the map and color of the rasters
r <<- raster("UMCA_den_100m.img")
pal <<- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(r), na.color = "transparent")
#usCounties <<- readOGR("layers/usLower48Counties.shp")
usStates <<- readOGR("layers/usLower48States.shp")
dataList <<- c()
pest_vars <<- list(host1_rast = NULL, host1_score = NULL, host2_rast=NULL, host2_score=NULL, host3_rast=NULL, host3_score=NULL, host4_rast=NULL, host4_score=NULL,
                  host5_rast=NULL, host5_score=NULL, host6_rast=NULL, host6_score=NULL, host7_rast=NULL, host7_score=NULL, host8_rast=NULL, host8_score=NULL,
                  host9_rast=NULL, host9_score=NULL, host10_rast=NULL, host10_score=NULL, allTrees=NULL, initialPopulation=NULL, start=2000, end=2010, 
                  seasonality = 'NO', s1 = 1 , s2 = 12, sporeRate = 4.4, windQ =NULL, windDir=NULL, tempQ="NO", tempData=NULL, precipQ="NO", 
                  precipData=NULL, kernelType ='Cauchy', kappa = 2, number_of_hosts = 1, scale1 = 20.57, scale2 = NULL, gamma = 1, seed_n = 42,
                  time_step ="weeks")

weather_coeff_vars <<- list(directory = NULL, output_directory = NULL, start = NULL, end = NULL, time_step = 'daily', states_of_interest = c('Maryland'), pest = NULL, 
                        prcp_index = 'NO', prcp_method = NULL,  prcp_a0 = 0, prcp_a1 = 0, prcp_a2 = 0, prcp_a3 = 0, 
                        prcp_thresh = 0, prcp_x1mod =0, prcp_x2mod = 0, prcp_x3mod = 0,
                        temp_index = 'NO', temp_method = NULL, temp_a0 = 0, temp_a1 = 0, temp_a2 = 0, temp_a3 = 0, 
                        temp_thresh = 0, temp_x1mod = 0, temp_x2mod = 0, temp_x3mod = 0)

function(input, output, session) {
  options(shiny.maxRequestSize=70000*1024^2) 
  ## Creates the text file that is downloaded upon model completion
  
  ## Create Raster Stack for holding output from model run
  modelRastOut <- r
  olg <- c()
  ## create list of all variables for pest function with defaults assigned exactly the same as defaults in function with mandatory variables assigned to null
  observeEvent(input$run, {
    years = seq(input$start, input$end, 1)
                           withBusyIndicatorServer("run",{dataList <<- do.call(pest, pest_vars)})
                             proxy <- leafletProxy("mapData")
                             modelRastOut <<- dataList[[2]]
                             dataReturn <<- dataList[[1]]
                             make1 <- dataReturn[,1:3]
                             make2 <- dataReturn[,c(1,4:5)]
                             names(make1) <- c('Year','Count','Area')
                             names(make2) <- c('Year','Count','Area')
                             make1$Host <- 'Tanoak'
                             make2$Host <- 'Oaks'
                             dataForPlot <<- rbind(make1,make2) 
                             rUnit <<- getUnit(rastHostDataM1, dataForPlot)
                           if (nlayers(modelRastOut)>1) {
                             for (i in 1:(nlayers(modelRastOut))){
                              pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(modelRastOut[[i]]), na.color = "transparent")
                              olg <<- c(olg, paste("year", (years[i])))
                              proxy <- proxy %>% 
                               addRasterImage(modelRastOut[[i]], opacity= 0.8, group = paste("year", (years[i]))) %>%
                               addLayersControl(
                                 overlayGroups = olg,
                                 baseGroups = c("Imagery", "Toner", "Toner Lite", "Terrain", "Carto", "Carto Dark"),
                                 options = layersControlOptions(collapsed = FALSE, opacity =0.6))
                           }}
                           })
  ## Download zip folder when download button is pressed
  observeEvent(input$zip, {
    zippath <- paste(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/output.zip",sep = "")
    zipcreator(dataList = dataList, path = zippath)
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
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))
  })

  # output$countyData <- renderLeaflet({
  #   leaflet(usCounties) %>%
  #     addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
  #                 opacity = 1.0, fillOpacity = 0.5,
  #                 highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                                     bringToFront = TRUE))
  # })
  
  ## Add/change parameter values when the inputs change in the GUI to pest_vars parameter list. This list is used to pass most recent parameter values to the model when the run button is pressed.
  observeEvent(input$start, {pest_vars$start <<- input$start})
  observeEvent(input$end, {pest_vars$end <<- input$end})
  observeEvent(input$seasonQ, {pest_vars$seasonality <<- input$seasonQ})
  observeEvent(input$seasonMonths, {pest_vars$s1 <<- input$seasonMonths[1]
  pest_vars$s2 <<- input$seasonMonths[2]})
  observeEvent(input$sporeRate, {pest_vars$sporeRate <<- input$sporeRate})
  observeEvent(input$kernelType, {pest_vars$kernelType <<- input$kernelType})
  observeEvent(input$scale_1, {pest_vars$scale1 <<- input$scale_1})
  observeEvent(input$scale_2, {pest_vars$scale2 <<- input$scale_2})
  observeEvent(input$seed, {pest_vars$seed_n <<- input$seed})
  observeEvent(input$gamma, {pest_vars$gamma <<- input$gamma})
  observeEvent(input$time_step, {pest_vars$time_step <<- input$time_step})
  observeEvent(input$hostMulti, {pest_vars$number_of_hosts <<- input$hostMulti})
  observeEvent(input$windQ, {pest_vars$windQ <<- input$windQ})
  observeEvent(input$windDir, {pest_vars$windDir <<- input$windDir})
  observeEvent(input$kappa, {pest_vars$kappa <<- input$kappa})
  observeEvent(input$temp, {pest_vars$tempQ <<- input$temp})
  observeEvent(input$tempData, {tempData <- input$tempData$datapath
    pest_vars$tempData <<- tempData})
  observeEvent(input$precip, {pest_vars$precipQ <<- input$precip})
  observeEvent(input$precipData, {precipData <- input$precipData$datapath
    pest_vars$precipData <<- precipData})
  observeEvent(input$hostIndexScore1, {pest_vars$host1_score <<- input$hostIndexScore1})
  observeEvent(input$hostIndexScore2, {pest_vars$host2_score <<- input$hostIndexScore2})
  observeEvent(input$hostIndexScore3, {pest_vars$host3_score <<- input$hostIndexScore3})
  observeEvent(input$hostIndexScore4, {pest_vars$host4_score <<- input$hostIndexScore4})
  observeEvent(input$hostIndexScore5, {pest_vars$host5_score <<- input$hostIndexScore5})
  observeEvent(input$hostIndexScore6, {pest_vars$host6_score <<- input$hostIndexScore6})
  observeEvent(input$hostIndexScore7, {pest_vars$host7_score <<- input$hostIndexScore7})
  observeEvent(input$hostIndexScore8, {pest_vars$host8_score <<- input$hostIndexScore8})
  observeEvent(input$hostIndexScore9, {pest_vars$host9_score <<- input$hostIndexScore9})
  observeEvent(input$hostIndexScore10, {pest_vars$host10_score <<- input$hostIndexScore10})
  
  ## Add/change parameter values when the inputs change in the GUI to weather_coeff_vars parameter list. This list is used to pass the most recent parameter values to the weather coeff creation fuction.
  observeEvent(input$start, {weather_coeff_vars$start <<- input$start})
  observeEvent(input$end, {weather_coeff_vars$end <<- input$end})
  observeEvent(input$time_step, {weather_coeff_vars$time_step <<- input$time_step})
  observeEvent(input$pest, {weather_coeff_vars$pest <<- input$pest})
  observeEvent(input$prcp, {weather_coeff_vars$prcp_index <<- input$prcp})
  observeEvent(input$prcp_method, {weather_coeff_vars$prcp_method <<- input$prcp_method})
  observeEvent(input$prcp_a0, {weather_coeff_vars$prcp_a0 <<- input$prcp_a0})
  observeEvent(input$prcp_a1, {weather_coeff_vars$prcp_a1 <<- input$prcp_a1})
  observeEvent(input$prcp_a2, {weather_coeff_vars$prcp_a2 <<- input$prcp_a2})
  observeEvent(input$prcp_a3, {weather_coeff_vars$prcp_a3 <<- input$prcp_a3})
  observeEvent(input$prcp_thresh, {weather_coeff_vars$prcp_thresh <<- input$prcp_thresh})
  observeEvent(input$prcp_x1mod, {weather_coeff_vars$prcp_x1mod <<- input$prcp_x1mod})
  observeEvent(input$prcp_x2mod, {weather_coeff_vars$prcp_x2mod <<- input$prcp_x2mod})
  observeEvent(input$prcp_x3mod, {weather_coeff_vars$prcp_x3mod <<- input$prcp_x3mod})
  observeEvent(input$temp_index, {weather_coeff_vars$temp_index <<- input$temp_index})
  observeEvent(input$temp_method, {weather_coeff_vars$temp_method <<- input$temp_method})
  observeEvent(input$temp_a0, {weather_coeff_vars$temp_a0 <<- input$temp_a0})
  observeEvent(input$temp_a1, {weather_coeff_vars$temp_a1 <<- input$temp_a1})
  observeEvent(input$temp_a2, {weather_coeff_vars$temp_a2 <<- input$temp_a2})
  observeEvent(input$temp_a3, {weather_coeff_vars$temp_a3 <<- input$temp_a3})
  observeEvent(input$temp_thresh, {weather_coeff_vars$temp_thresh <<- input$temp_thresh})
  observeEvent(input$temp_x1mod, {weather_coeff_vars$temp_x1mod <<- input$temp_x1mod})
  observeEvent(input$temp_x2mod, {weather_coeff_vars$temp_x2mod <<- input$temp_x2mod})
  observeEvent(input$temp_x3mod, {weather_coeff_vars$temp_x3mod <<- input$temp_x3mod})
  observeEvent(input$input_directory, {weather_coeff_vars$directory <<- input$input_directory})
  observeEvent(input$output_directory, {weather_coeff_vars$output_directory <<- input$output_directory})
  observeEvent(input$study_area, {weather_coeff_vars$states_of_interest <<- input$study_area})
  
  ## Run weather coefficient creator to create weather coefficients for model input
  observeEvent(input$weather_coeff_run, {
    withBusyIndicatorServer("weather_coeff_run",{do.call(weather_coeff, weather_coeff_vars)})
  })
  
  ## Set up GUI maps to be flexible
  observeEvent(input$initialInfection, {
    if (extension(input$initialInfection$datapath) %in% c(".tif", ".grd", ".asc", ".sdat", ".rst", ".nc", ".tif", ".envi", ".bil", ".img")) {
      rastInitialInfection <<- raster(input$initialInfection$datapath)
      pest_vars$initialPopulation <<- rastInitialInfection
      pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastInitialInfection), na.color = "transparent")
      olg <<- c(olg, "Initial Infection")
      proxy <- proxy %>%
        addRasterImage({rastInitialInfection}, opacity=0.5, colors = pal, group = "Initial Infection") %>%
        addLayersControl(
          overlayGroups = olg,
          baseGroups = c("Imagery", "Toner", "Toner Lite", "Terrain", "Carto", "Carto Dark"),
          options = layersControlOptions(collapsed = TRUE, opacity =0.6))
      # addLegend("bottomright", pal = pal, values = values(rastInitialInfection),
      #           title = "Host species",
      #           opacity = 1)
    } else {
      createAlert(session, "initialInfectionID", content = "Incorrect file type. Please select a raster file type.")
    }
  })
  observeEvent(input$totalSpeciesData, {
    inTotalSpeciesData <- input$totalSpeciesData
    rastTotalSpeciesData <<- raster(inTotalSpeciesData$datapath)
    pest_vars$allTrees <<- rastTotalSpeciesData
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastTotalSpeciesData), na.color = "transparent")
    olg <<- c(olg, "All Trees")
    proxy <- proxy %>%
        addRasterImage({rastTotalSpeciesData}, opacity=0.5, colors = pal, group = "All Trees") %>%
        addLayersControl(
          overlayGroups = olg,
          baseGroups = c("Imagery", "Toner", "Toner Lite", "Terrain", "Carto", "Carto Dark"),
          options = layersControlOptions(collapsed = TRUE, opacity =0.6))
  })
  observeEvent(input$hostDataM1, {
    inHostDataM1 <- input$hostDataM1
    rastHostDataM1 <<- raster(inHostDataM1$datapath)
    pest_vars$host1_rast <<- rastHostDataM1
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM1), na.color = "transparent")
    olg <<- c(olg, "Host 1")
    proxy <- proxy %>%
      addRasterImage({rastHostDataM1}, opacity=0.5, colors = pal, group = "Host 1") %>%
      addLayersControl(
        overlayGroups = olg,
        baseGroups = c("Imagery", "Toner", "Toner Lite", "Terrain", "Carto", "Carto Dark"),
        options = layersControlOptions(collapsed = TRUE, opacity =0.6))
  })
  observeEvent(input$hostDataM2, {
    inHostDataM2 <- input$hostDataM2
    rastHostDataM2 <<- raster(inHostDataM2$datapath)
    pest_vars$host2_rast <<- rastHostDataM2
    pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(rastHostDataM2), na.color = "transparent")
    olg <<- c(olg, "Host 2")
    proxy <- proxy %>%
      addRasterImage({rastHostDataM2}, opacity=0.5, colors = pal, group = "Host 2") %>%
      addLayersControl(
        overlayGroups = olg,
        baseGroups = c("Imagery", "Toner", "Toner Lite", "Terrain", "Carto", "Carto Dark"),
        options = layersControlOptions(collapsed = TRUE, opacity =0.6))
  })
  
  output$mapData <- renderLeaflet({
      leaflet(height = "300px") %>%
        addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
        addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
        addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
        addProviderTiles("Stamen.Toner", group = "Toner") %>%
        addProviderTiles("CartoDB.Positron", group = "Carto") %>%
        addProviderTiles("CartoDB.DarkMatterNoLabels", group = "Carto Dark") %>%
        addRasterImage(r, opacity=0.0) %>%
        addLayersControl(
          baseGroups = c( "Terrain","Imagery", "Toner", "Toner Lite", "Carto", "Carto Dark"),
          options = layersControlOptions(collapsed = TRUE)
        )
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
      yName <- expression("Infected Area "*~(ha))
    } else {
      yName <- expression("Number of Infected Trees (in thousands)")
    }
    if(!is.null(input$pest)){
      title = input$pest
    } else {
      title = "Model Output"
    }
    theme = theme_set(theme_classic())
    theme = theme_update(legend.position="top", legend.title=element_blank(),legend.spacing=unit(-0.5,"lines"), plot.background = element_rect(fill = "#3F3E3E", colour = "#3F3E3E"), panel.background = element_rect(fill = "#3F3E3E", colour = "#3F3E3E"), legend.background = element_rect(fill = "#3F3E3E"))
    theme = theme_update(axis.text = element_text(size = 12, colour="white",family = "Helvetica"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5,colour="white", size =18,family = "Helvetica"), axis.line = element_line(colour="white"),axis.title=element_text(size=16, vjust=0,35,colour="white", family = "Helvetica"),legend.text=element_text(size=12,colour="white",family = "Helvetica"))
    ggplot(data, aes(x=Year, y=Variable, color=factor(Host)))+geom_line(aes(Year, Variable), size = 1.5)+scale_color_manual(values=c("#54ACC1", "#ADBD60"))+scale_fill_manual(values=c("#54ACC1", "#ADBD60"))+
      ggtitle(title)+theme(text = element_text(family = "sans"))+
      scale_x_continuous(name="Year", breaks=seq(input$start, input$end, 2))+
      scale_y_continuous(name=yName)+guides(col=guide_legend(ncol=3),shape=guide_legend(ncol = 1))
  })
  
  ## Create reactive raster for slider object to update and allow for animation
  # host1Years <- reactive({subset(modelRastOut, input$yearSlider)})
  # ras_vals <- reactive({values(host1Years())})
  # pal2 <- reactive({colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), ras_vals(), na.color="transparent")})

  observe({
    # host1Years <- subset(modelRastOut, input$yearSlider)
    # ras_vals <- values(host1Years)
    # pal2 <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), ras_vals, na.color="transparent")
    #proxy <<- proxy %>% addRasterImage(host1Years(), colors=pal2(), opacity=0.8, layerId = "hostIMG")
    #proxy <<- proxy %>% removeImage(layerID = "hostIMG") %>% addRasterImage(host1Years, colors=pal2, opacity=0.8, layerId = "hostIMG")
  })
  
  ## Create slider for playing through years
  output$movie_slider <- renderUI({
    sliderInput("dec", "Year", min=1, max=11, value=1, step=1, animate= animationOptions(interval=1000))
  })
  
  ## Create Input for selecting host based on number of hosts
  output$host_selection <- renderUI({
    numericInput("host", label = "Choose species", min = 1, max = input$hostMulti, value = 1)
  })
  
  
  ## Allows for the downloading of the user manual when the download link is pressed
  output$userManual.pdf <- downloadHandler("user_manual.pdf", content = function(file){
    file.copy("Documentation\\User Manual.pdf",file)
  }, contentType = "pdf")
}