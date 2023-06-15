library(waiter)
library(shinyalert)
app2_init<-function(input,output,session){
  ###APp 2
  observe({
    dropdown_options <- addShapefileDropdown()
    print(dropdown_options)
    
    if (length(dropdown_options) == 0) {
      shinyalert("Oops!", "No shapefiles were found. Please return to the Data Cleaning tab and clean Movebank data first.", type = "error")
    } else {
      updateSelectInput(session, "shapefileDropdown", choices = dropdown_options)
    }
  })
  

  #Create a spinner
  w <- Waiter$new(
    html = tagList(
      spin_3(),
      h4("Loading Movebank data...", style = "color: grey") # Add style attribute to h4 element
    ),
    color = transparent(.5)
  )
  gis <- Waiter$new(
    html = tagList(
      spin_3(),
      h4("Loading GIS data...", style = "color: grey") # Add style attribute to h4 element
    ),
    color = transparent(.5)
  )
  
  gis$show()
  
  #Load in GIS Layers
  sf_list <- load_geojson(urls)
  
  MuleDeerCrucialRange <- sf_list[[1]]
  MuleDeerHerdUnits <- sf_list[[2]]
  MuleDeerSeasonalRange<- sf_list[[3]]
  DeerHuntAreas <- sf_list[[4]]
  AntelopeHerdUnits <- sf_list[[5]]
  AntelopeHuntAreas <- sf_list[[6]]
  BisonHerdUnits <- sf_list[[7]]
  BisonHuntAreas <- sf_list[[8]]
  ElkHerdUnits <- sf_list [[9]]
  ElkHuntAreas <- sf_list [[10]]
  MooseHerdUnits <- sf_list [[11]]
  MooseHuntAreas <- sf_list [[12]]
  BighornSheepHerdUnits <-sf_list [[13]]
  BighornSheepHuntAreas <- sf_list [[14]]
  BioDistricts <- sf_list [[15]]
  AdminRegions<- sf_list [[16]]
  
  layerNames <- c("MuleDeerCrucialRange", "MuleDeerHerdUnits", "MuleDeerSeasonalRange", "DeerHuntAreas", "AntelopeHerdUnits", "AntelopeHuntAreas", "BisonHerdUnits", "BisonHuntAreas", "ElkHerdUnits", "ElkHuntAreas", "MooseHerdUnits", "MooseHuntAreas", "BighornSheepHerdUnits", "BighornSheepHuntAreas", "BioDistricts", "AdminRegions")
  
  # Create select inputs for layer, column, and column value
  updateSelectInput(session, "selectLayer", choices = layerNames, selected = NULL)
  updateSelectInput(session, "selectColumn", choices = NULL, selected = NULL)
  updateSelectInput(session, "selectColumnValue", choices = NULL)
  
  
  # Update the choices of selectColumn based on the selected layer
  observeEvent(input$selectLayer, {
    layerIndex <- match(input$selectLayer, layerNames)
    if (!is.na(layerIndex)) {
      layer <- sf_list[[layerIndex]]
      columnNames <- colnames(layer)
      updateSelectInput(session, "selectColumn", choices = columnNames)
    }
  })
  
  # Update the choices of selectColumnValue based on the selected layer and column
  observeEvent(c(input$selectLayer, input$selectColumn), {
    layerIndex <- match(input$selectLayer, layerNames)
    if (!is.na(layerIndex)) {
      layer <- sf_list[[layerIndex]]
      column <- input$selectColumn
      columnValues <- unique(layer[[column]])
      updateSelectInput(session, "selectColumnValue", choices = columnValues)
    }
  })

  gis$hide()
  
  
  
  filtered_animals <- reactive({
    req(!is.null(movebankData()))
    req(input$connect)
    if (input$selectProject == "All") {
      return(unique(movebankData()$newuid))
    } else {
      return(unique(movebankData()$newuid[movebankData()$studyname == input$selectProject]))
    }
  })
  
  # Update the selectAnimal dropdown whenever the selectProject value changes
  observeEvent(input$selectProject, {
    req(!is.null(movebankData()))
    updateSelectInput(session, "selectAnimal", choices = c("All", sort(filtered_animals())), selected = NULL)
  })
  
  movebankData<- eventReactive(input$connect,{
    req(input$connect)
    w$show()
    movebankData<- combineprojects(MuleDeerHerdUnits,AntelopeHerdUnits,input$shapefileDropdown,DeerHuntAreas,AntelopeHuntAreas,AntelopeSeasonalRange,MuleDeerSeasonalRange,BisonHerdUnits,BisonHuntAreas)
    updateSelectInput(session, "selectAnimal", choices = c("All",sort(unique(movebankData$newuid), decreasing = FALSE)), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectProject", choices = c("All",sort(unique(movebankData$studyname ))), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectMonth", choices = c("All",sort(unique(movebankData$month ))), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectYear", choices = c("All",sort(unique(movebankData$year ))), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectUnit", choices = c("All",sort(unique(movebankData$first_loc_herdunit_name ))), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    updateSelectInput(session, "selectSpecies", choices = c("All",sort(unique(movebankData$species ))), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    #updateSelectInput(session, "selectHuntUnit", choices = c("All",sort(unique(movebankData$hunt_name_col ))), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)
    #updateSelectInput(session, "selectHerdUnit", choices = c("All",sort(unique(movebankData$herd_unit_col ))), selected = NULL) #c("All", sort(unique(data$tag.local.identifier))), selected = NULL)

     w$hide()
    # Return the combined sf object
    updateTabsetPanel(session, "QueryBuilder",selected = "Query Builder")
    return(movebankData)
  })
  
  set_token("pk.eyJ1IjoianNoYXBpcm8xIiwiYSI6ImNrdDA1OGR5MzAxeHIyb290am05MzF1c2IifQ.wuOxNF5KFK0pjUJ3O80OmA") #this is jessie's token
  
  output$myMap <- renderMapdeck({
    mapdeck(style = 'mapbox://styles/mapbox/outdoors-v11', zoom = 5,location = c(-107.290283, 43.075970) ) 
    
  })
  
  
  
  movebankFilter <- eventReactive(input$query,{
    req(input$query)
    data <- movebankData()
    movebankFilter <- execute_safely(queryFilter(data, input$dateRange, input$selectAnimal, input$selectProject, input$selectMonth, input$selectYear, input$selectUnit, 
                                                 input$selectSpecies, input$selectRange, merged_polygons, input$selectSpatialFilter,
                                                 input$selectUnitID, input$selectHuntUnit, input$selectHerdUnit,input$selectLayer, input$selectColumn, 
                                                 input$selectColumnValue, MuleDeerHerdUnits,MuleDeerSeasonalRange,DeerHuntAreas,AntelopeHerdUnits,AntelopeHuntAreas,BisonHerdUnits,BisonHuntAreas,ElkHerdUnits,ElkHuntAreas,MooseHerdUnits,MooseHuntAreas,BighornSheepHerdUnits,BighornSheepHuntAreas,BioDistricts,AdminRegions))
     if (is.null(movebankFilter) || nrow(movebankFilter) == 0) {
      showNotification("No data found. Please adjust your search criteria.", type = "error")
      return(NULL)
    }
    
    updateTabsetPanel(session, "QueryBuilder",selected = "Results")
    
    return(movebankFilter)
  })
  
  
  
  
  output$info_animals <- renderUI({
    req(nrow(movebankFilter()) > 1, "The data must have more than one record.")
    
    data<- movebankFilter()
    get_animal_info(data) 
    
  })
  animalSum <- reactive({
    req(input$query)
    data<- movebankFilter()
    animalSummary(data)
  })
  indvSum <- reactive({
    req(input$query)
    data<- movebankFilter()
    indivSummary(data)
  })
  
  output$herdsummary <- renderTable(animalSum())
  output$indvsummary <- renderTable(indvSum(),
                                    rownames = TRUE, 
                                    colnames = TRUE, 
                                    class = "compact-table")
  
  lines<- reactive({
    req(nrow(movebankFilter()) > 1, "The data must have more than one record.")
    
    data<- movebankFilter()
    setorder( data, newuid, datetest )
    lines<- sfheaders::sf_linestring(
      obj = data
      , x = "lon"
      , y = "lat"
      , linestring_id = "newuid"
    )
    return(lines)
  })
  
  mapzoom <- reactive({
    
    input$myMap_zoom
    
  })
  
  observe( {
    req(movebankData())
    data<- movebankData()
    data<- locationViewer(data)
    print(mapzoom)
    mapdeck_update(map_id = "myMap") %>%
      add_scatterplot(
        data = data,
        lat = "lat",
        lon = "lon",
        tooltip = c("datetest", "newuid"),
        fill_colour = "#6084D7",
       # radius = 150,
        
        radius_min_pixels = 2,
        stroke_colour = "black",
        stroke_width = 1,
        layer_id = "locationViewer"
      )
  })
  
  ## QUery Results
  observeEvent(input$query,{
    req(nrow(movebankFilter()) > 1, "The data must have more than one record.")
    mapdeck_update(map_id = "myMap") %>%
      
      add_path(
        data = lines(),
        stroke_width = 50,
        stroke_colour = "#505050",
        layer_id = "tracklines"
      ) %>% 
      add_scatterplot(
        data = movebankFilter(),
        lat = "lat",
        lon = "lon",
      #  radius = 150,
        radius_min_pixels = 2,
        stroke_colour = "black",
        stroke_width = 1,
        tooltip = c("datetest", "newuid"),
        #tooltip = 'datetest',
        auto_highlight = TRUE,
        update_view = TRUE,
        layer_id = "queryResults"
      )   %>%
      clear_scatterplot(layer_id = "locationViewer")
  
  })
  
  ## Heat Map Toggle
  observeEvent(input$heatMap,{
    if ( input$heatMap == TRUE) {
    req(nrow(movebankFilter()) > 1, "The data must have more than one record.")
    mapdeck_update(map_id = "myMap") %>%
      
      add_heatmap(
        data = movebankFilter()
        , lat = "lat"
        , lon = "lon"
        , layer_id = "heatmap_layer"
      )  
    } else {
        mapdeck_update(map_id = "myMap") %>%
          clear_heatmap(layer_id = "heatmap_layer")%>%
        clear_scatterplot(layer_id = "locationViewer")
      }
  })
  
  ## Trackline color toggle
  observeEvent(input$color,{
    if (input$color == TRUE) {
      
    req(nrow(movebankFilter()) > 1, "The data must have more than one record.")
      
    lines<- lines()
    
    unique_values <- unique(lines$newuid)
    # Define the number of colors to generate
    num_colors <- length(unique_values)
    colors <- colorRampPalette(brewer.pal(9, "Set1"))(num_colors)
    color_mapping <- setNames(colors, unique_values)
    
    # Create a new column of hex colors based on the values in the "newuid" column
    lines$color <- color_mapping[lines$newuid]
    
    mapdeck_update(map_id = "myMap") %>%
      
      add_path(
        data = lines,
        stroke_width = 50,
        stroke_colour = "color",
        layer_id = "colortracks"
      )  %>% 
      add_scatterplot(
        data = movebankFilter(),
        lat = "lat",
        lon = "lon",
         radius_min_pixels = 2,
        stroke_colour = "black",
        stroke_width = 1,
        tooltip = c("datetest", "newuid"),
        auto_highlight = TRUE,
        update_view = FALSE
      )
    } else {
      mapdeck_update(map_id = "myMap") %>%
        clear_path(layer_id = "colortracks")
    }
    
  })
  
  
  ## GIS Layers
  
  observeEvent({input$MuleDeerCrucialRange},{
    
    if ( input$MuleDeerCrucialRange == TRUE) {
      mapdeck_update(map_id = "myMap") %>%
        add_polygon(
          data = MuleDeerCrucialRange
          , layer_id = "MuleDeerCrucialRange"
          , update_view = FALSE
        )
    } else {
      mapdeck_update(map_id = "myMap") %>%
        clear_polygon(layer_id = "MuleDeerCrucialRange")
    }
  })
  
  observeEvent({input$MuleDeerHerdUnits},{
    
    if ( input$MuleDeerHerdUnits == TRUE) {
      mapdeck_update(map_id = "myMap") %>%
        add_polygon(
          data = MuleDeerHerdUnits,
          fill_colour = "#523A2875",
          stroke_colour = "#FFFFFF",
          tooltip = 'MD_HERDNAME',
          auto_highlight = TRUE,
          highlight_colour = "#523A2899",
          stroke_width = 350
          , layer_id = "MuleDeerHerdUnits"
          , update_view = FALSE
        )
    } else {
      mapdeck_update(map_id = "myMap") %>%
        clear_polygon(layer_id = "MuleDeerHerdUnits")
    }
  })
  
  
  observeEvent({input$MuleDeerSeasonalRange},{
    
    if ( input$MuleDeerSeasonalRange == TRUE) {
      mapdeck_update(map_id = "myMap") %>%
        add_polygon(
          data = MuleDeerSeasonalRange,
          fill_colour = "#698DDF75",
          stroke_colour = "#FFFFFF",
          tooltip = 'RANGE',
          auto_highlight = TRUE,
          highlight_colour = "#698DDF99",
          stroke_width = 350
          , layer_id = "MuleDeerSeasonalRange"
          , update_view = FALSE
        )
    } else {
      mapdeck_update(map_id = "myMap") %>%
        clear_polygon(layer_id = "MuleDeerSeasonalRange")
    }
  })
  observeEvent({input$DeerHuntAreas},{
    
    if ( input$DeerHuntAreas == TRUE) {
      mapdeck_update(map_id = "myMap") %>%
        add_polygon(
          data = DeerHuntAreas,
          fill_colour = "#698DDF75",
          stroke_colour = "#FFFFFF",
          tooltip = 'HUNTNAME',
          auto_highlight = TRUE,
          highlight_colour = "#698DDF99",
          stroke_width = 350
          , layer_id = "DeerHuntAreas"
          , update_view = FALSE
        )
    } else {
      mapdeck_update(map_id = "myMap") %>%
        clear_polygon(layer_id = "DeerHuntAreas")
    }
  })
  
  observeEvent({input$AntelopeHerdUnits},{
    
    if ( input$AntelopeHerdUnits == TRUE) {
      mapdeck_update(map_id = "myMap") %>%
        add_polygon(
          data = AntelopeHerdUnits,
          fill_colour = "#523A2875",
          stroke_colour = "#FFFFFF",
          tooltip = 'HERDNAME',
          auto_highlight = TRUE,
          highlight_colour = "#523A2899",
          stroke_width = 350
          , layer_id = "AntelopeHerdUnits"
          , update_view = FALSE
        )
    } else {
      mapdeck_update(map_id = "myMap") %>%
        clear_polygon(layer_id = "AntelopeHerdUnits")
    }
  })
  
  observeEvent({input$AntelopeHuntAreas},{
    
    if ( input$AntelopeHuntAreas == TRUE) {
      mapdeck_update(map_id = "myMap") %>%
        add_polygon(
          data = AntelopeHuntAreas,
          fill_colour = "#698DDF75",
          stroke_colour = "#FFFFFF",
          tooltip = 'HUNTNAME',
          auto_highlight = TRUE,
          highlight_colour = "#698DDF99",
          stroke_width = 350
          , layer_id = "AntelopeHuntAreas"
          , update_view = FALSE
        )
    } else {
      mapdeck_update(map_id = "myMap") %>%
        clear_polygon(layer_id = "AntelopeHuntAreas")
    }
  })
  
  observeEvent({input$BisonHuntAreas},{
    
    if ( input$BisonHuntAreas == TRUE) {
      mapdeck_update(map_id = "myMap") %>%
        add_polygon(
          data = BisonHuntAreas,
          fill_colour = "#698DDF75",
          stroke_colour = "#FFFFFF",
          tooltip = 'HUNTNAME',
          auto_highlight = TRUE,
          highlight_colour = "#698DDF99",
          stroke_width = 350
          , layer_id = "BisonHuntAreas"
          , update_view = FALSE
        )
    } else {
      mapdeck_update(map_id = "myMap") %>%
        clear_polygon(layer_id = "BisonHuntAreas")
    }
  })
  
  
  observeEvent({input$BisonHerdUnits},{
    
    if ( input$BisonHerdUnits == TRUE) {
      mapdeck_update(map_id = "myMap") %>%
        add_polygon(
          data = BisonHerdUnits,
          fill_colour = "#698DDF75",
          stroke_colour = "#FFFFFF",
          tooltip = 'HERDNAME',
          auto_highlight = TRUE,
          highlight_colour = "#698DDF99",
          stroke_width = 350
          , layer_id = "BisonHerdUnits"
          , update_view = FALSE
        )
    } else {
      mapdeck_update(map_id = "myMap") %>%
        clear_polygon(layer_id = "BisonHerdUnits")
    }
  })
  
observeEvent({input$BighornSheepHerdUnits},{
  
  if ( input$BighornSheepHerdUnits == TRUE) {
    mapdeck_update(map_id = "myMap") %>%
      add_polygon(
        data = BighornSheepHerdUnits,
        fill_colour = "#698DDF75",
        stroke_colour = "#FFFFFF",
        tooltip = 'HERDNAME',
        auto_highlight = TRUE,
        highlight_colour = "#698DDF99",
        stroke_width = 350
        , layer_id = "BighornSheepHerdUnits"
        , update_view = FALSE
      )
  } else {
    mapdeck_update(map_id = "myMap") %>%
      clear_polygon(layer_id = "BighornSheepHerdUnits")
  }
})

observeEvent({input$BighornSheepHuntAreas},{
  
  if ( input$BighornSheepHuntAreas == TRUE) {
    mapdeck_update(map_id = "myMap") %>%
      add_polygon(
        data = BighornSheepHuntAreas,
        fill_colour = "#698DDF75",
        stroke_colour = "#FFFFFF",
        tooltip = 'HUNTNAME',
        auto_highlight = TRUE,
        highlight_colour = "#698DDF99",
        stroke_width = 350
        , layer_id = "BighornSheepHuntAreas"
        , update_view = FALSE
      )
  } else {
    mapdeck_update(map_id = "myMap") %>%
      clear_polygon(layer_id = "BighornSheepHuntAreas")
  }
})


observeEvent({input$ElkHerdUnits},{
  
  if ( input$ElkHerdUnits == TRUE) {
    mapdeck_update(map_id = "myMap") %>%
      add_polygon(
        data = ElkHerdUnits,
        fill_colour = "#698DDF75",
        stroke_colour = "#FFFFFF",
        tooltip = 'HERDNAME',
        auto_highlight = TRUE,
        highlight_colour = "#698DDF99",
        stroke_width = 350
        , layer_id = "ElkHerdUnits"
        , update_view = FALSE
      )
  } else {
    mapdeck_update(map_id = "myMap") %>%
      clear_polygon(layer_id = "ElkHerdUnits")
  }
})

observeEvent({input$ElkHuntAreas},{
  
  if ( input$ElkHuntAreas == TRUE) {
    mapdeck_update(map_id = "myMap") %>%
      add_polygon(
        data = ElkHuntAreas,
        fill_colour = "#698DDF75",
        stroke_colour = "#FFFFFF",
        tooltip = 'HUNTNAME',
        auto_highlight = TRUE,
        highlight_colour = "#698DDF99",
        stroke_width = 350
        , layer_id = "ElkHuntAreas"
        , update_view = FALSE
      )
  } else {
    mapdeck_update(map_id = "myMap") %>%
      clear_polygon(layer_id = "ElkHuntAreas")
  }
})

observeEvent({input$MooseHuntAreas},{
  
  if ( input$MooseHuntAreas == TRUE) {
    mapdeck_update(map_id = "myMap") %>%
      add_polygon(
        data = MooseHuntAreas,
        fill_colour = "#698DDF75",
        stroke_colour = "#FFFFFF",
        tooltip = 'HUNTNAME',
        auto_highlight = TRUE,
        highlight_colour = "#698DDF99",
        stroke_width = 350
        , layer_id = "MooseHuntAreas"
        , update_view = FALSE
      )
  } else {
    mapdeck_update(map_id = "myMap") %>%
      clear_polygon(layer_id = "MooseHuntAreas")
  }
})


observeEvent({input$MooseHerdUnits},{
  
  if ( input$MooseHerdUnits == TRUE) {
    mapdeck_update(map_id = "myMap") %>%
      add_polygon(
        data = MooseHerdUnits,
        fill_colour = "#698DDF75",
        stroke_colour = "#FFFFFF",
        tooltip = 'HERDNAME',
        auto_highlight = TRUE,
        highlight_colour = "#698DDF99",
        stroke_width = 350
        , layer_id = "MooseHerdUnits"
        , update_view = FALSE
      )
  } else {
    mapdeck_update(map_id = "myMap") %>%
      clear_polygon(layer_id = "MooseHerdUnits")
  }
})

observeEvent({input$AdminRegions},{
  
  if ( input$AdminRegions == TRUE) {
    mapdeck_update(map_id = "myMap") %>%
      add_polygon(
        data = AdminRegions,
        fill_colour = "#698DDF75",
        stroke_colour = "#FFFFFF",
        tooltip = 'Region',
        auto_highlight = TRUE,
        highlight_colour = "#698DDF99",
        stroke_width = 350
        , layer_id = "AdminRegions"
        , update_view = FALSE
      )
  } else {
    mapdeck_update(map_id = "myMap") %>%
      clear_polygon(layer_id = "AdminRegions")
  }
})

observeEvent({input$BioDistricts},{
  
  if ( input$BioDistricts == TRUE) {
    mapdeck_update(map_id = "myMap") %>%
      add_polygon(
        data = BioDistricts,
        fill_colour = "#698DDF75",
        stroke_colour = "#FFFFFF",
        tooltip = 'BIOLOGIST',
        auto_highlight = TRUE,
        highlight_colour = "#698DDF99",
        stroke_width = 350
        , layer_id = "BioDistricts"
        , update_view = FALSE
      )
  } else {
    mapdeck_update(map_id = "myMap") %>%
      clear_polygon(layer_id = "BioDistricts")
  }
})






  ##------------------- Export queried data
  
  
  
  
  

  observeEvent(input$exportQuery,{
    showModal(modalDialog(
      title="Export queried data to a shapefile",
      textInput('fileNameQuery', 'Please provide a name for your shapefile', 
                value = ""),
      use_bs_popover(),
      selectInput("selectInterval", "Select Interval", choices = c("All Data", "1 Point a Month", "1 Point a Day")) %>%
      shinyInput_label_embed(
        shiny_iconlink() %>%
          bs_embed_popover(
            content = "Split out a subsample of the data queried. 1 point a month returns one point for every indidvual a month, 1 point a day returns one point a day for every indivdiaul in your queried data.", placement = "left"
          )
      ),
      footer = tagList(
        actionButton("exportSample","Export Sampled Points"),
        actionButton("exportLines","Export Lines"),
       # actionButton("exportHomeRange","Export Home Range"),
        
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$exportSample,{
    data <- movebankFilter()
    data<- exportSamplePoints(data,input$selectInterval)
    data <- subset(data, select = -geometry.y)
    coords <- cbind(data$lon, data$lat)
    spdf <- SpatialPointsDataFrame(coords, data, proj4string = CRS("+proj=longlat +datum=WGS84"))

    layername = input$fileNameQuery
    print(layername)
    output_shapefile <- normalizePath(file.path(exportQuery(), paste0(layername, ".shp")))
    print(output_shapefile)
    writeOGR(spdf, dsn = output_shapefile, layer = layername, driver = "ESRI Shapefile",overwrite_layer = TRUE)
    
   # writeOGR(spdf, exportQuery(), ".", layer = input$fileName, driver = "ESRI Shapefile", overwrite_layer = TRUE)
    shinyalert("Success!", paste0("Your shapefile was written to the following location:", output_shapefile), type = "success")
    removeModal()
  }) 
  
  observeEvent(input$exportLines,{
    lines <- lines()
    lines_sf <- sf::st_as_sf(lines)
    lines_spatial <- sf::as_Spatial(lines_sf)
    
    layername = input$fileNameQuery
    output_shapefile <- normalizePath(file.path(exportQuery(), paste0(layername, ".shp")))
    writeOGR(lines_spatial, dsn = output_shapefile, layer = layername, driver = "ESRI Shapefile",overwrite_layer = TRUE)
    
    shinyalert("Success!", paste0("Your shapefile was written to the following location:", output_shapefile ), type = "success")
    
  }) 
  
  
  
  
  
  
#-------------------- Home Range Calucation buttons
    
    
    
  observeEvent(input$exportCalcRange,{
    showModal(modalDialog(
      title="Calculate Home ranges",

      actionButton("runMCP", "Run MCP"),
      " ",
      HTML("<strong>MCP:</strong> Create Minimum Convex Polygon(MCP) of the movement data. These are the simplest defined areas; polygons defined by the outside extent of the data points."),
      
      br(),
      br(),
      
      
      actionButton("runKDE", "Run KDE"),
      " ",
      HTML("<strong>KDE:</strong> Create kernel density estimates to measure home ranges of the movement data. This uses contour lines to measure home ranges with kernels. "),
      
      br(),
      br(),
      
      actionButton("runLineBuffer", "Run Line Buffer"),
      " ",
      "Line Buffer: Explanation of Line Buffer button",
      
      footer = tagList(
        modalButton("Cancel")
      )
    ))
    
  })
  
  
    
    
  kernlUD <- function(data){
    data$datetest <- as.POSIXct(data$datetest, tz = "MST", format = "%Y-%m-%d %H:%M:%S")
    datapoints<-SpatialPoints(cbind(data$lon,data$lat))
    
    #KDE
    kud <- kernelUD(datapoints,h = "href") #kernel density utilization distribution (h=smoothing factor)
    image(kud)
    homerange99 <- getverticeshr(kud, percent=99) #convert into vector object, use 75,50,25 for quartic kernel
    homerange95 <- getverticeshr(kud, percent=95) #convert into vector object, use 75,50,25 for quartic kernel
    homerange90 <- getverticeshr(kud, percent=90) #convert into vector object, use 75,50,25 for quartic kernel
    homerange75 <- getverticeshr(kud, percent=75) #convert into vector object, use 75,50,25 for quartic kernel
    homerange50 <- getverticeshr(kud, percent=50) #convert into vector object, use 75,50,25 for quartic kernel
    homerange25 <- getverticeshr(kud, percent=25) #convert into vector object, use 75,50,25 for quartic kernel
    
    combined_vertices <- rbind(homerange99,homerange95, homerange90,homerange75,homerange50,homerange25)
    print(combined_vertices)
    output$plotKUD <- renderLeaflet({
      leaflet(combined_vertices) %>%
        addTiles() %>%
        addPolygons(fillColor = "red", weight = 2)
    })
    
    kde_result(combined_vertices)  # Store MCP result in reactive value
    
    #proj4string(WGScoor)<- CRS("+proj=longlat +datum=WGS84")

    #coordinates(data) <- ~ lon + lat
    #ltraj <- as.ltraj(xy= coordinates(data),date=data$datetest,id=data$newuid, typeII=TRUE)
  }
  
  kde_result <- reactiveVal() 
  observeEvent(input$runKDE,{
    data <- movebankFilter()
    kernlUD(data)
    showModal(modalDialog(
      title="Kernel Density Estimates",
      leafletOutput("plotKUD"),
      textInput('kdeFileName', 'Please provide a name for your shapefile', 
                value = ""),
      footer = tagList(
        actionButton("exportKDE","Export KDE"),
        
        modalButton("Cancel")
      )
    ))

  }) 
  
  observeEvent(input$exportKDE,{
    kdeResult<- kde_result()
    
    layername = input$kdeFileName
    output_shapefile <- normalizePath(file.path(exportQuery(), paste0(layername, ".shp")))
    writeOGR( kdeResult , dsn = output_shapefile, layer = layername, driver = "ESRI Shapefile",overwrite_layer = TRUE)
    shinyalert("Success!", paste0("Your shapefile was written to the following location:", output_shapefile ), type = "success")
    
  }) 
  
  mcp_result <- reactiveVal()  # Reactive value to store MCP result
  

  calculateMCP <- function(data){
    datapoints<-SpatialPoints(cbind(data$lon,data$lat))

    #MCP
    datapoints.mcp <- mcp(datapoints, percent = 95)
    output$plotMCP <- renderLeaflet({
      leaflet(datapoints.mcp) %>%
        addTiles() %>%
        addPolygons(fillColor = "red", weight = 2)
    })
    mcp_result(datapoints.mcp)  # Store MCP result in reactive value
  }
  
  observeEvent(input$runMCP,{
    data <- movebankFilter()
    calculateMCP(data)
    showModal(modalDialog(
      title="MCP 95%",
      use_bs_popover(),
     leafletOutput("plotMCP"),
      textInput('mcpFileName', 'Please provide a name for your shapefile', 
                value = ""),
      footer = tagList(
        actionButton("exportMCP","Export MCP"),
        
        modalButton("Cancel")
      )
    ))

  })
  
  
  observeEvent(input$exportMCP,{
     mcpResult<- mcp_result()
    
    layername = input$mcpFileName
    output_shapefile <- normalizePath(file.path(exportQuery(), paste0(layername, ".shp")))
    print(output_shapefile)
    writeOGR( mcpResult , dsn = output_shapefile, layer = layername, driver = "ESRI Shapefile",overwrite_layer = TRUE)
    shinyalert("Success!", paste0("Your shapefile was written to the following location:", output_shapefile ), type = "success")
    
  }) 
  
  # bbmm <- function(){
  #   X	= "lon" #	The	column	name	of	your	x	coordinate
  #   Y	= "lat" #	The	column	name	of	your	y	coordinate
  #   loc.error	= 20
  #   maxlag	= 60
  #   levels	= c(95)
  #   cell.size	= 50 #	change	to	NULL	if	area.grid	is	used
  #   grid	= NULL #	if	area.grid is	created	Null	status	will	be	removed
  # }
  


  
  
}
  