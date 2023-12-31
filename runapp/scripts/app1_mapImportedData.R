hasMapRendered<<-FALSE
mortalityColor<<-'#dfff00'
problemsColor<<-'#dd00ff'
pointsInDrawBox<<-NULL
pointIdsInDrawBox<<-NULL

whichBaseMap<<-'default'

mapInit<-function(){

  hide('loadProjectButton')
  show('exportDataButton')

  observeEvent(input$basemapButton, {
    if(whichBaseMap=='default'){
      runjs("mapboxer._widget.importedDataMapBox.map.setPaintProperty('satlayer','raster-opacity',1)")
      whichBaseMap<<-'sat'
    }else{
      runjs("mapboxer._widget.importedDataMapBox.map.setPaintProperty('satlayer','raster-opacity',0)")
      whichBaseMap<<-'default'
    }
  },ignoreInit=TRUE)



  hasMapRendered<<-TRUE

  showElement(id = 'calcMoveParamsButton', anim = TRUE)
  showElement(id = 'recalcInstructions', anim = TRUE)


  if(exists('naDatesLength') && naDatesLength>0){
    modalMessager(
      "No points",
      paste0("You had ",naDatesLength," dates in your dataset that resulted in na values. These were removed from your dataset. These were saved to your working directory as nadates.csv if you'd like to inspect them")
    )
  }

  w$hide()
  
  loadingScreenToggle('hide','')



  dummyPoint<<-importedDatasetMaster@data[1,]
  dummyPoint$lat<<-0
  dummyPoint$lon<<-0


  observeEvent(input$nextPointButton, {
    forwardBackHandler('forward')
  },ignoreInit=TRUE)

  observeEvent(input$previousPointButton, {
    forwardBackHandler('backward')
  },ignoreInit=TRUE)

  observeEvent(input$isMortalitySelector, {
    newValue<-FALSE
    if(input$isMortalitySelector=='yes'){
      newValue<-TRUE
    }
    importedDatasetMaster@data[which(importedDatasetMaster@data$rowIds==clickedId),'mortality']<<-newValue
    pointsForMap@data[which(pointsForMap@data$rowIds==clickedId),'mortality']<<-newValue
    updateTable('importedDatasetMaster','mortality',paste0('where rowIds = ',clickedId),newValue)
    updatePopupTable(clickedId)

    updateProblemAndMortPoints()
    # saveWorkingFile();
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(pointsForMap@data,lat="lat",lng='lon','pointsSource')%>%
      update_mapboxer()
  },ignoreInit=TRUE)

  observeEvent(input$commentInput, {
    pointToChange<-clickedMapPoint$props$rowIds
    importedDatasetMaster@data[which(importedDatasetMaster@data$rowIds==clickedId),'comments']<<-input$commentInput
    updateTable('importedDatasetMaster','comments',paste0('where rowIds = ',clickedId),paste0('"',input$commentInput,'"'))
    updatePopupTable(clickedId)
    pointsForMap@data[which(pointsForMap@data$rowIds==clickedId),'comments']<<-input$commentInput

    mapboxer_proxy("importedDataMapBox") %>%
      set_data(pointsForMap@data,lat="lat",lng='lon','pointsSource')%>%
      update_mapboxer()
    #saveShapefile()

  },ignoreInit=TRUE)

  observeEvent(input$isProblemSelector, {
    pointToChange<-clickedMapPoint$props$rowIds
    newValue<-FALSE
    if(input$isProblemSelector=='yes'){
      newValue<-TRUE
    }
    importedDatasetMaster@data[which(importedDatasetMaster@data$rowIds==clickedId),'problem']<<-newValue
    pointsForMap@data[which(pointsForMap@data$rowIds==clickedId),'problem']<<-newValue
    updateProblemAndMortPoints()
    updateTable('importedDatasetMaster','problem',paste0('where rowIds = ',clickedId),newValue)
    updatePopupTable(clickedId)
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(pointsForMap@data,lat="lat",lng='lon','pointsSource')%>%
      update_mapboxer()


  },ignoreInit=TRUE)
  
  observeEvent(input$animalID, {
    pointToChange<-clickedMapPoint$props$rowIds
    importedDatasetMaster@data[which(importedDatasetMaster@data$rowIds==clickedId),'newUid']<<-input$animalID
    updateTable('importedDatasetMaster','newUid',paste0('where rowIds = ',clickedId),paste0('"',input$animalID,'"'))
    updatePopupTable(clickedId)
    pointsForMap@data[which(pointsForMap@data$rowIds==clickedId),'newUid']<<-input$animalID
    #updateSelectInput(session, 'individualsSelector', selected= unique(importedDatasetMaster@data$newUid))
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(pointsForMap@data,lat="lat",lng='lon','pointsSource')%>%
      update_mapboxer()
    #saveShapefile()
    updateAnimalYears()
    saveWorkingFile()
    
    
  },ignoreInit=TRUE)
  
  

  observeEvent(input$forwardHandlerButton, {
    allAnimals<-unique(importedDatasetMaster@data$newUid)
    thisAnimalIndex<-which(allAnimals==selectedAnimal)
    if(thisAnimalIndex==length(allAnimals)){
      return()
    }
    newAnimalIndex=thisAnimalIndex+1
    selectedAnimal<<-allAnimals[newAnimalIndex]
    updateSelectInput(session, 'individualsSelector', selected=selectedAnimal)
  },ignoreInit=TRUE)

  observeEvent(input$backwardHandlerButton, {
    allAnimals<-unique(importedDatasetMaster@data$newUid)
    thisAnimalIndex<-which(allAnimals==selectedAnimal)
    if(thisAnimalIndex==1){
      return()
    }
    newAnimalIndex=thisAnimalIndex-1
    selectedAnimal<<-allAnimals[newAnimalIndex]
    updateSelectInput(session, 'individualsSelector', selected=selectedAnimal)
  },ignoreInit=TRUE)





  observeEvent(input$allPointsSelector, {
    if(input$allPointsSelector=='yes, show all'){
      allPoints<<-TRUE;
    }else{
      allPoints<<-FALSE;
    }
    addPointsToMap()
  },ignoreInit=TRUE)

  plotClickObserver<<-observeEvent(input$plot_click, {
  },ignoreInit=TRUE)

  plotHoverObserver<<-observeEvent(input$plot_hover, {
  },ignoreInit=TRUE)



  observeEvent(input$variableSelector, {
    selectedGraphVariable <<- input$variableSelector
    #plotData()
  })

  


  renderMap();
  delay(2500,showMortalityProblemBox())
  


}

showMortalityProblemBox<-function(){
  totalProblems<-length(which(importedDatasetMaster$problem==1))
  totalMortalities<-length(which(importedDatasetMaster$mortality==1))
  if(length(totalProblems)>0 | length(totalMortalities)>0){
    message<-"You had points which were flagged as mortalities or problems.<br>"
    if(length(totalProblems)>0){
      message<-paste0(message,'There were ',totalProblems,' points flagged as problems.<br>')
    }
    if(length(totalMortalities)>0){
      message<-paste0(message,'There were ',totalMortalities,' points flagged as mortalities.<br>')
    }
    message<-paste0(message,'<br>problem points are shown in <strong style="color:#dd00ff !important; background-color:black !important;  padding:3px !important;">magenta</strong> and those points classified as mortalities are shown in <strong style="color:#dfff00 !important; background-color:black !important; padding:3px !important;">yellow</strong>')
    modalMessager(
      "mortalities and problem points",
      HTML(message)
    )
  }

}

animalYearAverages<<-list()


getAnimalYearAverages <- function() {
  allAnimals <- unique(importedDatasetMaster@data$newUid)
  selectedAnimal <<- allAnimals[1]
  updateSelectInput(session, 'individualsSelector', label = NULL, choices = allAnimals, selected = selectedAnimal)
  animalYears <- reactive({
    req(!is.null(importedDatasetMaster))
    req(!is.null(input$individualsSelector))
    
    selectedAnimal <- input$individualsSelector
    filteredData <- importedDatasetMaster@data[importedDatasetMaster@data$newUid == selectedAnimal, ]
    uniqueYears <- unique(filteredData$year)
    return(uniqueYears)
  })
  
  selectedYear <<- NULL  # Initialize selectedYear with a default value
  
  updateSelectInput(session, 'yearSelector', label = NULL, choices = c('All Years', animalYears()), selected = selectedYear)
  
  uniqueMinDate <- reactive({
    req(!is.null(importedDatasetMaster))
    req(!is.null(input$individualsSelector))
    
    filteredData <- importedDatasetMaster@data[importedDatasetMaster@data$newUid == selectedAnimal, ]
    uniqueMinDate <- unique(filteredData$start_date)
    return(uniqueMinDate)
  })
  
  uniqueMaxDate <- reactive({
    req(!is.null(importedDatasetMaster))
    req(!is.null(input$individualsSelector))
    
    filteredData <- importedDatasetMaster@data[importedDatasetMaster@data$newUid == selectedAnimal, ]
    uniqueMaxDate <- unique(filteredData$end_date)
    return(uniqueMaxDate)
  })
  

  updateDateInput(session, 'beginDate', label = NULL, value = uniqueMinDate())
  updateDateInput(session, 'endDate', label = NULL, value = uniqueMaxDate())

  
  observeEvent(input$individualsSelector, {
    selectedAnimal <<- input$individualsSelector
    selectedYear <<- animalYears()[1]   # Update selectedYear based on animalYears
    updateSelectInput(session, 'yearSelector', label = NULL, choices = c('All Years', animalYears()), selected = selectedYear)
    updateDateInput(session, 'beginDate', label = NULL, value = uniqueMinDate())
    updateDateInput(session, 'endDate', label = NULL, value = uniqueMaxDate())
    addPointsToMap()
  }, ignoreInit = TRUE)
  
  observeEvent(input$yearSelector, {
    selectedYear <<- input$yearSelector
    addPointsToMap()
  }, ignoreInit = TRUE)
}


updateAnimalYears <- function() {
  allAnimals <- unique(importedDatasetMaster@data$newUid)
  selectedAnimal <<- input$individualsSelector
  updateSelectInput(session, 'individualsSelector', label = NULL, choices = allAnimals, selected = selectedAnimal)
  animalYears <- reactive({
    req(!is.null(importedDatasetMaster))
    req(!is.null(input$individualsSelector))
    
    selectedAnimal <- input$individualsSelector
    filteredData <- importedDatasetMaster@data[importedDatasetMaster@data$newUid == selectedAnimal, ]
    uniqueYears <- unique(filteredData$year)
    return(uniqueYears)
  })
  
  selectedYear <<- NULL  # Initialize selectedYear with a default value
  
  updateSelectInput(session, 'yearSelector', label = NULL, choices = c('All Years', animalYears()), selected = selectedYear)
  
  uniqueMinDate <- reactive({
    req(!is.null(importedDatasetMaster))
    req(!is.null(input$individualsSelector))
    
    filteredData <- importedDatasetMaster@data[importedDatasetMaster@data$newUid == selectedAnimal, ]
    uniqueMinDate <- unique(filteredData$start_date)
    return(uniqueMinDate)
  })
  
  uniqueMaxDate <- reactive({
    req(!is.null(importedDatasetMaster))
    req(!is.null(input$individualsSelector))
    
    filteredData <- importedDatasetMaster@data[importedDatasetMaster@data$newUid == selectedAnimal, ]
    uniqueMaxDate <- unique(filteredData$end_date)
    return(uniqueMaxDate)
  })
  

  updateDateInput(session, 'beginDate', label = NULL, value = uniqueMinDate())
  updateDateInput(session, 'endDate', label = NULL, value = uniqueMaxDate())

  
  observeEvent(input$individualsSelector, {
    selectedAnimal <<- input$individualsSelector
    selectedYear <<- animalYears()[1]   # Update selectedYear based on animalYears
    updateSelectInput(session, 'yearSelector', label = NULL, choices = c('All Years', animalYears()), selected = selectedYear)
    updateDateInput(session, 'beginDate', label = NULL, value = uniqueMinDate())
    updateDateInput(session, 'endDate', label = NULL, value = uniqueMaxDate())
    addPointsToMap()
  }, ignoreInit = TRUE)
  
  observeEvent(input$yearSelector, {
    selectedYear <<- input$yearSelector
    addPointsToMap()
  }, ignoreInit = TRUE)
}


# getAnimalYearAverages=function(){
#   allAnimals<-unique(importedDatasetMaster@data$newUid)
#   for(i in 1:length(allAnimals)){
#     thisAnimal<-allAnimals[i]
#     availableYears<-unique(importedDatasetMaster@data[which(importedDatasetMaster@data$newUid==thisAnimal),'year'])
#     print(availableYears)
#     animalYearAverages[[thisAnimal]]<<-availableYears
#   }
# 
#   selectedAnimal<<-allAnimals[1]
#   selectedYear<<-animalYearAverages[[selectedAnimal]][[1]]
#   print("Get animal Average years")
#   print(selectedYear)
#   
#   updateSelectInput(session, 'individualsSelector', label = NULL, choices = allAnimals, selected=selectedAnimal)
#   updateSelectInput(session, 'yearSelector', label = NULL, choices = c('All Years',animalYearAverages[[selectedAnimal]]), selected=animalYearAverages[[selectedAnimal]][2])
# 
# 
#   observeEvent(input$individualsSelector, {
#     selectedAnimal <<- input$individualsSelector
#     thisAnimalsYears<-c('All Years',animalYearAverages[[selectedAnimal]])
#     if(selectedYear!='All Years'){
#       selectedYear <<- thisAnimalsYears[2]
#     }
#     updateSelectInput(session, 'yearSelector', label = NULL, choices = thisAnimalsYears, selected=selectedYear)
#     addPointsToMap()
#   },ignoreInit=TRUE)
# 
#   observeEvent(input$yearSelector, {
#     selectedYear <<- input$yearSelector
#     addPointsToMap()
#   },ignoreInit=TRUE)
# 
# }







renderMap<-function(){

  hideElement(id = 'dateTimeElementsRow', anim = TRUE)
  showElement(id = 'importedDataMapRow', anim = TRUE)

  hideElement(id = 'loadProjectButton', anim = TRUE)
  showElement(id = 'exportDataButton', anim = TRUE)



  dataSetExtent<-importedDatasetMaster@bbox



  output$importedDataMapBox <- renderMapboxer({
   mapboxer(center = c(importedDatasetMaster@data[1,'lon'],importedDatasetMaster@data[1,'lat']), style = 'mapbox://styles/wmi-merkle/ckxqg5r429gpr14sd3o6dlno4' ,zoom = 6) %>% #''
    add_navigation_control()
  })


    observeEvent(input$importedDataMapBox_onclick, {
      clickedMapPoint<-input$importedDataMapBox_onclick
      clickedMapPoint<<-clickedMapPoint
      clickedId<<-clickedMapPoint$props$rowIds
      pointClickEvent(clickedId,FALSE)
    },ignoreInit=TRUE)




    delay(2500,getAnimalYearAverages())
}

isSourceAdded<-FALSE
isDrawAdded<<-FALSE

drawInit<-function(){

  runjs('

  mapboxer._widget.importedDataMapBox.map.addControl(new mapboxgl.ScaleControl({position: "bottom-right", unit:"imperial"}));
  mapboxer._widget.importedDataMapBox.map.addControl(new mapboxgl.ScaleControl({position: "bottom-right", unit:"metric"}));



  draw = new MapboxDraw({
    displayControlsDefault: false,
    // Select which mapbox-gl-draw control buttons to add to the map.
    controls: {
    polygon: true,
    trash: true
    },
  });
  mapboxer._widget.importedDataMapBox.map.addControl(draw, "top-left");

  mapboxer._widget.importedDataMapBox.map.on("draw.update", function(){
    drawnData = draw.getAll();
    console.log(drawnData)
    Shiny.setInputValue("polygonHolder", drawnData.features[0]);
  });

  mapboxer._widget.importedDataMapBox.map.on("draw.create", function(){
    drawnData = draw.getAll();
    console.log(drawnData)
    Shiny.setInputValue("polygonHolder", drawnData.features[0]);
  });

  mapboxer._widget.importedDataMapBox.map.on("draw.delete", function(){
    console.log("draw delete")
    drawnData = draw.getAll();
    console.log(drawnData)
    Shiny.setInputValue("polygonHolder", 999);
  });

  $(".modal-footer > .btn-default").click(function(){
    clearDrawnPoly()
  })

  $("#manyPointsSelectedModal").on("hidden.bs.modal", function (event) {
    clearDrawnPoly()
  });

  $(".close").click(function(){
    clearDrawnPoly()
  })

  clearDrawnPoly=function(){
    draw.deleteAll()
    drawnData = draw.getAll();
    console.log(drawnData)
    Shiny.setInputValue("polygonHolder", 999);
  }

  mapboxer._widget.importedDataMapBox.map.on("draw.modechange", (e) => {
    const data = draw.getAll();
    if (draw.getMode() == "draw_polygon") {
      var pids = []
      // ID of the added template empty feature
      const lid = data.features[data.features.length - 1].id

      data.features.forEach((f) => {
        if (f.geometry.type === "Polygon" && f.id !== lid) {
          pids.push(f.id)
        }
      })
      draw.delete(pids)
      Shiny.setInputValue("polygonHolder", 999);
    }
  });


  drawChange=function(){
    drawnData = draw.getAll();
    console.log(drawnData)
  }


  mapboxer._widget.importedDataMapBox.map.boxZoom.enable();




  '
)
  

  
  observeEvent(input$beginDate, {
    # Ensure that individualsSelector is not null
    req(!is.null(input$individualsSelector))
    
    selectedAnimal <- input$individualsSelector
    
    # Retrieve the startDate value from the reactive function
    start <- startDate()
    start_posixct <- as.POSIXct(paste0(start, " 00:00:00"))
    
    importedDatasetMaster$start_date[importedDatasetMaster$newUid == selectedAnimal] <<- start_posixct
    saveWorkingFile()
    updateProblemAndMortPoints()
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(pointsForMap@data, lat = "lat", lng = 'lon', 'pointsSource') %>%
      update_mapboxer()
  })
  
  
  observeEvent(input$endDate, {
    req(!is.null(input$individualsSelector))
    
    selectedAnimal <- input$individualsSelector
    
    end <- endDate()
    end_posixct <- as.POSIXct(paste0(end, " 00:00:00"))
    importedDatasetMaster$end_date[importedDatasetMaster$newUid == selectedAnimal] <<- end_posixct
    updateProblemAndMortPoints()
    
    saveWorkingFile();
    
  })

observeEvent(input$manyPointsIsProblemSelector, {

  if(input$manyPointsIsProblemSelector==''){
    return()
  }

  newValue<-FALSE
  if(input$manyPointsIsProblemSelector=='yes'){
    newValue<-TRUE
  }
  whichRows<-which(importedDatasetMaster@data$rowIds%in%pointIdsInDrawBox)
  importedDatasetMaster@data[whichRows,'problem']<<-newValue
  pointsForMap@data[pointsInDrawBox,'problem']<<-newValue
  updateTable('importedDatasetMaster','problem',paste0('where rowIds IN (',toString(pointIdsInDrawBox),') '),newValue)
  updateProblemAndMortPoints()
},ignoreInit=TRUE)

observeEvent(input$manyPointsIsMortalitySelector, {

  if(input$manyPointsIsMortalitySelector==''){
    return()
  }


  thisValue<-input$manyPointsIsMortalitySelector
  newValue<-FALSE
  if(thisValue=='yes'){
    newValue<-TRUE
  }
  whichRows<-which(importedDatasetMaster@data$rowIds%in%pointIdsInDrawBox)
  importedDatasetMaster@data[whichRows,'mortality']<<-newValue
  pointsForMap@data[pointsInDrawBox,'mortality']<<-newValue
  updateTable('importedDatasetMaster','mortality',paste0('where rowIds IN (',toString(pointIdsInDrawBox),') '),newValue)
  updateProblemAndMortPoints()
},ignoreInit=TRUE)

observeEvent(input$manyPointsCommentInput, {

  if(input$manyPointsCommentInput=='no comments added'){
    return()
  }

  thisValue<-input$manyPointsCommentInput
  whichRows<-which(importedDatasetMaster@data$rowIds%in%pointIdsInDrawBox)
  importedDatasetMaster@data[whichRows,'comments']<<-thisValue
  pointsForMap@data[pointsInDrawBox,'comments']<<-thisValue
  updateTable('importedDatasetMaster','comments',paste0('where rowIds IN (',toString(pointIdsInDrawBox),') '),paste0('"',thisValue,'"'))
},ignoreInit=TRUE)

observeEvent(input$manyAnimalID, {
  
  if(input$manyAnimalID==''){
    return()
  }
  
  thisValue <- input$manyAnimalID
  
  # Check if thisValue already exists in newUid
  if (thisValue %in% importedDatasetMaster@data$newUid) {
    # Display a warning message using shinyalert
    shinyalert::shinyalert(
      title = "Warning",
      text = "The animal ID entered already exists. Please confirm your animal ID as this action can not be undone.",
      type = "warning"
    )
  } 
    # If it doesn't exist, proceed with the updates
    whichRows <- which(importedDatasetMaster@data$rowIds %in% pointIdsInDrawBox)
    importedDatasetMaster@data[whichRows, 'newUid'] <<- thisValue
    pointsForMap@data[pointsInDrawBox, 'newUid'] <<- thisValue
    updateTable('importedDatasetMaster', 'newUid', paste0('where rowIds IN (', toString(pointIdsInDrawBox), ') '), paste0('"', thisValue, '"'))
    #getAnimalYearAverages()
    updateAnimalYears()
   # updateSelectInput(session, 'manyAnimalID', selected= "")
    saveWorkingFile()
}, ignoreInit = TRUE)



observeEvent(input$polygonHolder, {
  drawnData<<-input$polygonHolder

  if(!typeof(drawnData)=='list'){
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(dummyPoint,lat="lat",lng='lon','polygonSelectedSource')%>%
      update_mapboxer()

      pointsInDrawBox<<-NULL
      pointIdsInDrawBox<<-NULL

      return()
  }

  numberOfPoints<-length(drawnData$geometry[[1]][[1]])
  xCoords<-c()
  yCoords<-c()
  for(i in 1:numberOfPoints){
    thisPoint<-drawnData$geometry[[1]][[1]][[i]]
    thisX<-thisPoint[1][[1]]
    thisY<-thisPoint[2][[1]]
    xCoords<-c(xCoords,thisX)
    yCoords<-c(yCoords,thisY)
  }

  thisPolyCoords <- cbind(xCoords, yCoords)
  thisPoly = Polygon(thisPolyCoords)
  thisPoly = Polygons(list(thisPoly),1)
  thisPoly = SpatialPolygons(list(thisPoly),proj4string=CRS("EPSG:4326"))
  thisPoly<-spTransform(thisPoly, importedDatasetMaster@proj4string)

  pointsInDrawBox<<-over(pointsForMap,thisPoly)
  pointsInDrawBox<<-which(!is.na(pointsInDrawBox))

  if(length(pointsInDrawBox)==0){
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(dummyPoint,lat="lat",lng='lon','polygonSelectedSource')%>%
      update_mapboxer()
      return()
  }


  drawSelectedPointsToShow<-pointsForMap@data[pointsInDrawBox,]

  pointIdsInDrawBox<<-drawSelectedPointsToShow[,'rowIds']


  mapboxer_proxy("importedDataMapBox") %>%
    set_data(drawSelectedPointsToShow,lat="lat",lng='lon','polygonSelectedSource')%>%
    update_mapboxer()


    updateSelectInput(session, 'manyPointsIsMortalitySelector', selected='')
    updateSelectInput(session, 'manyPointsIsProblemSelector', selected='')
    updateTextInput(session, 'manyPointsCommentInput', value='no comments added')
    updateSelectInput(session, 'manyAnimalID', selected= '')

    toggleModal(session,'manyPointsSelectedModal',toggle='open')
})


isDrawAdded<<-TRUE
}


addPointsToMap<-function(){
  
  # importedDatasetMaster<- importedDatasetMaster[, c("newuid", "individ", "newMasterDate" ,"dateTest", "year"
  #                                                   "lat", "lon","timestamp", "datetst","problem","mortlty","dt","dist", "rowIds")]
  # 

  if(!isDrawAdded){
    drawInit()
  }

  clearHoverPoint()
  if(selectedYear=='All Years'){
    if(selectedAnimal=='All Individuals'){
      pointsForMap<<-importedDatasetMaster
    }else{
      pointsForMap<<-importedDatasetMaster[which(importedDatasetMaster$newUid==selectedAnimal),]

    }
  }else{
    if(selectedAnimal=='All Individuals'){
      pointsForMap<<-importedDatasetMaster[which(importedDatasetMaster$year==selectedYear),]
    }else{
      pointsForMap<<-importedDatasetMaster[which(importedDatasetMaster$newUid==selectedAnimal & importedDatasetMaster$year==selectedYear),]
      }
  }
  
  if(nrow(pointsForMap)>0){

    pointsForMap@data$idDate<<-paste0(pointsForMap$year,pointsForMap$month,pointsForMap$day,pointsForMap$newUid)
    if(allPoints==FALSE){
      pointsForMap <<- pointsForMap[!duplicated(pointsForMap$idDate),]
    }
    pointsForMap<<-spTransform(pointsForMap,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0', SRS_string='EPSG:4326'))
    thisBbox<-pointsForMap@bbox
    # if(!exists('emptyLine')){
    #   emptyLinePoints<-pointsForMap[1,]
    #   emptyLine<<-pointsToLines(emptyLinePoints)
    #   emptyLine<<-st_as_sf(emptyLine)
    #   print("Printed empty line")
    #   print(emptyLine)
    # }

    linesData<<-pointsToLines(pointsForMap)


    linesData<<-st_as_sf(linesData)

    if(!isSourceAdded){

      mapboxer_proxy("importedDataMapBox") %>%
        add_source(as_mapbox_source(linesData),'linesUnderSource')%>%
        add_line_layer(
          source = 'linesUnderSource',
          line_color = '#632782',
          line_width = 1.5,
          line_opacity = 0.85,
          id='linesUnderLayer'
        )%>%
        update_mapboxer()

      mapboxer_proxy("importedDataMapBox") %>%
        add_source(as_mapbox_source(linesData),'linesSource')%>%
        add_line_layer(
          source = 'linesSource',
          line_color='#ffffff',
          line_width = 1,
          id='linesLayer'
        )%>%
        update_mapboxer()
# 
      mapboxer_proxy("importedDataMapBox") %>%
        add_source(as_mapbox_source(pointsForMap@data,lat="lat",lng="lon"),'pointsSource')%>%
        add_circle_layer(
          source = 'pointsSource',
          circle_color = 'grey',
          circle_radius = 5,
          id='pointLayer'
        )%>%
        fit_bounds(c(c(thisBbox[1,1]-0.01, thisBbox[2,1]-0.01),c(thisBbox[1,2]+0.01, thisBbox[2,2]+0.01)))%>%
        update_mapboxer()

      mapboxer_proxy("importedDataMapBox") %>%
        add_source(as_mapbox_source(dummyPoint,lat="lat",lng="lon"),'mortalitiesSource')%>%
        add_circle_layer(
          source = 'mortalitiesSource',
          circle_color = mortalityColor,
          circle_radius = 4.5,
          id='mortalityLayer'
        )%>%
        update_mapboxer()
      
      mapboxer_proxy("importedDataMapBox") %>%
        add_source(as_mapbox_source(dummyPoint,lat="lat",lng="lon"),'activePoints')%>%
        add_circle_layer(
          source = 'activePoints',
          circle_color = "#000cff",
          circle_radius = 4.5,
          id='activeLayer'
        )%>%
        update_mapboxer()
      

        mapboxer_proxy("importedDataMapBox") %>%
          add_source(as_mapbox_source(dummyPoint,lat="lat",lng="lon"),'problemsSource')%>%
          add_circle_layer(
            source = 'problemsSource',
            circle_color = problemsColor,
            circle_radius = 4.5,
            id='problemsLayer'
          )%>%
          update_mapboxer()

          mapboxer_proxy("importedDataMapBox") %>%
            add_source(as_mapbox_source(dummyPoint,lat="lat",lng="lon"),'polygonSelectedSource')%>%
            add_circle_layer(
              source = 'polygonSelectedSource',
              circle_color = '#1dff00',
              circle_radius = 4.5,
              id='polygonsSelectedLayer'
            )%>%
            update_mapboxer()


      mapboxer_proxy("importedDataMapBox") %>%
        add_source(as_mapbox_source(pointsForMap@data[0,],lat="lat",lng="lon"),'hoverSource')%>%
        add_circle_layer(
          source = 'hoverSource',
          circle_color = '#000000',
          circle_radius = 8,
          id='hoverLayer'
        )%>%
        update_mapboxer()

      
      isSourceAdded<<-TRUE;
      
    }else{

      mapboxer_proxy("importedDataMapBox") %>%
        set_data(linesData,'linesUnderSource')%>%
        update_mapboxer()

      mapboxer_proxy("importedDataMapBox") %>%
        set_data(linesData,'linesSource')%>%
        update_mapboxer()




      mapboxer_proxy("importedDataMapBox") %>%
        set_data(pointsForMap@data,lat="lat",lng='lon','pointsSource')%>%
        fit_bounds(c(c(thisBbox[1,1]-0.01, thisBbox[2,1]-0.01),c(thisBbox[1,2]+0.01, thisBbox[2,2]+0.01)))%>%
        update_mapboxer()
    }




   plotData()

    updateSummaryStats()

    
    updateProblemAndMortPoints();


  }
  # else{
  # 
  # 
  # 
  #   mapboxer_proxy("importedDataMapBox") %>%
  #     set_data(pointsForMap@data,lat="lat",lng='lon','pointsSource')%>%
  #     update_mapboxer()
  # 
  #   if(exists('emptyLine')){
  #     mapboxer_proxy("importedDataMapBox") %>%
  #       set_data(emptyLine,'linesSource')%>%
  #       update_mapboxer()
  # 
  #     mapboxer_proxy("importedDataMapBox") %>%
  #       set_data(emptyLine,'linesUnderSource')%>%
  #       update_mapboxer()
  #   }
  # 
  # 
  #   # modalMessager(
  #   #   "No points",
  #   #   "no points for this selection"
  #   # )
  # 
  #   updateSummaryStats()
  # 
  #   #plotData()
  # 
  # 
  # }



}

startDate <- reactive({
  startDate <- input$beginDate
  return(startDate)
})

endDate <- reactive({
  endDate <- input$endDate
  return(endDate)
})




updateProblemAndMortPoints<-function(){
  if(any(pointsForMap@data$problem==1)){
    problemsToMap<-pointsForMap@data[which(pointsForMap@data$problem==1),]
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(problemsToMap,lat="lat",lng='lon','problemsSource')%>%
      update_mapboxer()
  }else{
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(dummyPoint,lat="lat",lng='lon','problemsSource')%>%
      update_mapboxer()
  }
  if(any(pointsForMap@data$mortality==1)){
    mortalitiesToMap<-pointsForMap@data[which(pointsForMap@data$mortality==1),]
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(mortalitiesToMap,lat="lat",lng='lon','mortalitiesSource')%>%
      update_mapboxer()
  }else{
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(dummyPoint,lat="lat",lng='lon','mortalitiesSource')%>%
      update_mapboxer()
  }
  
  if(any(pointsForMap@data$newMasterDate >= startDate() & pointsForMap@data$newMasterDate <= endDate())){
    activeToMap<-pointsForMap@data[which(pointsForMap@data$newMasterDate >= startDate()  & pointsForMap@data$newMasterDate <= endDate()),]
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(activeToMap,lat="lat",lng='lon','activePoints')%>%
      update_mapboxer()
  }else{
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(dummyPoint,lat="lat",lng='lon','activePoints')%>%
      update_mapboxer()
  }
  
  #shapefile_export <- st_write(importedDatasetMaster@data, paste0( "/",input$movebankStudyInput.shp), driver = "ESRI Shapefile")
}


hasAssignedPlotHandler=FALSE;

plotData=function(){

  if(selectedYear=='All Years'){
    if(selectedAnimal=='All Individuals'){
      pointsForMap<<-importedDatasetMaster
    }else{
      pointsForMap<<-importedDatasetMaster[which(importedDatasetMaster$newUid==selectedAnimal),]
    }
  }else{
    if(selectedAnimal=='All Individuals'){
      pointsForMap<<-importedDatasetMaster[which(importedDatasetMaster$year==selectedYear),]
    }else{
      pointsForMap<<-importedDatasetMaster[which(importedDatasetMaster$newUid==selectedAnimal & importedDatasetMaster$year==selectedYear),]
    }
  }

  pointsForMap$idDate<-paste0(pointsForMap$year,pointsForMap$month,pointsForMap$day,pointsForMap$newUid)
  if(allPoints==FALSE){
    pointsForMap <<- pointsForMap[!duplicated(pointsForMap$idDate),]
  }


  output$speedPlot <- renderPlot({
      speedPlot<-ggplot(pointsForMap@data, aes(x=newMasterDate, y=speed))+
      geom_line(size= 0.5, color="black")+
      ylab('Speed (km/hr) ')+
      xlab("Date") +
      geom_point(size = 3, color="blue")+
      theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9,face="bold"))
      speedPlot
  })

  output$fixRatePlot <- renderPlot({
      fixRatePlot<-ggplot(pointsForMap@data, aes(x=newMasterDate, y=fixRateHours))+
      geom_line(size= 0.5, color="black")+
      ylab("Fix rate (hours)") +
      xlab("Date") +
      geom_point(size = 3, color="blue")+
      theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9,face="bold"))
      fixRatePlot
  })
  
  output$last7days <- renderPlot({
    last_7_days <- pointsForMap@data %>% 
      filter(newMasterDate >= max(newMasterDate) - 6)
    
    last7days<-ggplot(last_7_days, aes(x=newMasterDate, y=dist))+
      geom_line(size= 0.5, color="black")+
     # ylab("Fix rate (hours)") +
      xlab("Date") +
      geom_point(size = 3, color="blue")+
      theme(axis.text=element_text(size=9),
            axis.title=element_text(size=9,face="bold"))
    last7days
  })

  output$nsdPlot <- renderPlot({
      nsdPlot<-ggplot(pointsForMap@data, aes(x=newMasterDate, y=nsdYear))+
      geom_line(size= 0.5, color="black")+
      ylab('Squared Displacement (KM^2; since 1 Jan)')+
      xlab("Date") +
      geom_point(size = 3, color="blue")+
      theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9,face="bold"))
      nsdPlot
  })


  plotClickObserver$destroy()
  plotHoverObserver$destroy()

  # print('-------daw')
  # jj<<-pointsForMap
  # print(!is.na(pointsForMap$speed))

  # if(!is.na(pointsForMap$speed)){
  if(any(!is.na(pointsForMap$speed))){
  plotClickObserver<<-observeEvent(input$plot_click, {
    clickedPlotPoint<<-nearPoints(pointsForMap, input$plot_click, threshold = 10, maxpoints = 1, addDist = TRUE)
    if(nrow(clickedPlotPoint)>0){
      plotClickEvent(clickedPlotPoint)
    }
  },ignoreInit=TRUE)
  plotHoverObserver<<-observeEvent(input$plot_hover, {
    hoveredPoint<<-nearPoints(pointsForMap, input$plot_hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if(nrow(hoveredPoint)>0){
      showHoverPoint(hoveredPoint)
    } else{
       clearHoverPoint()
     }
  },ignoreInit=TRUE)
  
  }
  
  
}

plotClickEvent=function(clickedPlotPoint){

  thisLon<<-clickedPlotPoint@data[1,'lon']
  thisLat<<-clickedPlotPoint@data[1,'lat']


  mapboxer_proxy("importedDataMapBox") %>%
    fit_bounds(c(c(thisLon-0.005,thisLat-0.005),c(thisLon+0.005,thisLat+0.005)))%>%
    update_mapboxer()
}

showHoverPoint=function(hoveredPoint){
  mapboxer_proxy("importedDataMapBox") %>%
    set_data(hoveredPoint@data,lat="lat",lng='lon','hoverSource')%>%
    update_mapboxer()
}

clearHoverPoint=function(){
  mapboxer_proxy("importedDataMapBox") %>%
    set_data(dummyPoint,lat="lat",lng='lon','hoverSource')%>%
    update_mapboxer()
}


forwardBackHandler=function(which){
  currentRow<-which(pointsForMap@data$rowIds==clickedId)
  if(which=='forward'){
    thisRow<-currentRow+1
  }else{
    thisRow<-currentRow-1
  }
  if(length(currentRow)==0){
    return()
  }
  if(thisRow>nrow(pointsForMap)){
    modalMessager(
      "outside sequence",
      "this point is the last in this animal/year"
    )
    return()
  }
  if(thisRow==0){
    modalMessager(
      "outside sequence",
      "this point is the first in this animal/year"
    )
    return()
  }
  clickedId<<-pointsForMap@data[thisRow,'rowIds']
  pointClickEvent(clickedId,TRUE)
}


pointClickEvent=function(clickedId,fromButton){

  if(is.null(clickedId)){
    return()
  }

  rowToMap<-importedDatasetMaster@data[which(importedDatasetMaster@data$rowIds==clickedId),]
  thisLon<-rowToMap$lon
  thisLat<-rowToMap$lat

  mapboxer_proxy("importedDataMapBox") %>%
    set_data(rowToMap,lat="lat",lng='lon','hoverSource')%>%
    update_mapboxer()

  mapboxer_proxy("importedDataMapBox") %>%
    fit_bounds(c(c(thisLon-0.01, thisLat-0.01),c(thisLon+0.01, thisLat+0.01)))%>%
    update_mapboxer()

  updateSelectInput(session, 'animalID', selected= rowToMap$newUid)

  if(rowToMap$mortality==1){
    updateSelectInput(session, 'isMortalitySelector', selected='yes')
  }else{
    updateSelectInput(session, 'isMortalitySelector', selected='no')
  }

  if(rowToMap$problem==1){
    updateSelectInput(session, 'isProblemSelector', selected='yes')
  }else{
    updateSelectInput(session, 'isProblemSelector', selected='no')
  }



  if(rowToMap$comments!=""){
    updateTextInput(session, 'commentInput', value=rowToMap$comments)
  }else{
    updateTextInput(session, 'commentInput', value='')
  }


  updatePopupTable(clickedId)

  if(!fromButton){
    toggleModal(session,'pointClickModal',toggle='open')
  }

}


updatePopupTable <- function(clickedId) {
  rowToMap <- importedDatasetMaster@data[which(importedDatasetMaster@data$rowIds == clickedId), ]
  allFields <- c('newMasterDate', 'lat', 'lon', 'newUid', 'species', 'start_date', 'end_date', 'burst', 'fixRateHours', 'dist', 'displacementOverall', 'nsdOverall', 'speed', 'abs.angle', 'rel.angle', 'mortality', 'problem', 'comments')
  
  htmlToRender <- ''
  mortalityPosition <- which(allFields == 'mortality')
  allFields <- allFields[-mortalityPosition]
  problemPostion <- which(allFields == 'problem')
  allFields <- allFields[-problemPostion]
  commentPostion <- which(allFields == 'comments')
  allFields <- allFields[-commentPostion]
  allFields <- c(allFields, 'mortality')
  allFields <- c(allFields, 'problem')
  allFields <- c(allFields, 'comments')
  
  for (i in 1:length(allFields)) {
    thisColumn <- allFields[i]
    thisValue <- rowToMap[[1, thisColumn]]  # Use double brackets to access the value

    if (!is.null(thisValue)) {
      if (is.numeric(thisValue)) {
        thisValue <- round(thisValue, 1)
      }
      thisValue <- as.character(thisValue)
      if (is.na(thisValue)) {
        thisValue <- ' - '
      }
      if (nchar(thisValue) == 0) {
        thisValue <- ' - '
      }
    } else {
      thisValue <- ' - '  # Handle NULL values as needed
    }
    
    thisHtml <- paste0('<div style="display:inline-block !important; padding:10px; text-align:center !important;"><span style="font-weight:bold !important; font-size:14px !important;">', thisColumn, '</span><br><span style="">', thisValue, '</span></div>')
    htmlToRender <- paste0(htmlToRender, thisHtml)
  }

  output$pointClickData <- renderUI({
    HTML(htmlToRender)
  })
}

