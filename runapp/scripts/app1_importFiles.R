prepareFileImport<<-function(){
  filesToImport<<-input$fileUploadSelector
  fileImportTracker<<-list()
  importedShapefilesHolder<<-list()
  
  loadingScreenToggle('show','importing files')
  
  for(i in 1:length(filesToImport)){
    
    if(i==1){
      configOptions$firstFileName<<-strsplit(filesToImport[i],'.shp')[[1]][1]
    }
    
    fileToImport <- tools::file_path_sans_ext(filesToImport[i])
    # HANDLER TO INDICATE THIS IS THE LAST ONE
    if(i<length(filesToImport)){
      importShapefile(fileToImport,FALSE,i)
    } else{
      importShapefile(fileToImport,TRUE,i)
    }
  }
  
  
  loadingScreenToggle('hide','importing files')
}



importShapefile<-function(fileToImport,lastOne,i){
  ##--------------------------------read the shapfile and overwrite the
  ##--------------------------------global importedDataset variable
  # first check if the file has already been imported.. this weeds out multiple
  # clicks on upload button or re-uploads etc
  if(!is.null(fileImportTracker[[fileToImport]])){
    if(fileImportTracker[[fileToImport]]=="inProgress"){
      return()
    }
    if(fileImportTracker[[fileToImport]]=="failed"){
      return()
    }
    if(fileImportTracker[[fileToImport]]==TRUE){
      return()
    }
  }
  
  importedDataset <<- tryCatch({
    fileImportTracker[[fileToImport]]<<-"inProgress"
    # progressIndicator(paste('Importing ',fileToImport,' Please wait',sep=""),'start')
    sf::as_Spatial(sf::st_read(paste0(dataFolder,'\\',fileToImport,'.shp')))
  },
  error = function(cond) {
    fileImportTracker[[fileToImport]]<<-'failed'
    #progressIndicator('Import Error','stop')
    
    modalMessager(
      "DATA IMPORT ERROR",
      paste(
        "There was a fatal error importing
          your datasets. Please check the data and try again. Detailed error from
          R is : ",
        cond,
        sep = ""
      )
    )
    return()
  },
  warning = function(cond) {
    fileImportTracker[[fileToImport]]<<-'failed'
    #progressIndicator('Import Error','stop')
    
    modalMessager("DATA IMPORT WARNING", cond)
    return()
  })
  importSuccessHandler(fileToImport,lastOne,i,mergingFiles)
}




importSuccessHandler<-function(fileToImport,lastOne,i,mergingFiles){
  # # keep track of position in imports --- to do.. this will probs get goofy if we delete and reimport over and over again
  importIterator<-i
  #progressIndicator(paste(fileToImport,' Imported successfully!',sep=""),'stop')
  #loadingScreenToggle('hide','')
  
  # add this shapefile to list holder of imported shapefiles
  importedShapefilesHolder[[fileToImport]]<<-assign(fileToImport,importedDataset)
  
  # change columns names to lowercase!!
  # names(importedShapefilesHolder[[fileToImport]]@data)<<-tolower(names(importedShapefilesHolder[[fileToImport]]@data))
  # change columns names to UPPERCASE!!
  names(importedShapefilesHolder[[fileToImport]]@data)<<-toupper(names(importedShapefilesHolder[[fileToImport]]@data))
  colsToCheck<-c("LAT","LON","newUid","elev","comments","rowIds","newMasterDate","burst","month","day","year","jul","id_yr","x","y","nsdYear","displacementYear","nsdOverall","displacementOverall","dist","dt","speed","abs.angle","rel.angle","fixRateHours","problem","mortality")
  colsToCheck<-toupper(colsToCheck)
  
  for(i in 1:length(colsToCheck)){
    thisCol<-colsToCheck[i]
    if(thisCol %in% names(importedShapefilesHolder[[fileToImport]]@data)){
      whichCol<-which(names(importedShapefilesHolder[[fileToImport]]@data) == thisCol)
      names(importedShapefilesHolder[[fileToImport]]@data)[whichCol]<<-paste0(thisCol,'_')
    }
  }
  
  
  
  
  
  # temp ui element for reference
  tempUiElement<-paste("uploadedShapefile",importIterator,sep="")
  
  clearShapefileSelector()
  
  if(lastOne==TRUE){
    output$importSuccessText<-renderUI({
      HTML(paste0('<strong>You succesfully imported ',length(importedShapefilesHolder),' files</strong>'))
    })
    
    
    loadingScreenToggle('hide','')
    checkColumnsPrjs()
    
  }
  
  
}





checkColumnsPrjs<-function(){
  #### check if columns are the same between datasets
  firstNames<-names(importedShapefilesHolder[[1]]@data)
  for(i in 1:length(importedShapefilesHolder)){
    theseNames<-names(importedShapefilesHolder[[i]]@data)
    if(!setequal(firstNames,theseNames)){
      modalMessager('COLUMN NAMES ERROR',"Column names are not the same between your datasets.
        Reformat your data and try again")
      #### TO DO --- if this happens, we need to remove the shapefiles and the loaded buttons
      clearShapefileSelector()
      return()
    }
  }
  
  
  # check that projections are the same between datasets
  for(i in 1:length(importedShapefilesHolder)){
    if(importedShapefilesHolder[[1]]@proj4string@projargs!=importedShapefilesHolder[[i]]@proj4string@projargs){
      modalMessager('PROJECTION ERROR',"Projections are not the same between your datasets.
        Reformat your data and try again")
      #### TO DO --- if this happens, we need to remove the shapefiles and the loaded buttons
      clearShapefileSelector()
      return()
    }
  }
  configOptions$originalProjection<<-importedShapefilesHolder[[1]]@proj4string@projargs
  configOptions$originalColumns<<-names(importedShapefilesHolder[[1]])
  saveConfig()
  showWorkingDirectorySelect();
}

showFilesUploadedIndicator<-function(){
  
}

showWorkingDirectorySelect<-function(){
  hideElement(id = 'importDataRow', anim = TRUE)
  showElement(id = 'folderSelectSection', anim = TRUE)
  
  hide('loadProjectButton')
  show('exportDataButton')
  output$workingDirectoryTitle<-renderUI({
    HTML('<strong>(2) Click the button below to choose and empty Project Folder where all outputs will be stored.</strong>')
  })
  
  output$chooseWorkingDirButton<-renderUI({
    actionButton("chooseWorkingDirButton", "Click to Choose Directory")
  })
  
  ##------------------choose a folder where all export files will be stored
  
  
  observeEvent(input$chooseWorkingDirButton, {
    masterWorkingDirectory<<-NULL
    
    shinyjs::disable("chooseWorkingDirButton")
    
    masterWorkingDirectory<<-choose.dir(dataFolder)
    
    if(is.na(masterWorkingDirectory) | is.null(masterWorkingDirectory)){
      shinyjs::enable("chooseWorkingDirButton")
      return()
    }
    
    if(!dir.exists(masterWorkingDirectory)){
      modalMessager('error','Please try selecting this folder again')
      shinyjs::enable("chooseWorkingDirButton")
      return()
    }
    
    
    
    
    files<-list.files(masterWorkingDirectory)
    
    if(length(files)>0){
      modalMessager('error','The folder you chose is not empty.
        This will cause errors in analysis. Please empty the folder or
        choose a different directory and try again.')
      shinyjs::enable("chooseWorkingDirButton")
      masterWorkingDirectory<<-NULL
      return()
    }
    
    configOptions$masterWorkingDirectory<<-masterWorkingDirectory
    saveConfig()
    
    
    output$selectedWorkingDirectoryLabel<-renderUI({
      strong(paste0('Your data will be exported to: ',masterWorkingDirectory))
    })
    
    
    output$selectedWorkingDirectoryLabel<-renderUI({
      HTML(paste0('<strong>',masterWorkingDirectory,'</strong>'))
    })
    
    showColumnChoiceInfo()
    shinyjs::enable("chooseWorkingDirButton")
  },ignoreInit=TRUE)
  
}


showColumnChoiceInfo<-function(){
  
  hideElement(id = 'importDataRow', anim = TRUE)
  hideElement(id = 'folderSelectSection', anim = TRUE)
  showElement(id = 'uidSeletorRow', anim = TRUE)
  
  
  if(!exists('importedDatasetMaster')){
    columnNames<-names(importedShapefilesHolder[[1]])
    ##------------------ show the first 20 rows of data
    rowsToShow<-importedShapefilesHolder[[1]][1:20,]
  }else{
    columnNames<-names(importedDatasetMaster@data)
    importedDatasetMaster@data['comments']<<-''
    
    rowsToShow<-importedDatasetMaster[1:20,]
  }
  
  if('timestamp'%in%names(rowsToShow)){
    rowsToShow$timestamp<-as.character(rowsToShow$timestamp)
  }
  output$aidConfigTable <- renderTable(rowsToShow)
  
  # output$uniqueIdSelector<-renderUI({
  #   selectInput(
  #     "uniqueIdSelector",
  #     "",
  #     c("",'NaN',columnNames),
  #     selected = NULL,
  #     multiple = FALSE
  #   )
  # })
  updateSelectInput(session, "studynameSelector", choices = c("",'NaN',columnNames))
  updateSelectInput(session, "speciesSelector", choices = c("",'NaN',columnNames))
  updateSelectInput(session, "uniqueIdSelector", choices = c("",'NaN',columnNames))
  
  # output$uniqueIdSelectorGo<-renderUI({
  # })
  observeEvent(input$speciesSelector, {
    selectedSpeciesField <- input$speciesSelector
    
    if (selectedSpeciesField == "NaN") {
      showModal(modalDialog(
        title = "Missing Field Value",
        "Select species type",
        selectInput(
          inputId = "customSpeciesInput",
          label = "individ",
          choices = c("Alces alces", "Antilocapra americana", "Bison bison", "Cervus canadensis", "Odocoileus hemionus", "Ursus americanus")
        ),
        footer = tagList(
          actionButton("addValueBtn", "Add Value", class = "btn-primary")
        )
      ))
      output$speciesresult <- renderText({
        paste("Species Entered:", input$customSpeciesInput)
      })
      return()
    }else {
      output$speciesresult <- renderText(NULL)
    }
  })
  
  observeEvent(input$addValueBtn, {
    removeModal()
  })
  
  # 
  observeEvent(input$addStudyBtn, {
    removeModal()
  })
  
  # 
  #
  observeEvent(input$studynameSelector, {
    selectedstudyField <- input$studynameSelector
    
    if (selectedstudyField == "NaN") {
      showModal(modalDialog(
        title = "Missing Field Value",
        "Select study type",
        textInput(
          inputId = "customStudyInput",
          label = "Study Name",
          value = ""
        ),
        footer = tagList(
          actionButton("addStudyBtn", "Add Value", class = "btn-primary")
        )
      ))
      output$studyNameresult <- renderText({
        paste("Study NameEntered:", input$customStudyInput)
      })
      return()
    }else {
      output$studyNameresult <- renderText(NULL)
    }
    
  })
  
  
  
  
  
  observeEvent(input$uniqueIdSelectorGo, {
    # if (is.null(masterWorkingDirectory)) {
    #   modalMessager('Error', 'You need to select an empty working directory to continue')
    #   return()
    # }
    
    if (!exists('importedDatasetMaster')) {
      mergeShapfilesHandler()
    } else {
      newUid <- input$uniqueIdSelectorGo
      
      species <- if (!is.null(input$customSpeciesInput)) {
        input$customSpeciesInput
      } else {
        input$speciesSelector
      }
      
      studyname <- if (input$studynameSelector == "NaN") {
        input$movebankStudyInput
      } else {
        input$studynameSelector
      }
      
      importedDatasetMaster$newUid <<- importedDatasetMaster@data[, newUid]
      importedDatasetMaster$species <<- importedDatasetMaster@data[, species]
      importedDatasetMaster$studyname <<- studyname
      
      
      showDateTimeSelectionPanel()
    }
  })
  
  
  if(thisIsTestingRun){
    mergeShapfilesHandler()
  }
  
}



filesToImport<-c("gps_318.shp","gps_318dead.shp","gps_320.shp","gps_317.shp","gps_321.shp","gps_344.shp","gps_346.shp")
availableShapefiles<<-c("gps_318.shp","gps_318dead.shp","gps_320.shp","gps_317.shp","gps_321.shp","gps_344.shp","gps_346.shp")
dataFolder<-"D:\\sampleData"
fileImportTracker<<-list()
importedShapefilesHolder<<-list()
thisIsTestingRun<<-FALSE

testImport<<-function(){
  
  thisIsTestingRun<<-TRUE
  dateColumns<<-c('date','hour')
  selectedUid<<-"aid"
  validatorObject<<-list()
  validatorObject$year<<-c('year','1',"%Y","date")
  validatorObject$month<<-c('month','2',"%m","date")
  validatorObject$day<<-c('day','3',"%d","date")
  validatorObject$hour<<-c('hour','1',"%H","hour")
  
  
  loadingScreenToggle('show','importing files')
  for(i in 1:length(filesToImport)){
    fileToImport <- tools::file_path_sans_ext(filesToImport[i])
    # HANDLER TO INDICATE THIS IS THE LAST ONE
    if(i<length(filesToImport)){
      importShapefile(fileToImport,FALSE,i)
    } else{
      importShapefile(fileToImport,TRUE,i)
    }
  }
}

w <- Waiter$new(
  html = tagList(
    spin_3(),
    h4("Downloading Movebank data... please be patient... depending on file size, this can take a while.", style = "color: grey") # Add style attribute to h4 element
  ),
  color = transparent(.5)
)
downloadMovebankData <- function(user, pw, movebankId) {
  show('downloadSpinner')
  
  storedLogin <- movebankLogin('username' = user, 'password' = pw)
  
  w$show()
  
  movebankData <- tryCatch({
    execute_safely(getMovebankData(as.numeric(movebankId), 'login' = storedLogin, removeDuplicatedTimestamps = TRUE))
  })
  w$hide()
  
  if (!is.null(movebankData)) {
   # loadingScreenToggle('show', 'downloading movebank data.. please be patient.. depending on file size, this can take a while.')
    
    processMovebankData(movebankData)
    # processMovebankData(data.frame(movebankData))
  }
}


processMovebankData<-function(movebankData){
  
  
  theseDataNames<-names(movebankData)
  print(theseDataNames)
  if('location_long'%in%theseDataNames){
    thisLongField<-'location_long'
  }
  if('location_long.1'%in%theseDataNames){
    thisLongField<-'location_long.1'
  }
  if('location_lat'%in%theseDataNames){
    thisLatField<-'location_lat'
  }
  if('location_lat.1'%in%theseDataNames){
    thisLatField<-'location_lat.1'
  }
  
  movebankData <- as.data.frame(movebankData)
  
  library(dplyr)
  
  if ('location_long' %in% theseDataNames) {
    thisLongField <- 'location_long'
    movebankData <- movebankData %>% rename(location_long_new = location_long)
  }
  
  # if ('location_long.1' %in% theseDataNames) {
  #   thisLongField <- 'location_long.1'
  #   movebankData <- movebankData %>% rename(location_long_new = location_long.1)
  # }
  
  if ('location_lat' %in% theseDataNames) {
    thisLatField <- 'location_lat'
    movebankData <- movebankData %>% rename(location_lat_new = location_lat)
  }
  
  
  movebankData <- movebankData %>%
    filter(location_lat_new         >= -90) %>%
    filter(location_long_new  >= -180)
  # xy <- mydf[,c(1,2)]
  
  importedDatasetMaster <<- SpatialPointsDataFrame(
    coords = movebankData[, c('location_long_new', 'location_lat_new')],
    data = movebankData,
    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  )
  
  

  print(movebankData)
  importedDatasetMaster@data[["lon"]]<<-as.numeric(importedDatasetMaster@data[,'location_long_new'])
  importedDatasetMaster@data[["lat"]]<<-as.numeric(importedDatasetMaster@data[,'location_lat_new'])
  importedDatasetMaster@data$rowIds<<-row.names(importedDatasetMaster@data)
  
  # back to 5072
  # prj to utm
  # importedDatasetMaster<<-spTransform(importedDatasetMaster,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  midLatLong <- c(importedDatasetMaster@data[1,'lat'],importedDatasetMaster@data[1,'lon'])
  # zone <- find_UTM_zone(midLatLong[1], midLatLong[2])
  zone <- find_UTM_zone(midLatLong[2], midLatLong[1])
  UTMcrs <<- paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  importedDatasetMaster <<- spTransform(importedDatasetMaster, CRS(UTMcrs))
  
  
  loadingScreenToggle('hide','')
  showFilesUploadedIndicator();
  showWorkingDirectorySelect();
  
  # toggleModal(session,'movebankModal',toggle='close')
}
