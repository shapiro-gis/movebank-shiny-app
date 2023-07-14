app1_init<-function(input,output,session){

  input<<-input
  output<<-output
  session<<-session

  whichAppIsRunning<<-'app1'
  observeEvent(input$changeAppsButton, {
    changeToOtherApp()
  },ignoreInit=TRUE)
  
  observeEvent(input$closeMappButton, {
    closeApp()
  },ignoreInit=TRUE)

  observeEvent(input$exportDataButton,{

    if('firstFileName'%in%names(configOptions)){
      fileExportName<<-configOptions$firstFileName
    }else{
      fileExportName<<-'exportedFile'
    }

    if(exists('exportObserver')){
      exportObserver$destroy()
    }

    isExportRunning<<-FALSE
    showModal(modalDialog(
           title="Choose a name for file to export",
           "Choose a name below for the file that will be exported. The default name is the name of the first imported file. The file will be exported into a subfolder called EXPORTS within your working folder. Once you choose a name click the EXPORT SHAPEFILE to continue.",
           br(),
           textInput('fileExportInput', '', value = fileExportName, width = NULL, placeholder = NULL),
           actionButton("exportShapefileButton", "EXPORT SHAPEFILE"),
           footer = modalButton("CANCEL EXPORT")
       ))

       exportObserver<<-observeEvent(input$exportShapefileButton,{
           exportShapefile()
       },ignoreInit=TRUE,once = TRUE)


       observeEvent(input$fileExportInput,{
         fileExportName<<-input$fileExportInput
       },ignoreInit=TRUE)

  },ignoreInit=TRUE)



  observeEvent(input$timezoneSelector,{
      selectedTimezone<<-input$timezoneSelector
  },ignoreInit=TRUE)




  observeEvent(input$movebankLoginButton,{
    toggleModal(session,'movebankModal',toggle='open')
  },ignoreInit=TRUE)
  
  observeEvent(input$downloadMovebankDataButton,{
    toggleModal(session,'movebankModal',toggle='close')
    delay(100,
          downloadMovebankData(input$movebankUserInput,input$movebankPasswordInput,input$movebankStudyInput)
    )
  },ignoreInit=TRUE)
  


  observeEvent(input$loadProjectButton,{
      tryCatch({
        rdsLocation <- choose.dir(caption = "select your project folder and press OK")
        appOneReload(rdsLocation)
      }, error = function(ex) {
        modalMessager('Error',paste0('Try choosing a file again'))
      })
  },ignoreInit=TRUE)

  
  w <- Waiter$new(
    html = tagList(
      spin_3(),
      h4("Exporting shapefile..", style = "color: grey") # Add style attribute to h4 element
    ),
    color = transparent(.5)
  )
  
  
  observeEvent(input$exportShapefile,{
    showModal(modalDialog(
      title="Export Shapefile",
      textInput('fileName', 'Please provide a file name', ),
      footer = tagList(actionButton("confirmExport", "Export"),
                       modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirmExport,{
    w$show()
    layername = input$fileName
    output_shapefile <- normalizePath(file.path(exportQuery(), paste0(layername, ".shp")))
    print(output_shapefile)
    print(names(importedDatasetMaster))
    writeOGR(importedDatasetMaster, dsn = output_shapefile, layer = layername, driver = "ESRI Shapefile",overwrite_layer = TRUE)
    w$hide()
    
    shinyalert("Success!", paste0("Your shapefile was written to the following location:", output_shapefile), type = "success")
    
    removeModal()
  })
  
  
  
  
  observeEvent(input$finishDataCleaning,{
    
    
    # Export the SpatialPointsDataFrame as a shapefile
    showModal(modalDialog(
      title = "Are you sure you're done cleaning all movement data for this project?",
      "Click below to confirm",
      footer = tagList(
        actionButton("confirmBtn", "Confirm", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
    
  })
  observeEvent(input$confirmBtn, {
    w <- Waiter$new(
      html = tagList(
        spin_3(),
        h4("Exporting data...", style = "color: grey") # Add style attribute to h4 element
      ),
      color = transparent(.5)
    )
    
    w$show()
    names(importedDatasetMaster) <- tolower(names(importedDatasetMaster))  # Convert colnames to lowercase
     print(names(importedDatasetMaster))
    
    requiredFields <- c("newuid", "species","studyname", "lat", "lon", "datetest", "problem", "mortality", "dt", "dist","burst","speed","id_yr","nsdoverall")
    
    importedDatasetMaster <- importedDatasetMaster[, requiredFields]
    
    layername = unique(importedDatasetMaster$studyname)
    
    output_shapefile <- normalizePath(file.path(MovebankFolder(), paste0(layername, ".shp")))
    #writeOGR(importedDatasetMaster, dsn = output_shapefile, layer = layername, driver = "ESRI Shapefile",overwrite_layer = TRUE)
    
    output_rds <- normalizePath(file.path(MovebankFolder(), paste0(layername, ".rds")))
    saveRDS(importedDatasetMaster, file = output_rds)
    
   w$hide()
     removeModal()
    updateTabsetPanel(session, "navibar",selected = "mapviewer")
    
  })
  

  observeEvent(input$maxSpeedSelector,{
      configOptions$maxSpeedParameter<<-input$maxSpeedSelector
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$mortDistance,{
      configOptions$mortDistance<<-input$mortDistance
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$mortTime,{
      configOptions$mortTime<<-input$mortTime
      saveConfig()
  },ignoreInit=TRUE)
  
  wc <- Waiter$new(
    html = tagList(
      spin_3(),
      h4("Calculating movement parameters...", style = "color: grey") # Add style attribute to h4 element
    ),
    color = transparent(.5)
  )
  observeEvent(input$calcMoveParamsButton,{
    wc$show()
      findProblemPoints()
      wc$hide()
  },ignoreInit=TRUE)




  observeEvent(input$getStartedButton, {
    toggleModal(session,'welcomeModal',toggle='close')
  },ignoreInit=TRUE)

  toggleModal(session,'welcomeModal',toggle='open')


  observeEvent(input$parametersButton, {
    toggleModal(session,'configModal',toggle='open')
  },ignoreInit=TRUE)

  # choose.dir <- function() {
  #   system("osascript -e 'tell app \"R\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
  #          intern = FALSE, ignore.stderr = TRUE)
  #   p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
  #   return(ifelse(length(p), p, NA))
  # }
  # 

  observeEvent(input$chooseDirButton, {
  dataFolder<<-choose.dir()
  availableShapefiles <<- list.files(dataFolder, pattern = '.shp$')
  if (length(availableShapefiles) == 0) {
    modalMessager(
      "Folder Selection Error",
      "No valid shapefile are present in this directory. Please check the
      directory and try again"
    )
    return
  }
  availableShapefiles <- append("", availableShapefiles)

  ##--------------------------------make a label showing selected folder
  output$selectedDirectoryLabel <- renderUI({
    p(paste("You successfully imported ", dataFolder, sep = ""))
  })

  ##--------------------------------render the dropdown for available shapes
  output$selectedShapefileHeaderLabel <- renderUI({
    strong('(2) Choose shapefile(s) from the selected data directory')
  })
  output$fileUploadSelectorHolder <- renderUI({
    selectInput(
      "fileUploadSelector",
      "",
      availableShapefiles,
      selected = NULL,
      multiple = TRUE
    )
  })

  ##------------------ start file import
  output$fileUploadExecute<-renderUI({
      actionButton('fileUploadExecute','Begin File Import')
  })
})


observeEvent(input$fileUploadExecute, {
    if(exists('importedDatasetMaster')){
      toggleModal(session,'moreDataModal',toggle='toggle')
      return()
    }
    if(is.null(input$fileUploadSelector)){
      modalMessager('Error','You need to select a shapefile to continue import')
      return()
    }
    prepareFileImport()
  })


  dtvRunning<<-FALSE;
  observeEvent(input$processDatesButton,{
          if(!dtvRunning){
            dateTimeValidator()
          }
      })
  set_token("pk.eyJ1IjoianNoYXBpcm8xIiwiYSI6ImNrdDA1OGR5MzAxeHIyb290am05MzF1c2IifQ.wuOxNF5KFK0pjUJ3O80OmA") #this is jessie's token
  

}


exportShapefile=function(){
    fileExportFolder<-paste0(masterWorkingDirectory,'\\EXPORTS')
    if(dir.exists(fileExportFolder)==FALSE){
      dir.create(fileExportFolder)
    }

    if(file.exists(paste0(fileExportFolder,'\\',fileExportName,'.shp'))){
      time<-Sys.time()
      time<-gsub(" ", "", time, fixed = TRUE)
      time<-gsub("-", "", time, fixed = TRUE)
      time<-gsub(":", "", time, fixed = TRUE)
      fileExportName<<-paste0(fileExportName,'_',time)
    }

    # selectedTimezone<<-input$timezoneSelector
    tryCatch({
      if('originalProjection' %in% names(configOptions)){
        dataToExport<-spTransform(importedDatasetMaster, CRS(configOptions$originalProjection))
        dataToExport<-dataToExport[,c(configOptions$originalColumns,'problem','mortality','comments')]
      }else{
        dataToExport<-importedDatasetMaster
        dataToExport<-dataToExport[,c(configOptions$originalColumns,'problem','mortality','comments')]
      }
      loadingScreenToggle('show',paste0('exporting file to ',fileExportFolder))
      writeOGR(dataToExport, fileExportFolder, fileExportName, driver = "ESRI Shapefile")
      modalMessager('File Exported',paste0('File exported succesfully.'))
      loadingScreenToggle('hide',paste0('exporting file to ',exportDirectory))
    }, error = function(ex) {
      modalMessager('Error',paste0('Try choosing a directory again'))
      loadingScreenToggle('hide',paste0('exporting file to ',fileExportFolder))
    })
  }





clearShapefileSelector<-function(){
    updateSelectInput(session=session, "fileUploadSelector",
      label = "",
      choices = availableShapefiles,
      selected = NULL
    )
  }
