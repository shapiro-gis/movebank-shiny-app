objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")
rm(list = ls())
gc()
options(warn=-1)

list.of.packages<-c("shiny", "waiter", "mapdeck","dplyr","adehabitatHR","shinythemes","shinyWidgets","mapboxer",
     "sf","rgdal", "httr","shinyBS","RSQLite","move","shinycssloaders","raster","shinyjs", "data.table", "leaflet",
     "bsplus","RColorBrewer", "bslib","furrr", "shinyalert", "ggplot2")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# lapply(list_of_packages, 
#        function(x) if(!require(x,character.only = TRUE)) install.packages(x))
#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
lapply(list.of.packages, require, character.only = TRUE)


# Read in scripts
source("scripts/movebankData.R",local=TRUE)
source("scripts/animalinfo.R",local=TRUE)
source("scripts/gisLayers.R",local=TRUE)
source("scripts/exports.R",local=TRUE)
#source("scripts/lineBuffer.R",local=TRUE)


source("scripts/app2_init.R",local=TRUE)

source("scripts/globalVars.R",local=TRUE)
source("scripts/app1_ui.R",local=TRUE)

source("scripts/app1_importFiles.R",local=TRUE)
source("scripts/app1_mergeProjectFiles.R",local=TRUE)
source("scripts/app1_dateTimeFormatting.R",local=TRUE)
source("scripts/app1_calculateMovementParams.R",local=TRUE)
source("scripts/app1_mapImportedData.R",local=TRUE)
source("scripts/app1_summarizeAid.R",local=TRUE)
source("scripts/exportData.R",local=TRUE)


source("../globalScripts/globalUiFunctions.R",local=TRUE)
source("../globalScripts/sqlLiteQueries.R",local=TRUE)

source("scripts/dropDuplicates.R",local=TRUE)
source("scripts/creat.burst.R",local=TRUE)
source("scripts/find.problem.pts.R",local=TRUE)
source("scripts/mov.param.R",local=TRUE)
source("scripts/mort.check.R",local=TRUE)


dependencies<-c("shiny","sf","mapdeck", "circular","shinyjs","shinyBS","sp","ggplot2","mapboxer","rgdal","adehabitatHR",'RSQLite','move','shinycssloaders','raster','terra','R.utils','waiter', 'shinythemes','shinyWidgets')
loadDependencies(dependencies)
# lubridate can cause issues when loaded in app2 if the R session is not terminated before reloading app1
if("lubridate" %in% (.packages())){
  detach("package:lubridate", unload=TRUE)
}

Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1Ijoid21pLW1lcmtsZSIsImEiOiJja3RrYmluMnMxazRlMm9xbnN3bXluYjQzIn0.wOmx_vSC944YRdF8LSjZRQ")

ui <- navbarPage(id = "navibar",
  title = div(#img(src = "deertracks.png",height = "30px", width = "30px"),
    "Movebank Explorer",
    tags$style(HTML("
      #logo {
        position: absolute;
        top: 0;
        right: 80px;
        margin-top: 10px;
        align-items: center;

      }
    ")),
    tags$style(HTML("
      #logo2 {
        position: absolute;
        top: 0;
        right: 20px;
        margin-top: 5px;
        align-items: center;

      }
    ")),img(src = "logo.png", id = "logo", height = "30px", width = "100px"),
        img(src = "wgfd_logo.png", id = "logo2",height = "50px", width = "40px"),
  ),
  theme = shinytheme("sandstone"),
  # tabPanel("Connect", 
  # ),
tabPanel("Home", id = "homepage",
         # parent container
         tags$div(class="landing-wrapper",
                  
                  # child element 1: images
                  tags$div(class="landing-block background-content",
                           
                           # top left
                           img(src= "pronghorn.jpg"),
                           
                           # top right
                           img(src="muledeer.jpg"),
                           
                           # bottom left
                           img(src="elk.jpg"), 
                           
                           # bottom right
                           
                           img(src="wmi.jpg")
                           
                  ),
                  
                  # child element 2: content
                  tags$div(class="landing-block foreground-content",
                           tags$div(class="foreground-text",
                                    tags$h1("Welcome!"),
                                    tags$p("Version 1.0.0 Beta 1"),
                                    tags$p("Welcome to the R Shiny App designed to explore historical point location data for diverse wildlife species across Wyoming. This user-friendly application streamlines data processing, extraction from Movebank, and visual representation of wildlife movement. 
                                    Additionally, it enables users to gain insights into basic inquiries regarding wildlife movements and spatial utilization.
                                    To begin, upload your movement data by clicking on the Data Cleaning button below. If you've used the app before, click on the Map Viewer to continue your data exploration.
                                    "),
                                    tags$a(href = "https://shapiro-merkle.gitbook.io/wgfd-movebank-guide/",
                                           target = "_blank",
                                           "Explore the handbook here!"),
                                    tags$div(
                                      style = "display: flex; justify-content: center;",
                                      fluidRow(
                                        style = "display: flex; justify-content: center;",
                                        card(
                                          width = 6,
                                          status = "primary",
                                          title = "Go to Data Cleaning",
                                          h3(strong("1.Data Cleaning")),
                                          p("Perform data cleaning tasks to prepare your data for the Map Viewer tab."),
                                          actionButton("cleaningButton", "Go to Data Cleaning", width = "200px", height = "200px"),
                                          
                                        ),
                                        card(
                                          width = 6,
                                          status = "primary",
                                          title = "Go to Map Viewer",
                                          h3(strong("2.Map Viewer")),
                                          p("Explore and visualize data processed and cleaned on an interactive map."),
                                          actionButton("mapButton", "Go to Map Viewer", width = "200px", height = "200px"),
                                          
                                        )
                                      )
                                    )
                                    
                           )
                  )
         ),
),
           
tabPanel( "Data Cleaning",value = "app1",
         tags$head(tags$style("body{ overflow-x:hidden}")),
         tags$style(type = "text/css", ".container-fluid {padding-left:15px;
                    padding-right:15px;}"),
         tags$style(type = "text/css", ".navbar {margin-bottom: 0;}"),
         tags$style(type = "text/css", ".container-fluid .navbar-header 
.navbar-brand {margin-left: 0px; margin-top: 0px;}"),
         # HTML(id="underSurface","<div id='underSurface' style='width:100%; height:100%; background-color:rgba(255, 255, 255,0); color:rgba(255, 255, 255,0); position:absolute; top:0px; left:0px; z-index:1000;'></div>"),
         uiOutput("loading"),
         HTML("<div id='loadingScreen' style='width:100%; display:none; height:200%; background-color:rgba(0, 0, 0,0.5); color:white; position:absolute; top:0px; left:0px; z-index:5000;'>
  <div id='loadingMessage' style='position:absolute; top:10%; text-align:center; font-size:15px; color:white; width:100%;'></div>
  <img src='spinner.gif' style='position:absolute; top:25%; left:45%;'>
  </div>"),
         useShinyjs(),
         
         bsModal("movebankModal", "Movebank Project Download", NULL, size = "medium",
                 strong('user name'),
                 textInput('movebankUserInput', '', value = "", width = NULL, placeholder = NULL),
                 br(),
                 strong('password'),
                 passwordInput('movebankPasswordInput', '', value = "", width = NULL, placeholder = NULL),
                 br(),
                 strong('Movebank ID'),
                 textInput('movebankStudyInput', '', value = "", width = NULL, placeholder = NULL),
                 actionButton("downloadMovebankDataButton", "Download Data"),
                 hidden(
                   fluidRow( id="downloadSpinner",withSpinner(plotOutput("downloadSpinnerDummy")))
                 ),
                 tags$head(tags$style("#moreDataModal .modal-footer{ display:none}"))
         ),
         bsModal("configModal", "Configuration Options", NULL, size = "medium",
                 p('Adjusting these parameters will influence how mortalities and problem points are flagged in your dataset. If these flags occur, points will not be dropped, they will just be flagged in new columns named "mortality" and "problem".'),
                 h4('Maximum Speed',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
                 numericInput("maxSpeedSelector", "Max Speed (km/hr)", 10.8, step=0.1,),
                 h4('Mortality Options',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
                 numericInput("mortDistance", "Minimum Distance (meters)", 50),
                 numericInput("mortTime", "Time unit (hours)", 48),
                 # strong('the button below will rebuild your migtime table used in app2. Note if you have already selected dates, these will all be lost.'),
                 hidden(
                   fluidRow(id='recalcInstructions',
                            p('Press the button below to recalculate movement parameters. Note that any flags already added to your data will be lost.')
                   )),
                 hidden(actionButton("calcMoveParamsButton", "Recalculate")),
                 tags$head(tags$style("#moreDataModal .modal-footer{ display:none}"))
         ),
         
         # column(12,
         # hidden(actionButton("exportDataButton", style = "font-weight:bolder; position:absolute !important; top:49px !important; left:-5px !important;", "Export Updated File"))
         # ),
         fluidRow(id='importDataRow',
          #         column(12,
          #                HTML(
          #                  "You can upload one ESRI shapefile with many individuals, or multiple
          # shapefiles each representing a single individual. If importing unique
          # files for each individual, it is necessary that all files have identical
          # columns, data formats and projections. If importing a merged file, it
          # is necessary that the file includes a column delineating unique animal IDs.
          # <br>
          # <br>
          # You also may way want to review additional parameters by clicking on the
          # button titled 'CONFIGURATION PARAMETERS'. This  will open a window which
          # allows you to configure the maximum speed that the program will flag as an error as well as the parameters (minimum movement rate over a specified period of time) that the program will flag as a potential mortality event.
          # <br>
          # <br>
          # Using the button below, choose the directory containing
          # your dataset(s). If you're uploading multiple files, they must
          # all be in the same directory<br><br>
          # "
          #                )
          #     d    ),

                column(12,
                       tags$div(
                         style = "text-align: center;",
                        # h2(strong("Data Cleaning")),
                        br(),
                        tags$h1("Select an option to start data cleaning!"),
                        # (style = "width: 60%; margin: 0 auto;",
                        #    "Choose an option below to initiate movement data cleaning")
                        #  
                       ),
                       br(),
                       column(4,
           h3(strong('Upload Shapefile')),
                              p("Choose the directory containing your dataset(s) below. If uploading multiple files, they must all be in the same directory. You can upload one ESRI shapefile with many individuals, or multiple
           shapefiles each representing a single individual. If importing unique files for each individual, it is necessary that all files have identical columns, data formats and projections. If importing a merged file, it
           is necessary that the file includes a column delineating unique animal IDs."),
                             p("If you have not already downloaded data from Movebank, please find further instructions at the link below."),
           tags$a(href = "https://shapiro-merkle.gitbook.io/wgfd-movebank-guide/app-workflow/part-1-data-cleaning/movebank-download",
                  target = "_blank",
                  "Click here to view instructions"),
           br(),
           
                              actionButton("chooseDirButton", "Click to Choose Folder"),
           br(),
                              uiOutput("selectedDirectoryLabel"),
                              uiOutput("fileUploadSelectorHolder"),
                              uiOutput("fileUploadExecute"),
                              strong('Uploaded File(s): Click to delete'),
                              uiOutput("uploadedShapefile1"),
                              uiOutput("uploadedShapefile2"),
                              uiOutput("uploadedShapefile3"),
                              uiOutput("uploadedShapefile4"),
                              uiOutput("uploadedShapefile5"),
                              uiOutput("uploadedShapefile6"),
                              uiOutput("uploadedShapefile7"),
                              uiOutput("uploadedShapefile8"),
                              uiOutput("uploadedShapefile9"),
                              uiOutput("uploadedShapefile10"),
                              uiOutput("uploadedShapefile11"),
                              uiOutput("uploadedShapefile12"),
                              uiOutput("uploadedShapefile13"),
                              uiOutput("uploadedShapefile14"),
                              uiOutput("uploadedShapefile15"),
                              uiOutput("uploadedShapefile16"),
                              uiOutput("uploadedShapefile17"),
                              uiOutput("uploadedShapefile18"),
                              uiOutput("uploadedShapefile19"),
                              uiOutput("uploadedShapefile20"),        
                            #  actionButton("app1Button", style = "width:100%; font-weight:bolder; margin-top:15px !important;", "Open module 1")
                              ),
                              column(4,
                                     
                                     h3(strong('Download from Movebank')),
                                     img(src = "movebank-logo.png",height = "100px", width = "350px"),
                                     
                                     p("Download data directly from movebank using your login credentials and the Movebank project ID."),
                                     actionButton("movebankLoginButton", "retrieve movebank data"),
                                     
                                   ),
                              column(4,
                                     h3(strong('Reload Existing Project')),
                                     p("Select the button below to continue working on an existing Movebank project."),
                                     actionButton("loadProjectButton", "Reload Existing Project Folder"),
                                     
                                     )),
                  column(12,
                         uiOutput("importSuccessText"),
                         uiOutput("importSuccessOverview"),
                  )
         ),
         fluidRow(id='filesUploadedSection',
                  column(12,
                         
                  )),
         hidden(
           fluidRow(id='folderSelectSection',
                    column(6,
                           column(12,
                                  uiOutput("workingDirectoryTitle"),
                                  br(),
                                  uiOutput("chooseWorkingDirButton"),
                                  uiOutput("selectedWorkingDirectoryLabel")
                           )
                    ))),
         hidden(
           fluidRow( id="uidSeletorRow",
                     column(12,
                            HTML('<p>Your dataset(s) have been successfully imported. You now need to
          identify a unique animal identifier. Using the dropdown below, select the
          column that contains a unique ID for each animal in your dataset. This
          will be mandatory if you have imported a single shapefile with merged
          individuals. If you have imported many files with multiple individuals
          and do not have a unique ID column, choose "NaN" from the dropdown, and
          a unique ID will be created from the name of each file.'),
                            column(12,
                                   HTML('<p>Once you make a choice in the dropdown, press the EXECUTE button to
            continue preparing your data. If "NaN" is selected below, please enter the correct value.</p>'),
                                   selectInput(
                                     "studynameSelector",
                                     "Select Study Name Column",
                                     choices = NULL,
                                     selected = NULL,
                                     multiple = FALSE
                                   ),
                                   textOutput('studyNameresult'),
                                   
                                     selectInput(
                                       "speciesSelector",
                                       "Select Species Column",
                                       selected = NULL,
                                       choices = NULL,
                                       multiple = FALSE
                                     ),
                                   textOutput('speciesresult'),
                                   
                                   selectInput(
                                     "uniqueIdSelector",
                                     "Select Unique Animal Column",
                                     choices = NULL,
                                     selected = NULL,
                                     multiple = FALSE
                                   ),
                                   actionButton('uniqueIdSelectorGo','Execute'),
                                   
                                   #uiOutput("uniqueIdSelector"),
                                   #uiOutput("uniqueIdSelectorGo"),
                                   uiOutput("selectedShapefileLabel")
                            ),
                            column(12,
                                   tags$div(
                                     style = "max-height: 300px; max-width: 1000px; overflow: auto;",
                                     
                                     #style = "max-height: 500px; overflow-y: scroll;",
                                     tableOutput("aidConfigTable")
                                   )                            )
                     )
           )),
         hidden(
           fluidRow( id="dateTimeRow",
                     column(12,
                            p('Date/Time data comes in a variety of formats. We understand your
        date/time information could be stored in one column or spread out across
        several columns. To start, pick the column or columns from the list below
        that contain date/time information. Once you finish selecting the columns,
        click "DONE SELECTING DATE COLUMN(S)."'),
                            uiOutput("dateColumnSelector"),
                            checkboxGroupInput(
                              "dateColumnSelector",
                              "",
                              choiceNames=NULL,
                              choiceValues=NULL,
                              selected=NULL,
                              inline=TRUE
                            ),
                            actionButton("doneChoosingDateColumnsButton", "DONE SELECTING DATE COLUMN(S).")
                     ),
                     column(8,align="right",
                            tags$div(
                              style = "max-height: 300px; max-width: 1200px; overflow: auto;",
                              
                              #style = "max-height: 500px; overflow-y: scroll;",
                              tableOutput("dateConfigTable1")
                            )        
                     )
           )
         ),
         hidden(
           fluidRow(id="dateTimeElementsRow",
                    column(12,
                           HTML('<p>The next step is select which date/time elements are contained
      in each column and in what order they appear. This is important so the App
      can interpret the date/time data correctly.
      <br><br>
      Shown here are menus that allow you to indicate which date/time elements
      are in each column you selected on the previous page. Using the dropdown
      menu chose the elements from each column in the exact order they appear.
      <br><br>
      When choosing a column with multiple date/time elements, delineators can be ignored. This software will
      automatically detect these and parse accordingly.
      <br><br>
      Once you are ready to proceed click the "PROCESS DATES" button.
      <br><br>
      Note that when you click the process dates button, certain points and individuals
      may be removed from your dataset. For example, if an individual had unreasonable speeds
      or was categorized as mortalities, they could be removed from your dataset. It is recomended
      that you review the configuration tab before clicking the process dates button.
      </p>'
                           ),
                           uiOutput("timeProcessingResults")
                    ),
                    column(12,
                           uiOutput("dateConfigUi1"),
                           uiOutput("dateConfigUi2"),
                           uiOutput("dateConfigUi3"),
                           uiOutput("dateConfigUi4"),
                           uiOutput("dateConfigUi5"),
                           uiOutput("dateConfigUi6"),
                           uiOutput("dateConfigUi7"),
                           uiOutput("dateConfigUi8"),
                           uiOutput("dateConfigUi9"),
                           uiOutput("dateConfigUi10"),
                           uiOutput("dateConfigUi11"),
                           uiOutput("dateConfigUi12"),
                           uiOutput("dateSeperatorSelector"),
                           uiOutput("timeSeperatorSelector"),
                           HTML('Time zone will default to GMT. If you would like to format your times using another timezone, choose the UTC offset associated with you timezone below'),
                           selectizeInput('timezoneSelector', 'What time zone is your data stored in (default is GMT)?', OlsonNames(), selected = 'GMT', multiple = FALSE, options = NULL),
                           uiOutput("timeFormatResults"),
                           actionButton("processDatesButton", "PROCESS DATES")
                    ),
                    column(12,
                           tableOutput("dateConfigTable2")
                    )
           )
         ),
hidden(
  fluidRow( id="importedDataMapRow",
    tags$head(tags$script(src="js/mapboxer.js")),
    tags$head(tags$script(src="https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-draw/v1.2.2/mapbox-gl-draw.js")),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-draw/v1.2.2/mapbox-gl-draw.css"),
  bsModal("pointClickModal", "PointData", NULL, size = "large",
        column(5,
          actionButton("previousPointButton", "← previous point"),
        ),
        column(2,
        ),
        column(5,
          actionButton("nextPointButton", "next point →"),
        ),
        column(6,
        selectInput('isMortalitySelector', 'Is this point classified as mortality?',c('yes','no')),
        ),
        column(6,
        selectInput('isProblemSelector', 'Is this point classified as a problem point?',c('yes','no')),
        ),
        column(12,
        textInput('commentInput', 'Comments for this point'),
        ),
        uiOutput("pointClickData")
  ),
  bsModal("manyPointsSelectedModal", "Points Selected", NULL, size = "large",
        column(12,
          p('You have made a selection of many points. You can do a batch action and classify all these points as mortalities, problems or add comments to all points. **This action is not reversable**'),
        ),
        column(6,
          selectInput('manyPointsIsMortalitySelector', 'Reclassify all these points as mortalities?',c('','yes','no'),selected=''),
        ),
        column(6,
          selectInput('manyPointsIsProblemSelector', 'Reclassify all these points as problem points?',c('','yes','no'),selected=''),
        ),
        column(12,
          textInput('manyPointsCommentInput', 'Comments for this group of points'),
        )
  ),
# uiOutput('polygonHolder'),
div(class = "outer",
    
    
  mapboxerOutput('importedDataMapBox', width = "100%", height = "105vh"),
  
 
  absolutePanel(
    id = "controls", class = "panel panel-default", fixed = TRUE,
    draggable = TRUE, top = 120, 
    left = "0.4%", bottom = "auto",
    width = 0, height = 0,
    actionBttn(
      inputId = "parametersButton",
      label = "Configuration Options",
      style = "material-circle",
      color = "primary",
      icon = icon("gears")
    ),
    bsTooltip(id = "parametersButton", title = "Configuration Parameters",
              placement = "right", trigger = "hover"),
    br(),
    actionBttn(
      inputId = "exportShapefile",
      label = "Export Data",
      style = "material-circle",
      color = "primary",
      icon = icon("download")
      ),

  bsTooltip(id = "export", title = "Export data to shapefile",
            placement = "right", trigger = "hover"),
    br(),
    actionBttn(
      inputId = "basemapButton",
      label = "Toggle Basemap",
      style = "material-circle",
      color = "primary",
      icon = icon("map")
    ),
  bsTooltip(id = "basemapButton", title = "toggle basemap options",
            placement = "right", trigger = "hover"),
    
    
    ),

  absolutePanel(
    id = "plot", class = "panel panel-default", fixed = TRUE,
    draggable = TRUE, top = 70, 
    right =0, bottom = "auto",
    width = "35%", height = "auto",
    style = "padding: 20px 20px 20px 20px;
             opacity: 0.90;",
    div(
      style = "text-align: center;",
      fluidRow(
        div(
          style = "display: flex; justify-content: space-between;",
          div(style = "padding: 5px; width: 40%;",
              selectInput('individualsSelector', 'Choose animal', multiple = FALSE, c())
          ),
          div(style = "padding: 5px; width: 40%;",
              selectInput('yearSelector', 'Choose year', multiple = FALSE, c())
          )
        )
      ),
      # fluidRow(
      #   div(
      #     style = "display: flex; justify-content: space-between;",
      #     div(style = "padding: 5px; width: 40%;",
      #         dateInput('beingDate', 'Choose Start Date of Animal',  c())
      #     ),
      #     div(style = "padding: 5px; width: 40%;",
      #         dateInput('endDate', 'Choose End Date of Animal', c())
      #     )
      #   )
      # ),
      fluidRow(
        div(
          class = "btn-group",
          style = "text-align: center;",
          
      #    style = "display: inline-block;",
          actionButton('backwardHandlerButton', 'Previous Animal'),
          actionButton('forwardHandlerButton', 'Next Animal')
        )
      )
    )
    
    
    
 ,
    br(),
    tabsetPanel(
      tabPanel("NSD Plot",
   # plotOutput("speedPlot",height='25vh',click = "plot_click",hover = hoverOpts(id = "plot_hover", delay = 0)),
    #plotOutput("fixRatePlot",height='25vh',click = "plot_click",hover = hoverOpts(id = "plot_hover", delay = 0)),
    plotOutput("nsdPlot",height='25vh',click = "plot_click",hover = hoverOpts(id = "plot_hover", delay = 0))
      ),
    tabPanel("Speed Plot",
      plotOutput("speedPlot",height='25vh',click = "plot_click",hover = hoverOpts(id = "plot_hover", delay = 0))
             
    ),
   tabPanel("Fix Rate Plot",
      plotOutput("fixRatePlot",height='25vh',click = "plot_click",hover = hoverOpts(id = "plot_hover", delay = 0))
      ),
            
    tabPanel("Summary",
             uiOutput(outputId = "info_animals"),
             br(),
             uiOutput("fixRateInfo"),
             uiOutput("pointsPerTimeInfo"),
             br(),
             uiOutput("dateRangeInfo"),
             #actionButton("export",("Download Shapefile")),
             uiOutput('polygonHolder'),
             )),
 br(),
 div(
   style = "text-align: center;",
  actionButton("finishDataCleaning",("Finish Data Cleaning"),style="background-color: #a3cf84;"),
 ))
  
  )))
),
tabPanel(#icon = icon("new-window", lib =  "glyphicon"),
         "Map Viewer",value = "mapviewer",
         div(class = "outer",
             tags$head(
               includeCSS("styles.css")
             ),
         #     
              useWaiter(),
              mapdeckOutput("myMap",width = "100%", height = "100%"),
         #     
             absolutePanel(
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
               ),
               id = "hist_panel", class = "panel panel-default",
               fixed = TRUE, draggable = FALSE,
               top = 80, left = "1%", right = "auto",
               bottom = "auto",
               width = "25%", height = "auto", #430
               h4("Query Movebank Data"),
               tabsetPanel(id = "QueryBuilder",
                           tabPanel("Connect",
                                    selectInput("shapefileDropdown", "Select a Movebank project(s)", choices = NULL,multiple=TRUE),
                                    actionButton("connect",("Connect")),

                           ),

                           tabPanel("Query Builder", id ="Query1",style = "overflow-y:scroll; max-height: 600px",
                                    div(
                                      div(
                                        style="width:85%; display:inline-block; vertical-align: middle;",
                                        br(),
                                        selectInput("selectProject", "Study Name:",
                                                    choices =  ""),##style = "font-size: 11px; padding: 1px;",
                                        selectInput("selectAnimal", "Animal:",
                                                    choices =  "",multiple=TRUE),
                                        selectInput("selectSpecies", "Species:",
                                                    choices =  "",multiple=TRUE),
                                        selectInput("selectRange", "Select Date Range Input Type", choices = c("Month & Year","Custom Date Range"),selected = NULL),
                                        div(
                                          style = "text-align: center;",
                                          conditionalPanel(
                                            condition = "input.selectRange == 'Month & Year'",
                                            fluidRow(
                                              column(
                                                width = 5,
                                                selectInput("selectMonth", "Month:",
                                                            choices = "", multiple = TRUE)
                                              ),
                                              column(
                                                width = 5,
                                                selectInput("selectYear", "Year:",
                                                            choices = "", multiple = TRUE)
                                              )
                                            )
                                          )),

                                        conditionalPanel(
                                          condition = "input.selectRange == 'Custom Date Range'",
                                          dateRangeInput("dateRange", "Select Date Range:",
                                                         start = "2016-08-30",
                                                         end   = Sys.Date())
                                        ),

                                        use_bs_popover(),

                                        selectInput("selectUnit", "Capture Herd Name:",
                                                    choices =  "",multiple=TRUE) %>%
                                          shinyInput_label_embed(
                                            shiny_iconlink() %>%
                                              bs_embed_popover(
                                                content = "Returns animals based on the herd it was captured in", placement = "left"
                                              )
                                          ),
                                        prettySwitch("selectWithin", strong("Select Features Within:"), FALSE),
                                        conditionalPanel(
                                          condition = "input.selectWithin == true",
                                        selectInput("selectLayer", "Select GIS Layer:",
                                                    choices =  "",multiple=FALSE),
                                        selectInput("selectColumn", "Select Column:",
                                                    choices =  "",multiple=FALSE),
                                        selectInput("selectColumnValue", "Select Value:",
                                                    choices =  "",multiple=TRUE)
                                        ),
                                        # selectInput("selectHuntUnit", "Hunt Name:",
                                        #             choices =  "",multiple=TRUE),
                                        # selectInput("selectHerdUnit", "Herd Name:",
                                        #             choices =  "",multiple=TRUE),
                                        actionButton("query",("Query")),
                                        # bsTooltip(id = "query", title = "This is an input",
                                        #           placement = "right", trigger = "hover"),
                                      )
                                    ),
                           ),
                           tabPanel("Results", id = "Results",
                                    br(),
                                    div(style = "height: 300px; overflow-y: scroll;",
                                        h5("Species Summary"),
                                        div(tableOutput('herdsummary'),style = "font-size:85%"),
                                        h5("Animal Summary"),
                                        div(tableOutput('indvsummary'),style = "font-size:85%"),

                                    ),
                                    prettySwitch("color","Trackline Color", FALSE),
                                   # prettySwitch("pointLayer","Point Layer", FALSE),
                                    prettySwitch("heatMap","Heat Map", FALSE),
                            
                                   # actionButton("color", label = "Change Trackline Color", icon = icon("paintbrush")),
                                    br(),
                                   # actionButton("heatMap",("Heat Map")),
                                    br(),
                                    actionButton("exportQuery",("Download Query Results")),
                                    actionButton("exportCalcRange","Calculate Home Range"),
                                   
                                    
                           ))
             ),
         
             absolutePanel(
               id = "controls", class = "panel panel-default", fixed = TRUE,
               draggable = TRUE, top = 70, left = "auto",
               right = "5%", bottom = "auto",
               width = 0, height = 0,
               dropdownButton(
                 label = "",
                 icon = icon("map"),
                 right = TRUE,
                 status = "primary",
                 circle = TRUE,
                 width = 250, 
                 bs_accordion(id = "GIS_Layer_List") %>%
                   bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
                   bs_append(title = "Mule Deer Layers", content = list(
                     prettySwitch("MuleDeerCrucialRange", "Crucial Range", FALSE),
                     prettySwitch("MuleDeerSeasonalRange", "Seasonal Range", FALSE),
                     prettySwitch("MuleDeerHerdUnits", "Herd Units", FALSE),
                     prettySwitch("DeerHuntAreas", "Hunt Areas", FALSE)
                   )) %>%
                  # bs_set_opts(panel_type = "info") %>%
                   bs_append(title = "Pronghorn Layers", content = list(
                     prettySwitch("AntelopeHerdUnits", "Herd Units", FALSE),
                     prettySwitch("AntelopeHuntAreas", "Hunt Areas", FALSE)
                   )) %>%
                   bs_append(title = "Bison Layers", content = list(
                     prettySwitch("BisonHerdUnits","Herd Units", FALSE),
                     prettySwitch("BisonHuntAreas","Hunt Areas", FALSE)
                   )) %>%
                   #bs_set_opts(panel_type = "info") %>%
                   bs_append(title = "Elk Layers", content = list(
                     prettySwitch("ElkHerdUnits","Herd Units", FALSE),
                     prettySwitch("ElkHuntAreas","Hunt Areas", FALSE)
                   )) %>%
                   #bs_set_opts(panel_type = "info") %>%
                   bs_append(title = "Moose Layers", content = list(
                     prettySwitch("MooseHerdUnits","Herd Units", FALSE),
                     prettySwitch("MooseHuntAreas","Hunt Areas", FALSE)
                   )) %>%
                 #bs_set_opts(panel_type = "info") %>%
                   bs_append(title = "BighornSheep Layers", content = list(
                     prettySwitch("BighornSheepHerdUnits","Herd Units", FALSE),
                     prettySwitch("BighornSheepHuntAreas","Hunt Areas", FALSE)
                   )) %>%
                   bs_append(title = "Boundaries", content = list(
                     prettySwitch("BioDistricts","WGFD Biologist Districts", FALSE),
                     prettySwitch("AdminRegions","WGFD Regions", FALSE)
                     
                   ))
                   )
               
             ))
         )
         

)

server <- function(input, output, session) {
  
  observeEvent(input$cleaningButton,{
    updateTabsetPanel(session, "navibar",selected = "app1")
  })
  observeEvent(input$mapButton,{
    updateTabsetPanel(session, "navibar",selected = "mapviewer")
  })
  app1_initialized <- reactiveVal(FALSE)
  
  observeEvent(input$navibar, {
    if (input$navibar == "app1" && !app1_initialized()) {
      app1_init(input, output, session)
      
      elevation<<- raster("/vsicurl/https://pathfinder.arcc.uwyo.edu/devise/MerkleLabGIS/Topo/etopocompressed.tif")
      
      checkForSession('app1')
      hide(id = 'dateTimeRow')
      onStop(function() {
        stopApp()
      })
      
      # Set app1_initialized to TRUE after initialization
      app1_initialized(TRUE)
    }
  })
  
  observeEvent(input$navibar, {
    if (input$navibar == "mapviewer") {
      app2_init(input, output, session)
    }
  })
}

appOneReload <- function(filePath){

  rdsLocation<-paste0(filePath,'//workingFile.rds')
  print(rdsLocation)
  if(file.exists(rdsLocation)){
    #loadingScreenToggle('show','loading existing project file')
    workingFile<<-readRDS(rdsLocation)
    importedDatasetMaster<<-workingFile$importedDatasetMaster
    workingFile$masterWorkingDirectory<<-filePath
    masterWorkingDirectory<<-filePath
    loadConfig()
    dbConnection <<- dbConnect(RSQLite::SQLite(), paste0(masterWorkingDirectory,'//workingDb.db'))
    updateMasterTableFromDatabase()
    removeModal()
    print(!'newMasterDate'%in%names(importedDatasetMaster@data))
    hideElement(id = 'importDataRow', anim = TRUE)
    showElement(id = 'importedDataMapRow', anim = TRUE)
    hide('loadProjectButton')
    showElement('exportDataButton')
    mapInit()
    #loadingScreenToggle('hide','')
    saveWorkingFile();
    #writeOGR(importedDatasetMaster,"Movebank//" ,".", layer="test5", driver="ESRI Shapefile")
    
  }else{
    modalMessager('Error',paste0('Data file from this session does not exist at ',filePath,'. Please try loading the data file manually using the "Reload Existing Project Folder" button.'))
    sessionCheckLocation<-paste0(dirname(getwd()),'//session.rds')
    file.remove(sessionCheckLocation)
  }

}

loadConfig<-function(){
  configOptions<<-readRDS(paste0(masterWorkingDirectory,'//configOptions.rds'))
  configOptions$masterWorkingDirectory<<-masterWorkingDirectory
  updateNumericInput(session, 'maxSpeedSelector', value= configOptions$maxSpeedParameter )
  updateNumericInput(session, 'mortDistance', value= configOptions$mortDistance)
  updateNumericInput(session, 'mortTime', value=configOptions$mortTime)
}


shiny::devmode(TRUE)
shinyApp(ui, server)
