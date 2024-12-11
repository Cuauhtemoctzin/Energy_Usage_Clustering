
# Dashboard for energy buildings comparison
# Guillermo C. Granados-Garcia
# Lancaster University
# Department of school of Mathematical Sciences
# Project: Reducing End Use Energy Demand in Commercial Settings Through 
# Digital Innovation
# EP/T025964/1

library(shiny)
library(bslib)
library(shinyBS)
library(shinyjs)
library(markdown)
source("anomalyscorebuilding.R")



# Define UI for application that compares buildings energy use
ui <- page_sidebar(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
.tooltip-container {
  display: inline-flex;
  align-items: center;
}

.tooltip-container .tooltip-icon {
  margin-left: 5px;
  cursor: pointer;
  color: #007bff; /* Blue info icon */
  font-size: 14px;
}

.tooltip-container .tooltip-text {
  display: none;
  position: absolute;
  background-color: #000;
  color: #fff;
  padding: 10px;
  left: 10px;
  border-radius: 5px;
  font-size: 12px;
  white-space: nowrap;
  z-index: 999999;
}

.tooltip-container:hover .tooltip-text {
  display: block;
}

.tooltip-container .tooltip-text::after {
  content: '';
  position: absolute;
  top: 1%; /* Below the tooltip */
  left: 50%;
  margin-left: -5px;
  border-width: 5px;
  border-style: solid;
  border-color: #000 transparent transparent transparent;
  z-index: 999999;
}
.form-group {
      margin-bottom: 0px; /* Adjust this value to reduce spacing */
    }
.tooltip-label {
      margin-bottom: 0;
  }
"))
  ),
  
  div(
    id = "welcome-message",
    class = "alert alert-info",
    style = "text-align: center; font-size: 16px; margin-top: 20px;",
    HTML("<strong>Welcome!</strong> To get started, click the <strong>Run Query</strong> button."),
    actionButton("dismiss_message", "Got it", class = "btn btn-primary", style = "margin-top: 10px;")
  ),  
  
  
  # Application title
  titlePanel(
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      h1("Energy Usage Clustering"),
      img(src = "LUlogo.png", height = "60px")  # Adjust the image path and size
    ),
    windowTitle = "Energy Usage Clustering"
  ),
  
  # Sidebar layout with input and output definitions ----
  
  # Sidebar panel for inputs ----
  sidebar= sidebar(
    tags$style(type='text/css', ".checkbox {font-size: 12px !important} "),
    tags$style("#num {font-size:15px;}"),
    
    #Input: download button
    downloadButton("downloadData", "Download", style="color: #fff; background-color: green; border-color: Black;"),
    div(
      class = "form-group",
    div(
      class = "tooltip-container",
      tags$label("Source supply:"),
      span(
        class = "tooltip-icon",
        HTML("&#9432;"), # Info icon
        span(class = "tooltip-text", "Select the meter type.")
      )
    ),
    # Input: drop down selection of the meter type
    selectInput(inputId ="variable", label = NULL,
                list(
                  "Water" ="Water",
                  "Electricity"="Electricity",
                  "Gas"="Gas"
                )
                , selected = "Electricity" )
    
    ),
    
    
    
    div(
      class = "form-group",
    div(
      class = "tooltip-container",
      tags$label("Date Range:"),
      tags$span(
        class = "tooltip-icon",
        HTML("&#9432;"),
        span(class = "tooltip-text", "Pick a start and end date for your data.")
      ),
    ),
    dateRangeInput(inputId = "date", label = NULL,
                   start = "2023-09-01",
                   end   = "2023-09-07",
                   min = "2023-01-01",
                   max = "2023-12-31" )
    ),
    

    
    
    #actionButton("run_query", "Run Query"),
    div(
      class = "tooltip-container",
      actionButton("run_query", "Run Query"),
      span(
        class = "tooltip-icon",
        HTML("&#9432;"),
        span(class = "tooltip-text", "Click to load and process data.")
      )
    ),
    
    div(
      class = "form-group",
    div(
      class = "tooltip-container",
      tags$label("Standardization:"),
      span(
        class = "tooltip-icon",
        HTML("&#9432;"),
        span(class = "tooltip-text", HTML("Choose if you want to <br>standardize the data."))
      )
    ),
    
    checkboxGroupInput(inputId ="standarization", label = NULL, 
                       choices = list('Standard deviation scaling' = 1,
                                      'Mean subtraction' = 2
                       ),selected = c(1))
    ),
    
    div(
      class = "form-group",
    div(
      class = "tooltip-container",
      tags$label("Distance:"),
      span(
        class = "tooltip-icon",
        HTML("&#9432;"),
        span(class = "tooltip-text", HTML("Select the distance to <br>calculate the anomaly scores.") )
      )
    ),
    
    #Input selection of distance
    selectInput("distance", label = NULL,
                list('Cort' = 'Cort',
                     'Wasserstein' = 'Wasserstein',
                     'Mahalanobis' = 'Mahalanobis',
                     'CortNorm'='CortNorm',
                     'Coherence'='Coherence',
                     'PDC'='PDC',
                     'CGCI'='CGCI',
                     'RGPDC'='RGPDC',
                     'PMIME'='PMIME',
                     'mvLWS'='mvLWS',
                     'Band depth'='Band depth'
                )
                , selected = 'Cort' )
    ),
    
    
    div(
      class = "form-group",
      div(
        class = "tooltip-container",
        tags$label("Tree Depth:"),
        span(
          class = "tooltip-icon",
          HTML("&#9432;"),
          span(class = "tooltip-text", HTML("Adjust the depth of the decision tree.") )
        )
      ),
    #Input: Depth of the tree
    numericInput( 
      inputId = "depth", 
      label=NULL, 
      value = 4, 
      min = 1, 
      max = 100 
    ) 
    ),    
    
    #Input: Node selection to highlight the analysis
    # numericInput( 
    #   inputId = "node", 
    #   "Risk level:", 
    #   value = 0, 
    #   min = 0, 
    #   max = 1000 
    # ),

    div(
      class = "form-group",
      div(
        class = "tooltip-container",
        tags$label("Risk Level (RL):"),
        span(
          class = "tooltip-icon",
          HTML("&#9432;"),
          span(class = "tooltip-text", HTML("Filter results by risk level. Higher <br>risk means higher anomaly score") )
        )
      ),
    #Input selection of distance
    selectInput("node", label=NULL,
                list('All' = 0
                )
                , selected = NULL )
    )
    
    
  ),
  
  navset_card_underline( 
    # Main panel for displaying outputs ----
    
    nav_panel("Analysis", 
              
              layout_columns(
                
                card( # left side panel with variable selection
                  checkboxGroupInput("checkGroup", label = h6("Classification Factors"),
                                     choices = list('Floor area' = 5,
                                                    'Usage' = 6,
                                                    'Year built' = 7,
                                                    'Zone'=8
                                                  #  ,'Weekend'=11
                                     ),
                                     selected = c(5,6,7,8) )
                ),
                
                layout_columns(
                  card( plotOutput("scoreplot",  width = "100%") ), # regression tree
                ),col_widths = c(3, 9)
              ),
              card( plotOutput("allplot") ,  width = "100%")
    ),
    
    nav_panel("Parameters", 
              style = "font-size:15px;",
              
              layout_columns(
                card(card_header("All Distances"), numericInput("num", "Number of nearest neighbors", value = 4 )),
                card(card_header("Cort"),numericInput("cortK", "k", value = 3)),
                card(card_header("Normalized cort"),numericInput("cortnormK", "k", value = 3)),
                card(card_header("CGCI"),numericInput("cgcipmax", "Maximum lag", value = 10))
              ),
              
              layout_columns(
                card(card_header("RGPDC"),numericInput("rgpdcpmax", "Maximum lag", value = 10),numericInput("rgpdcperiod", "Period", value = 5)),
                card(card_header("PMIME"),numericInput("pmimepmax", "Maximum lag", value = 10),numericInput("pmimeahead", "Steps ahead", value = 1),numericInput("pmimennei", "# Density neighbors", value = 10),numericInput("pmimethres", "Threshold", value = .95) ),
                card(card_header("Coherence"),numericInput("coherencespan1", "Span 1", value = 3),numericInput("coherencespan2", "Span 2", value = 3),numericInput("coherenceperiod", "Period", value = 5) ),
                card(card_header("PDC"), textInput('pdclags', 'Autoregressive lags (comma delimited)', "1,2"),numericInput("pdcperiod", "Period", value = 5)  )
              )
              
    ),
    
    nav_panel("Instructions", includeMarkdown("Instructions.md") )
    
  )
  
)

# Define server logic
server <- function(input, output,session) {
  observeEvent(input$dismiss_message, {
    # Hide the welcome message
    removeUI(selector = "#welcome-message")
  })
  
  observe({
    bsPopover("date", title = NULL, content = "Choose start and end dates.", 
              placement = "right", trigger = "hover", options = list(container = "body"))
  })
  ### dates observation
  
  # Use debounce to delay validation until the user finishes selecting dates
  debounced_dates <- reactive({
    input$date
  }) %>% debounce(500)  # Delay validation by 500 milliseconds
  
  # Observe changes to the debounced date range
  observe({
    start_date <- debounced_dates()[1]
    end_date <- debounced_dates()[2]
    
    # Only validate when both start and end dates are selected
    if (!is.null(start_date) && !is.null(end_date) && start_date > end_date) {
      showModal(modalDialog(
        title = "Invalid Date Range",
        "Start date cannot be after the end date. Please select valid dates.",
        easyClose = TRUE,
        footer = NULL
      ))
      
      # Reset date range to avoid crashing the app
      updateDateRangeInput(session, "date", start = as.Date(Sys.Date()) - 6, end = as.Date(Sys.Date())   )
    }
    
  })
  
  # update the meter type  selectInput
  varupdate<-eventReactive(input$run_query,{updateSelectInput(session, "variable") })
  
  
  
  mydata <- eventReactive({
    input$run_query
    }
                          ,{
    
    start_date <- as.Date( input$date[[1]] )
    end_date <- as.Date( input$date[[2]] )
    varinuse<-input$variable
    # Ensure valid date range before executing the query
    req(start_date)
    req(end_date)
    req(start_date <= end_date)  # Validate that start is before end
    
    
    
    tryCatch(
      {
        df<-energyframe(metype=as.character(varinuse) , from=as.character(start_date), to=as.character(end_date),metatable=metatable  )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(df)
  })
  
  ## given the index of the selected distance to use to calculate the scores 
  indexdist <- reactive(  {
    ii= which(dist_names==input$distance  )
    return(ii)
  })
  
  

  
  
  ## organize theinput parameters from the parameters tab 
  dparams <- reactive({
    par=list(
      list(unit=mydata(), k=input$cortK),
      list(unit=mydata() ),
      list(unit=mydata() ),
      list(unit=mydata(), k=input$cortnormK),
      list(unit=mydata(), span1=input$coherencespan1, span2=input$coherencespan2, period = input$coherenceperiod),
      list(unit=mydata(), ar=as.numeric(unlist(strsplit(input$pdclags,","))), period = input$pdcperiod ),
      list(unit=mydata() , pmax=input$cgcipmax ),
      list(unit=mydata() , pmax=input$rgpdcpmax, period=input$rgpdcperiod),
      list(unit=mydata(), Lmax=input$pmimepmax, Tl=input$pmimeahead, nnei=input$pmimennei, A=input$pmimethres ),
      list(unit=mydata()  ),
      list(unit=mydata() )
    )
    return(  par[[as.numeric( indexdist() ) ]] )
  })
  
  # anomaly scores calculation 
  scores <- eventReactive({
    input$num
    input$standarization
    input$run_query
    input$distance 
    },{
    start_date <- as.Date( input$date[[1]] )
    end_date <- as.Date( input$date[[2]] )
    stand<- input$standarization
    # Ensure valid date range before executing the query
    req(start_date)
    req(end_date)
    req(start_date <= end_date)  # Validate that start is before end
    req(mydata())
    mynum<-as.numeric(input$num)
    req(indexdist())
    return(  Anomalyscore_periods_LU(full_series=mydata(), startday=start_date,endday=end_date, knn=mynum,distance=functions[[as.numeric( indexdist() ) ]],dparams=dparams(), standvec=stand )  )
  })
  
  modeltree <- eventReactive({
    input$depth
    input$checkGroup
    input$standarization
    input$run_query
    input$distance 
    }, {
      req(scores() )
      myscores<-scores()
      
    return(target_tree(datascores =myscores, variables=c(2,as.numeric(input$checkGroup)), targetdepth=as.numeric(input$depth), maxiter=500 ))
  })
  

  
  #in case of having an update on the regression tree the highliting of nodes is limited to the number of leafs of the new tree
  observeEvent( modeltree(),{
    x<-modeltree()
    req(scores() )
    myscores<-scores()
    wherenodes<- unique(  x$where )
    maxnode= length( wherenodes )
    labr= risklevels(x,myscores)
    
    updateSelectInput(session, inputId = "node", choices = labr)
    
    
  })
  
  ## output right upper plot with the regression tree
  output$scoreplot <- renderPlot({
    plottreenodes(mod=modeltree(), nodeindex=as.numeric(input$node) , distaname=dist_names[as.numeric( indexdist() ) ]   )
  })
  ## output with the time series plot at the bottom 
  
  plot_data <- eventReactive({
    input$node
    input$standarization
    input$run_query
    input$distance 
    }, {
    # Extract the start and end dates from input
    start_date <-as.Date( input$date[[1]] )
    end_date <- as.Date( input$date[[2]])
    
    # Ensure that the dates are valid
    req(start_date, end_date)
    req(start_date <= end_date)
    
    # Return the plot, passing data() and date range
   return( highlightplot(optimal_tree=modeltree()  ,unit_train=scores(),fulldata= mydata() ,node=as.numeric(input$node) ,datestart= as.character( start_date), dateend=as.character( end_date) ,standvec=input$standarization) )
  })
  
  output$allplot<-renderPlot({
    
    plot_data()
    
    
    #plotseries(unit=mydata(),frame_scores=scores(),measures=as.numeric(input$checkGroup),colorblind=input$cblind)
  }, height = 350)
  
  # Downloadable csv with the scores and covariates
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("LUframescores", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(scores(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
