library(shiny)
library(DT)

source("helper.R")
source("global.R")

shinyServer <- function(input, output, session) {
  
  g_filters <<- getPolygonFilters()
  g_all_filters <<- sort(unique(g_filters$FILTER))
  
  output$districtSelector <- renderUI({
    special_states <- c("Kerala", "Karnataka", "Tamil Nadu", "Maharashtra")
    
    if (input$state %in% special_states) {
      valid_districts <- g_districts$COUNTY[g_districts$STATE == input$state]
      selectInput("district", "Select District:", choices = sort(na.omit(valid_districts)))
    } else {
      NULL 
    }
  })
  
  current_district <- reactive({
    if (is.null(input$district)) "None" else input$district
  })
  
  observe({
    state <- input$state
    district <- current_district()
    special_states <- c("Kerala", "Karnataka", "Tamil Nadu", "Maharashtra")
    
    # 1. If no state is selected, reset to None
    if (is.null(state) || state == "None") {
      updateSelectInput(session, "filterRegion", choices = c("None"))
      return()
    }
    
    # 2. If it is a "Big 4" State, filter by BOTH State and District
    if (state %in% special_states) {
      if (district == "None" || district == "") {
        updateSelectInput(session, "filterRegion", choices = c("None"))
      } else {
        
        # Strip hidden spaces from the sheet data and match using g_filters
        sheet_states <- trimws(as.character(g_filters$STATE))
        sheet_districts <- trimws(as.character(g_filters$COUNTY))
        
        # Subset the filters based on exact matches
        filters <- unique(g_filters$FILTER[sheet_states == state & sheet_districts == district])
        
        updateSelectInput(session, "filterRegion", choices = c("None", sort(na.omit(filters))))
      }
      
      # 3. For normal states, just filter by State as usual
    } else {
      sheet_states <- trimws(as.character(g_filters$STATE))
      filters <- unique(g_filters$FILTER[sheet_states == state])
      updateSelectInput(session, "filterRegion", choices = c("None", sort(na.omit(filters))))
    }
  })
  
  output$minutes <- renderText( {
    
    if (input$state %in% c("Kerala", "Karnataka", "Tamil Nadu", "Maharashtra")) {
      req(current_district() != "None" && current_district() != "")
    }
    
    paste ("List duration <= ",
           getMinutes (input$duration, input$state, current_district(), input$filterRegion), 
           " minutes &  percentile of counts at ", input$countPercentile, 
           sep='')
  })

output$downloadData <- downloadHandler(
  filename = function() { paste('Filter_',
                                Sys.Date(),'_',
                                '.csv', sep='') },
  content = function(file) {
    
    if (input$state %in% c("Kerala", "Karnataka", "Tamil Nadu", "Maharashtra")) {
      req(current_district() != "None" && current_district() != "")
    }
    
    currentfilter<-generateFilter (state = input$state,
                                   district = current_district(),
                                   filterRegion =  input$filterRegion,
                                   fortnightly = (input$fortnight=='Fortnight'), 
                                   duration = getMinutes (input$duration, input$state, current_district(), input$filterRegion),
                                   filterPercentile = input$countPercentile,
                                   makeXAs1 = input$Xas1,
                                   dataView = as.numeric(input$alldata))
    write.csv(currentfilter, file)
  }  
)

output$filter <- renderDT({
  if (input$state %in% c("Kerala", "Karnataka", "Tamil Nadu", "Maharashtra")) {
    req(current_district() != "None" && current_district() != "")
  }
  
  currentfilter <- generateFilter(state = input$state,
                                  district = current_district(),
                                  filterRegion =  input$filterRegion,
                                  fortnightly = (input$fortnight=='Fortnight'), 
                                  duration = getMinutes (input$duration, input$state, current_district(), input$filterRegion),
                                  filterPercentile = input$countPercentile,
                                  makeXAs1 = input$Xas1,
                                  dataView = as.numeric(input$alldata))
  
  if (nrow(currentfilter) > 0) {
    if (currentfilter$Species[1] == "Number of Complete Lists") {
      custom_index <- c("", seq_len(nrow(currentfilter) - 1))
    } else {
      custom_index <- seq_len(nrow(currentfilter))
    }
    display_filter <- cbind("Number " = custom_index, currentfilter)
  } else {
    display_filter <- currentfilter
  }
  
  DT::datatable(display_filter, 
                rownames = FALSE, # Turn off default row numbers
                options = list(
                  lengthMenu = list(c(20, 50, 100, 300, 500, -1), c('20', '50', '100', '300', '500', 'All')),
                  pageLength = 100
                )) |>
    DT::formatStyle(
      columns = 1:ncol(display_filter), 
      valueColumns = 'Species',
      target = 'row',
      fontStyle = DT::styleEqual('Number of Complete Lists', 'italic'),
      color = DT::styleEqual('Number of Complete Lists', '#555555') 
    )
})

}
shinyUI <- fluidPage(
  titlePanel('Filter Generator'),
  fluidRow(
    column(12,
           p("Uses eBird data to generate a fortnightly/monthly eBird filter automatically (Sensistive species data excluded)"),
           p("Created and maintained by Praveen J & Alen Alex, Bird Count India",
             a("(@Praveen J)", href = "Email:paintedstork@gmail.com")),
           p("An interactive visualization of Indian eBird Filters and Polygons can be accessed by clicking",a("here.", href = "https://birdcountindia.github.io/eBird-filter-generator-V2/")),
           p("Last Date of Update. Data: 31 March 2026. Code: 30 April 2026. Filter Configuration: Dynamic - Managed by Bird Count India"))
  ), 
  
  
  sidebarPanel(
    width = 3,  
    selectInput("state", "Select State:", choices = c("None", sort(unique(g_states$STATE)))),
    
    uiOutput("districtSelector"),
    
    selectInput("filterRegion", "Select Filter Region:", choices = c("None")),
                            
    selectInput('fortnight', 'Period', c('Month', 'Fortnight')),

    selectInput('alldata', 'Display Data', c("Counts Only"=1, "Lists Only"=2, "Both"=3)),

    sliderInput('duration', "List Duration Percentile", min=1, max=100,
                value=90, step=1, round=0),
    
    sliderInput('countPercentile', 'Count Percentile', min=1, max=100,
                value=90, step=1, round=0),
    
    checkboxInput('Xas1', 'Consider X as 1'),
    
    helpText('These filter suggestions are created at monthly/fortnightly scale using the aggregated eBird data from a selected region.
              All lists are sorted on duration and lists below a certain duration are only considered,
              as typical lists. Long lists may have bigger counts but are atypical.
              All counts for each species is sorted and filter will be set at a percentile to catch only
              counts above that value. This is a preference of the filter editor, as lower value would mean
              longer review queues. In data poor areas, we should consider counts with X as 1, else we will
              get lot of zeros, when the species is actually present. No of complete lists where it was reported
              can be optionally shown in brackets based on user selection')
    ),

  mainPanel(
    headerPanel(textOutput ("minutes")),
    tabsetPanel(
        tabPanel("Filters", dataTableOutput('filter')),
        tabPanel("About", 
                 br(), h1("About Filter Generator"), 
                 br(), p("eBird Central implemented filters that support custom boundaries."), 
                 br(), p("This program is useful in creating/customising filters using existing eBird data which typically filter editors use past experience."), 
                 br(), p("For more information on Filters, check out these articles:"), 
                 br(), a("Understanding the eBird review and data quality process", href = "https://support.ebird.org/en/support/solutions/articles/48000795278-the-ebird-data-quality-and-review-process"), 
                 br(), br(), a("Understanding eBird Filters", href = "https://teamebirdmichigan.wordpress.com/2014/04/04/understanding-the-ebird-filters//"),
                 br(), br(), a("eBird Data Quality and Review Process", href = "http://www.birdcount.in/ebird-data-quality-review/") 
        )
      ),
    downloadButton('downloadData', 'Download')
  )
)

shinyApp(ui = shinyUI, server = shinyServer)