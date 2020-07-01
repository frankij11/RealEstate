#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("scripts/sdat.R")
source("scripts/predict.R")
library(shiny)
#library(leaflet)
library(DT)
library(rhandsontable)


# Define UI for application that draws a histogram
ui <- navbarPage("Maryland Real Estate Predictions", id="nav",
                 
                 tabPanel("Predictions",
                          div(class="outer",
                              div( h1("Search"), shiny::textInput("txt_address","Search Address",""),
                                shiny::actionButton("btn_search", "Search Address"),
                                actionButton("btn_upload", "Upload Data"),
                                actionButton("btn_save", "Save Data")),
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              #leafletOutput("map", width="100%", height="100%"),
                              #DT::dataTableOutput("dt_inputs"),
                              div(h1("Property Meta Data"),
                              rHandsontableOutput("dt_inputs")),
                              div(h1("Property Estimate"),
                              rHandsontableOutput("dt_predictions")),
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              tags$div(id="cite",
                                       'Data compiled for ', tags$em('OS4106: Advanced Data Analysis')
                              )
                          )
                 ),
                 
                 tabPanel("Data explorer",
                          fluidRow(
                              column(3,
                                     selectInput("sel_address", "Choose Address", c(), multiple=TRUE)
                              ),
                              column(3,
                                     conditionalPanel("input.states",
                                                      selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                                     )
                              ),
                              column(3,
                                     conditionalPanel("input.states",
                                                      selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                                     )
                              )
                          ),
                          fluidRow(
                              column(1,
                                     numericInput("minScore", "Min score", min=0, max=100, value=0)
                              ),
                              column(1,
                                     numericInput("maxScore", "Max score", min=0, max=100, value=100)
                              )
                          ),
                          hr(),
                          DT::dataTableOutput("dt_comps")
                 ),
                 
                 conditionalPanel("false", icon("crosshair"))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  inp <- sdat_get_meta(data.frame(address=c("1406 Iron Forge Rd","1303 Alberta Dr", "12615 Perrywood Ln"))) %>% 
      sdat_add_features() %>%
      sdat_prepare_predict() %>%
      mutate(flip=T,isCompany=F,isQualityTop5 = F, Nick.Name="NA", List.Price=0)

  values = reactiveValues(prop_meta=inp, prop_comps=NULL)
  

  # Finds new property
  observeEvent(input$btn_search, {
    try({
      inp <- sdat_get_meta(data.frame(address=input$txt_address)) %>% 
        sdat_add_features() %>%
        sdat_prepare_predict() %>%
        mutate(flip=T,isCompany=F,isQualityTop5 = F,List.Price=0, Nick.Name=NA, List.Price=0) 
      
      inp_comps <- lapply(1:nrow(inp),function(i) {
                          df <- sdat_query(where=where_comps(lat=inp$lat[[i]], lon =inp$lon[[i]], miles = 2)) %>%
                            mutate(COMP_ADDRESS = inp$address[[i]]) %>%
                            sdat_add_features()
                          df %>% mutate(
                            Distance = as.vector(distm(x=df[,c("lon", "lat")] %>% as.matrix(), y = c(inp$lon[[i]], inp$lat[[i]])) ) * 0.000621371
                            )
                          df
                            }
                        ) %>%
                        bind_rows()
      
      values$prop_meta <- rbind(hot_to_r(input$dt_inputs),inp) %>% distinct()
      
      values$prop_comps <- rbind(values$prop_comps, inp_comps) %>% distinct()
      
      # Update data explorer page
      updateSelectInput(session, "sel_address", label = "Choose Address", choices = values$prop_meta$address,
                        selected = input$sel_address)
      
    })
  })
  
  observeEvent(input$btn_upload, {
   print("Upload Data Routine") 
  })
  
  observeEvent(input$btn_save, {
    print("Save Data Routine") 
  })
  
  
  preds <- reactive({
    df<- sdat_predict(hot_to_r(input$dt_inputs), "address","List.Price", get_meta=F) %>% 
      mutate(Estimate.RF=round(Estimate.RF/1000,1),
             Estimate.ENET=round(Estimate.ENET/1000,1),
             Low.Est = round(price.05.est/1000,1),
             Median.Est = round(price.50.est/1000,1),
             High.Est = round(price.95.est/1000,1)) %>%
      select(county, address,Nick.Name,List.Price,Estimate.RF, Estimate.ENET,Low.Est, Median.Est, High.Est)
    df
  })
  
  output$dt_inputs <- renderRHandsontable({
    DF= NULL
    inp <- values$prop_meta
    inp <- inp %>% select(county,land_use,address,Nick.Name,List.Price, sqft,dplyr::starts_with("qual"),dplyr::starts_with("is"),-dplyr::starts_with("isMat"), everything())
    if (is.null(input$hot)) {
      DF = inp
    }
    
    rhandsontable(DF) %>%
      # hot_table(readOnly = TRUE) # ok
      hot_col(col = c(1,2), readOnly = TRUE) 
  })
  
  output$dt_predictions <- renderRHandsontable({
    #preds <- values$preds
    preds <- preds()

    rhandsontable(preds %>% select(county,address,List.Price,everything())) %>%
      hot_table(readOnly=T)
    })

  output$dt_comps <- DT::renderDataTable({
    #preds <- values$preds
    df <- values$prop_comps
    if(!is.null(input$sel_address)){
      df <- df %>% filter(COMP_ADDRESS %in% input$sel_address)
      }
    
    df %>% dplyr::select(COMP_ADDRESS,county,address,price, sqft,age,acre,everything())
  })
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)