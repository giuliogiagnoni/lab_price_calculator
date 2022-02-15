#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(autheme)
library(readxl)
library(dplyr)
library(writexl)
library(plotly)
library(DT)
library(gtools)
library(quantmod)

ui <- navbarPage("Navigation Bar",
                 tabPanel("Samples",
                          sidebarLayout(
                              sidebarPanel(uiOutput("Analyses_input"),
                                           
                                           br(),br(),br(),br(),
                                           div("Want to use your own price list?", style = "text-align:center"),
                                           br(),
                                           fileInput('newprices', 'Choose xlsx file',
                                                     accept = c(".xlsx")),
                                           actionButton('resetnewprices', 'Reset Input'),
                                           br(),br(),
                                           div("Want to use a predifined set?", style = "text-align:center"),
                                           br(),br(),
                                           fileInput('preset', 'Choose xlsx file',
                                                     accept = c(".xlsx")),
                                           actionButton('resetpreset', 'Reset Input'),
                                           br(),br(),
                                           downloadButton("downloadvalues", "Download the actual set")
                              ),
                              
                              mainPanel(
                                  lapply(1:7, function(x)
                                      fluidRow(id = paste0("fr",x),
                                               column(width = 3, uiOutput(paste0("item",x,".1"))), 
                                               column(width = 2, uiOutput(paste0("item",x,".2"))),
                                               column(width = 2, uiOutput(paste0("item",x,".3"))),
                                               column(width = 5, uiOutput(paste0("item",x,".4"))))
                                  )
                                )
                              )
                          ),
                 
                 tabPanel("Costs",
                          sidebarLayout(
                              sidebarPanel(div("Select column for prices", style = "text-align:center"),
                                           br(),br(),
                                           uiOutput("Price_input"),
                                           br(),
                                           fluidRow(
                                           column(width = 4, uiOutput("Price_currency")),
                                           column(width = 4, uiOutput("Convert_to")),
                                           column(width = 4, uiOutput("conversion_rate"))
                                           ),
                                           br(),br(),br(),
                                           downloadButton('downloadtable',"Download table with individual analyses [right] (excel)"),
                                           br(),br(),
                                           downloadButton('downloadtablesummary',"Download table with totals [below] (excel)"),
                                           br(),br(),
                                           DTOutput("tablesummary")
                              ),
                              
                              mainPanel(
                                  DTOutput("tablefinal")
                              )
                          )
                 ),
                 tabPanel("Graph",
                          sidebarLayout(
                              sidebarPanel(),
                              mainPanel( plotlyOutput("Analyses_plot"))
                          )
                 )
)                 


server <- function(input, output, session) {
    
    output$nsets <- reactive({ as.numeric(input$nsets) })
    
    
    values1 <- reactiveValues(
        upload_state = NULL
    )
    
    observeEvent(input$newprices, {
        values1$upload_state <- 'uploaded'
    })
    
    observeEvent(input$resetnewprices, {
        values1$upload_state <- 'reset'
    })
    
    dataset <- reactive({
        if (is.null(values1$upload_state)) {
            readxl::read_xlsx("analyticalfeed.xlsx")
        } else if (values1$upload_state == 'uploaded') {
            readxl::read_xlsx(input$newprices$datapath, sheet = 1)
        } else if (values1$upload_state == 'reset') {
            readxl::read_xlsx("analyticalfeed.xlsx")
        }
    })
    
    valuesnul <- reactive({ 
        lapply(1:7, function(x) list(Set = NA, Items = 0, Periods = 0, Analyses = NA))
        })
    
    valuesinput <- reactive({ 
        d <- readxl::read_xlsx(input$preset$datapath, sheet = 1)
        d <- split(d, d[,1])
        d <- lapply(d, as.list)
        lapply(1:length(d), function(x) lapply(1:4, function(y) d[[x]][[y]] <- unique(d[[x]][[y]])))
    })
    
    values2 <- reactiveValues(
        upload_state = NULL
    )
    
    observeEvent(input$preset, {
        values2$upload_state <- 'uploaded'
    })
    
    observeEvent(input$resetpreset, {
        values2$upload_state <- 'reset'
    })
    
    val <- reactive({
        if (is.null(values2$upload_state)) {
            valuesnul()
        } else if (values2$upload_state == 'uploaded') {
            valuesinput() 
        } else if (values2$upload_state == 'reset') {
            valuesnul()
        }
    })
    

       valupdate <- reactive({
           bind_rows(
           lapply(1:7, function(x) data.frame( Type = rep(input[[paste0("item",x,".1")]], length(input[[paste0("item",x,".4")]])),
                                               Units = rep(input[[paste0("item",x,".2")]], length(input[[paste0("item",x,".4")]])),
                                               Periods = rep(input[[paste0("item",x,".3")]], length(input[[paste0("item",x,".4")]])),
                                               Analysis = input[[paste0("item",x,".4")]])
           )
           )
          })
       
       output$downloadvalues <- downloadHandler(
           filename = function(){"Analyses_cost_setup.xlsx"}, 
           content = function(fname){ writexl::write_xlsx(valupdate(), fname) })
       
   output$nsets <- renderUI({  numericInput("nsets", "Select number of analyses sets (not working yet)", 
                                                 value = 3, min = 1,max=7) })
   


   

   output$Analyses_input <- renderUI(varSelectInput("Analyses_input", label = "Select column for analyses:", dataset()))
    

    
    sapply(1:7, function(x) 
        output[[paste0('item', x, ".1")]] <- renderUI({
            textInput(paste0('item', x, ".1"), label = "Set name:", value = val()[[x]][[1]])
        })
    )
    
    sapply(1:7, function(x) 
        output[[paste0('item', x, ".2")]] <- renderUI({
            numericInput(paste0('item', x, ".2"), label = "Number of items:", value = val()[[x]][[2]])
        })
    )
    
   sapply(1:7, function(x) 
        output[[paste0('item', x, ".3")]] <- renderUI({
            numericInput(paste0('item', x, ".3"), label = "Number of periods:", value = val()[[x]][[3]])
        })
    )
    
    sapply(1:7, function(x) 
        output[[paste0('item', x, ".4")]] <- renderUI({
            selectizeInput(paste0('item', x, ".4"), label = "Type of analyses:", choices = dataset()[[input$Analyses_input]], 
                           multiple = TRUE, selected = val()[[x]][[4]])
        })
    )

    currencies <- reactive({ c("DKK", "EUR", "USD") }) 
    
    output$Price_input <- renderUI(varSelectInput("Price_input", label = "Select column for analyses' prices:", dataset()))
    
    output$Price_currency <- renderUI(selectizeInput("Price_currency", label = "Currency:", 
                                                     choices = currencies(), selected = "DKK"))
    
    output$Convert_to <- renderUI(selectizeInput("Convert_to", label = "Convert to:", 
                                                             choices = currencies(), selected = "DKK"))
    
    currencyexchange <- reactive({
        
         currex <- ifelse(input$Price_currency == input$Convert_to, 1, 
                         getQuote(paste0(input$Price_currency, input$Convert_to, "=X")) %>% pull(Last) )
        currex
    })
    
    output$conversion_rate <- renderUI({ paste0("Conversion rate: \n \n", currencyexchange()) })
    
    
    
    
    
    table <- reactive({
        
    #    req(length(unique(valupdate[,1])) != 1,
     #       length(unique(valupdate[,2])) != 1,
       #     length(unique(valupdate[,3])) != 1,
      #      length(unique(valupdate[,4])) != 1)
        
        
    COST <- list()
    TABLE <- list()
    
    COST <- lapply(1:7, function(x) 
        
    dataset() %>% dplyr::filter(!!rlang::sym(input$Analyses_input) %in% input[[paste0("item",x,".4")]]) %>% 
        dplyr::select(!!rlang::sym(input$Analyses_input), !!rlang::sym(input$Price_input)) )
    
    COST <- bind_rows(COST)
    COST <- distinct(COST)
    colnames(COST) <- c("Analysis", "Unit_price")
    COST$Unit_price <- as.numeric(as.character(COST$Unit_price))
    COST$Analysis <- as.character(COST$Analysis)
    
    DATA <- lapply(1:7, function(x) data.frame( Type = rep(input[[paste0("item",x,".1")]], length(input[[paste0("item",x,".4")]])),
                                                Units = rep(input[[paste0("item",x,".2")]], length(input[[paste0("item",x,".4")]])),
                                                Periods = rep(input[[paste0("item",x,".3")]], length(input[[paste0("item",x,".4")]])),
                                                Analysis = input[[paste0("item",x,".4")]]))
    
    DATA <- bind_rows(DATA)
    DATA$Analysis <- as.character(DATA$Analysis)

    
    DATA <- left_join(DATA, COST)
                                              
    
    
    DATA <- bind_rows(DATA)
    
    DATA$Unit_price <- round(DATA$Unit_price * currencyexchange(), 1)
        
    DATA$Cost <- round(DATA$Units * DATA$Periods * (DATA$Unit_price * currencyexchange()), 1)

    DATA
    })    
    
    
    tablesummary <- reactive({ tabsum <- table() %>% group_by(Type) %>% summarise(Cost = sum(Cost,na.rm = TRUE)) 
                                tabsumtotal <- data.frame(Type = "Total", Cost = sum(tabsum$Cost,na.rm = TRUE))
                                tabsum <- rbind(tabsum, tabsumtotal)
                                tabsum})
    
    

    output$tablefinal <- renderDT({ table() })
    
    output$tablesummary <- renderDT({ tablesummary() })
    
    output$downloadtable <- downloadHandler(
        filename = function(){"Analyses_cost.xlsx"}, 
        content = function(fname){
            write_xlsx( table(), fname) })
    
    output$downloadtablesummary <- downloadHandler(
        filename = function(){"Analyses_cost_totals.xlsx"}, 
        content = function(fname){
            write_xlsx( tablesummary(), fname) })
    
    
    
    output$Analyses_plot <- renderPlotly({
        
        data <-  table()
        
        req( table())
        
        ggplotly(
            ggplot(data,
                   aes(Analysis, Cost, fill = Analysis)) + 
                geom_col() +
                facet_wrap(~ Type, scales = "free_x", ncol = 2) +
                scale_fill_manual(values = au_pal_full) +
                theme_au_bw_col() +
                labs(x = "")
        )%>% layout(height = 200 * length(unique(data$Type))) 
    })
    
    
    
    
}

shinyApp(ui = ui, server = server)
