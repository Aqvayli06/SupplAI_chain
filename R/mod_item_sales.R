#' orders_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sale_manager_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::box(title = "Available Items",status = "info",collapsible   = TRUE,width = 12,
                 fluidRow(
                   column(width = 3, textInput(inputId = ns("sales_article_name"),label = "Nom d'article",value = "")),
                   column(width = 3, textInput(inputId = ns("sales_article_ref"),label = "Reference",value = ""))
                 ),
                 DT::dataTableOutput(ns("available_items_DT"))
    ), # box :available items
    fluidRow(
      col_8(uiOutput(ns("selected_items_box"))),
      col_4(mod_costumer_manager_ui(ns("sales_costumer")))
    )
  )
}

#' orders_manager Server Functions
#'
#' @noRd 
mod_sale_manager_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    dir.create("./temp/")
    selected_articles_file <- "./temp/selected_articles_for_order.rds"
    saveRDS(NULL,selected_articles_file)
    
    available_items <- reactive({
      req(input$sales_article_name)
      az_con <- open_warehouse_db()
      # query_req <-paste0("SELECT * FROM product_table WHERE item_name = '", input$sales_article_name,"'")
      query_req <-paste0("SELECT * FROM product_table")
      res <- odbc::dbSendQuery(az_con, query_req)
      article_warehouse <- odbc::dbFetch(res)
      odbc::dbClearResult(res)
      odbc::dbDisconnect(az_con)
      dispo_article <- article_warehouse%>%
        dplyr::filter(grepl(input$sales_article_name,item_name))
      return(dispo_article)
    })
    
    output$available_items_DT <- DT::renderDataTable({
      req(available_items())
      available_items()
    }, server = FALSE)
    
    output$target_articles <- renderUI({
      req(available_items())
      articles_choices <- available_items()$item_name
      shinyWidgets::pickerInput(inputId = ns("target_articles"),
                                label = "Selected Item",
                                selected = NULL,
                                choices = articles_choices,
                                multiple = FALSE)
    })
    
    output$target_quantity <- renderUI({
      req(available_items())
      
      quantity_disponible <- compute_available_quantity(target_article = input$target_articles,
                                                        selected_articles = selected_articles(),intial_quantity = available_items()$quantity)
      quantity_disponible <- 0:quantity_disponible
      
      shinyWidgets::pickerInput(inputId = ns("target_quantity"),
                                label = "Quantity",
                                selected = NULL,
                                choices = quantity_disponible,
                                multiple = FALSE)
    })
    output$sell_price_u <- renderUI({
      req(available_items())
      sell_price_u <- available_items()$sell_price
      numericInput(inputId = ns("sell_price_u"),
                   label = "Price (u)",
                   value = sell_price_u)
    })
    
    output$validate_sales_article <- renderUI({
      req(input$sell_price_u)
      req(input$target_quantity)
      if(input$target_quantity >0){
        shinyWidgets::actionBttn(
          inputId = ns("validate_sales_article"),
          label = "Add"
        )  
      }
    })
    # add costumer module
    mod_costumer_manager_server("sales_costumer")
    
    output$delete_article <- renderUI({
      if(!is.null(selected_articles())){
        shinyWidgets::actionBttn(
          inputId = ns("delete_article"),
          label = "Supprimer Article",
          color = "danger"
        )  
      }
    })
    
    observeEvent(input$validate_sales_article,{
      item_ID <- available_items()%>%dplyr::filter(item_name == input$target_articles)%>%dplyr::pull(item_ID)
      new_article <- data.frame(item_name = input$target_articles,item_ID = item_ID ,sell_price_u = input$sell_price_u, quantity = as.numeric(paste0(input$target_quantity)) )      
      selected_articles <- readRDS(selected_articles_file)
      
      if(is.null(selected_articles)){
        selected_articles <- new_article
      }else{
        selected_articles <- rbind(selected_articles,new_article)
      }
      selected_articles <- selected_articles%>%
        dplyr::group_by(item_name, sell_price_u,item_ID)%>%
        dplyr::summarise(quantity = sum(quantity),.groups = "drop")
      
      saveRDS(selected_articles,selected_articles_file)
    })
    
    output$sales_client_discount <- renderUI({
      req(selected_articles())
      selectInput(inputId = ns("sales_client_discount"), label = "Discount % ", choices = 0:100, selected = 0)
    })
    selected_articles <- reactiveFileReader(intervalMillis = 1000,session = session ,filePath = selected_articles_file,readFunc = readRDS)
    
    
    selected_articles_augmented <- reactive({
      req(selected_articles())
      req(input$sales_client_discount)
      augmented_sell_table <- get_augmented_sales_data(selected_articles = selected_articles(),input$sales_client_discount)
      return(augmented_sell_table)
    })
    
    output$selected_articles_DT <- DT::renderDataTable({
      selected_articles_augmented()
    })
    
    output$validate_sales_total <- renderUI({
      req(selected_articles())
      shinyWidgets::actionBttn(
        inputId = ns("validate_sales_total"),
        label = "Finalize",
        color = "success"
      )
    })
    
    observeEvent(input$validate_sales_total,{
      update_warehouse(selected_articles_augmented(), update_mode = "sales")
      shinyalert::shinyalert("Sales", text = "Articles Vendus", type = "success")
      saveRDS(NULL,selected_articles_file) # reset list to Zero
      updateTextInput(session, "sales_article_name",
                      value = "")
      
    })
    
    output$selected_items_box <- renderUI({
      bs4Dash::box(title = "Selected Items",status = "success",collapsible   = TRUE,width = 12,
                   fluidRow(
                     col_3(uiOutput(ns("target_articles"))),
                     col_2(uiOutput(ns("target_quantity"))),
                     col_2(uiOutput(ns("sell_price_u"))),
                     col_2(uiOutput(ns("sales_client_discount"))),
                     col_2(uiOutput(ns("validate_sales_article")))
                   ),
                   DT::dataTableOutput(ns("selected_articles_DT")),
                   fluidRow(
                     col_4(uiOutput(ns("delete_article"))),
                     col_3(uiOutput(ns("validate_sales_total")))
                   )
      ) # Selected Items
    })# box : selected items
  })
  
}

## To be copied in the UI
# mod_sale_manager_ui("orders_manager_1")

## To be copied in the server
# mod_sale_manager_server("orders_manager_1")
