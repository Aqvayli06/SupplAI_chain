#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    waiter::autoWaiter(html = waiter::spin_three_bounce()),
    bs4Dash::dashboardPage(
      # preloader = list(html = tagList(waiter::spin_circle_square(), "Loading ..."), color = "darkcyan"),
      bs4Dash::dashboardHeader(title ="SupplyChainAnalytics"),
      bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
          bs4Dash::menuItem("Sales", tabName = "item_sales", icon = icon("fa-money-bill-trend-up")),
          bs4Dash::menuItem("Production", tabName = "item_production", icon = icon("fa-solid fa-industry")),
          bs4Dash::menuItem("Replenishment", tabName = "item_replenishment", icon = icon("fa-solid fa-chart-line-up"))
          )# sidebar menu
      ),
      bs4Dash::dashboardBody(
        
        bs4Dash::tabItems(
          bs4Dash::tabItem("item_sales",
                           bs4Dash::tabsetPanel(id = "vente_tabset",
             tabPanel("Articles/Commande",value = "articles",
                      
                      mod_sale_manager_ui("mod_sale_manager_1")
                      ),# articles panel
             tabPanel("Client", value = "client", 
                      fluidPage(
                        fluidRow(
                          col_6(mod_costumer_manager_ui("costumer_manager_1")),
                          col_6(uiOutput("costumer_bills_list_box"))
                        ),
                        col_12(uiOutput("costumer_bill_display_box"))
                      ) # client: fluidPage
                      ),#panel client
             tabPanel("Factures",value = "facture" ,tableOutput("table")),
             tabPanel("Orders",value = "Commandes" ,
                      mod_order_manager_ui("order_manager_1")
                      ),
             
             tabPanel("Overview",value = "overview" ,mod_business_overview_ui("business_overview_1")),
             tabPanel("Sales Forecasts",value = "sales_forecast" ,mod_sales_forecaster_ui("sales_forecaster_1"))
           ) # tabsetPanel : vente_tabset 
          ),#item_sales
          bs4Dash::tabItem("item_production",
                           
          ),#item_production
          bs4Dash::tabItem("item_replenishment",
                           bs4Dash::tabsetPanel(id = "replenishment_tabset",
                                                tabPanel("Projected Inventory",value = "projected_inventory",
                                                         mod_inventory_manager_ui("inventory_manager_1") 
                                                ),
                                                tabPanel("Strategy",value = "replenishment_strategy",
                                                         mod_replenishment_strategy_ui("replenishment_strategy_1")
                                                ),
                                                tabPanel("Simulation",value = "replenishment_simulation",
                                                         mod_replenishment_simulation_ui("replenishment_simulation_1")
                                                )
                                                
                                                
                                                
                           )
                                         
          )#item_replenishment
          
        )# menu items
        
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'DataCollector'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

