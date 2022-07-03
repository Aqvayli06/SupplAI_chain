# library(shiny)
# library(shiny.i18n)
# library(googleLanguageR)
# 
# 
# # setting up credentials to google cloud API (see more in googleLanguageR docs)
# i18n <- Translator$new(automatic = TRUE)
# 
# i18n$set_translation_language("de")
# 
# ui <- fluidPage(p(i18n$at("Hello")))
# 
# server <- function(input, output) {}
# 
# shinyApp(ui, server)

# library(shiny)
# library(shiny.i18n)
# 
# i18n <- Translator$new(translation_json_path = "translation.json")
# 
# i18n$set_translation_language("en")
# 
# ui <- fluidPage(usei18n(i18n),
#                 p(i18n$t("Hello")),
#                 actionButton("go", "GO!")
# )
# 
# server <- function(input, output, session) {
#   observeEvent(input$go,{
#     update_lang(session, "pl")
#   })
# }
# 
# shinyApp(ui, server)