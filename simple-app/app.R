library(shiny)

ui <- fluidPage(
  titlePanel("Data Level Security"),
  mainPanel(
    tags$h2("Logged in as: "), 
    tags$h3(textOutput("user")), 
    tags$h2("Security Groups: "),
    tags$h3(textOutput("groups"))
    )
  )

server <- function(input, output, session) {
  # Allows the app to be work while developing and when
  # publishing to RStudio Connect without changing code
  if(Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect") {
    user_name <- session$user
    user_groups <- session$groups
  } else {
    user_name <- "test"
    user_groups <- c("group1", "group2")
  }
  output$user <- renderText(user_name)
  output$groups <- renderText(paste0(user_groups, collapse = ", "))
}

shinyApp(ui = ui, server = server)
