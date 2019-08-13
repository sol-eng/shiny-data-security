library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)
library(purrr)
library(r2d3)
library(lubridate)
library(leaflet)
library(DBI)
library(RSQLite)
library(dbplyr)

# Formatting functions --------------------------
month_date <- function(x) floor_date(today(), unit = "month") - months(x)
month_calc <- function(x) {
  fd <- floor_date(today(), unit = "month") - months(x)
  str_c(month(fd), "/", str_sub(year(fd), 3, 4))
}
pretty_num <- function(nmbr) {
  res <- nmbr
  if (nmbr[[1]] > 1000) res <- paste0(round(nmbr / 1000, 1), "k")
  if (nmbr[[1]] > 1000000) res <- paste0(round(nmbr / 1000000, 1), "M")
  if (nmbr[[1]] > 1000000000) res <- paste0(round(nmbr / 1000000000, 1), "B")
  res
}
# Variable sets ---------------------------------
years <- c(year(today()), year(today()) - 1)
cust_colors <- tibble(
  rank = 1:10,
  color = c(
    "blue", "orange", "lightgray", "green", "lightblue",
    "gray", "pink", "purple", "lightgreen", "red"
  )
)
# Connects to database --------------------------
con <- dbConnect(SQLite(), "sales.sqlite")
# Lat/Lon city coordinates ----------------------
cities <- tbl(con, "cities")
# Shiny UI --------------------------------------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "Sales Dashboard",
    tags$li(
      class = "dropdown messages-menu",
      tags$a(tags$b(textOutput("logged")))
    ),
    dropdownMenuOutput("entitlements"),
    dropdownMenuOutput("sec_groups")
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(selectInput("year", "Year", choices = years), width = 3, background = "light-blue"),
      valueBoxOutput("total_sales", width = 3),
      valueBoxOutput("shipped", width = 3),
      valueBoxOutput("cancelled", width = 3)
    ),
    tabsetPanel(
      tabPanel(
        "Top 10 Customers",
        br(),
        fluidRow(
          box(
            title = "Rankings",
            status = "success",
            solidHeader = TRUE,
            d3Output("customers"),
            width = 5
          ),
          box(
            title = "Locations",
            status = "success",
            solidHeader = TRUE,
            leafletOutput("map"),
            width = 7
          )
        )
      ),
      tabPanel(
        "Orders",
        br(),
        fluidRow(
          box(
            title = "Product Breakdown",
            status = "primary",
            solidHeader = TRUE,
            d3Output("products")
          ),
          box(
            title = "Last 10 Orders",
            status = "primary",
            solidHeader = TRUE,
            d3Output("recent")
          )
        )
      )
    )
  )
)
# Shiny Server ----------------------------------
server <- function(input, output, session) {
  # Sets user and groups ------------------------
  if (Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect") {
    user_name <- session$user
    user_groups <- session$groups
  } else {
    user_name <- "edgar"
    user_groups <- c("sec1", "sec2")
  }

  # Pulls entitlements --------------------------
  entitlements_raw <- tbl(con, "entitlements")
  users <- entitlements_raw %>%
    group_by(user) %>%
    summarise() %>%
    pull()

  if (user_name %in% users) {
    user_ent <- user_name
  } else {
    user_ent <- "generic"
  }
  # Only allowed data to be seen by user --------
  entitlements <- entitlements_raw %>%
    filter(user == user_ent) %>%
    select(country)
  sales <- tbl(con, "sales") %>%
    inner_join(entitlements, by = "country")
  # Month range based on the year selection -----
  year_range <- reactive({
    if (input$year == max(years)) {
      list(from = 0, to = 11)
    } else {
      list(from = 12, to = 23)
    }
  })
  # Shared order data set -----------------------
  filtered_orders <- reactive({
    ft <- year_range()
    sales %>%
      filter(month_relative >= !!ft$from, month_relative <= !!ft$to) %>%
      group_by(order_number, month_relative, customer_name, country, city, state, postal_code, status) %>%
      summarise(total_sale = sum(quantity * unit_price, na.rm = TRUE)) %>%
      ungroup()
  })
  # Output section ------------------------------
  # >> User security section --------------------
  output$logged <- renderText(user_name)
  output$entitlements <- renderMenu({
    ent <- map(pull(entitlements, country), notificationItem, icon = icon("map-marker-alt"))
    dropdownMenu(
      headerText = "Assignments",
      type = "notifications",
      badgeStatus = NULL,
      .list = ent,
      icon = icon("map-marker-alt")
    )
  })
  output$sec_groups <- renderMenu({
    grps <- map(user_groups, notificationItem, icon = icon("users"))
    dropdownMenu(
      headerText = "Security Groups",
      type = "notifications",
      badgeStatus = NULL,
      .list = grps,
      icon = icon("users")
    )
  })
  # >> Value boxes section ----------------------
  output$total_sales <- renderValueBox(
    filtered_orders() %>%
      summarise(sum(total_sale, na.rm = TRUE)) %>%
      pull() %>%
      pretty_num() %>%
      paste0("$", .) %>%
      valueBox(subtitle = "Total Sales", icon = icon("credit-card"), color = "teal")
  )
  output$shipped <- renderValueBox(
    filtered_orders() %>%
      filter(status == "Shipped") %>%
      summarise(n()) %>%
      pull() %>%
      pretty_num() %>%
      valueBox(subtitle = "Shipped Orders", icon = icon("truck"), color = "light-blue")
  )
  output$cancelled <- renderValueBox(
    filtered_orders() %>%
      filter(status == "Cancelled") %>%
      summarise(n()) %>%
      pull() %>%
      pretty_num() %>%
      valueBox(subtitle = "Canceled Orders", icon = icon("times"), color = "purple")
  )
  # >> Top customer section ---------------------
  top_customers <- reactive({
    filtered_orders() %>%
      left_join(cities, by = c("country", "city")) %>%
      group_by(customer_name, country, city, lat, lng) %>%
      summarise(total_sale = sum(total_sale)) %>%
      arrange(desc(total_sale)) %>%
      head(10) %>%
      collect() %>%
      ungroup() %>%
      mutate(rank = row_number()) %>%
      inner_join(cust_colors, by = "rank")
  })
  output$customers <- renderD3({
    top_customers() %>%
      mutate(
        y_label = paste0("$", pretty_num(total_sale)),
        fill = color
      ) %>%
      rename(x = customer_name, y = total_sale, label = customer_name) %>%
      r2d3("bar-plot.js")
  })
  output$map <- renderLeaflet({
    icons <- awesomeIcons(
      icon = "ios-open",
      iconColor = "black",
      library = "ion",
      markerColor = cust_colors
    )
    top_customers() %>%
      leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(
        lng = ~ lng * (1 + (rank / 100)),
        lat = ~lat,
        popup = ~ paste(
          paste0("<b>", customer_name, "</b>"),
          paste0("<i>", city, ", ", country, "</i>"),
          paste0("$", prettyNum(total_sale, big.mark = ",")),
          sep = "<br/>"
        ),
        icon = awesomeIcons(
          icon = "ios-open",
          iconColor = "black",
          library = "ion",
          markerColor = ~color
        )
      )
  })
  # >> Order tab section ------------------------
  output$recent <- renderD3({
    filtered_orders() %>%
      arrange(desc(order_number)) %>%
      head(10) %>%
      collect() %>%
      mutate(y_label = paste0("$", pretty_num(total_sale))) %>%
      mutate(label = paste0(order_number, " - ", customer_name)) %>%
      rename(x = customer_name, y = total_sale) %>%
      r2d3("bar-plot3.js")
  })
  output$products <- renderD3({
    ft <- year_range()
    sales %>%
      filter(month_relative >= !!ft$from, month_relative <= !!ft$to) %>%
      group_by(product) %>%
      summarise(total_sale = sum(quantity * unit_price, na.rm = TRUE)) %>%
      arrange(product) %>%
      collect() %>%
      mutate(y_label = paste0("$", pretty_num(total_sale))) %>%
      rename(x = product, y = total_sale, label = product) %>%
      r2d3("bar-plot2.js")
  })
}

shinyApp(ui = ui, server = server)
