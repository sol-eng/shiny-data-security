library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(r2d3)
library(lubridate)
library(leaflet)

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

cities <- read_csv("data-cities.csv")

years <- c(year(today()), year(today()) - 1)

cust_colors <- c(
  "blue", "orange", "lightgray", "green", "lightblue",
  "gray", "pink", "purple", "lightgreen", "red"
)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    #title = textOutput("title"),
    title = "Sales Dashboard",
    titleWidth = 250,
    dropdownMenuOutput("user_info"),
    dropdownMenuOutput("sec_groups"),
    dropdownMenuOutput("entitlements")
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

server <- function(input, output, session) {
  if (Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect") {
    user_name <- session$user
    user_groups <- session$groups
  } else {
    user_name <- "frodo"
    user_groups <- c("sec1", "sec2")
  }
  if (user_name != "frodo") {
    user_ent <- "generic"
  } else {
    user_ent <- "frodo"
  }
  entitlements <- read_csv("data-entitlements.csv") %>%
    filter(user == user_ent)

  sales <- read_csv("data-sales.csv") %>%
    inner_join(select(entitlements, country), by = "country")

  filtered_orders <- reactive({
    if (input$year == max(years)) {
      from <- 0
      to <- 11
    } else {
      from <- 12
      to <- 23
    }
    sales %>%
      filter(month_relative >= from, month_relative <= to) %>%
      group_by(order_number, month_relative, customer_name, country, city, state, postal_code, status) %>%
      summarise(total_sale = sum(quantity * unit_price)) %>%
      ungroup()
  })
  filtered_products <- reactive({
    if (input$year == max(years)) {
      from <- 0
      to <- 11
    } else {
      from <- 12
      to <- 23
    }
    sales %>%
      filter(month_relative >= from, month_relative <= to) %>%
      group_by(product) %>%
      summarise(total_sale = sum(quantity * unit_price))
  })
  output$total_sales <- renderValueBox(
    filtered_orders() %>%
      summarise(sum(total_sale)) %>%
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
  output$messages <- renderMenu({
    latest <- filtered_orders() %>%
      filter(order_number == max(order_number)) %>%
      select(customer_name, total_sale) %>%
      collect()
    dropdownMenu(
      headerText = "Newest sale",
      type = "messages",
      messageItem(
        latest$customer_name,
        paste0("$", prettyNum(latest$total_sale, big.mark = ",")),
        icon = icon("credit-card")
      )
    )
  })
  output$user_info <- renderMenu({
    user <- list(notificationItem(user_name, icon = icon("user")))
    dropdownMenu(
      headerText = "Logged in as",
      type = "notifications",
      badgeStatus = NULL,
      .list = user,
      icon = icon("user")
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
  output$products <- renderD3({
    filtered_products() %>%
      arrange(product) %>%
      collect() %>%
      mutate(y_label = paste0("$", pretty_num(total_sale))) %>%
      rename(x = product, y = total_sale, label = product) %>%
      r2d3("bar-plot2.js")
  })
  output$customers <- renderD3({
    filtered_orders() %>%
      group_by(customer_name) %>%
      summarise(total_sale = sum(total_sale)) %>%
      arrange(desc(total_sale)) %>%
      head(10) %>%
      collect() %>%
      mutate(
        y_label = paste0("$", pretty_num(total_sale)),
        fill = cust_colors
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
    filtered_orders() %>%
      left_join(cities, by = c("country", "city")) %>%
      group_by(customer_name, country, city, lat, lng) %>%
      summarise(total_sale = sum(total_sale)) %>%
      arrange(desc(total_sale)) %>%
      head(10) %>%
      collect() %>%
      leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(
        lng = ~lng, lat = ~lat,
        popup = ~ paste(
          paste0("<b>", customer_name, "</b>"),
          paste0("<i>", city, ", ", country, "</i>"),
          paste0("$", prettyNum(total_sale, big.mark = ",")),
          sep = "<br/>"
        ),
        icon = icons
      )
  })
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
  output$title <- renderText(
    paste0(str_to_title(user_name), "'s Sales Dashboard")
  )
}

shinyApp(ui = ui, server = server)
