library(tidyverse)
library(janitor)
library(lubridate)

#https://www.kaggle.com/kyanyoga/sample-sales-data/version/1
sales_raw <- read_csv("data-prep/sales_data_sample.csv")

rel_month <- sales_raw %>%
  group_by(MONTH_ID, YEAR_ID) %>%
  summarise() %>%
  ungroup() %>%
  arrange(desc(YEAR_ID + MONTH_ID / 100)) %>%
  mutate(month_relative = row_number() - 1)

cus_id <- sales_raw %>%
  group_by(CUSTOMERNAME) %>%
  summarise() %>%
  ungroup() %>%
  mutate(customer_id = row_number())

sales <- sales_raw %>%
  inner_join(rel_month, by = c("YEAR_ID", "MONTH_ID")) %>%
  inner_join(cus_id, by = "CUSTOMERNAME") %>%
  clean_names() %>%
  mutate(
    contactfirstname = iconv(enc2utf8(contactfirstname), sub = "byte"),
    contact = paste(contactlastname, contactfirstname)
  ) %>%
  select(
    -orderdate, -addressline2, -dealsize, -qtr_id, -month_id,
    -year_id, -contactlastname, -contactfirstname, -sales
    ) %>%
  rename(
    quantity = quantityordered,
    unit_price = priceeach,
    product = productline,
    address = addressline1,
    line_number = orderlinenumber,
    customer_name = customername
    ) %>%
  rename_at(vars(contains("order")), ~str_replace(., "order", "order_")) %>%
  rename_at(vars(contains("code")), ~str_replace(., "code", "_code")) %>%
  mutate(
    city = ifelse(city == "NYC", "New York", city),
    country = ifelse(country == "USA", "United States", country),
    country = ifelse(country == "UK", "United Kingdom", country),
    city = ifelse(city == "North Sydney", "Sydney", city),
    city = ifelse(city == "Brickhaven", "Houston", city),
    city = ifelse(city == "Chatswood", "Sydney", city)
  )

orders <- sales %>%
  group_by(order_number, month_relative, customer_name, country, city, state, postal_code, status) %>%
  summarise(total_sale = sum(quantity * unit_price)) %>%
  ungroup()

write_csv(orders, "data-prep/orders.csv")

world <- read_csv("data-prep/worldcities.csv")

world2 <- world %>%
  group_by(country, city) %>%
  mutate(num = row_number()) %>%
  filter(num == 1) %>%
  ungroup() %>%
  select(- num)

cities <- orders %>%
  left_join(world2, by = c("country", "city"))  %>%
  group_by(country, city, lat, lng) %>%
  summarise() %>%
  filter(!is.na(lng)) %>%
  ungroup()

# orders %>% count(country, sort = TRUE)
frodo <- tibble(
  user = "frodo",
  country = c("United States", "Canada")
)
generic <- tibble(
  user = "generic",
  country = c("France", "Spain",  "United Kingdom", "Ireland", "Italy", "Belgium")
)
entitlements <- frodo %>%
  bind_rows(generic)

library(RSQLite)
library(DBI)
con <- dbConnect(SQLite(), "app/sales.sqlite")
dbWriteTable(con, "cities", cities, overwrite = TRUE)
dbWriteTable(con, "entitlements", entitlements, overwrite = TRUE)
dbWriteTable(con, "sales", sales, overwrite = TRUE)
dbDisconnect(con)




