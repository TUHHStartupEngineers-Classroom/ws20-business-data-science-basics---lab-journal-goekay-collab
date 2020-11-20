# SOLUTION SCRIPT FOR CHALLANGE FROM INTRO TO THE TINYVERSE

# Load libraries ----
library(tidyverse)
library(readxl)

# Importing Files ----
bikes_tbl <- read_excel("data/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
bikeshops_tbl <- read_excel("data/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
orderlines_tbl <- read_excel("data/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")


# Joining Data ----

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # Separation of location column to city and state columns
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%
  
  # Calculate total price (price * quantity) and add a total.price column
  mutate(total.price = price * quantity) %>%
  
  # Columns of interests are selected, reorgonized
  select(total.price,city,state,order.date)

# Analysis of sales ----

library(lubridate)
# Manipulate data
sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Grouping by state and summarizing sales
  group_by(state) %>% 
  summarize(sales = sum(total.price)) %>%
  # Currency is adjusted to be euros
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))


# Visualization of sales of the states
sales_by_state_tbl %>%
  
  # Setup canvas with the columns states (x-axis) and sales (y-axis)
  ggplot(aes(y = sales,x = state)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Total sale per state",
    subtitle = "Upward Trend",
    x = "States", # Override defaults for x and y
    y = "Revenue"
  )


# Visualization of sales over years per state
# Extract the data of interest
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
    
    # Select data of interest
    select(order.date, total.price, state) %>%
    mutate(year = year(order.date)) %>%
    
    # Group selected data and summarize year and main catgegory
    group_by(year, state) %>%
    summarise(sales = sum(total.price)) %>%
    ungroup() %>%
    # Formatting
    mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Create the plot
sales_by_year_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  # Currency is adjusted to be euros
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €"))+
  
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )