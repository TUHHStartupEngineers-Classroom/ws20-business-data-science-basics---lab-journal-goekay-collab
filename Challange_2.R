# API CHALLANGE----

# 1.0 LIBRARIES ----
library(httr)
library(jsonlite)

url_home <- "https://the-one-api.dev/v2/character"

# Access token received manually from the server
token="YkqtnI_bcHsdKwh6ZFgy"

# Send a GET request to the server by authorizing the request with token
resp <- GET(url_home,add_headers(Authorization = paste("Bearer",token)))

# Check response status to see whether the request was successfull
resp

# Data conversion
resp_JSON <- resp %>% 
  .$content %>% 
  # Convert raw Unicode into char
  rawToChar() %>% 
  # Convert char into JSON
  fromJSON()

# Create a tibble out of the interesting data
tibble(name=resp_JSON$docs$name,
       gender=resp_JSON$docs$gender,
       race=resp_JSON$docs$race,
       death=resp_JSON$docs$death,
       spouse=resp_JSON$docs$spouse,
       wikiUrl=resp_JSON$docs$wikiUrl
       )

# WEBSCRAPPING CHALLANGE----
# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

url_home <- "https://www.rosebikes.com/bikes"

html_main=read_html(url_home)

bike_category_name_tbl <- html_main %>%
# Get the nodes for the bike categories ...
  html_nodes(css = ".catalog-navigation__link") %>%
  html_attr('title') %>%

  # Convert vector to tibble
  enframe(name = "position", value = "category_class") %>%

  # Add a hashtag so we can get nodes of the categories by id (#)
  mutate(
    category = str_glue("{category_class}")
  ) %>%
  
  select(category)

bike_category_name_tbl

bike_category_url_tbl <- html_main %>%
  
  # Get the nodes for the catagories ...
  html_nodes(css = ".catalog-navigation__link")%>%
  
  # ...and extract the information of the id attribute
  html_attr('href') %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "category_url") %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.rosebikes.com{category_url}")
  ) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url) %>%
  
  select(url)

bike_category_data <- tibble(bike_category_name_tbl,bike_category_url_tbl)

# urls for sale and kids are eliminated
bike_category_data <- bike_category_data[-(8:9),(1:2)]

get_subcategory_data <- function(category,url){

  html_category  <- read_html(url)

  # Get the name of the subcategories inside the category
  bike_subcategory_name <- html_category %>%
  
    # Get the nodes for the families ...
    html_nodes(css = ".catalog-category-bikes__title")%>%
    html_text() 

  # Remove unnecessary character "\n"
  bike_subcategory_name <- str_replace_all(bike_subcategory_name,"\n","")
  # Create a list of subcatagory names
  bike_subcategory_name <- str_split(bike_subcategory_name, "\n")
  unlist(bike_subcategory_name,use.names=FALSE)
  
  # Convert vector to tibble
  bike_subcategory_name <- tibble(subcategory=bike_subcategory_name) 
  
  bike_subcategory_url_tbl <- html_category %>%
    
    # Get the nodes for the families ...
    html_nodes(css = ".catalog-category-bikes__content> a")%>%
    
    # ...and extract the information of the id attribute
    html_attr('href') %>%
    
    # Convert vector to tibble
    enframe(name = "position", value = "subcategory_url") %>%
    
    # Add the domain, because we will get only the subdirectories
    mutate(
      url = glue("https://www.rosebikes.com{subcategory_url}")
    )  %>%
    
    select(url)
  
  category_vec <- tibble(category=rep(category,count(bike_subcategory_url_tbl))) 
  
  tibble(category_vec,
          bike_subcategory_name,
          bike_subcategory_url_tbl)
}

get_model_data <- function(category,subcategory,url){

  html_subcategory <- read_html(url)

  bike_subcategory_modelName <- html_subcategory %>%
  
    # Get the nodes for the families ...
    html_nodes(css = ".catalog-category-model__title")%>%
    html_text()

  # Remove unnecessary character "\n"
  bike_subcategory_modelName <- bike_subcategory_modelName %>%
    str_replace_all("\n","") %>%
    str_split("\n") %>%
    unlist(bike_subcategory_modelName,use.names=FALSE)
  
  if (is.null(bike_subcategory_modelName)){
    bike_subcategory_modelName = NA
  }
  
  # Convert vector to tibble
  bike_subcategory_modelName <- tibble(model=bike_subcategory_modelName) 

  bike_subcategory_modelPrice <- html_subcategory %>%
  
    # Get the nodes for the families ...
    html_nodes(css = ".catalog-category-model__price-current-value")%>%
    html_text()

  # Remove unnecessary character "\n"
  bike_subcategory_modelPrice <- bike_subcategory_modelPrice %>%
    str_replace_all("\n","") %>%
      str_split("\n") %>%
  unlist(bike_subcategory_modelPrice,use.names=FALSE)
  
  if (is.null(bike_subcategory_modelPrice)){
    bike_subcategory_modelPrice = NA
  }
  
  # Convert vector to tibble
  bike_subcategory_modelPrice <-  tibble(price=bike_subcategory_modelPrice)

  # Count the no. of bike models exist for that subcategory
  subcategory_vec <- tibble(subcategory=rep(subcategory,count(bike_subcategory_modelPrice)))
  
  category_vec <- tibble(category=rep(category,count(bike_subcategory_modelPrice)))
  
  tibble(category_vec,
         subcategory_vec,
         bike_subcategory_modelName,
         bike_subcategory_modelPrice)
}


category <- bike_category_data$category
url <- bike_category_data$url

total_categoryNo=length(category)

for (i in 1:total_categoryNo){
  if (i==1){
    subcategory_data <- get_subcategory_data(category[[i]],url[[i]])
  }
  else{
    subcategory_data <- subcategory_data %>% full_join(get_subcategory_data(category[[i]],url[[i]]))
  }
}

category <- subcategory_data$category
subcategory <- subcategory_data$subcategory
subcategory_url <- subcategory_data$url

total_subcategoryNo <- length(subcategory)

for (i in 1:total_subcategoryNo){
  if (i==1){
    module_data <- get_model_data(category[[i]],subcategory[[i]],subcategory_url[[i]])
  }
  else{
    module_data <- module_data %>% full_join(get_model_data(category[[i]],subcategory[[i]],subcategory_url[[i]]))
  }
}

