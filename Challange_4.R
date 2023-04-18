library(tidyverse)
library(ggrepel)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

# Challange-1 

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# Add column to keep data of cumulative case number
covid_data_tbl$cumulative_cases <- covid_data_tbl$cases*.0

# Reverse the order of rows to sort data from the very past to the future
covid_data_tbl <- covid_data_tbl %>% map_df(rev)

covid_selected_data_tbl <- covid_data_tbl %>% 
      group_by(geoId) %>% 
      filter(geoId=="US" |
             geoId=="DE" |
             geoId=="FR" |
             geoId=="ES" |
             geoId=="UK") %>%
      ungroup()
                                                   
  
# Function to get cumulative case numbers
get_cumulative_cases <- function(country_data){
  
  amount_of_days =
    count(country_data%>%select(cumulative_cases))[[1]]
  
  for (i in 1:amount_of_days){
    if (i==1){
      country_data$cumulative_cases[i] <- country_data$cases[i] 
    }
    else{
      country_data$cumulative_cases[i] <- country_data$cumulative_cases[i-1]+country_data$cases[i]
    }
  }
  country_data
}

  
# Calculate cumulative case numbers 
cumulative_data_USA <- get_cumulative_cases(covid_selected_data_tbl %>%
                                            filter(geoId=="US"))


cumulative_data_Germany <- get_cumulative_cases(covid_selected_data_tbl %>%
                                                  filter(geoId=="DE"))
                                                

cumulative_data_France <- get_cumulative_cases(covid_selected_data_tbl %>%
                                                 filter(geoId=="FR"))
                                               


cumulative_data_Spain <- get_cumulative_cases(covid_selected_data_tbl %>%
                                                filter(geoId=="ES"))
                                              

cumulative_data_UK <- get_cumulative_cases(covid_selected_data_tbl %>%
                                             filter(geoId=="UK"))
                                           
                    
cumulative_data <-bind_rows(cumulative_data_USA,
                    cumulative_data_Germany,
                    cumulative_data_France,
                    cumulative_data_Spain,
                    cumulative_data_UK)

cumulative_data$dateRep <- cumulative_data$dateRep%>% as.Date("%d/%m/%Y")

max_case_US_cases <- cumulative_data%>%
  filter(geoId=="US") %>%
  select(cumulative_cases) %>%
  tail(n=1)
max_case_US_date <- cumulative_data%>%
  filter(geoId=="US") %>%
  select(dateRep) %>%
  tail(n=1)

p<-cumulative_data %>%

  ggplot(aes(x=dateRep,y=cumulative_cases,color=countriesAndTerritories)) + 
  
  theme_dark() +
  
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "Sales are trending up",
    x = "Year 2020",
    y = "Cumulative cases",
    color = "Continent/Country" # Legend text
  ) +
  
  scale_x_date(date_breaks="1 month",date_labels = "%B") +
  
  scale_y_continuous(breaks=seq(0,15e6,25e5),labels = scales::dollar_format(scale = 1e-6, 
                                                  prefix = "",
                                                  suffix = "M")) +
  
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 45,hjust=1,vjust=1)) + 
  
  geom_line(size=1) +
  
  geom_label_repel(aes(x=max_case_US_date$dateRep,
                            y=max_case_US_cases$cumulative_cases
                           ),
                 size=3,
                 color="yellow",
                 fill="navy",
                 label=max_case_US_cases$cumulative_cases %>%
                           number(big.mark="."),
                 label.size=0.1,
                 direction = "x",
                 nudge_x=-30,
                 nudge_y=0,
                 max.iter=0)
#Time course of the cumulative Covid-19 cases
p

# Challange-2 : Time course of the cumulative Covid-19 cases

library(maps)

world <- map_data("world")
world_data_selected <- world %>% select(long,lat,region)

covid_data_selected_tbl <- covid_data_tbl %>% 
                  group_by(geoId) %>% 
                  mutate(mortality_rate=(sum(deaths)/popData2019)) %>%
                  select(countriesAndTerritories,mortality_rate) %>%
                  unique()

covid_data_tbl <- covid_data_tbl %>% 
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))

world_mortality_data <- merge(covid_data_selected_tbl,world_data_selected,by.x="countriesAndTerritories",by.y="region")

gg <- ggplot() +
  
  labs(
    title = "Confirmed COVID-19 cases relative to the size of the population",
    subtitle = "More than 1.4 Million confirmed COVID-19 deaths worldwide",
    x = "",
    y = "",
    caption = "Date:28/11/2020"
  ) +
  geom_map(data=world,map=world, aes(x=long, y=lat, map_id=region), col="grey55", fill="grey")+
  
  geom_map(data=world_mortality_data, map=world,
           aes(fill=mortality_rate,
               map_id=countriesAndTerritories),
           color="grey44", size=0.15) +
  
  scale_x_continuous(labels=NULL)+
  
  scale_y_continuous(labels=NULL)+
  
  scale_fill_gradient(name="Mortality Rate",low="red1", high="red4", labels=scales::percent, n.breaks=6) +
  
  theme_dark() +
  
  theme(panel.grid.major = element_line(colour="grey55",size=1),
        panel.grid.minor = element_blank())
 