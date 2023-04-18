#Load libraries
library(tidyverse)
library(vroom)
library(data.table)

# Patent Dominance
# Import assignee data
col_types <- list(
  assignee_id = col_character(),
  type = col_integer(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "data/Data Wranling/assignee.tsv", 
  delim      = "\t", 
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# Convert table into data table
assignee_dt <- setDT(setDF(assignee_tbl))
class(assignee_dt)

# Since "type"=2 and 3 represents granted patents for companies, it is accordingly filtered
assignee_Companies_dt <- assignee_dt[,list(assignee_id,organization,type)][type==2|3] %>%
  # Some values exist multiple times, needs to be taken uniquely
  unique(by="assignee_id")

# Ease memory by removing data that is nonrequired anymore
remove(assignee_tbl)
remove(assignee_dt)

# Import patent assignee data
col_types <- list(
  patent_id = col_character(),
  assignee_id= col_character(),
  location_id= col_character()
)

patent_assignee_tbl <- vroom(
  file       = "data/Data Wranling/patent_assignee.tsv", 
  delim      = "\t", 
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# Convert table into data table
patent_assignee_dt <- setDT(patent_assignee_tbl)
class(patent_assignee_dt)

patent_amount_dt <- patent_assignee_dt[
  # Data sorted by "assignee_id"
  order(assignee_id)][
  # Each individual assignee id is counted to realize no. of total patents
  ,.N,by=.(assignee_id)] %>% 
  # Column "N" named to "patent_amount"
  setnames(c("N"),c("patent_amount"))

# Ease memory by removing data that is nonrequired anymore
remove(patent_assignee_tbl)
 
patentNo_company_rankList_dt <- patent_amount_dt[
  # assignee_Companies_dt and patent_amount_dt data tables are merged
  assignee_Companies_dt, on="assignee_id"][
  # NA values are filtered out
  !is.na(patent_amount)][
  # Table is filtered through decreasing "patent_amount"
  order(patent_amount,decreasing=TRUE)]

# Companies with the max. number of patents
companies_worldwide_top10 <- patentNo_company_rankList_dt[1:10,c("organization","patent_amount")]

# US Companies with the max. number of patents
companies_onlyUS_top10 <- patentNo_company_rankList_dt[type==2][1:10,c("organization","patent_amount")]

# Ease memory by removing data that is nonrequired anymore
remove(patent_amount_dt)
remove(patentNo_company_rankList_dt)

# Recent patent acitivity

# Create a table to be able to link companies from the patent ID
patent_id_company_dt <- patent_assignee_dt[
  # companies from US and patent assignee data tables are merged
  assignee_Companies_dt, on="assignee_id"][
    # Table is filtered through "patent_id" and "organization"
    ,c("patent_id","organization","type")]

remove(patent_assignee_dt)
remove(assignee_Companies_dt)

# Import patent data
col_types <- list(
  patent_id = col_character(),
  type = col_character(),
  patent_number = col_character(),
  country = col_character(),
  date = col_date(),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_integer(),
  filename = col_character(),
  withdrawn = col_character()
)

patent_tbl <- vroom(
  file       = "data/Data Wranling/patent.tsv", 
  delim      = "\t", 
  col_names  = names(col_types),
  col_types  = col_types,
  col_select = list(patent_id,date),
  na         = c("", "NA", "NULL")
)

# Convert table into data table
patent_id_date_dt <- setDT(patent_tbl)
remove(patent_tbl)

# Year information is extracted from the date column
patent_id_date_dt <- patent_id_date_dt[,date_year:=year(date)][
  date_year==2019,list(patent_id,date_year)]

# Link companies from the patent ID and year 2019
patentNo_company_rankList_2019_dt <- patent_id_date_dt[
  # Only US companies and their IDs are merged into the same table
  patent_id_company_dt[type==2], on="patent_id"][
  # NA values are filtered out
  !is.na(date_year)][
    # Each individual organization is counted to realize no. of total patents
    ,.N,by=.(organization)] %>% 
  # Column "N" named to "patent_amount_2019"
  setnames(c("N"),c("patent_amount_2019"))

patentNo_company_rankList_2019_dt[
    # Table is filtered through decreasing "patent_amount_2019"
    order(patent_amount_2019,decreasing=TRUE)][1:10]

# Innovation in tech

remove(patentNo_company_rankList_2019_dt)

# Import uspc data
col_types <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_integer()
)

uspc_tbl <- vroom(
  file       = "data/Data Wranling/uspc.tsv", 
  delim      = "\t", 
  col_names  = names(col_types),
  col_types  = col_types,
  col_select = list(patent_id,mainclass_id),
  na         = c("", "NA", "NULL")
  )
  
uspc_tbl <- uspc_tbl %>% select(patent_id,mainclass_id)

uspc_dt <- setDT(uspc_tbl)
remove(uspc_tbl)

# patent_id_company_dt and companies_worldwide_top10 data tables are merged
patent_id_company_top10_dt <- patent_id_company_dt[
  companies_worldwide_top10[,organization], on="organization"]

# The top 5 USPTO tech main classes 
top5_USPTO <- uspc_dt[  patent_id_company_top10_dt, on="patent_id"][
    # NA values are filtered out
    !is.na(mainclass_id)][
    # Each individual organization is counted to realize no. of total main tech. class
    ,.N,by=.(mainclass_id)][
    # Amount of tech. classes are sorted in decreasing order
    order(N,decreasing=TRUE)][1:5]%>% 
    # Column "N" named to "mainclass_amount"
    setnames(c("N"),c("mainclass_amount"))

top5_USPTO