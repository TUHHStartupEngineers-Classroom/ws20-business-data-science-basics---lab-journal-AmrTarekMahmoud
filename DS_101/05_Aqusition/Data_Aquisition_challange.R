library(RSQLite)
library(dplyr)
library(httr)
library(glue)
library(jsonlite)
library(keyring)
library(rvest)
library(stringr)
library(stringi)
library(purrr)
library(xopen)
library(tibble)
library(curl)

########################################CHALLENGE_PART_A######################################################

url= "https://api.agify.io?name=bella"
resp <- GET(url)
rawToChar(resp$content)
resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()


#########################################CHALLENGE_PART_B#####################################################




get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".catalog-category-bikes__title-text") %>%
    html_text()%>%
    enframe(name = "No.", value = "Bike.Name")
   bike_database_tbl<-bike_url_tbl%>% mutate(price=html_bike_category%>%html_nodes(css =".catalog-category-bikes__price-title")%>% html_text())
}
url= "https://www.rosebikes.de/ebike"
bike_tableout<-get_bike_data(url)
saveRDS(bike_tableout,"Task2_table.rds")

