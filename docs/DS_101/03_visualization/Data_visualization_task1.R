library(tidyverse)
library(scales)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(dbplyr)

#importing data


covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")


# creating table for chart
covid_data_modified<-covid_data_tbl%>%
  select(countriesAndTerritories,cases,day,month,year)%>%
  relocate(month,day,year)%>%
  filter(year==2020,month>1)%>%
  filter(day!=1,countriesAndTerritories=="France" | countriesAndTerritories=="Germany" | countriesAndTerritories=="Spain" | countriesAndTerritories=="United_Kingdom" | countriesAndTerritories=="United_States_of_America")%>%
  group_by(month,countriesAndTerritories)%>%
  summarize(total_cases_per_month=sum(cases))%>%
  ungroup()

#data visualization 
covid_data_modified%>% ggplot(aes(month, total_cases_per_month, color = countriesAndTerritories))+
  geom_smooth(method = "loess", span=0.2) +
  
  # same as above, with explicit scales
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, 
                                                    prefix = "",
                                                    suffix = "M"))+
  scale_x_continuous(breaks = seq(2, 11, by = 1), labels = c("Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov"))+
  geom_label(aes(label=total_cases_per_month), 
             hjust = "inward",
             size  = 3,
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11])+

  labs(
    title = "CovID-19 confirmed cases worldwide ",
    subtitle = "as of 11/02/2020. Europe is not included",
    caption = "",
    x = "Year 2020",
    y = "Cumulative cases(M)",
    color = "Country" # Legend text
  )

theme_dark () +
  theme(
    title = element_text(face = "bold", color = "#08306B")
    
  )