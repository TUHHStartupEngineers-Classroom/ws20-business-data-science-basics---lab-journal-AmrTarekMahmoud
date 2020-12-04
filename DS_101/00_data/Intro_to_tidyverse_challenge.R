#importing libraries 
library(tidyverse)
library(readxl)
library(lubridate)
library("writexl")
#importing data

bikes_tbl <- readxl:: read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")

orderlines_tbl <- readxl::read_excel("DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

bikeshops_tbl  <- readxl::read_excel("DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")







#####################################################################################################


###################################  Assignment  ###################################################


####################################################################################################


left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))


bike_orderlines_wrangled_city_separated_tbl <- bike_orderlines_joined_tbl %>%
  # 5.1 Separate category name
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>% 
  
  # Separate city and state 
  
  separate(col    = location,
           into   = c("City", "State"),
           sep    = ", ")%>% 
  
  # 5.2 Add the total price (price * quantity) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = quantity* price) %>%
  
  # 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  # 5.3.1 by exact column name
  select(-...1, -gender) %>%
  
  # 5.3.2 by a pattern
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
  select(-ends_with(".id"))%>%
  
  # 5.3.3 Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  # 5.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))



##################################
### NEW VARIABLE FOR PLOTTING
#################################


# 6.2 Sales by Year and State ----
# Step 1 - Manipulate
sales_by_state_year_tbl <-bike_orderlines_wrangled_city_separated_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, State) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(State, year) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Rotate plot: 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  labs(
    title    = "States revenue by year",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )
sales_by_state_year_tbl%>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = State)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ State) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by State and year",
    subtitle = "Each product category has an upward trend",
    fill = "State" # Changes the legend name
  )
# Rotate plot: 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  labs(
    title    = "States revenue by year",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 7.0 Writing Files ----

# 7.1 Excel ----










library("writexl")
sales_by_state_year_tbl %>%
  write_xlsx("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
sales_by_state_year_tbl%>% 
  write_csv("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
sales_by_state_year_tbl %>% 
  write_rds("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")