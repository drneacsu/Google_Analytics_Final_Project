# Read in Data

install.packages("readr")
library(readr)

sales_data <- read_csv("Fact_Sales_Data.csv")
View(sales_data)

# Delete empty columns

install.packages("dplyr")
library(dplyr)

workable_table <- select(sales_data, -c(8, 9)) #deleting by index columns 8 and 9

# cleaning headers of columns

install.packages("janitor")
library(janitor)

table_headers_cleaned <- clean_names(workable_table)

# Approaching first question (Did Adidas' total turnover and sales volume exceed Nike's, in NSW, during the period 01/01/2016-01/01/2017?)
# Profitability analysis included.

# Step 1: change column names (total_cost_per_unit should be prices and prices should be total_cost_per_unit)

col_renamed_1 <- table_headers_cleaned %>%
  rename(price_items = total_cost_per_unit)

final_col_renamed <- col_renamed_1 %>%
  rename(cost_items = prices)

# Step 2: Creating a Profit Column (after checking for NULLS/NA in RAW data)

table_profit <- final_col_renamed %>%
  mutate(profit = price_items - cost_items)

# Step 3: Joining with the Dim_Location_Table:

location_data <- read_csv("Dim2_Locations_CSV.csv") # adding the new table into a variable

profit_location_joined <- table_profit%>%           # creating an inner join between the table
  inner_join(location_data, by= c('postcode' = "Postcode"))

# Step 4: New amendments (clean column headers and delete the last column)

joined_table <- select(profit_location_joined, -10)
cleaned_joined_table <- clean_names(joined_table)

View(cleaned_joined_table) # Final cleaned table

# Step 5: Filter by date

install.packages("lubridate")
library(lubridate)

filtered_by_date_table <- cleaned_joined_table %>%
  select(date, chain, postcode, category, total_units, price_items, cost_items, profit, state) %>%
  filter(date >= as.Date("01/01/2016") & date <= as.Date("01/01/2017"))
View(filtered_by_date_table)

# Step 6: Compare total profit from units sold (contribution margin in absolute values) between Nike and Adidas:

profit_by_brand_total <- filtered_by_date_table %>%
  filter(state == 'NSW') %>%
  group_by(chain) %>%
  summarize(sum(profit))
View(profit_by_brand_total)
# Adidas_profit: 1609.79; Nike_profit: 3193.06 (during the period 01/01/2016-/01/01/2017, for NSW)

# Step 7: Illustrate total profit from units sold by brand.

install.packages("ggplot2")
library(ggplot2)

profit_total_frame <- data.frame(brand = c("Adidas", "Nike"),
                                 profit = c(1609.79, 3193.06)
                                 )
profit_total_graph <- ggplot(data = profit_total_frame, aes(x = brand, y = profit, fill = brand)) + geom_bar(stat = "identity", width = 0.6) + geom_text(profit_total_frame, mapping = aes(label = profit), vjust = -0.4) + labs(title = "Profit from goods sold by brand", subtitle = "New South Wales, 01/01/2016-01/01/2017") +xlab("Brand") + ylab("Profit Value")
profit_total_graph

# Step 8: Compare total sales volume

sales_volume_by_brand_total <- filtered_by_date_table %>%
  filter(state == 'NSW') %>%
  group_by(chain) %>%
  summarize(sum(total_units))
View(sales_volume_by_brand_total)
# Adidas_sales_volume: 42148; Nike_sales_volume: 273453 (during the period 01/01/2016-/01/01/2017, for NSW)

# Step 9: Illustrate total sales volume by brand

options(scipen = 100, digits = 4) # Eliminate scientific values without having the illustration biased (global variable)

brand <- c("Adidas", "Nike")
sales_scientific <- c(42148, 273453)

sales_total_frame <- data.frame( brand,
                                 sales_scientific
                                )
sales_total_graph <- ggplot(data = sales_total_frame, aes(x = brand, y = sales_scientific, fill = brand)) + geom_text(sales_total_frame, mapping = aes(label = sales_scientific), vjust = -0.4) + geom_bar(stat = "identity", width = 0.5) + labs(title = "Total number of goods sold by brand", subtitle = "New South Wales, 01/01/2016-01/01/2017") + xlab("Brand") + ylab("Sales Number")
sales_total_graph

# Calculating Gross Profit Margin ((total revenue - cost of goods sold) / total revenue x 100)

gross_profit_margin <- filtered_by_date_table %>%
  filter(state == "NSW") %>%
  group_by(chain)%>%
  summarize((sum(price_items)-sum(cost_items))/ sum(price_items) * 100)
View(gross_profit_margin)
# Adidas: 33.62043%;Nike: 32.84960%

# Illustration the gross profit margin

gross_profit_frame <- data.frame(brand = c("Adidas", "Nike"), 
                                 gross_profit = c(33.62, 32.85)
                                 )
gross_profit_graph <- ggplot(data = gross_profit_frame, aes(x = brand, y = gross_profit, fill = brand)) + geom_text(gross_profit_frame, mapping = aes(label = gross_profit), vjust = -0.4) + geom_bar(stat = "identity", width = 0.5) + labs(title = "Gross Profit Margin by Brand", subtitle = "New South Wales, 01/01/2016-01/01/2017") + xlab("Brand") + ylab("Gross Profit Margin (%)")
gross_profit_graph


# Approaching the second question: Did Adidas' turnover and sales volume for women products exceeded Nike's, in NSW during the period 01/01/2016-01/01/2017?


# Step 1: profit from units sold for women products in NSW during 01/01/2016-01/01/2017

profit_by_brand_women <- filtered_by_date_table %>%
  filter(state == 'NSW', category == 'Womens') %>%
  group_by(chain) %>%
  summarize(sum(profit))
View(profit_by_brand_women)
# Adidas: 194.90 dollars; Nike: 250.53 dollars

# Step 2: profit from units sold for women products illustration

profit_by_brand_women_frame <- data.frame(brand = c("Adidas", "Nike"),
                                          profit_women = c(194.90, 250.53))
profit_by_brand_women_graph <- ggplot(data = profit_by_brand_women_frame, aes(x = brand, y = profit_women, fill = brand)) + geom_text(profit_by_brand_women_frame, mapping = aes(label = profit_women), vjust = -0.4) + geom_bar(stat = "identity", width = 0.5) + labs(title = "Profit from goods sold by brand", subtitle = "Women products, New South Wales, 01/01/2016-01/01/2017") + xlab("Brand") + ylab("Profit Value")
profit_by_brand_women_graph

# Step 3: number of sales for women products in NSW during 01/01/2016-01/01/2017

sales_by_brand_women <- filtered_by_date_table %>%
  filter(state == 'NSW', category == 'Womens') %>%
  group_by(chain) %>%
  summarize(sum(total_units))
View(sales_by_brand_women)
# Adidas: 3400 products sold; Nike: 26178 products sold

# Step 4: number of sales for women products illustration

sales_by_brand_women_frame <- data.frame(brand = c("Adidas", "Nike"),
                                         sales_women = c(3400, 26178))
sales_by_brand_women_graph <- ggplot(data = sales_by_brand_women_frame, aes(x = brand, y = sales_women, fill = brand)) + geom_text(sales_by_brand_women_frame, mapping = aes(label = sales_women), vjust = -0.4) + geom_bar(stat = "identity", width = 0.5) + labs(title = "Number of goods sold by brand", subtitle = "Women products, New South Wales, 01/01/2016-01/01/2017") + xlab("Brand") + ylab("Sales Number")
sales_by_brand_women_graph

# Other comparisons:

# Price Comparison by brand

prices_by_brand <- filtered_by_date_table%>%
  filter(state == "NSW")%>%
  group_by(chain)%>%
  summarize(median(price_items), mean(price_items), mode(price_items), max(price_items), min(price_items))
View(prices_by_brand)

# Revenue Comparison by brand

revenue_by_brand <- filtered_by_date_table%>%
  filter(state == "NSW")%>%
  group_by(chain)%>%
  summarize(revenue = sum(price_items) * sum(total_units))
View(revenue_by_brand)