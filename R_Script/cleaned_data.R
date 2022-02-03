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
# Nike generated almost 2x more profit from goods sold than Adidas

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
# Nike sold approximately 6.5x more products than Adidas

# Step 9: Illustrate total sales volume by brand

options(scipen = 100, digits = 4) # Eliminate scientific values without having the illustration biased (global variable)

brand <- c("Adidas", "Nike")
sales_scientific <- c(42148, 273453)

sales_total_frame <- data.frame( brand,
                                 sales_scientific
                                )
sales_total_graph <- ggplot(data = sales_total_frame, aes(x = brand, y = sales_scientific, fill = brand)) + geom_text(sales_total_frame, mapping = aes(label = sales_scientific), vjust = -0.4) + geom_bar(stat = "identity", width = 0.5) + labs(title = "Total number of goods sold by brand", subtitle = "New South Wales, 01/01/2016-01/01/2017") + xlab("Brand") + ylab("Sales Number")
sales_total_graph

# Step 10: Interpret results

# Even though Nike had a bigger profit for the mentioned time frame in NSW, the volume of sales differ significantly
# between the two brands.

# Having shown that Nike needed to sell around 6.5x more products than Adidas to generate only 2x more profit, can lead to 2 posibilities:
# 1. Nike managed worst their costs of production (labor, fixed costs, cost of raw materials...)
# 2. Nike had smaller prices for their products.

# The question is, did Adidas actually managed better their costs? 
# We could test that by analysing the Gross Profit Margin (profitability ratio)

# Calculating Gross Profit Margin ((total revenue - cost of goods sold) / total revenue x 100)

gross_profit_margin <- filtered_by_date_table %>%
  filter(state == "NSW") %>%
  group_by(chain)%>%
  summarize((sum(price_items)-sum(cost_items))/ sum(price_items) * 100)
View(gross_profit_margin)
# Adidas: 33.62043%;Nike: 32.84960%

# Even though the relative difference of 0.77% between the two results seem to be small, for a better understanding of the facts, absolute values are needed as well
# For every dollar in revenue generated, Adidas would retain with 0.008 more dollars than Nike.
# OR
# For every 1 dollar generated, Nike keeps 0,33 dollars whereas Adidas keeps almost 0,34 dollars.
# For a bigger picture about the facts, if Nike would have gained 48.000 dollars, it would keep 15.840 whereas Adidas would keep 16.320 (480 dollars difference)

# Results: In terms of final profit amounts, Nike performed better than Adidas.
# In terms of profitability, Nike did not perform extremely well as it needed to sell around 6.5x more products than Adidas to generate only 2x more profit
# This phenomenon was produced to a small extent by cost management issues, and perhaps to a larger extent, due to the relatively smaller prices of goods sold.


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

#Step 5: Interpretation

# As in the above, first scenario, the same issue occurs.
# Nike had to sell 7.7x more products than Adidas to only generate 55.6 more dollars.
# As well as earlier, the conclusion for this situation would result in a low profitability of the company:
# 1. The COGS (Cost of Goods Sold) may be too high, or the prices of the products too low in comparison to Adidas' products.

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