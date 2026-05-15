# 1. load liabraries and packages
install.packages("janitor")
library(readr)
library(dplyr)
library(ggplot2)
library(janitor)
library(car)
# 2. import and clean data
data <- read_csv("rack_prices.csv")
# clean column names and handle missing data
data <- clean_names(data)
data$door_type[is.na(data$door_type)] <- "Unknown"
# remove unnecessary columns and quick data checks
data <- subset(
  data,
  select = -c(currency_conversion, rate_to_usd))

colSums(is.na(data))
str(data)
head(data)
summary(data)
glimpse(data)

#Exploratory data analaysis- check for outliers 
# 3. Price distribution by cabinet type

ggplot(data,
       aes(x = cabinet_type,
           y = normalized_price_usd)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Price Distribution by Cabinet Type",
    x = "Cabinet Type",
    y = "Normalized Price (USD)"
  )

# identify outliers
outliers <- by(
  data$normalized_price_usd,
  data$cabinet_type,
  boxplot.stats
)

lapply(outliers, function(x) x$out)

# Country level benchmarking
# 4. country pricing summary
country_summary <- data %>%
  group_by(country) %>%
  summarise(
    products = n(),
    
    avg_price = mean(normalized_price_usd, na.rm = TRUE),
    median_price = median(normalized_price_usd, na.rm = TRUE),
    
    avg_price_per_u =
      mean(normalized_price_per_unit, na.rm = TRUE),
    
    min_price = min(normalized_price_usd, na.rm = TRUE),
    max_price = max(normalized_price_usd, na.rm = TRUE),
    
    sd_price = sd(normalized_price_usd, na.rm = TRUE),
    
    q1 = quantile(normalized_price_usd, 0.25, na.rm = TRUE),
    q3 = quantile(normalized_price_usd, 0.75, na.rm = TRUE),
    
    iqr = IQR(normalized_price_usd, na.rm = TRUE)
  ) %>%
  arrange(desc(median_price))

country_summary

# 5. country price distribution 
ggplot(data,
       aes(x = country,
           y = normalized_price_usd,
           fill = country)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Price Distribution by Country",
    x = "Country",
    y = "Normalized Price (USD)")
  

## Competitor benchmarking
# 6. competitor summary
competitor_summary <- data %>%
  group_by(company) %>%
  summarise(
    products = n(),
    
    avg_price = mean(normalized_price_usd, na.rm = TRUE),
    
    avg_price_per_u =
      mean(normalized_price_per_unit, na.rm = TRUE),
    
    avg_load_capacity =
      mean(load_capacity_kg, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_price))

competitor_summary
## Cabinet Sizes analysis
# 7. create size categories
data <- data %>%
  mutate(
    size_category = case_when(
      cabinet_size_u <= 9  ~ "Small",
      cabinet_size_u <= 18 ~ "Medium",
      cabinet_size_u <= 22 ~ "Large",
      cabinet_size_u <= 42 ~ "Enterprise"))
    
# Size distribution by country
size_distribution <- data %>%
  count(country, size_category)

size_distribution 
## Product Inspection
# 8. cheapest products
data %>%
  arrange(normalized_price_usd) %>%
  select(
    company,
    country,
    cabinet_size_u,
    depth,
    normalized_price_usd
  ) %>%
  head(20)
# 9. most expensive products
data %>%
  arrange(desc(normalized_price_usd)) %>%
  select(
    company,
    country,
    cabinet_size_u,
    depth,
    normalized_price_usd
  ) %>%
  head(20)
## Regression Modelling
# 10. Pricing Model
model <- lm(
  log(normalized_price_usd) ~
    cabinet_size_u +
    depth +
    load_capacity_kg +
    country,
  data = data)

summary(model)
## Model Diagnostics
# 11. Regression Diagnostics
par(mfrow = c(2, 2))
plot(model)

#  Multicollinearity Check
vif(model)

# 12. Price Prediction and Market Segmentation 
data$predicted_price <- exp(predict(model))

# Price efficiency ratio
data$price_ratio <-
  data$normalized_price_usd /
  data$predicted_price

# Market Segmentation 
data <- data %>%
  mutate(
    market_segment = case_when(
      price_ratio > 1.2 ~ "Premium",
      price_ratio < 0.8 ~ "Budget",
      TRUE ~ "Mid-market"))
# Segment counts
data %>%
  count(market_segment)

# Segment distribution by country
 data %>%
 count(country, market_segment)
## export to tableau
write.csv(
  data,
  "clean_rack_prices.csv",
  row.names = FALSE)

write.csv(
  country_summary,
  "country_summary.csv",
  row.names = FALSE)

write.csv(
  competitor_summary,
  "competitor_summary.csv",
  row.names = FALSE)

write.csv(
  size_distribution,
  "size_distribution.csv",
  row.names = FALSE)
