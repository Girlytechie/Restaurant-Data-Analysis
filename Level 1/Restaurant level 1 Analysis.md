:::::::::: {.container-fluid .main-container}
::: {#header}
# R Notebook {#r-notebook .title .toc-ignore}
:::

``` r
Dataset_bA <- read.csv("C:/Users/Mideh/Downloads/Dataset_bA.csv")
```

``` r
# checking the structure and the first 7 rows of the dataset and renaming the dataset
head(Dataset_bA,7)
```

::: {pagedtable="false"}
:::

``` r
structure(Dataset_bA)
```

::: {pagedtable="false"}
:::

``` r
D_A <- Dataset_bA
```

``` r
# checking to see for blanks in the entire dataset and in the Cuisines column
sum(D_A == "", na.rm = TRUE)  
```

    ## [1] 9

``` r
sum(D_A$Cuisines == "")  
```

    ## [1] 9

``` r
#cleaning the dataset by removing the NA
D_A[D_A == ""] <- NA               
D_A_clean <- na.omit(D_A)
```

``` r
# loading necesaary libraries
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
```

::: {#task-1-top-cuisines-and-their-percentages .section .level3}
### Task 1: TOP CUISINES AND THEIR PERCENTAGES

``` r
#Split the 'Cuisines' column to handle multiple cuisines per restaurant
D_A_cuisines <- D_A_clean %>%
  separate_rows(Cuisines, sep = ",") %>%
  mutate(Cuisines = trimws(Cuisines))  # Remove extra spaces
print(D_A_cuisines)
```

    ## # A tibble: 19,710 × 21
    ##    Restaurant.ID Restaurant.Name        Country.Code City       Address Locality
    ##            <int> <chr>                         <int> <chr>      <chr>   <chr>   
    ##  1       6317637 Le Petit Souffle                162 Makati Ci… Third … Century…
    ##  2       6317637 Le Petit Souffle                162 Makati Ci… Third … Century…
    ##  3       6317637 Le Petit Souffle                162 Makati Ci… Third … Century…
    ##  4       6304287 Izakaya Kikufuji                162 Makati Ci… Little… Little …
    ##  5       6300002 Heat - Edsa Shangri-La          162 Mandaluyo… Edsa S… Edsa Sh…
    ##  6       6300002 Heat - Edsa Shangri-La          162 Mandaluyo… Edsa S… Edsa Sh…
    ##  7       6300002 Heat - Edsa Shangri-La          162 Mandaluyo… Edsa S… Edsa Sh…
    ##  8       6300002 Heat - Edsa Shangri-La          162 Mandaluyo… Edsa S… Edsa Sh…
    ##  9       6318506 Ooma                            162 Mandaluyo… Third … SM Mega…
    ## 10       6318506 Ooma                            162 Mandaluyo… Third … SM Mega…
    ## # ℹ 19,700 more rows
    ## # ℹ 15 more variables: Locality.Verbose <chr>, Longitude <dbl>, Latitude <dbl>,
    ## #   Cuisines <chr>, Average.Cost.for.two <int>, Currency <chr>,
    ## #   Has.Table.booking <chr>, Has.Online.delivery <chr>,
    ## #   Is.delivering.now <chr>, Switch.to.order.menu <chr>, Price.range <int>,
    ## #   Aggregate.rating <dbl>, Rating.color <chr>, Rating.text <chr>, Votes <int>

``` r
# Count occurrences of each cuisine
top_cuisines <- D_A_cuisines %>%
  group_by(Cuisines) %>% 
  summarise(restaurant_count = n_distinct(Restaurant.ID)) %>%  # Count distinct Restaurant_ID
  arrange(desc(restaurant_count)) %>%
  top_n(3, restaurant_count)

# Calculate percentage of restaurants serving each top cuisine
total_restaurants <- nrow(D_A_clean)
top_cuisines <- top_cuisines %>%
  mutate(percentage = (restaurant_count / total_restaurants) * 100)

# View results
print(top_cuisines)
```

    ## # A tibble: 3 × 3
    ##   Cuisines     restaurant_count percentage
    ##   <chr>                   <int>      <dbl>
    ## 1 North Indian             3960       41.5
    ## 2 Chinese                  2733       28.6
    ## 3 Fast Food                1986       20.8
:::

::: {#task-2-city-analysis .section .level3}
### Task 2: CITY ANALYSIS

``` r
# Step 1: Identify the City with the Highest Number of Restaurants

city_restaurant_count <- D_A_cuisines %>%
  group_by(City) %>%
  summarise(restaurant_count = n_distinct(Restaurant.ID)) %>%  # Count distinct Restaurant_ID
  arrange(desc(restaurant_count))

# Display the city with the highest number of restaurants
city_with_most_restaurants <- city_restaurant_count[1, ]
print(city_with_most_restaurants)
```

    ## # A tibble: 1 × 2
    ##   City      restaurant_count
    ##   <chr>                <int>
    ## 1 New Delhi             5473

``` r
# Step 2: Calculate the Average Rating for Restaurants in Each City

city_avg_rating <- D_A_cuisines %>%
  group_by(City) %>%
  summarise(average_rating = mean(Aggregate.rating, na.rm = TRUE)) %>%
  arrange(desc(average_rating))

# Display the average rating for each city
print(city_avg_rating)
```

    ## # A tibble: 140 × 2
    ##    City             average_rating
    ##    <chr>                     <dbl>
    ##  1 Inner City                 4.9 
    ##  2 Quezon City                4.8 
    ##  3 Makati City                4.72
    ##  4 Mandaluyong City           4.6 
    ##  5 Beechworth                 4.6 
    ##  6 Pasig City                 4.53
    ##  7 London                     4.53
    ##  8 Taguig City                4.53
    ##  9 Lincoln                    4.5 
    ## 10 Secunderabad               4.5 
    ## # ℹ 130 more rows

``` r
# Step 3: Determine the City with the Highest Average Rating

city_with_highest_avg_rating <- city_avg_rating[1, ]
print(city_with_highest_avg_rating)
```

    ## # A tibble: 1 × 2
    ##   City       average_rating
    ##   <chr>               <dbl>
    ## 1 Inner City            4.9
:::

::: {#task-3-range-distribution .section .level3}
### Task 3: RANGE DISTRIBUTION

``` r
library(ggplot2)
# Step 1: Create a Bar Chart of Price Range Distribution
# Assuming the 'Price range' column is already present in your dataset

ggplot(D_A_cuisines, aes(x = Price.range)) +
  geom_bar(fill = "yellow") +
  labs(title = "Price Range Distribution", x = "Price Range", y = "Number of Restaurants") +
  theme_minimal()
```

![](data:image/png;base64,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ZKWJlWQlPCRJw4C2gjSSlSZWkpXwkCQNA9oK0khWmlhJVsJDkjQRA/pTP9GHaTDSSD3/CwU8vgEMaDUeZhVII1lpYiV5mOUhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKkYUBbQRrJShMryUp4SJKGAW0FaSQrTawkK+EhSRoGtBWkkaw0sZKshIckaRjQVpBGstLESrISHpKk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){role="img"
width="672"}

``` r
# Step 2: Calculate the Percentage of Restaurants in Each Price Range Category
price_range_distribution <- D_A_cuisines %>%
  group_by(Price.range) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Print the price range distribution with percentages
print(price_range_distribution)
```

    ## # A tibble: 4 × 3
    ##   Price.range count percentage
    ##         <int> <int>      <dbl>
    ## 1           1  7428      37.7 
    ## 2           2  7133      36.2 
    ## 3           3  3758      19.1 
    ## 4           4  1391       7.06
:::

::: {#task-4-online-delivery .section .level3}
### Task 4: ONLINE DELIVERY

``` r
# Step 1: Determine the Percentage of Restaurants that Offer Online Delivery

# Calculate the percentage of restaurants that offer online delivery
online_delivery_percentage <- D_A_cuisines %>%
  group_by(Has.Online.delivery) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Print the percentage of restaurants that offer online delivery
print(online_delivery_percentage)
```

    ## # A tibble: 2 × 3
    ##   Has.Online.delivery count percentage
    ##   <chr>               <int>      <dbl>
    ## 1 No                  13909       70.6
    ## 2 Yes                  5801       29.4

``` r
# Step 2: Compare the Average Ratings of Restaurants With and Without Online Delivery

# Calculate the average rating for restaurants with and without online delivery
avg_ratings_online_delivery <- D_A_cuisines %>%
  group_by(Has.Online.delivery) %>%
  summarise(average_rating = mean(Aggregate.rating, na.rm = TRUE))

# Print the comparison of average ratings
print(avg_ratings_online_delivery)
```

    ## # A tibble: 2 × 2
    ##   Has.Online.delivery average_rating
    ##   <chr>                        <dbl>
    ## 1 No                            2.68
    ## 2 Yes                           3.31
:::
::::::::::
