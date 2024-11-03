:::::: {.container-fluid .main-container}
::: {#header}
# Restaurant Analysis level 1 {#restaurant-analysis-level-1 .title .toc-ignore}
:::

``` r
library(knitr)
```

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

There were 9 blanks in the dataset which were found in the Cuisines
column

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

Task 1: TOP CUISINES AND THEIR PERCENTAGES

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

Task 2: CITY ANALYSIS

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

Task 3: RANGE DISTRIBUTION

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

Task 4: ONLINE DELIVERY

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

``` r
# Step 1 Analyze the Distribution of Aggregate Ratings

# Create a histogram of the aggregate ratings
ggplot(D_A_cuisines, aes(x = Aggregate.rating)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Aggregate Ratings", x = "Aggregate Rating", y = "Count of Restaurants") +
  theme_minimal()
```

![](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAABUAAAAPACAMAAADDuCPrAAABaFBMVEUAAAAAADoAAGYAOjoAOmYAOpAAZrY6AAA6OgA6Ojo6OmY6OpA6ZmY6ZpA6ZrY6kJA6kLY6kNtNTU1NTW5NTY5Nbo5NbqtNjshmAABmADpmOgBmOjpmOpBmZgBmZjpmZmZmZpBmkGZmkJBmkLZmkNtmtrZmtttmtv9uTU1ubm5ubo5ujqtujshuq+SHzuuOTU2Obk2Obm6Oq6uOq8iOq+SOyOSOyP+QOgCQOjqQZgCQZjqQZmaQZpCQkDqQkGaQkLaQtraQttuQtv+Q29uQ2/+rbk2rbm6rjm6ryOSr5P+2ZgC2Zjq2Zma2kDq2kGa2kJC2tpC2tra2ttu229u22/+2///Ijk3Ijm7IyKvI5P/I///bkDrbkGbbtmbbtpDbtrbbttvb27bb29vb2//b/7bb///kq27kyI7kyKvk///r6+v/tmb/yI7/25D/27b/29v/5Kv/5Mj/5OT//7b//8j//9v//+T///9bImTSAAAACXBIWXMAAB2HAAAdhwGP5fFlAAAgAElEQVR4nO2djZvcVJaf1YPtxgyQpQzGazaYwAZ3whDPbLI4gc3ukOB4aXbI8jGeQA/MArF3TPszdrv+/ZRUH61buqpuqe65OufofZ9nxq52VR3d+zv3RSqp1MUUAAB6UQy9AQAAVkGgAAA9QaAAAD1BoAAAPUGgAAA9QaAAAD1BoAAAPUGgAAA9QaAAAD1BoAAAPUGgAAA9QaAAAD1BoAAAPUGgAAA9QaAAAD0ZRqD3d4sV51/8ePXzJ5eKnU9O/zaLpx9dL4qrHap3rLKR787PhvDc/2j8fL8ofvH7RDUEqAcw2/4XP7598mtSThuAD4YX6Iyddxc/37RGv3+j8S9dBbp4i4Qm+Gy+/bEtK4pX0tToSmSiGqwFUBRnNrwm/bQBOEGFQIvi5fnP29foTJJRTXUQ6Oot0pngcL71zX3N6h/OnmK3LjnRiWrQCKD9RQLTBuCFwQS6tMvRDx8Wp9hb27R6TylQAQEczMTzceTnpXSe7/a5QipON8paALOt/f7yLIFzW70hwCgZXKDzRyeuUZ0C3W/xTjm+79qdJEkPgc5mcL89AQQK0IoGgVYGPUE2agUa3XWe7Zm+MpB4egm0SqDlIACBArSiQqDVofDmRWpKoPOPDdvsKks/gZZTiEABuqJDoOVJ61em9cX64MPny+trXvpq+fzVmY6ZbK8efbZb7LzwSXAS6Yc3Zv++eH6gtoNq97b2FjUl/PBG+fHBS18fv+rqrPTsZ8+9EzsDFDx9dSKmcQ7psDqBdBj+y1F5xdPOy7drm9b4UW1s80kIti76HtPvyyfNtven9YmKvUFbAHWBtr3hctqikxTZsjBCAI/oEGi5fstj+OUaPfrs+Pqar6dNge7PH9QFerB4/kvz9z2VQMsXLq4CWL3q6rJ0c7dr7entAp0XD3eMH1xevu1q05o/qo2tPgmLrdv0HrMfXl2bqMgbtAZwfAjf/oZ1ga5PUnPL1iME8IgOgZbmKH9QW6MrSj2tCfR89eDs7ZpAr6yeP3/j0wi0ulhz/VXn1350zPrTWwW6fP+D2nscv/gX/3axaZEf1cY2Pdb18gPije8xn5z6RDXfoDWA8qmra5Xa3rAWzvokRbZsPUIAj+gRaLnKFmu0XLUvl0eQD39XHJtisbdT7Wr+xe3pw5/q14HOlvLXix2h5foNBVp7i7qmy8PbH+qvmv+oeqO1z1WbT2/5mHMpztkwVrux5YvL/cDvKiG90vKj2tiqv5eT8PCzDS8oz56/O/vR0ee70YkK36AlgIefP79y7IY3DP7rFk5Sc8siEQL4Q4lADwOBHhzvMjXsV3lhsSDrAp2/4Gj5FcpTCPT4sLX+qsWPlp/KBtu8/vS4QFeH7rUPFssXz79u9eBSTTFrP1ob29Lgh8e74WsvCJ9UTWl9lGtvsDaYkDO/b76o8YYrgYaTFNmySIQA/tAq0DMth8XBGfuaQJeCWBrgFALdPz7Crr1q+UaNSzwjT48LdDa4xZsci6smkcOmYg5rAl2Ora6d6rRN7AWHwTaFvmu+QbiNIYuPjje9YU2ga5MUH916hAD+UCnQ6ouQL/72p9oTAoHW1vhSoCuPLZx2skCDczwLE+y37zfFnh4X6PGbrERbf/Fity3yo2Bsdecdlv8cfUEwQZXWVqNsvEGdtZuJvB05zdN4w5pAw0mKbVkkQgB/KBFo+Bno6vTHC/9QOwlzLNBz4Q8Dtx2sVHiCQIPrGxd7XpFXLYk9PSrQ+mmYYv0weLp6UeRH9aL1M0BF9QlF9AWLJ//4zx/tFqHvmm9QZxXA0e9mO72NS7Zib1gXaDhJsS2LRAjgDyUC3Q/PwtdW/5n5d81PEGjgtmEFehiKa3m+//hDyJVA13+0SaDVJQfNF0ynP1RXW1ZsFGg437UADnfDf2x7w40CbW5ZM0IAf+gQ6Pp1oOVV2Kv11zy5vEmgQ++BrotrOS4ZgR5ftdlboJXxVw/a37CjQJsRAvhDh0DLj+TKg/DAUg//9xu79X04yUP49lctiT09JtD6tUvrH03MOfUhfHjeJ/aCxccF5198+8v/e6kp0PbvtwYBHBTBp7Ytb9jtEH5OECGAP3QI9GB9b2fJ0fIqwpMEuuEk0n5coC0nkdoEetqTSAfBzt7i2qf6ixdbG/lRULTx1rEXHCyu2pzGTyK17/iFt7Mrd1avLre+9Q1bBRodyvF2cyEo+EWFQFd3Y4qr7TQCXb7dcgdwP9iXi17GdBC9jKlNoLGnRyS1dn58uWmnv4ypdoJ7NUUH1T9HXlCrf7j+36DmG9Ro3k+wcWFr4w1bBRrZsliEAP7QINDj+4E21bbcozlJoMtr3JcuPahfgtnlQvpWgZ7uQvrDtePVw+NvRL673Ob1q8+fXIoItHaDucV/FJovWLtSP/Rd8w1aA1jN0cY3bBdoZCiRCAH8MbhAH/6pOtcw312pqe1M9XvOyq9NVsv4eBW2CLT64uD3qy8XlntCZ/7n8kxG9YLVW9S/UtP4KmerQE/3Vc716+9Loyw+HKi+7FhtYfjNzNqP1i5+L858tfhS5bn4C5ZfvKy+rbk+Uc03iAawnJrlLnjrG7YLNLJlkQgB/KHkdyKF382cHjT+bXFS+WqbQK+svVXtJPTbu8cCrd5iWaV+xWbLLUjqNJ/eFOiTS+unbvZXp5HWhxT5Ub1o/TT6L34ff0E4j/Wz71djbxAE0DiLV79rS+wNNwg0PpS1nwD4Q4VAVxcKrg4X/7h6wvI3dh4uV2LLWfiDtbX6ZHmDtav3FwJdvcXxQenqmp2XG0KMfIO78fSmQA8arlp+XLm6Puil/7Z8UfNHQdGj1WVAy/vBRV+wGOa/v17b915czN54g3oAjW8yLD/ObHvDDQKNja4ZIYA7hhfo+beO77h7/Ennw8+rW6a9cPwLy78rr+/+i/bLmL5/I3bz4dkPVgJdvkXtXP/35WU2z9VvqLxBoI2nNwS6OmEV/qhyVbU9swHVvmPZ+NFa0R+qWxu/cHwdevM95rdMLqdp9drlRMXeYElDoKuD+PY33CTQ2OiaEQJ4YxiBjpnIBZqn/sX2vV+QDb1bBiAAAs1Cba8tsiPXuPz1tO+hBL1bBiALAs1C7bPRg8aFVmsX33d4DyXo3TIAWRBoFqqrqsrPen/4cHmiK/Kjzu+hBL1bBiALAs1D/VcEHV8Dtf6jzu+hBL1bBiAKAs3D8SVFq2ugIj/q/B5K0LtlAKIg0Fwsfk16/ebvkR91fg8l6N0yAEEQKABATxAoAEBPECgAQE8QKABATxAoAEBPECgAQE8QKABATxAoAEBPECgAQE8QKABATxAoAEBPECgAQE8QKABATxAoAEBPPAv055+H3oJUOBkIgWiDRLYFgVrAyUAIRBsksi0I1AJOBkIg2iCRbUGgFnAyEALRBolsCwK1gJOBEIg2SGRbEKgFnAyEQLRBItuCQC3gZCAEog0S2RYEagEnAyEQbZDItiBQCzgZCIFog0S2BYFawMlACEQbJLItCNQCTgZCINogkW1BoBZwMhAC0QaJbAsCtYCTgRCINkhkWxCoBZwMhEC0QSLbgkAt4GQgBKINEtkWBGoBJwMhEG2QyLYgUAs4GQiBaINEtgWBWsDJQAhEGySyLQjUAk4GQiDaIJFtQaAWcDIQAtEGiWwLArWAk4EQiDZIZFsQqAWcDIRAtEEi24JALeBkIASiDRLZFgRqAScDIRBtkMi2IFALOBkIgWiDRLYFgVrAyUAIRBsksi0I1AJOBkIg2iCRbUGgFnAyEALRBolsCwK1gJOBEIg2SGRbEKgFnAyEQLRBItuCQC3gZCAEog0S2RYEagEnAyEQbZDItiBQCzgZCIFog0S2BYFawMlACEQbJLItCNQCTgZCINogkW1BoBZwMhAC0QaJbAsCtYCTgYw5kCILnccx4kTSgEAt4GQgIw4kjz8RaHYQqAWcDGTEgRTFf5IHgeYHgVrAyUBGHAgCFQaBpofuUMaIA0GgwiDQ9NAdyhhxIAhUGASaHrpDGSMOBIEKg0DTQ3coY8SBIFBhEGh66A5ljDgQBCoMAk0P3aGMEQeCQIVBoOmhO5Qx4kAQqDAIND10hzJGHAgCFQaBpofuUMaIA0GgwiDQ9NAdyhhxIAhUGASaHrpDGSMOBIEKg0DTQ3coY8SBIFBhEGh66A5ljDgQBCoMAk0P3aGMEQeCQIVBoOmhO5Qx4kAQqDAIND10hzJGHAgCFQaBpofuUMaIA0GgwiDQ9NAdyhhxIAhUGASaHrpDGSMOBIEKg0DTQ3coY8SBIFBhLAn06bVJxavflo+e3dybTN77Zv5P7Q8GgO5QxogDQaDCWBLoo72aQBc2ncu0/cEQ0B3KGHEgCFQYSwK9N3n9+MGtycVvpo9vTC7e3fhgCOgOZYw4EAQqjCWB3pq8v/r7o73FfuiFTzc9GAS6QxkjDgSBCmNIoM9u1JR4Z7E3eqeUavuDQaA7lDHiQBCoMIYE+vTaxf/z68nkP1Znh25NPqh+WB3Wtz8YBLpDGSMOBIEKY0igy3NIpSBXe6OP9i7ebX8QvsHPACMjk0CHHuYI2F6g9yaTX92d/r+bk5khESjAKUCgXtheoMsPN8tzSTVNvvpt+4PuRVLA8YkyRhwIh/DCGDqEX3Jvsmmnc8MeaC7oDmWMOBAEKoxBga7tZyJQOZwMZMSBIFBhTAp0ZkbOwufAyUBGHAgCFcaOQJ/dqJtxeZXn4tLPtgeDQHcoY8SBIFBh7Ah0tmv5evnHXKR8EykHTgYy4kAQqDCGBPpor7yM6fGvqy+5zzT62uob7+0PBoHuUMaIA0GgwhgS6OygfH6bpeqrSI/r91xqfzAEdIcyRhwIAhXGkkCnj38zmVz41WLH8vHNmSXfO/HBANAdyhhxIAhUGFMCNQLdoYwRB4JAhUGg6aE7lDHiQBCoMAg0PXSHMkYcCAIVBoGmh+5QxogDQaDCIND00B3KGHEgCFQYBJoeukMZIw4EgQqDQNNDdyhjxIEgUGEQaHroDmWMOBAEKgwCTQ/doYwRB4JAhUGg6aE7lDHiQBCoMAg0PXSHMkYcCAIVBoGmh+5QxogDQaDCIND00B3KGHEgCFQYBJoeukMZIw4EgQqDQNNDdyhjxIEgUGEQaHroDmWMOBAEKgwCTQ/doYwRB4JAhUGg6aE7lDHiQBCoMAg0PXSHMkYcCAIVBoGmh+5QxogDQaDCIND00B3KGHEgCFQYBJoeukMZIw4EgQqDQNNDdyhjxIEgUGEQaHroDmWMOBAEKgwCTQ/doYwRB4JAhUGg6aE7lDHiQBCoMAg0PXSHMkYcCAIVBoGmh+5QxogDQaDCIND00B3KGHEgCFQYBJoeukMZIw4EgQqDQNNDdyhjxIEgUGEQaHroDmWMOBAEKgwCTQ/doYwRB4JAhUGg6aE7lDHiQBCoMAg0PXSHMkYcCAIVBoGmh+5QxogDQaDCIND00B3KGHEgCFQYBJoeukMZIw4EgQqDQNNDdyhjxIEgUGEQaHroDmWMOBAEKgwCTQ/doYwRB4JAhUGg6aE7lDHiQBCoMAg0PXSHMkYcCAIVBoGmh+5QxogDQaDCIND00B3KGHEgCFQYBJoeukMZIw4EgQqDQNNDdyhjxIEgUGEQaHroDmWMOBAEKgwCTQ/doYwRB4JAhUGg6aE7lDHiQBCoMAg0PXSHMkYcCAIVBoGmh+5QxogDQaDCIND00B3KGHEgCFQYBJoeukMZIw4EgQqDQNNDdyhjxIEgUGEQaHroDmWMOBAEKgwCTQ/doYwRB4JAhUGg6aE7lDHiQBCoMAg0PXSHMkYcCAIVBoGmh+5QxogDQaDCjEmgPwOMjEwCHXqYI0CBQHPBf16VMeJA2AMVZkx7oLmgO5Qx4kAQqDAIND10hzJGHAgCFQaBpofuUMaIA0GgwiDQ9NAdyhhxIAhUGASaHrpDGSMOBIEKg0DTQ3coY8SBIFBhEGh66A5ljDgQBCoMAk0P3aGMEQeCQIVBoOmhO5Qx4kAQqDAIND10hzJGHAgCFQaBpofuUMaIA0GgwiDQ9NAdyhhxIAhUGASaHrpDGSMOBIEKg0DTQ3coY8SBIFBhEGh66A5ljDgQBCoMAk0P3aGMEQeCQIVBoOmhO5Qx4kAQqDAIND10hzJGHAgCFQaBpofuUMaIA0GgwiDQ9NAdyhhxIAhUGASaHrpDGSMOBIEKg0DTQ3coY8SBIFBhEGh66A5ljDgQBCoMAk0P3aGMEQeCQIVBoOmhO5Qx4kAQqDAIND10hzJGHAgCFQaBpofuUMaIA0GgwiDQ9NAdyhhxIAhUGASaHrpDGSMOBIEKg0DTQ3coY8SBIFBhEGh66A5ljDgQBCoMAk0P3aGMEQeCQIVBoOmhO5ShNZAiBwhUFASaHrpDGUoDyeJPBCoLAk0P3aEMpYHkkBsCFQaBpofuUIbSQBCoAxBoeugOZSgNBIE6AIGmh+5QhtJAEKgDEGh66A5lKA0EgToAgaaH7lCG0kAQqAMQaHroDmUoDQSBOgCBpofuUIbSQBCoAxBoeugOZSgNBIE6AIGmh+5QhtJAEKgDEGh66A5lKA0EgToAgaaH7lCG0kAQqAMQaHroDmUoDQSBOgCBpofuUIbSQBCoAxBoeugOZSgNBIE6AIGmh+5QhtJAEKgDEGh66A5lKA0EgToAgaaH7lCG0kAQqAMQaHroDmUoDQSBOgCBpofuUIbSQBCoAxBoeugOZSgNBIE6AIGmh+5QhtJAEKgDEGh66A5lKA0EgToAgaaH7lCG0kAQqAMQaHroDmUoDQSBOgCBpofuUIbSQBCoAxBoeugOZSgNBIE6AIGmh+5QhtJAEKgDzAn00d7Fu9Vfnt3cm0ze+2Z6woMBoDuUoTQQBOoAawJ9dmMyF+jTa5OSV7/d/GAI6A5lKA0EgTrAmkDvTBYCvTW5+M308UKn7Q+GgO5QhtJAEKgDjAn00d5CoI/2qj3Mp9cufLrpwSDQHcpQGggCdYAtgc4O4P9m/hnoncnr1U/uTN7f9GAQ6A5lKA0EgTrAlkBvTV5fnES6Nfmg+sm9UpftDwaB7lCG0kAQqANMCfTe7PB9LtBnNxYH6OXD9gfhy38GUIQjgQ49lSMggUCrjzURKDgBgcLpSSDQW+Wnmg2Bvvpt+4MeRRLA8YkylAbiSKBdh640kR4YOoS/U51/32IPNBd0hzKUBoJAHWBHoI/2KjMi0Iw4GYjSQBCoA+wI9M5kxezYnLPwOXAyEKWBIFAHWBXo8irPxaWfbQ8Gge5QhtJAEKgD7Ah0weLInG8i5cDJQJQGgkAdYFWgz25MXlt94739wSDQHcpQGggCdYBVgU4f1++51P5gCOgOZSgNBIE6wKxAp49vziz53mIvs/3BANAdylAaCAJ1gDmBGoDuUIbSQBCoAxBoeugOZSgNBIE6AIGmh+5QhtJAEKgDEGh66A5lKA0EgToAgaaH7lCG0kAQqAMQaHroDmUoDQSBOgCBpofuUIbSQBCoAxBoeugOZSgNBIE6AIGmh+5QhtJAEKgDEGh66A5lKA0EgToAgaaH7lCG0kAQqAMQaHroDmUoDQSBOgCBpofuUIbSQBCoAxBoeugOZSgNBIE6AIGmh+5QhtJAEKgDEGh66A5lKA0EgToAgaaH7lCG0kAQqAMQaHroDmUoDQSBOgCBpofuUIbSQBCoAxBoeugOZSgNBIE6AIGmh+5QhtJAEKgDEGh66A5lKA0EgToAgaaH7lCG0kAQqAMQaHroDmUoDQSBOgCBpofuUIbSQBCoAxBoeugOZSgNBIE6AIGmh+5QhtJAEKgDEGh66A5lKA0EgToAgaaH7lCG0kAQqAMQaHroDmUoDQSBOgCBpofuUIbSQBCoAxBoeugOZSgNBIE6AIGmh+5QhtJAEKgDEGh66A5lKA3EkUBzMHRccRBoepSu1x44GYjSQBAoAu2N0ulIgtL12gMnA1EaiCOB5qgxdFxxEGh6lK7XHjgZiNJAEGinGkPHFQeBpkfpeu2Bk4EoDQSBdqoxdFxxEGh6lK7XHjgZiNJAEGinGkPHFQeBpkfpeu2Bk4EoDQSBdqoxdFxxEGh6lK7XHjgZiNJAEGinGkPHFQeBpkfpeu2Bk4EoDQSBdqoxdFxxEGh6lK7XHjgZiNJAEGinGkPHFQeBpkfpeu2Bk4EoDQSBdqoxdFxxEGh6lK7XHjgZiNJAEGinGkPHFQeBpkfpeu2Bk4EoDQSBdqoxdFxxEGh6lK7XHjgZiNJAEGinGkPHFQeBpkfpeu2Bk4EoDQSBdqoxdFxxEGh6lK7XHjgZiNJAEGinGkPHFQeBpkfpeu2Bk4EoDQSBdqoxdFxxEGh6lK7XHjgZiNJAEGinGkPHFQeBpkfpeu2Bk4EoDQSBdqoxdFxxlAr06E9ffJlnQwRQul574GQgSgNBoJ1qDB1XHHUCffB3t6fTJ5eLojjzSc4NSojS9doDJwNRGggC7VRj6LjiaBPoQfGL30+n+9UvQSn/ZhGl67UHTgaiNBAE2qnG0HHFUSbQw0qb93eLs7cfXCpeybtNqVC6XnvgZCBKA0GgnWoMHVccZQLdn5mz1OjOJ+X/l383iNL12gMnA1EaCALtVGPouOLoEujR9dKcC40+uWT0GF7peu2Bk4EoDQSBdqoxdFxxdAl07swnl4pzUwSqACcDURoIAu1UY+i44mgU6P3d4uoUgSrAyUCUBoJAO9UYOq44ugQ6P4Q/qD4C5TPQ4XEyEKWBINBONYaOK44ugU73i3Pl6ffSnOnPwv8MoAgE2qnG0HENzOkEelhdADo7gj/6sJjvhxpE6Q5PD5wMRGkgCLRTjaHjiqNsD3R2+D7jXHUiaedq1i1Kh9L12gMnA1EaCALtVGPouOJoE+j0wUdvfTz748lfvvR1xs1JitL12gMnA1EaCALtVGPouOKoE6gDlK7XHjgZiNJAEGinGkPHFUeXQI8+euv4xPv9N3/JWfhhcTIQpYEg0E41ho4rji6BBpd+ch3o4DgZiNJAEGinGkPHFUexQO/v6hBokYOhBxlHp3g6g0DtF2GJhDSm48mlplVUXEifxZ90hyQI1H4RlkhIczoOm1ZRcR0T3WEeBGq/CEskpDkdR/905cqbuzsvXlny9lcDbFcTusM8CNR+EZZIyCk+A1UC3WEeBGq/CEsk5BSXMSmB7jAPArVfhCUSonQ6ItAd5kGg9ouwREI2TcePS37KtjkboDvMg0DtF2GJhLRNx4MPa2fhVXwgSneYB4HaL8ISCWmZjvBqUAQ6MDrF0xkEar8ISySkZToOiuLM218s+VLDGSW6wzwI1H4RlkhI26/0qH6dnCroDvMgUPtFWCIhbdeB6rsLPd1hHgRqvwhLJIQL6emObCBQ+0VYIiGbfiunLugO8yBQ+0VYIiGtJ5ES/ybO7aE7zINA7RdhiYS0TMdsF/TdrNtxMnSHeRCo/SIskZC278K/WRTHN2RS8cV4usM8CNR+EZZISNtJpIIL6RWhUzydQaD2i7BEQhAo3ZENBGq/CEskROl0RKA7zINA7RdhiYQonY4IdId5EKj9IiyREKXTEYHuMA8CtV+EJRKyYTqOlrcD/f6v+Ax0WHSKpzMI1H4RlkgI9wOlO7KBQO0XYYmEnOp+oGcR6LDoFE9nEKj9IiyRkPb7ge68WP5y4zd3CyXfSaI7zINA7RdhiYS03g/07O3y/6+WLj2r4YtICNQ+CNR+EZZIyMb7gc5vKbJfanR46A7zIFD7RVgiIRvvB3pY3Zf+UMfd6ekO8yBQ+0VYIiEnCLQ8en9yScUxPN1hHgRqvwhLJGTjDZXv75YeVXJ7errDPAjUfhGWSEjLdOxXn37OPwqda3Rw6A7zIFD7RVgiIS3TcX+3eOnr6dH10qP7Ok7D0x3mQaD2i7BEQtqmY7/6/tFhUezsFjp+vQfdYR4Ear8ISySkdTr+WB64H+1XX0TSsAOKQO2DQO0XYYmEbJiOf5l58+i78+ffUeFPBGofBGq/CEskROl0RKA7zINA7RdhiYS0nYV/6eu823EydId5EKj9IiyRkNbfiaTi65t16A7zIFD7RVgiIRu/iaQKusM8CNR+EZZIyMZvIqmC7jAPArVfhCUS0no/0LPaPgSlO8yDQO0XYYmEtEzHw98VxXMvXlnwloYLmegO8yBQ+0VYIiGtJ5EKfieSHnSKpzMI1H4RlkgIAqU7soFA7RdhiYQonY4IdId5EKj9IiyREKXTEYHuMA8CtV+EJRKidDoi0B3mQaD2i7BEQtrOwv9Y56e82xSH7jAPArVfhCUSwkkkuiMbCNR+EZZICAKlO7KBQO0XYYmEtHyV809fLPj7y8XOb7/kQvph0SmeziBQ+0VYIiEnT8f9Xe5IPzQ6xdMZBGq/CEsk5BTTccDvRBoaneLpDAK1X4QlEnKK6VCyC0p3mAeB2i/CEgk5xXQouTko3WEeBGq/CEsk5FR7oOsC/fOvJ5ML//nu/MGzm3uTyXvfnPRg+y2lO6yDQO0XYYmEnDwdR/vrv9f4zqTitW/LB0+vVQ9ePeFBgi2lO6yDQO0XYYmEtFzG9NHyVqBX3twt1k4iPdq78LfT6eNfT14vH92aXPxm+vjG5OLdjQ8SbCndYR0Ear8ISyTkNBfSr+2A3pq8X/7xaK/ct5z//2xv88Knmx6k2FK6wzoI1H4RlkjIyQJ97p34Ofin10pB3pnvh87+fH/TgxRbSndYB4HaL8ISCek/HY/2ymPzW5MPqkf3Sl22P0gA3WEeBGq/CEskpPd0/OteKchnNxYH6KVO2x+EL/25F3m6o9+2gWUQaKcaQ8c1MGkEemsyufCPUwQKDkCgnWoMHdfAnF6gR8vbgX7/V+sX0j/7X/9hb3LhvwQCffXb9gedBR2B4xPzcAhvvwhLJKRtOh58eNLt7P5cHsP32gPtuaV0h3UQqP0iLJGQlulYu44p+lXOe5NNzkSgCdEpns4gUPtFWCIhLdNxUJHDRiQAACAASURBVBQ7L765W/6v2Hk3/pzKjJyFz4FO8XQGgdovwhIJafkm0vXy6vnZ/18tXRpeSP/sxsKMlUCXV3kuLv1se5BiS+kO6yBQ+0VYIiFtF9LvfDJd3gl0v9RojVuLXcrqT76JlAOd4ukMArVfhCUS0ibQ6rzRYXFu9f/HPNqb/Oru9NkfJqUZZ/ujr62+8d7+IMWW0h3WQaD2i7BEQk4QaHn0/uTS2pfh783vxnShOpJ/XL/nUvuDBFtKd1gHgdovwhIJafsMtDqEn98JtHlD5ce/melzeaPPxzdnlnzv7kkPtt9SusM6CNR+EZZISMt07Feffs4/Cm3eUHkQ6A7zIFD7RVgiIS3TcX+3eOnr6dH10qONGyoPA91hHgRqvwhLJKRtOvar7x8dFsVO44bKA0F3mAeB2i/CEglpnY4/lgfuR/uRGyoPBN1hHgRqvwhLJGTDdPzLzJtH350/33JD5dzQHeZBoPaLsERClE5HBLrDPAjUfhGWSEjbL5V763i38/6bv9SwD0p3mAeB2i/CEgnZeCF95MFw0B3mQaD2i7BEQk4hUK4DHRyd4ukMArVfhCUS0piO8E6g0d9rPAx0h3kQqP0iLJGQ5nQcNgV6NfLC7NAd5kGg9ouwREKa03H0T1eulLdSvrLk7a8G2K4mdId5EKj9IiyRkFN8BqoEusM8CNR+EZZIyCkuY1IC3WEeBGq/CEskROl0RKA7zINA7RdhiYScMB1Hf/riyzwbciJ0h3kQqP0iLJGQ1ul48HflzegvF0Vx5pOcG9QK3WEeBGq/CEskpG06Dsq72U2rmzEVOk4o0R3mQaD2i7BEQlqm47DS5v3d4uztB5e4H+jQ6BRPZxCo/SIskZDWX+lRfvvosCh/pcch30QaGp3i6QwCtV+EJRKy8ZfK7S9+K6eKY3i6wzwI1H4RlkjIpgvpn1yqfiM8Ah0cneLpDAK1X4QlErJJoPd3q2/BI9DB0SmeziBQ+0VYIiGbDuEPqo9A+Qx0eHSKpzMI1H4RlkhI60mkc+Xp99KcnIUfHp3i6QwCtV+EJRLSfhnT/D52Rx8W8/3QwaE7zINA7RdhiYS0X0g/41x1ImlHxe1AEah9EKj9IiyRkPavcn701sezP5785UtfZ9ycDdAd5kGg9ouwREKUTkcEusM8CNR+EZZIyMbpOPop12acArrDPAjUfhGWSEj7dHz/Rvl9+Cd/+Y6Ga5imCNQBCNR+EZZISNt0HH02vw/Tk0vFGQ2X0SNQByBQ+0VYIiFt07FfFGf+3e4vfn/0X5X8VmMEah8Ear8ISySk/TrQdxff4fxul19rPDQ6xdMZBGq/CEskpPWbSK+svgR/UN1SZHDoDvMgUPtFWCIhm74LvxDo/V1uJjIwOsXTGQRqvwhLJGTz7ewqc3I3psHRKZ7OIFD7RVgiIQiU7sgGArVfhCUS0nYIX544WpiT29kNjk7xdAaB2i/CEglpmY6D+Y1ESoHOZMpJpIHRKZ7OIFD7RVgiIS3TcX+3ePl2JdAHl7md3eDoFE9nEKj9IiyRkE23szu/u/Pi87M/VdxPGYHaB4HaL8ISCWmdjj/uFgt0+BOB2geB2i/CEglpn46Hn5+f2fM5LbcDRaD2QaD2i7BEQpRORwS6wzwI1H4RlkjIqabjX7iMaVh0iqczCNR+EZZIyCmm4+gzLqQfGJ3i6QwCtV+EJRISm47v3zh//oWPl4/uXy4Q6MDoFE9nEKj9IiyRkOZ0PLg8P/k+//pRdWNlBDowOsXTGQRqvwhLJKQxHU8uLS9fKg1a2fQMF9IPjE7xdAaB2i/CEglpTEd5Bf278z+ull9IKoqXNZxCQqAOQKD2i7BEQtan4+j64sr5/aI4V/pTx+7nFIE6AIHaL8ISCYkIdP7V95k8z1xWs/s5RaAOQKD2i7BEQtan48mlxSmj6rPQnXcH2KQW6A7zIFD7RVgiIRsFquM2TAvoDvMgUPtFWCIhGwWq5DYic+gO8yBQ+0VYIiGbBKri8s8VdId5EKj9IiyREARKd2QDgdovwhIJQaB0RzYQqP0iLJEQBEp3ZAOB2i/CEglBoHRHNhCo/SIskZCIQJskNunPvcjTHf22DSyDQDvVGDqugVEg0H7wn1fzsAdqvwhLJKTxVc6PrjR5S8P3OekO8yBQ+0VYIiFKpyMC3WEeBGq/CEskROl0RKA7zINA7RdhiYQonY4IdId5EKj9IiyREKXTEYHuMA8CtV+EJRKidDoi0B3mQaD2i7BEQpRORwS6wzwI1H4RlkiI0umIQHeYB4HaL8ISCVE6HRHoDvMgUPtFWCIhSqcjAt1hHgRqvwhLJKT5S+Wq3+Px8KcBtmUzdId5EKj9IiyRkOZ34csvvs//Xxd0h3kQqP0iLJGQpkDLPVAEqgud4ukMArVfhCUSEvm98Ge//PGHS7/46sdjVBzP0x3mQaD2i7BEQhrTccDt7PShUzydQaD2i7BEQhrTcfQZAlWHTvF0BoHaL8ISCYlMx9GPX/zz7s5vvzjmS+4HOiw6xdMZBGq/CEskJD4dnETShU7xdAaB2i/CEgmJT8fRRypuQh9Ad5gHgdovwhIJUTodEegO8yBQ+0VYIiHt0/Hw8/NFsXP+HRXXME0RqAMQqP0iLJGQ1uk4vpzplZzb0w7dYR4Ear8ISySkbTpKfz734pU3n1djULrDPAjUfhGWSEjLdNzfLc5+Xf3twfWiur3I4NAd5kGg9ouwREJapmO/OLs8DX90vTiXa2s2QXeYB4HaL8ISCWm5jOl6ba/z/u5ZDdc00R3mQaD2i7BEQk5xIb2Sq+rpDvMgUPtFWCIhCJTuyAYCtV+EJRLSdghfXF09OCw4hB8YneLpDAK1X4QlEsJJJLojGwjUfhGWSEj7ZUxnvqr+9sNlLmMaHJ3i6QwCtV+EJRKy6UL64vz583q+ikR3mAeB2i/CEglpnY7vdhff5Nx5N+f2tEN3mAeB2i/CEglpn46j79+c7YG++LGGE0gldId5EKj9IiyREKXTEYHuMA8CtV+EJRKidDoi0B3mQaD2i7BEQpRORwS6wzwI1H4RlkiI0umIQHeYB4HaL8ISCVE6HRHoDvMgUPtFWCIhSqcjAt1hHgRqvwhLJETpdESgO8yDQO0XYYmEKJ2OCHSHeRCo/SIskRCl0xGB7jAPArVfhCUS0nI7u4/eOv4C0v03f6nh20h0h3kQqP0iLJEQbqhMd2QDgdovwhIJOYVA7+8i0IHRKZ7OIFD7RVgiIY3peHKpaMAd6QdGp3g6g0DtF2GJhDSn47Ap0KuRF2aH7jAPArVfhCUS0pyOo3+6cuXN3Z0Xryx5+6sBtqsJ3WEeBGq/CEsk5BSfgSqB7jAPArVfhCUScorLmJRAd5gHgdovwhIJUTodEegO8yBQ+0VYIiGbpuPHJT9l25wN0B3mQaD2i7BEQtqm48GHtbPwKj4QpTvMg0DtF2GJhLRMR3g1KAIdGJ3i6QwCtV+EJRLSMh0HRXHm7S+WfKnhjBLdYR4Ear8ISySk5Sz89eJc3u04GbrDPAjUfhGWSEjbdaA7n2TekBOhO8yDQO0XYYmEcCE93ZENBGq/CEskpO0Qnj1QTegUT2cQqP0iLJGQ1pNIr+TdjpOhO8yDQO0XYYmEtEzHbBf03fYX/fk3k8mF976ZP3h2c28yOfnB9ltKd1gHgdovwhIJafsu/JtFcXxDprUvxv9hUnHh0/LB02vVg1e/3fwgwZbSHdZBoPaLsERC2k4iFe0X0t+bXPjb6fTxjbkab00uflM+uHh344MEW0p3WAeB2i/CEgnpLtBnNyYflH/OdjBnfz7aqzT69Fq5P9r+IMWW0h3WQaD2i7BEQrpPx9Nri4PyW5P3p9M7k9erB3c2P0gA3WEeBGq/CEskZIvpqAR6a747Ojuuf33TgwTQHeZBoPaLsERC+k9HdWz+7MbiAP3R3sW77Q/CV/7cizzd0W/bwDIItFONoeMamNMJ9OGPdaL3A60O0REomAeBdqoxdFwDcyqBbjwLP+dedRlTTZOvftv+IG7pbnB8Yh4O4e0XYYmE9BXovb0L5UecvfZAe24p3WEdBGq/CEskpOVC+j8tbwX695eLnd827wd6Z3EZPQLNgk7xdAaB2i/CEgk5eTru755t+PMPk+XFnZyFz4FO8XQGgdovwhIJOcV0NG4s8uzW5LXlx5rLqzwXl362PUixpXSHdRCo/SIskZBTTEdjF/RW7duZfBMpBzrF0xkEar8ISyTkFNOxfnflO/Vvtz+7MXlt9Y339gcptpTusA4CtV+EJRJyqj3QQKCLmyyVvD57+Lh+z6X2Bwm2lO6wDgK1X4QlEnLydBztF8Eh/L1JINDp45uzv7232Mtsf7D9ltId1kGg9ouwRELa7ge6vBXolTd3Cx13p6c7zINA7RdhiYSc5kL65mVMQ0B3mAeB2i/CEgk5WaDPvaPCnwjUPgjUfhGWSIjS6YhAd5gHgdovwhIJUTodEegO8yBQ+0VYIiFKpyMC3WEeBGq/CEskpH06Hn5+vih2zr8TvRnoANAd5kGg9ouwREJap+NgdRZJxUVMCNQBCNR+EZZISNt0lP587sUrbz6vxqB0h3kQqP0iLJGQlum4v1uc/br624Prxc4nGTeoFbrDPAjUfhGWSEjLdNS+vnl0vTiXa2s2QXeYB4HaL8ISCWn5Kuf12l5n7I7KA0B3mAeB2i/CEglp+yZS7QZM67ezGwi6wzwI1H4RlkgIAqU7soFA7RdhiYS0HcIXV1cPDnXcTYTuMA8CtV+EJRLCSSS6IxsI1H4RlkhI+2VMZ76q/vbDZS5jGhyd4ukMArVfhCUSsulC+uL8+fN6vopEd5gHgdovwhIJaZ2O73YX3+TceTfn9rRDd5gHgdovwhIJaZ+Oo+/fnO2BvvixhhNIJXSHeRCo/SIskRCl0xGB7jAPArVfhCUSonQ6ItAd5kGg9ouwRELi0/FwcRPQ+y+oOYJHoPZBoPaLsERCYtPx4PLyxPuBnnNICNQ+CNR+EZZISGQ6yvPvi0vnf6fnKiYEah8Ear8ISySkOR2HM2cuLqKfTo8+mz262njOENAd5kGg9ouwREIa01H+Svj6PufsKF7FvUQQqH0QqP0iLJGQxnTMhBncO+ToupKDeLrDPAjUfhGWSMj6dJS+DA/ZDwvuxjQ0OsXTGQRqvwhLJGR9OmZH8Gv3Drm/q+MYnu4wDwK1X4QlEhIR6Joumz8ZBrrDPAjUfhGWSAgCpTuygUDtF2GJhEQ+A20ewvMZ6MDoFE9nEKj9IiyRkMZ07K+fdD8ouCP90OgUT2cQqP0iLJGQxnSsn3TnMiYF6BRPZxCo/SIskZATL6Tf50L64dEpns4gUPtFWCIh8a9yvrzcB33woZovw9Md5kGg9ouwREIi01H9OqQz//DFF198frn8q4pPQBGoAxCo/SIskZDYdPxx+euQql+J9NfZtykO3WEeBGq/CEskJDod1YH7XJ8v/5R7i9qgO8yDQO0XYYmEtE3HD//80ZW3v1BjzykCdQACtV+EJRKidDoi0B3mQaD2i7BEQpRORwS6wzwI1H4RlkiI0umIQHeYB4HaL8ISCVE6HRHoDvMgUPtFWCIhSqcjAt1hHgRqvwhLJETpdESgO8yDQO0XYYmEKJ2OCHSHeRCo/SIskRCl0xGB7jAPArVfhCUSonQ6ItAd5kGg9ouwREKUTkcEusM8CNR+EZZIiNLpiEB3mAeB2i/CEglROh0R6A7zIFD7RVgiIUqnIwLdYR4Ear8ISyRE6XREoDvMg0DtF2GJhCidjgh0h3kQqP0iLJEQpdMRge4wTx+BFjnI0VnyNRDoACidjgh0h3l6CDSLPxFolxoCjZEABHoSdId5egk0R+o+aiDQAVA6HRHoDvMgUPtFWCIhSqcjAt1hHgRqvwhLJGSI6fi5F3m6o9+2gRRe5OZIoEO3xMAoEGg/+M+redgDtV+EJRKidDoi0B3mQaD2i7BEQpRORwS6wzwI1H4RlkiI0umIQHeYB4HaL8ISCVE6HRHoDvMgUPtFWCIhSqcjAt1hHgRqvwhLJETpdESgO8yDQO0XYYmEKJ2OCHSHeRCo/SIskRCl0xGB7jAPArVfhCUSonQ6ItAd5kGg9ouwREKUTkcEusM8CNR+EZZIiNLpiEB3mAeB2i/CEglROh0R6A7zIFD7RVgiIUqnIwLdYR4Ear8ISyRE6XREoDvMg0DtF2GJhCidjgh0h3kQqP0iLJEQpdMRge4wDwK1X4QlEqJ0OiLQHeZBoPaLsERClE5HBLrDPAjUfhGWSIjS6YhAd5gHgdovwhIJUTodEegO8yBQ+0VYIiFKpyMC3WEeBGq/CEskROl0RKA7zINA7RdhiYQonY4IdId5EKj9IiyREKXTEYHuMA8CtV+EJRKidDoi0B3mQaD2i7BEQpRORwS6wzwI1H4RlkiI0umIQHeYB4HaL8ISCVE6HRHoDvMgUPtFiiz06K3uL0mCUmNEQKDmQaD2iyDQEKXGiIBAzYNA7RfRugwR6EloTS4HCFQ2dR813AwEgUqgNbkcIFDZ1H3UcDMQBCqB1uRygEBlU/dRw81AEKgEWpPLAQKVTd1HDTcDQaASaE0uBwhUNnUfNdwMBIFKoDW5HCBQ2dR91HAzEAQqgdbkcoBAZVP3UcPNQBCoBFqTywEClU3dRw03A0GgEmhNLgcIVDZ1HzXcDASBSqA1uRwgUNnUfdRwMxAEKoHW5HKAQGVT91HDzUAQqARak8sBApVN3UcNNwNBoBJoTS4HCFQ2dR813AwEgUqgNbkcIFDZ1H3UcDMQBCqB1uRygEBlU/dRw81AEKgEWpPLAQKVTd1HDTcDQaASaE0uBwhUNnUfNdwMBIFKoDW5HCBQ2dR91HAzEAQqgdbkcoBAZVP3UcPNQBCoBFqTywEClU3dRw03A0GgEmhNLgcIVDZ1HzXcDASBSqA1uRwgUNnUfdRwMxAEKoHW5HKAQGVT91HDzUAQqARak8sBApVN3UcNNwNBoBJoTS4HCFQ2dR813AwEgUqgNbkcIFDZ1H3UcDMQBCqB1uRygEBlU/dRw81AEKgEWpPLAQKVTd1HDTcDQaASaE0uBwhUNnUfNdwMBIFKoDW5HCBQ2dR91HAzEAQqgdbkcoBAZVP3UcPNQBCoBFqTywEClU3dRw03AxmDQJ9ee33xt2c39yaT97456cHWaE0uBwhUNnUfNdwMZAwCvTV5ff6Xp9cmJa9+u/nB9mhNLgcIVDZ1HzXcDMS/QJ/dmiwFemty8Zvp4xuTi3c3PkiwpUqTywEClU3dRw03A3Ev0D//erIU6KO9ag/z6bULn256kGJLlSaXAwQqm7qPGm4G4l2gdyaTX/3rQqB3Vn++v+lBArQmlwMEKpu6jxpuBuJeoK/94/TeQo+3Jh9Uf1aP2x8kQGtyOUCgsqn7qOFmIN4FWrLQ4rMbiwP0R3sX77Y/CF/7cy/yJNdv20AKP06Qr+FmIKqXIQK1mtwo8eME+RpuBqJ6GYoJ9NVv2x/0LBKg9dghBxzCy6buo4abgXAIv3kPtB9ak8sBApVN3UcNNwNBoAg0LQhUNnUfNdwMZEQC5Sx8FhCobOo+argZyJgEurzKc3HpZ9uDBGhNLgcIVDZ1HzXcDGRMAuWbSDlAoLKp+6jhZiBjEuizG5PXVt94b3+QYkuVJpcDBCqbuo8abgYyJoFOH9fvudT+YHu0JpcDBCqbuo8abgYyKoFOH9+cWfK9uyc92BqtyeUAgcqm7qOGm4GMQaD50ZpcDhCobOo+argZCAKVQGtyOUCgsqn7qOFmIAhUAq3J5QCByqbuo4abgSBQCbQmlwMEKpu6jxpuBoJAJdCaXA4QqGzqPmq4GQgClUBrcjlAoLKp+6jhZiAIVAKtyeUAgcqm7qOGm4EgUAm0JpcDBCqbuo8abgaCQCXQmlwOEKhs6j5quBkIApVAa3I5QKCyqfuo4WYgCFQCrcnlAIHKpu6jhpuBIFAJtCaXAwQqm7qPGm4GgkAl0JpcDhCobOo+argZCAKVQGtyOUCgsqn7qOFmIAhUAq3J5QCByqbuo4abgSBQCbQmlwMEKpu6jxpuBoJAJdCaXA4QqGzqPmq4GQgClUBrcjlAoLKp+6jhZiAIVAKtyeUAgcqm7qOGm4EgUAm0JpcDBCqbuo8abgaCQCXQmlwOEKhs6j5quBkIApVAa3I5QKCyqfuo4WYgCFQCrcnlAIHKpu6jhpuBIFAJtCaXAwQqm7qPGm4GgkAl0JpcDhCobOo+argZCAKVQGtyOUCgsqn7qOFmIAhUAq3J5QCByqbuo4abgSBQCbQmlwMEKpu6jxpuBoJAJdCaXA4QqGzqPmq4GQgClUBrcjlAoLKp+6jhZiAIVAKtyeUAgcqm7qOGm4EgUAm0JpcDBCqbuo8abgaCQCXQmlwOEKhs6j5quBkIApVAa3I5QKCyqfuo4WYgCFQCrcnlAIHKpu6jhpuBIFAJtCaXAwQqm7qPGm4GgkAl0JpcDhCobOo+argZCAKVQGtyOUCgsqn7qOFmIAhUAq3J5QCByqbuo4abgSBQCbQmlwMEKpu6jxpuBoJAJdCaXA4QqGzqPmq4GQgClUBrcjlAoLKp+6jhZiAIVAKtyeUAgcqm7qOGm4EgUAm0JpcDBCqbuo8abgaCQCXQmlwOEKhs6j5quBkIApVAa3I5QKCyqfuo4WYgCFQCrcnlAIHKpu6jhpuBIFAJtCaXAwQqm7qPGm4GgkAl0JpcDhCobOo+argZCAKVQGtyOUCgsqn7qOFmIAhUAq3J5QCByqbuo4abgSBQCbQmlwMEKpu6jxpuBoJAJdCaXA4QqGzqPmq4GQgClUBrcjlAoLKp+6jhZiAIVAKtyeUAgcqm7qOGm4EgUAm0JpcDBCqbuo8abgaCQCXQmlwOEKhs6j5quBkIApVAa3I5QKCyqfuo4WYgCFQCrcnlAIHKpu6jhpuBIFAJtCaXAwQqm7qPGm4GgkAl0JpcDhCobOo+argZCALdzM+9yJNcv20DKfw4Qb6Gm4GoXoYKBNoPrf/pywF7oLKp+6jhZiDsgUqgNbkcIFDZ1H3UcDMQBCqB1uRygEBlU/dRw81AEKgEWpPLAQKVTd1HDTcDQaASaE0uBwhUNnUfNdwMBIFKoDW5HCBQ2dR91HAzEAQqgdbkcoBAZVP3UcPNQBCoBFqTywEClU3dRw03A0GgEmhNLgcIVDZ1HzXcDASBSqA1uRwgUNnUfdRwMxAEKoHW5HKAQGVT91HDzUAQqARak8sBApVN3UcNNwNBoBJoTS4HCFQ2dR813AwEgUqgNbkcIFDZ1H3UcDMQBCqB1uRygEBlU/dRw81AEKgEWpPLAQKVTd1HDTcDQaASaE0uBwhUNnUfNdwMBIFKoDW5HCBQ2dR91HAzEAQqgdbkcoBAZVP3UcPNQBCoBFqTywEClU3dRw03A0GgEmhNLgcIVDZ1HzXcDASBSqA1uRwgUNnUfdRwMxAEKoHW5HKAQGVT91HDzUAQqARak8sBApVN3UcNNwNBoBJoTS4HCFQ2dR813AwEgUqgNbkcIFDZ1H3UcDMQBCqB1uRygEBlU/dRw81AEKgEWpPLAQKVTd1HDTcDQaASaE0uBwhUNnUfNdwMBIFKoDW5HCBQ2dR91HAzEAQqgdbkcoBAZVP3UcPNQBCoBFqTywEClU3dRw03A0GgEmhNLgcIVDZ1HzXcDASBSqA1uRwgUNnUfdRwMxAEKoHW5HKAQGVT91HDzUAQqARak8sBApVN3UcNNwNBoBJoTS4HCFQ2dR813AwEgUqgNbkcIFDZ1H3UcDMQBCqB1uRygEBlU/dRw81AEKgEWpPLAQKVTd1HDTcDQaASaE0uBwhUNnUfNdwMBIFKoDW5HCBQ2dR91HAzEAQqgdbkcoBAZVP3UcPNQBCoBFqTywEClU3dRw03A0GgEmhNLgcIVDZ1HzXcDASBSqA1uRwgUNnUfdRwM5AiD0l6WqkxIiBQ8yBQ+0UQ6FqDJnmXHCBQ8yBQ+0W81Ei11pUaIwICNQ8CtV/ESw0EqnhSk4NAZVP3UcPNQBCoBAjUPAjUfhEvNRCo4klNjk6B5vmoP0fqPmq4GQgClQCBKiOPP52sVwSqrAYCVTypyVEqUPlA3KxXJktZDQSqeFKTg0CpoaSIlxoIVPGkJgeBUkNJES81EKjiSU0OAqWGkiJeaiBQxZOaHARKDSVFvNRAoIonNTkIlBpKinipgUAVT2pyECg1lBTxUgOBKp7U5CBQaigp4qUGAlU8qclBoNRQUsRLDQSqeFKTg0CpoaSIlxoWBPrs5t5k8t43id4NgSoDJyir4WYgCLTi6bVJyavfpnk7BKoMnKCshpuBINCKW5OL30wf35hcvJvk7RCoMnCCshpuBoJASx7tVfueT69d+DTJ+yFQZeAEZTXcDASBltyZvL748/0k74dAlYETlNVwMxAEWnJr8kH1572FSLcFgSoDJyir4WYgCHTGsxuLQ/dHe+sfgv7ci0z37wWAUdDPQwgUAMCOQFNdyNSVHr/DTClOBkIg2iCRbRliDzQXdIcyCEQbJLItCNQCTgZCINogkW2xcxa+O3SHMghEGySyLZLXgb4f/JkfukMZBKINEtkWO99E6g7doQwC0QaJbIucQJ/dmLyW8rvw3aE7lEEg2iCRbRH86s3jtHdj6g7doQwC0QaJbIvkdxcf35z5872h9j/pDnUQiDZIZFuUfvk7CXSHMghEGySyLQjUAk4GQiDaIJFtQaAWcDIQAtEGiWwLArWAk4EQiDZIZFsQqAWcDIRAtEEi24JALeBkIASiDRLZFgRqAScDIRBtkMi2IFALOBkIgWiDRLYFgVrAyUAIRBsksi0I1AJOBkIg2iCRbUGgFnAyEALRBolsCwK1gJOBEIg2SGRbEKgFnAyEQLRBItuCQC3gZCAEog0S2RYEagEnAyEQbZDI1r30FAAAB4dJREFUtiBQCzgZCIFog0S2BYFawMlACEQbJLItCNQCTgZCINogkW1BoBZwMhAC0QaJbAsCtYCTgRCINkhkWxCoBZwMhEC0QSLbgkAt4GQgBKINEtkWBGoBJwMhEG2QyLYgUAs4GQiBaINEtgWBWsDJQAhEGySyLQjUAk4GQiDaIJFt8SxQAABRECgAQE8QKABATxAoAEBPECgAQE8QKABATxAoAEBPECgAQE8QKABATxAoAEBPECgAQE8QKABATxAoAEBPECgAQE8QKABATxAoAEBP3Ar02c29yeS9b4beDFjy599MJhcIRBmP9i7eHXobTONVoE+vTUpe/XboDYE5f6jymFz4dOgNgRrPbkwQ6FZ4FeitycVvpo9pDy3cm1z422kZCP9J08SdCStkO5wK9NFetVCfXmOPRwWzPZ0Pyj9nBwYfDL0tsOLRHgLdEqcCvTN5ffHn+8NuCFQ8vbbY87xFIHqY/Wftb/gMdDucCvTWYkfn3kKkoAQEqohbk9c5ibQlPgX67Mbi0J3+0AWfqSji3uzwnQWyJQgUMnKHIwI1VP8xY4FsiXuBctZXEfe4jEkP1acpCHRL3AuU/tDDvb0LnIPXwp3q/DsLZEsQKOTiDvufeni0V4XBAtkSnwLlLLxC/oA/FXFnsoJPubbAqUCX139yHagWnt2avMZC1QMCTYNTgfJNJG3c4isvGuEQfkucCvTZjclrfBdeEXdIQiUIdEucCnT6mLsxaWJxc6yS14feFqiBQLfEq0Cnj2/OFut7NIcO7k0QqEoQ6Ja4FSgAgDQIFACgJwgUAKAnCBQAoCcIFACgJwgUAKAnCBQAoCcIFACgJwgUAKAnCBQAoCcIFACgJwgUAKAnCBQAoCcIFHJwdL0ozt4eeitm7Bcrds6/c8IW7evYZtALAoUc3N+dCeuTHJW+++VG59UEWrRv0uJNECicAAKFHBwUZy4X5zIUOsl5oUCLX/x+05sgUDgBBAoZmB3BnztosVVaThboaisefj7bLX6lz5sALECgkIHZEfzVw6K4Kl+pg0Cn08OWD2YRKJwSBAoZKLX15FIOLXUSaNsmIVA4JQgU5JmJ6lz5OWjtnM33bxTFzsu3V8f1wePZEf/V7y8XxXPl8x98WJ6Aeunrtlf+UP57MT+jfjD/ZLM6Ll9/3ZwWgcbfZC7S2bNeOfr8+dnmrM7ar28DjBUECvLMj95nx/HLTxzLq5qqczgfzhW09nj28MryFM/B8nzPy/FXfrb89zO/DwS69rolgUC/WxzCt73JSqD/5vLxPze3AcYLAgV59pdaXB4Z78+8dnvurUpBa49LQ832Vh98XNms3Il8+NnChOuvPKgeTx/MDHdu/u/zGuuvCzel4sfPljurbW+yEmix89e3pw+uL56+vg0wXhAoiDM/gq88NT+NtNoXPZgraP1xKdDVMxcXP80/AFh/5vKtV0fjC/etv27F2mVM1ZPb3qQm0GprFv8FWN8GGDEIFMRZOmxltdVnhzM3zQ/Tw8czUy1+cPwpY3klVPOZh0s9Ll+ycN/661bsN/3Z+ibHAl3sOe9HtxZGDAIFaVaH7ks/1ZxWKWn9cfCKlf1KmzWeecx+3X3rr6s/7ZgXvlrf1P0Wga72ZmNbCyMGgYI05dc4V5QHv+Vp7cW/VUpafzxdOao8eq5/bajxzIqHP3zx0fNF3X3rrzvemIXzjj7fLc4EJ+ibb1I/C79pa2HEIFCQ5qAus9BICQT63fOBJ08p0ErrO6sL+6NvgkDhRBAoCBPKrPy08fiYeKWk4PG0LtBza+/VeOaM81f+4af9NYG2fPH++Kj7sKh99Bl7k3aBhtsAIwaBgjCHtS+cH1bXCZ3mM9D5D2pXPi0fh89cXIA0bX4G2vJdotrHlvvLfdOWN2kRKJ+BwjEIFITZr11H1HLWPXIW/tz6i+dOXHvmsSiXZ8T3lxciha+rbc3KeeWO57ngGWtvEhcoZ+HhGAQKsoTfN98Pv5PUfh3oQqCzf1m8+jD2ymP3HYQfX66/rrYBx84rz25dnba/SYtAuQ4UViBQkOUwuGXc4v5HB9HvE9W/iXT8MWNx5uPZT35XLC/wDJ65OPr+4fLi49XqmtOjn5qvWxIcdR+c8CZxgTa2HsYLAgVRZi6sfxNo8fBocTnmmf++uKoofFy/jHN1Dn9xZWj4zCeLL6kXL/1uvqd5WD4413zdkkCgi4P41jdpEej61sN4QaAgyvGx9JyDxQ7pSXdjOj6J/uDD8hqjFz5ePg5fWd0maeelr1Ynx7+bHZefux153ZzwvM/yIL7lTVoEyt2YYAkChSFZv/Pm6e/EqeGenRq2AQYFgUJ+VruBi13N9cenf+UQaNgGUAIChfys35apeZum075yCDRsAygBgUJ+yq9Rvjvbg/tud74zt/749K8cAg3bAEpAoDAAh8v7iyxu6LH++PSvHAIN2wA6QKAwBA8/m58iv93y+PSvHAIN2wAqQKAAAD1BoAAAPUGgAAA9QaAAAD1BoAAAPUGgAAA9QaAAAD1BoAAAPUGgAAA9QaAAAD1BoAAAPUGgAAA9QaAAAD1BoAAAPfn/w3KxeWc8FvkAAAAASUVORK5CYII=){role="img"
width="672"}

``` r
# Step 1.2: Identify the most common rating range
rating_range <- D_A_cuisines %>%
  group_by(Aggregate.rating)%>%
  summarise(count = n()) %>%
  arrange(desc(count)) 
most_common_rating_range <- rating_range[1,]
print(most_common_rating_range)
```

    ## # A tibble: 1 × 2
    ##   Aggregate.rating count
    ##              <dbl> <int>
    ## 1                0  3443

``` r
# Step 2: Calculate the Average Number of Votes Received by Restaurants

# Calculate the average number of votes
Average_votes <- D_A_cuisines %>%
  summarise(average_votes = mean(Votes, na.rm = TRUE))


# Step 1: Identify the Most Common Combinations of Cuisines
# Count how many times each cuisine combination appears
Common_cuisines_combination <- D_A_cuisines %>%
  group_by(Cuisines)%>%
  summarise(count = n())%>%
  arrange(desc(count)) 
print(Common_cuisines_combination)
```

    ## # A tibble: 145 × 2
    ##    Cuisines     count
    ##    <chr>        <int>
    ##  1 North Indian  3960
    ##  2 Chinese       2735
    ##  3 Fast Food     1986
    ##  4 Mughlai        995
    ##  5 Italian        764
    ##  6 Bakery         745
    ##  7 Continental    736
    ##  8 Cafe           703
    ##  9 Desserts       653
    ## 10 South Indian   636
    ## # ℹ 135 more rows

``` r
library(sf)
```

    ## Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.3.1; sf_use_s2() is TRUE

``` r
# Step 2: Determine if Certain Cuisine Combinations Tend to Have Higher Ratings
# Calculate the average aggregate rating for each cuisine combination
cuisines_combinations_ratings <- D_A_cuisines %>%
  group_by(Cuisines)%>%
  summarise(Avg_rating = mean(Aggregate.rating, count = n()))%>%
  arrange(desc(Avg_rating))

# location of restaurants on a map
# Convert to sf object using longitude and latitude
restaurants_sf <- st_as_sf(D_A_cuisines, coords = c("Longitude", "Latitude"), crs = 4326)

# Create the plot
ggplot() +
  geom_sf(data = restaurants_sf) +
  theme_minimal() +
  labs(title = "Restaurant Locations", x = "Longitude", y = "Latitude")
```

![](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAABUAAAAPACAMAAADDuCPrAAABdFBMVEUAAAAAADoAAGYAOjoAOmYAOpAAZrY6AAA6OgA6Ojo6OmY6OpA6ZmY6ZpA6ZrY6kJA6kLY6kNtNTU1NTW5NTY5Nbm5Nbo5NbqtNjqtNjshmAABmADpmOgBmOjpmOpBmZjpmZmZmZpBmkGZmkJBmkLZmkNtmtrZmtttmtv9uTU1uTY5ubk1ubo5ujqtujshuq8huq+SOTU2Obk2Obm6Oq6uOq8iOyOSOyP+QOgCQOjqQZjqQZmaQkDqQkGaQkLaQtraQttuQ29uQ2/+rbk2rbm6rjk2rjm6rq8iryOSr5Mir5P+2ZgC2Zjq2Zma2kDq2kGa2kJC2tpC2tra2ttu229u22/+2///Ijk3Ijm7Iq27Iq47I5KvI5P/I///bkDrbkGbbtmbbtpDbtrbbttvb27bb29vb2//b/7bb///kq27kyI7kyKvk5Mjk5P/k///r6+v/tmb/yI7/25D/27b/29v/5Kv/5Mj//7b//8j//9v//+T////ArKcqAAAACXBIWXMAAB2HAAAdhwGP5fFlAAAgAElEQVR4nO3de4Mc5ZXf8RIIZFggS42QIoJjHJNYg9BGY+8iYmdjx0O0NkPsxGtodi3JkGBCW7tCA8yg6X7zqVtX17X7qdNV1ec59f38AepbdZ1+Tv1UdwVLAIBIsO8ZAABfEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAILSfAP36SlDw7Eu//HIvs+HuYRA83+8Uv3s9uPSrficJYGQaAjRyuVOWfPb9YaOnPv0eAzSbOAEK+E9JgHYJk4v3ho2epun3FqD5xAlQwH97C9Dn8q32i8/eCLrk09DR0zT93gKU3AQMURCgUYSedFkFJUAB6KAiQJNN+lddP0yAAtBBR4BevEeAAvCOvgD95mfR6uillz/KX/zmZ9+Lz3V6+Q+rjxaPOn0Wvzt49q0v0veerKezyrzo/zcvPrwSXHrxV80fuJl+5bNvfdkw/crEyj7/fmVWlxe/i+f1xV+unyl9X2HihSStTKY2S/UfAYAKOgJ0vQl/8eHquPwrX1aeCC5/tKwE3Ddv5EfxbybvbgnQk9Unmj5wc/UNySTdAzRO/dKsLpd/Wp1dsKqu8n1NAVqbTG2W6j8CABVUBGicIdU0WSXWSeFcp2c+LgdcFEKV86CaA/SFVag1fuCF/Kl4npwDtDitrJiH1Weq39cQoPXJ1Gap/iMAUEFBgH6bbPam+RRH0CvR1u638SrXq+k7kyeW3/42e6aw8RsfvP/xl/GG82oNtjlAg+Cvv1x++0XLB4Ig3npO1hVvlqe/1hCgJ9kHP39jNWPxvF7+Zba+uJp65fvyia/+UJ9MfZYafgQACmg5kf5ysl4Vr46lm9bLJ+m6ViG5Vn8sZlDh3UkitwRo9lzzB7JX4xcrAb1WD9DCXoeTbLXwpLgqGv+p4fuqAdo8mcosNfwIABRQEqAv58GTx0NyLCV+5nJlmzXPoCfrfY3RcxsCdJWHzR9YbROfVAO6oJ5bJ6VpxbNaiMtsEg3fVw3Q+mQaZqnhRwCggIYAffZHpQPQmSdJFj6J3/DSL74ofLgp4KLnVmtvDQFaOmBV+0DrGm5BLUDjnbX5rKYTedL4RZXvqwRow2QaZqnhRwCgwJ73gV78Nlo/zE/WKR5CCtL9ovlTL/631buqAXfx5//98yvBpgCtrDxu+YBTgJbelEbnk7at6+L3VQK0YTINs9TwIwBQYO8HkZ5cWa+4VQM0fr7w3OX09Mpi6HyenB65Pjq9NUAdPiAN0Mbdk9XvEwVow48AQIG9B2iyfZo9aArQ+Bzy/InKQZ71WZaOAer0gR4DtP59sgCt/wgAFNh/gCaHydOYKO0RLPr2f30/2Wu6OnNyfRQ+8sJLP/rH/7dxH+gq19w+0D1A01frAdrwfZsC9GG+D7QeoNUfAYACCgI0We+sXUhUc7E6BzIPnYfZWZbLxoNIJ/UA3fyBPg8i5ZFa/T63g0iNAVr6EQAooCBAk2Py6Yk7D8sZ9GotYEoBWkiaJ0H1oHr8yWqANn9AFKDFWa2fxpTNdsP3VU9jqk+mPktNPwIABTQE6HojvnBbu+iPlYBZ3XFklUGFO5DkEfwwP4cyn+bDUqZWPyAO0I0n0qdx2fR9bifSV2ap4UcAoICKAF1vxMdX4Vz+Q3bt4/PL1eWR8Xvjax2TgMkzZHWhZHLdZ/pafETq8n9fHXNpWAOtfqDlkHc1o7pdyrnazm74vnziGy/lrMxSw48AQAEVAbpeQSseh8+36teSYMnec7NyOn6+Vpr50ZVagDZ8oP2codLRrNJcrK/TLD4uvyt5pn0Gb26+mUjj9fylHwGAAjoCdL3BfZGfrrO6b1t+i7hobS595skqR/JcufSfVrsJv8tOHLp08+t6gDZ8oOmITT798vxVAjT/qsLt7D6t3M6uYQZXE19vy1cn0zRL9R8BwP4pCdDCkfjPkzsQF+5J/O3vkru7vbj+x+M/jU9O/+vl6ubL8St51Fx8+kJ6d+KmAK1/oPGQdz79XFOAxv9CcXzX4003VG6YwWzihT2tlck0zlL9RwCwd/sJUAAwgAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAEBIF6OKTwzB87d3swb3owe37pTc8PQrv5H+8+mCXGQQArSQBen4UJv4mfvA0fZCm5KNb4cG76ZMHH6RvJkABWCUI0MVxeO3+cvFPaUbOwuv3l+fH4fXHy+Vp9Mz50d00VW+k7yZAAVglCNDTbHVzHmfk2WHy4OlRlJ2L47vxy1GURg9vhXeTdxOgAKzqHqDRCujd9aN5tqY5D+8sF+/Hq6Rnbz+IU/OPabISoADM6h6g5UScZWl6GgVpcQ306oMsWo0E6Fdf7XsOekEZmlCG/7oH6Nnh9cd/uRWG1369jFdHs4NF8bPFfaBXH2Rrqk0B+hUA6DJegN5Lj8LfqQZo4Sh8lJrp7tGGAN33DwUAVSMF6Gl8AtPj5eKTMDlulAdoMSfT1JzFG/FswmtCGZpQhv9EAZqeJD9LdnuW1kBX0tRMDs0ToJpQhiaU4T/JJnwhMzcGaJS11x8ToJpQhiaU4T9JgGaJmPyheBR+bZWas/AOAaoJZWhCGf6TnAearXTG65fJ+Z+xeX7xe2yVmtFG/G8IUEUoQxPK8J/gSqRZtrI5izOzeCXSWr7aeRq+dkiA6kEZmlCG/wQBenYY33wpPQqfXRifXQufW2+3z1b3GfGckSahDE0ow3+SuzGdHiangR4kez/Pi3djWlkHaJS2BKgelKEJZfhPdD/Q8/gWoO/cXz2I8vP249IbCkeO5gSoIh6XEURWf/a4jCLK8B93pHdjpEn8LSMICgnqbxkllOE/AtSNkSbxtowgKCaot2WUUYb/CFA3RprE1zKCoJSgvpZRQRn+I0DdGGkSX8sgQBUzUoYMAerGSJP4WgYBqpiRMmQIUDdGmsTXMghQxYyUIUOAujHSJN6WwUEkvYyUIUOAujHSJP6WwRqoWkbKkCFA3RhpEo/LKCaox2UUUYb/CFA3RprE5zIKCepzGQWU4T8C1I2RJvG4jOJxJI/LKKIM/xGgbow0icdlEKBaGSlDhgB1s7FJive50M3jXidAtTJShownC/7ebWqS0n0udPO414NCgnpcRhFl+M+P5X7/NjRJ+RRF3Xzu9aBo3zPTC59Ho8BIGTI2OnF4tSbJl+ItC7Wupd3nXg/MJajPo1FgpAwZE404gmqTrJfizcu0sqXd514PzCWoz6NRYKQMGQt9OIZKkxSW4o2LtLal3ederwSomt9UzufRKDBShoyBNhxFuUmKS/GmJVrd0u51rxOgOhkpQ8ZAG46iPUA3rWWqW9r97nUCVCUjZcgYaMNRbAjQDfs51S3tnvc6AaqRkTJkDLThKDYFaPuRdnVLu4FeV/aL7sLAaMSMlCFjoQ/H0H4QaSNtS7uFXtf1i+7CwmgszZQhY6IRR9B+GtNmypZ2E72u6hfdhYnRMFOGjI1OHF77ifRb6FrajfQ6ZWhipAwZRQu3akaahDI0oQz/EaBujDQJZWhCGf4jQN0YaRLK0IQy/EeAujHSJJShCWX4jwB1Y6RJKEMTyvAfAerGSJNQhiaU4T8C1M3YTTLQ2U9Gep0yNDFShgwB2qqUYSM3yVDn3xvpdcrQxEgZMgRom3KGjdskg10BaqTXKUMTI2XIEKAtKhk2apMMdw8SI71OGZoYKUOGAG1WzTACVBPK0MRIGTIEaLPqfScJUE0oQxMjZcgQoM0IUM0oQxMjZcgQoM32GqAcRNqCMjQxUoYMAdpirwHKaUybUYYmRsqQIUBbtB5EGucOyZxIvwllaGKkDBkCtFl1L2TeJENtXI/DSK9ThiZGypDxNQkGVjuKs2qS6gujhunuX2ak1ylDEyNlyBCgTeqHwbMmqR5bEq6OypKwh3VfI71OGZoYKUOGAG3iGqDCo+UN67CbJpS90MeheSO9ThmaGClDhgBtUg3QyhpnUIm0jqG2/lBQ0/r2fk4ONdLrlKGJkTJkCNAmrRvqfQRoPTU3JqhTyLoy0uuUoYmRMmQI0EYN+VlJ0NKDLqG2OT9rU3LJWHdGep0yNDFShgwB2qwhP0uhWXmFAB0VZWhipAwZArTFOqkqyRU0vTJmgO5SlZFeH7iMXX9lV4yG/8SNcnZ4/XHyh8W9wzC8fb/04tOj8E7+x6sPxHOnwoboGjVAOY0pN2wZPfzObhgN/0n7ZHEcpgEaZWUsTclHt8KDd9MnDz5I32g6QEWnFnXKz9rbqy91q8VIrw9aRh9r+m4YDf9J22QeZgE6C6/fX56ncXoapeb50d00VW+kb/Q/QDcuUfL8bA7SDR+ovSxYzo30+pBlVH7sIaOU0fCfsDnODrMAPTtM8vHpUZSdi+MoO5en0fPRw1vh3eSdBgJ0Y1L1u/65IaRrr0vWlIz0+ngBOujKKKPhP1lvRBvwf5vuA51na5rz8M5y8X682X729oM4Nf+YJquJAO3zDiKF5dMxPyuf2vDUdkZ6fbQAlfwl5Y7R8J+sNWbhjewg0ixb0TyNgrS4Bnr1QRatJgK0xzWR4vJZC1KnT7U/tZ2RXh8rQEW/sTtGw3+izjiNNt/TAF0cZweLkoeFfaBXH0RrqXGeNgXoV95ZLUf9TSmZVuF/m6de+lTrU+hDZXz4kaditABNdng2BWjhKHyUmunu0YYA3fcP1V2fy5FoSgToiNa/KgE6JaMF6Cw+ybMWoMWcTFNzFm/Em9iE73VTTjShhu+XzJKRra2xTqQPeh34GkbDf4LGmCfH3xvXQFfS1EzWVAnQxqkJZ2HbU9sY6fWxyiBAXRgpQ6Z7Y5wdJpnpEKDJvlICtMd52P7UFkZ6fbQyhh13RsN/3TtjHuaiaCwehV9bpWa0sW8iQEe8OmVYRnp9vDIGHXdGw3+7Bug8u+h9nl/8HlulZrQR/xsTATre9dHDMtLrI5Yx5LgzGv7b9WYixSuR1vLVztPwtUMTAfqVify00uuUoYmRMmR2DdDFcXgtvxY+t95un63uM+I5I01CGZpQhv92vp3defFuTCvrAD07nGCA6l1bNdLrlKGJkTJkdr8f6Pm9KD9vPy69WDhyNJ9egCreX2qk1ylDEyNlyOhczvXp0CSaj9gb6XXK0MRIGTIqF3OF3JtEwzmjrYz0OmVoYqQMGY1LuUbjB+ggEWyk18cuY6C/DhkN/xGgbkYP0GGuIjTS6yOXMdQGBaPhPwLUzdgBGgSDJKiRXh+3jMF2yTAa/iNA3Yx8ECkIhklQI70+ahnD7dRmNPxHgLoZ+TSmYKAENdLrBKgmRsqQIUDdjHwifTVA+1p4jfQ6AaqJkTJkCFA3ezls0X+CGul1AlQTI2XIEKBu9h+gvYyUkV7nIJImRsqQIUDdrJtkkEWpZqAENdLrnMakiZEyZAhQN3mTDLUwVRGgG3AivSZGypAhQN2smmSwzbkqAnQDytDESBkyBKibrEmGO6BQ0bQJ38N3Gun1iZYxzrZPZ0ZGQ0bjgGikIkB3/1IjvT7NMsbae9SVkdGQUTgeKukI0J2/1UivT7KM0fYedWVkNGT0DYdOBKgmUyyj7705/TEyGjKqRkKxEQ8itWQnAbo2xTIG2KHTEyOjIaNmFJQb7zSmTflJgKamWMYA3dATI6Mho2UQtBvtRPpB89NKr0+xjEH6oRdGRkNGyRioN1aTDJufVnp9kmWoTVAjoyGjYwj023OA9rXea6TXp1kGAaqQjiHQbx8BOsAxfyO9PtEyCFB9dAyBfnsJ0PUx/74WFiO9PtUyCFB1dAyBfnsI0PXD/g79G+n1yZZBgGqjYwj0G61JKstGz4uLjV5XEx07EoyGwvw00lRCSsZAvfGapLps9LrAmOh1TeGxE8lo6MtPG00lpWUQtBuxSdrzkwCNKYuPHRCg/tMyCNrtq0l63mQz0Ovq8kOOAPWflkHQbk9NEhCgVeryQ27XfaD5E73PWScGmkrO/y4cBwGqxaQDtNoPKn4KA00l538XjkNFgO4+PQO9PuUADVpt/Zh8Lrcy0FRy/nfhODQEaA/Ts9DrZvKzhyuRnJpj6J/LQlOJGWjDUSg4iNTH5Ez0upX8HCBAm36Ywf/CMdFUUhb6cAx7a5J+u99Gr/uTn6Wxqw1knwEarC/9bfuQvIzNbDSVkCeNuHf7a5Jee99Ir/tSRm3tsDyUPQdoc1ISoIMiQN0YaRLKGFMpuhpybKgADZo/tHtBzTwZjWEQoG6MNAlljKiUaE3xJj4Kv31jvmku+qipiR+jMRAC1I2RJqGMERUTrTHepOeBNnwBAbonBKgbI01CGSPaGm/CK5E6fEPxTeI6tvFjNAZCgLox0iSUMaIhAtTpKxrftOtXtfNjNAZCgLox0iSUMaZt8SYtozSZrfnJlUhDIkDdGGkSyhjVltVD1zIqH6tMqDjtoVc2G/kyGoMgQN0YaRLKGFcp12rh5lhG8YNNUVzNz7ET1JvRGAIB6sZIk1DGyLI42yVAi59sXZmt5PRy8O32An9GYwAEqBsjTUIZe9K4buhURjEV2/cGtLy1r9nfyL/R6BEB6sZIk1DGvjQFWscArWl/66gb8x6ORn+8CdDxd+2UGGkSytibhgYeLkDb3zMAH0ejN74E6IhbJI2MNAllaLJrgLac8VndGzo0I6Mhs/EHvvhirNnYZty/UhsYaRLK0KTbQST3BB35tv1GRkOm/Qf+7PtB8MzH3/27t76svfTop2F4cPt++mBx7zAMVw8yT4/CO/kfrz7YfS5H3iipM9IklKFJp9OY3BN0SYCOp+0Hvvgw/vWjAH09uPxx5bVPwsTBB/GDKCtjaUo+uhUevJs+mb46RIDuJUGNNAllaNJ1DbQxS5vePe6/e2JkNGTafuGTILj8H6488/HFfwmC58rroKdJSJ4fp6E5C6/fjx9cfxy9EqXm+dHdNFVvpO8eIED3kaBGmoQyNOkeoMvS/xsWhkrU9j/PDYyMhkzLT/wkCH68/O71Z6KVz0+vBDeLLy2Ow7vx/6OQjP5/dpjk49OjKDsXx/ELp1GURg9vpe8iQFWhDE067wNdH8pvWRgqK6v9z3ITI6Mh0/IbnwSvLrMAXT4Mni++lCfiLN7POc/WNOfRg8X78Wb72dsP4vf8MU1WAlQVytBEeBCp/GTLW4eY3xZGRkOm+Ye+eO/Sr/IA/frKM9W9oIkkQGfZiuZpFKTFNdCrD7Jo7SVA978T1EiTUIYm0oNISwJUjeYfOo3OLECz/1VlW+3ZwaKzwyg3C/tArz7INvWbAvSr7sodJJgA4Ktaghaea3vjfubUayMHaLKGWQ7QwlH4KDXT3aMNASqqjgDFRNXyM1g/2/rOfcyo53oM0Iv34gNHWXI+qR6GT5wmJyoVArSYk2lqzuKI7WcTPp3XPW7DG9lMoQxNxCfS58+3vrfnOd3IyGjItPzSyYGjNECjMH2+/obTw4N4A72yBrqSpmaykU+AakIZmuwWoJve3NccOjEyGjItP/XXV4JXvkwC9Js3gviAUsU8O1F+Y4BGa6nXH/cYoHu6X2zCSJNQhibSAG17V/7uPmdyOyOjIdP2Wz+MhuGFK5de+l70/1drr36SX2hUPAq/tkrNWXinzwDd4y1FjDQJZWiyw2lMzf/MxxAzuZ2R0ZBp/c3/dGU1XrX8XMzCa6tQnGcXvc/zi99jq9SMNuJ/02eA7u+mdkaahDI0kQVo4cnae4aZzy2MjIZM+0/+7e9eiEbk2Zc/qr0yC9eb68Urkdby1c7T8LXDPgN0b4w0CWVoIjkPtPxU7R3DzOhmRkZDRvCLzwv5GV/YeS2/Fj633m6fre4z4jkjTUIZmnT5Vzkb87Pt8biMjIZM9188u/1SLN7teV68G9P6PavHZ4cEqCKUoYlzGdV8JEDVqP3i3/65rnRf5dOwFKDL83vRn24/Lk2kcORoToAqQhmadCmjFI8EqBrVX/y716v7rIP4tqB7mTdNjDQJZWjSW4ByEGlvCFA3RpqEMjTpUEYlIGuBucf8tDIaMtXf/OL//D7x2yC49MNf/P73f/+94NJb/9hwKefEGGkSytDEvYzaJnotMPeXn1ZGQ6blR49WRFenf37KCujSTJNQhibdDyKVE3Sg2eoqvYnJvudiTzbdUDnzsOla+KmZ2iKr29TKaAhQRb5SO2cj2HRD5czXV5ruxjQxU1tkdZtaGboDVPGsDW/T/UCbHkzV1BZZ3aZWhuoA1Txvw2sL0NIaKAE6uUVWt8mVoTmkNM/b8Fr3gT7f+OfJmtwiq9r0yihnVP4nDbFFgDZ4EgSvpDs+Lz4Myv+s8TRNb5HVbIJlFEMq/5OK3CJAm5xEv8eLb775Znw/0FdGnSOdJrjIKjbFMtYplf9JR3ARoE3iFc9M/X7KEzTFRVavSZYRtBtqBjvO115nY0823Q80Xvt89q0vWt8xJZNcZNWaYhkb8lPJKuh+52JPpll1d1NcZPWaYhl6A5QT6bHVFBdZvaZYhuIA5VLOmm/b7wc6TVNcZPWaYhmaA3S/379XbSfSl8aHE+mtNAllaNLXQaSh5s+RkdGQIUDdGGkSytCkYxkEqEItNxPJ7gr6+9///RvBpV9wP1ArTUIZmnQtgwDVZ/uPz82YYkaahDI0kZRRiE0l+WllNGQcfv2HnEpvpkkoQ5MdA1THlZxmRkPG4ednFXRppkkoQxNBGeUNdxX5aWU0ZBx+f+4HujTTJJShSfcy9Oz4LDAyGjJOa6AEqJUmoQxNdgnQIeZHyMhoyGwfiIuTgE14K01CGZoQoP5rOY3p52+u/OAK92NammkSytCEAPWfy4n0rICaaRLK0GSHg0gDzI2YkdGQ2R6gz75FfpppEsrQRH4aU//zsgMjoyGjayj0MtIklKGJqAx1+WllNGSUjYVaRpqEMjTZuQwdYWpkNGTaDiL9cL3d/vUP/oqNeCNNQhma7FqGks15I6Mh07YPtHDqJyfSL800CWVosmMZWg4oGRkNGYcA5UT6pZkmoQxNditDzSlNRkZDpvbjV24FynlMKSNNQhmaEKD+q//4T+oBenMPM6aMkSahDE0IUP/Vf/yL/xlffnTppfxapB/9YQ/zpY2RJqEMTQhQ/znsA8XSTJNQhiYcRPKfw2lMWJppEsrQhNOY/Lf/n98PRpqEMjThRHr/VX//i5+/Ga18Fu7GFGN11EqTUIYmlOG/aoB+93r8jxjzzxpXGWkSytCEMvxHgLox0iSUoQll+E/BLhQvGGkSytCEMvxHgLox0iSUoQll+I+7Mbkx0iSUoQll+I+7Mbkx0iSUoQll+I+7Mbkx0iSUoQll+I+7Mbkx0iSUoQll+I+7Mbkx0iSUoYm2MoQXNmkrY1TcjcmNkSahDE2UlSG9tF5ZGeMa4G5Mi3uHYXj7vvjzKhlpEsrQRFcZ4ps76SpjZP3fjenpURi7+iB+8OhWePCudEqaGGkSytBEVRny24uqKmNs/Z9IPwuv31+eH4fXHy+XpwcfLM+P7vb+HeMz0iQ2ytBxE6LdqRqNiQVoXz20YSIXf8589u87bM+fHSbrnk+PouxcHMfZeRpHqe+8bJI6E2UouQ3m7lSNxrQCtLceapvGNz8T3kxkHt7I/n9nuXj/g+hPZ28/2AjJUekAABiKSURBVHk2987HJmlgoQwtN2LfnarRmFSA9tdDLZMonw36XIcAnYXpFvtpFKSsgWpjoAz5gq6OrtGY0EGkHnuoZQoPg+DSS/HJTD+4Elz6cYfpLY4PPkj+cHYY5WbLPtCvAKl18+97TqyZzq/a3EOS/Gw7Cv9efPVR9N+bcZZ2uRCpHKDNR+H39bPBAgJ0MJP5UQcP0O9ev/SrZZydr0b/PelyJVIhQK8a2PWZ828zpZGBMtiE18a/MgbfhM9OpH8SPJ//11FlDdQM/5qkkYUyzOSnidFYelnG0AeR8gCNt96/e73DNjwBqpqJMqzkp43R8LOM3nqobR9osgmf3siu23WdxaPwhvjYJA1slGEkP42Mhp9l9NVDLRM5SfZ+prtCu90PND7/s/h/I7xskjrK0IQy/NcSoF9fCV7+KD4M/2ocpl0OwxevRDLESJNQhiaU4b+21diT5PqjJ0Fw6UqQrI26WhyH1/Jr4e0w0iSUoQll+K91P8Cf4g33i5PkQqROd2Y6L96NyQwjTUIZmlCG/zbsSP2/UW5efPrCC291vLPd+b0oP2+bWv800ySUoQll+M/E0cwRGGkSytCEMvznEKDf/vmL4edDOyNNQhmaUIb/tgfod693up2dUUaahDI0oQz/EaBujDQJZWhCGf4jQN0YaRLK0IQy/EeAujHSJJShid4yOl3oqLeMERCgbow0CWVooraMbrfaUFvGGAhQN0aahDI00VpGx5u9aS1jFASoGyNNQhmaKC2j6+2GlZYxDgLUjZEmoQxNlJYRdExQpWWMgwB1Y6RJKEMTpWUEHRNUaRnjIEDdGGkSytBEaRlBxwRVWsY4qj9Q+R+EzxCgVpqEMjRRWgYB2gEB6sZIk1CGJkrLIEA7qP5AFz9/s+6HHW9oZ5CRJqEMTbSWQYC643Z2bow0CWVoorYMDiI5I0DdGGkSytBEbxmcxuSKAHVjpEkoQxPVZXAivRMC1I2RJqEMTSjDfwSoGyNNQhmaUIb/CFA3RpqEMjShDP8RoG6MNAllaEIZ/iNA3RhpEsrQhDL8R4C6MdIklKEJZfiPAHVjpEkoQxPK8B8B6sZIk1CGJpThPwLUjZEmoQxNKMN/BKgbI01CGZpQhv8IUDdGmoQyNKEM/xGgbow0CWVoQhn+I0DdGGkSytCEMvxHgLox0iSUoQll+I8AdWOkSShDE8rwHwHqxkiTUIYmlOE/AtSNkSahDE0ow38EqBsjTUIZmlCG/whQN0aahDI0oQz/EaBujDQJZWhCGf4jQN0YaRK3Mtz/QcY9mdRoqGekDBnVy4kiRprEqYwu/6TtfkxpNPQzUoaM5sVEEyNN4lJG0OkfBd+LCY2GB4yUIaN4KVHFSJM4lBEE+hN0OqPhgyHKUN1+RX7M5f5Np9cJ0NFQRhvl/VfgxUwqMJ1eJ0BHQxkt1Dfgmg/zqMF0ep0AHQ1lNPOgA3MezKIKE+p1D7p3QqPhAQIUW02p1/U375RGQz8CFFtNqtfV9+6kRkM9AhRb0euaUIYmHETCVvS6JpShCacxdffop2F4cPt++mBx7zAMVw9yi3++FYbhO7/eafbUoNc1oQxNOJG+s0/CxMEH8YOnR8mDqw/iB49uhQfvxn84O0zfE1570NvM7hG9rgllaGKkDBlRgJ4mIXl+nIbmLLx+P35w/XH0SpSp50d3o/XP4/jZKFB/Et7od473w0iTUIYmlOE/SYBG4Xg3/n+06nk3XtVMYvTpUZSdi+P4hdMoSk+TPF097z8jTUIZmlCG/yQB+vQo3VyP1j3vLJfzbBVzHj1YvB+H5dnbD/Jnszd5z0iTUIYmlOG/nfbUJtk4S1dHo3XOG6U10Ksm9n2uGGkSytCEMvy3S4BmW+3ZJvrZYZybq32g0eb9wd/9a8sHvwIAXUQhuEuAJpvp5QBdH4U//0l8CP6df2gI0X3/UABQJQrBHQL0NDmNqRCg5Y32xaMkQsPbj+VfoYeRzRTK0IQy/CcP0NPDg3iHZ2UNtOxf4pPpTewMNdIklKEJZfhPHKDz7DT6jQEaOT/iKLwelKEJZfhPGqCfhKvTO4tH4XOFsz/zM0K9ZqRJKEMTyvCfLEAXs/UVmvNsDXNeXNNcnWu/JEBVkZWh7s4Okx4NdYyUISNbLmaFUCxeibQ2z3d9zkxcy2mkSURl6Lu32JRHQx8jZciIFot5caUyWtm8ll8Ln4vPA303euL8XsilnHp0KCPPTIV3t53eaGhmpAwZ2aWc4Uq8cnlevBtTLj0PNL5l093mqfjFSJO4l5FnpsZ/ZG5yo6GakTJkJAvFaVgK0Hgts+F0z0V8z9DwtdbLkfxipEmcy1hnJgE6GMrwn56FQjcjTeJaRiE0CdDBUIb/9CwUuhlpEgJUE8rwn56FQjcjTSIIUA4iDYYy/KdoqVDNSJNIApTTmIZCGf7TtFhoZqRJBAeRlhM7kX7EYqfWVBapWi4UM9IkgtOYNBpwNMZc3Z5cUxmkdRHRxkiTSE6kV2i40Rh1h+/0msoetcuIMkaahDI2G/eUA0ajM3V/reuaG73odU0IUE1GLEPfjiVVM6MYva4JAarJeGXoO6GOAHVEr2tCgGoyWhkKL+kgQB3R65qMEKADfUEJo9HNuH+9udEzJ7rR65oQoJoQoNiKXtdk+NOYhpp+CaPRDQHqL3pdk8FPpB9s8iWMRkcEqLfodU2GvpRzuKmXMBpdqctPAtQRva4JZWgy9on0BKiH6HVNKEOTPQSontjSMye60euaUIYme9iE15OgamZEOXpdE8rQhADFVvS6JpShyVhlBEXjfOV2amZEOXpdE8rQZKQyAgLUY/S6JpShyThl6MxPAtQRva4JZWgyShlK85MAdUSva0IZmowfoCN8nytN86IZva4JZWgycoCO8GUdKJsdteh1TShDEwIUW9HrmlCGJuMeRBrjuzrQNj9a0euaUIYmo57GNMpXdaBuhpSi1zWhDE1GXQMd46u6UDdDStHrmlCGJuPuAx3hu7rQNj9a0eua7FKGooXQ19Go/IRjlKHzHKYlAerK116voAxNC6Gno1H9CQlQbOVpr1dNvgxVS6Gfo1H7CQlQbOVnr9dMvQxdi6GXo1H/CXcrw20wCFDPednrdVMvQ9di6OVoBCXxMzuV4ToaqgauQNv8aOVlr9dNvQwCdGdBLUF3PaTnMhyqxq1I3Qwp5WWv1029DAJ0Z5UADXo4pLd9PFQNW4m+OdLJy16vm3oZBOjuagk6fIDqGrYSfXOkk5+9XjP5MlQtiJ6OBgFaoG+OdPK016soQ9Ny6Oto9BagwaZkLDxLgHrP116voAyuROpHzwHa9tr29+3wzb1MqI+JTIDPvV5AGZp4XcY60gYJ0PILg+RnL5MjQN143etrlKGJ32XkGdTLJnzT9notQXef6cpX9DCp3ScxCX73eo4yNPG8jFUC9XIQKY+zSn4WY3XnOa5/6+7T6mF+psDzXl+hDE0oo56g1fzsMTebvnT3afUwP1NAr2tCGZr0cClnnmcN+dl/ghKg46PXNaEMTXYuY1uA9h1SBOj46HVNKEOT4QN0uATdfVI9zM4U0OuaUIYmPQZom17ms+Ebe5iS/KNnh9cfJ39Y3DsMw9v3Ky8v/vlWGIbv/Fr+BZrQ65pQhia7lzF6gPZ3WpR8GovjMA3Qp0dh7OqD+MGjW+HBu/Efzg7D1LUHPczm3tHrmlCGJj2UsXkDfohLx/qaqnwi8zAL0Fl4/f7yPI3T04MPludHd9N4jddJH/0kvNHHfO4bva4JZWjSRxlpno2Xn70Rz1u8hpkE6Nlhsu759CjKzsVxlJ3L0+j50yxd0+f9R69rQhma9FeGf/kpDtBoDfNv032g82wVcx7eWS7ej8Py7O0H+bPxCuqdHuZz3+h1TShDkx7LqCan+vwUB+gsvJEdRJqFd5NnTqPILK6BXjWx73OFXteEMjTpswxv1jxXhDMZb6GnAbo4zjbRk4f5PtCnR+HB3/1ry6e/AoAGcXCm/w2G/prqF4wYoMmOzaYAXR+FP/9JfAj+nX9oCNEhfxgA2CJbxS0/OWKAJvs1awFa3mhfPEoiNLz9WPQVyrC1pQllaOJfGT3uJBBNYp4cYm9cAy37l/hkehM7Q/1rkkaUoQll7Eefu1klUzg7TDLTIUAj50cchdeDMjShjP3Yd4DOw1y0dlk8Cp8rnP2ZnxHqNe+apBllaEIZ+6ErQOfZGua8uKa5OM5ilQBVhTI0oYz92HeAZrJt9uKVSGvzfNfnzMS1nN41STPK0IQy9mTfB5FSWYBGK5vX8mvhc/F5oO9GT5zfC7mUUw/K0IQy9qW/M/V7uJ3defFuTLn0PNDIwd3GT3vGwyZpQhmaUMbe9HalUx/3A43WMhtO91w8+mn09GutlyP5xccmaUAZmlCG/7y43lQBI01CGZpQhv8IUDdGmoQyNKEM/xGgbow0CWVoQhn+I0DdGGkSytCEMvxHgLox0iSUoQll+I8AdWOkSShDE8rwHwHqxkiTUIYmlOE/AtSNkSahDE0ow38EqBsjTUIZmlCG/whQN0aahDI0oQz/EaBujDQJZWhCGf4jQN0YaRLK0IQy/EeAAoAQAQoAQgQoAAgRoAAgRIACgBABCgBCBCgACBGgACBEgAKAEAEKAEIEKAAIEaAAIESAAoAQAQoAQgRo7unRjexPj34ahge376cPFvcOw3D1IHUart45C8O71ed0WHwSzfZr72YP/KyhTUM5Htg2Isuz6KmVu3uYwS3clw/tlfSJAM3NVvHxSTryBx/ED54eJQ+uPogfPLoVHrwbP5c+TF68sfr0nT3Mc6vzdLbDv4kfeFpDm6Zy9Ns6Itpjx3350F5JnwjQzGK2ypHTpAnOj9OmmIXX78cPrj+OXol65vzo7nJxnDZP9NbXDuMXluundFgch9fuLxf/lHa5nzW0aipHve0jEsVOOg4qdVk+dFfSLwI09egnYdYgUacny2P0V2vSCkmbPD2KemNxHL9wGvXGPPt7dRb+5yx0lLXMabZKMI9r8rSGNs3laOcwIpp//27Lh+ZKekaAJubRptVf0gbJt22TDdp59tfuPHqweD/OmbO3H0QNkr314IN5utk7V7X1u2rylJ81tGouRzm3EVEbO52XD7WV9I0ATcyv/bp2CCVpkFnW9/GL679hnx4lDRL3SZZDM1X7evImT/hZQ6vmcpRzGRHFsdNx+VBcSd8I0FylQbKtkmy3YNIS6x1us+Tp+K/fdMkoLx97F8/tX26FYdT2S19raNNWjm4uI6I8djosH8or6RMBmqs0SLJxUm6Q9SHf0/gv3nSzLMmhbB1Oi2hu76XHQO94W0ObtnJ0cxmRwrFrjfnTYflQXkmfCNBcuUFOk8OlhQYprZ4lYZM+N093BalaCTqNT5d5vFx8EpbXEnyqoU1bObq5jIjy2OmwfCivpE8EaK7UIKeHB3GcVP6GzS2Ok+PYN7JXtJ0AdBpmx4Nmya4pL2to01aObi4jorygDsuH8kr6RIDmig0yz04Tbl1a59Hz6Q70eNdhdkBGjbPDwmx7WkMbPwPUZUSUF9Rh+VBeSZ8I0FyhQT4JV2tjxaOM5TffXR10id5yquwEoHyDKvmDnzW0aitHNZcRUR47HZYP5ZX0iQDN5T2wmIXXVjt0VudGVs+RfHp04zTbvxMlz0zZ1m/hMqNkM93HGlq1laOay4goj50Oy4fySvpEgObyBpkV9nwXr7QomV3/H1nPnB3+22NthzNW1y0n5+p5WkOb1nJUcxgR5bHTYflQXkmfCNDcqkHmxSOH6SXM2bW+RfODW1nPRG9RtzF5dhjfICc95utrDW1ay1HNYUSUx06H5UN5JX0iQHOnq0vV8lMw4sfnxbvNFEQLxKpJZgpvOXOankiSHCr1tYY2beXotn1EivcwUhhAHZYP5ZX0iQDNZQ1yGpYaZHkenwB9u9YF0V+9d/IPKlyWz+PbNL5zf/XAyxratJSj3NYRUR47HZYP5ZX0iQAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFOqcBM99OcbnHwbPfLzL9wAEKNTpK0A//avNkyFAsSsCFOr0FKBbJ0OAYlcEKNTZNUBdJ0OAYlcEKNQhQOELAhTqEKDwBQEKderJ983PrgTBpZc/Sh5893rw6sXvvhcEz761ettn349efuXLLBGTzz8MEq8mb0/flQdm+e2VyQPuCFCoUwvQLAyDS/8xfhQl4r95I33ichKAF++lj575mVOAVt+eTz54ZcQiYQIBCnWqARoF3OU/LJefR6l5c5kEaBSlXy6/eS9Io/Ekir4vlxcfxqH48bJ6FL4WoNW3R5OPVz6//ZAERVcEKNSpBGiUgOnjaNUxTrw4QG9mj+MXvr6SJeRDpwCtvj16/Pzq9Uu/GqM+2EGAQp1KgK5zLc2+PFCjN8YJmO/KjF5wCNCTytvXh5KiQH5+8OJgCgEKdSoBmideHHnPr/4bS7KvEHsnDgFafXsxNXs6/I/pIEChTjnHsg319R8ribh+uHzoEKDr+M0fF3BeEzohQKFOLUDLq4g7B2jp7QQodkCAQp3Oa6ClVcrua6Ds+IQUAQp1tu8D3bRTsz1Aq/s8V4/Z8QkpAhTq1I7Cp2ctLZdPguwofOmw+vaj8GliZmdB5W/PHp/kB/nJUnRFgEKd7eeBbjyxsxKgeSw+qZz3uT4PNPu6J3lSA24IUKhzUjmqU7gSKU7K2qVFD5uvRIrPH734Inn5uY+WF7+7UrryqHQl0uVfRkH724AVUHREgEKdaoDWr4WvXNyefeDyfy0F6JP4uefX174/99ts4/3D8tvX18KTn+iIAIU6tQDNbpf0yhfJy1tvr5T/kx7Rh56P/pTeuunH67d/XrsbU/SG4MVfjlolLCBAYQdXEmFkBCh8d1I4rM4pnRgVAQrf5ac55YfjgZEQoPBdlJuXfhytf356hSsxMTICFN57ciU74nSZf5QD4yJA4b9vP0yPonMECSMjQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFAiAAFACECFACECFAAECJAAUCIAAUAIQIUAIQIUAAQIkABQIgABQAhAhQAhAhQABAiQAFA6P8Dc9K8VGze85MAAAAASUVORK5CYII=){role="img"
width="672"}

``` r
# Step 1: Identify restaurant chains (restaurants with multiple locations)
restaurant_chains <- D_A_cuisines %>%
  group_by(Restaurant.Name) %>%
  summarise(
    location_count = n_distinct(Restaurant.ID),  # Count distinct Restaurant IDs
    avg_rating = mean(Aggregate.rating),  # Calculate average rating
    total_votes = sum(Votes)  # Sum of votes (popularity)
  ) %>%
  filter(location_count > 1)  # Keep only restaurant chains (more than 1 location)


# Step 2: View the result
print(restaurant_chains)
```

    ## # A tibble: 734 × 4
    ##    Restaurant.Name           location_count avg_rating total_votes
    ##    <chr>                              <int>      <dbl>       <int>
    ##  1 10 Downing Street                      2       4           1340
    ##  2 221 B Baker Street                     3       3.37         215
    ##  3 34 Parkstreet Lane                     2       3.05          31
    ##  4 34, Chowringhee Lane                  12       2.39         777
    ##  5 4700BC Popcorn                         2       3.5          176
    ##  6 6 Pack Momos                           2       1.4            8
    ##  7 A Piece of Paris                       2       3.75         162
    ##  8 AB's - Absolute Barbecues              4       4.82       40200
    ##  9 AB's Absolute Barbecues                2       4.85        6302
    ## 10 Aap Ki Khatir                          2       0              0
    ## # ℹ 724 more rows

``` r
# You can sort by average rating or total votes to find the most popular chains
top_rated_chains <- restaurant_chains %>%
  arrange(desc(avg_rating))  # Sort by average rating

print(top_rated_chains)
```

    ## # A tibble: 734 × 4
    ##    Restaurant.Name           location_count avg_rating total_votes
    ##    <chr>                              <int>      <dbl>       <int>
    ##  1 Talaga Sampireun                       3       4.9        11028
    ##  2 AB's Absolute Barbecues                2       4.85        6302
    ##  3 Silantro Fil-Mex                       2       4.85        2728
    ##  4 AB's - Absolute Barbecues              4       4.82       40200
    ##  5 Naturals Ice Cream                     2       4.8         3094
    ##  6 Gymkhana                               2       4.7          756
    ##  7 The Cheesecake Factory                 2       4.65        6020
    ##  8 Dishoom                                2       4.61        4771
    ##  9 Chili's                                5       4.6        30215
    ## 10 Garota de Ipanema                      2       4.6          118
    ## # ℹ 724 more rows

``` r
most_popular_chains <- restaurant_chains %>%
  arrange(desc(total_votes))  # Sort by total votes

print(most_popular_chains)
```

    ## # A tibble: 734 × 4
    ##    Restaurant.Name           location_count avg_rating total_votes
    ##    <chr>                              <int>      <dbl>       <int>
    ##  1 Barbeque Nation                       26       4.33       58631
    ##  2 Big Chill                              4       4.47       43412
    ##  3 AB's - Absolute Barbecues              4       4.82       40200
    ##  4 Tea Villa Cafe                         4       3.92       31002
    ##  5 Chili's                                5       4.6        30215
    ##  6 Truffles                               2       4.32       29016
    ##  7 Haldiram's                            16       3.63       28445
    ##  8 Pirates of Grill                       4       4.03       27342
    ##  9 Subway                                63       2.91       24496
    ## 10 Out Of The Box                         2       3.89       23209
    ## # ℹ 724 more rows
::::::
