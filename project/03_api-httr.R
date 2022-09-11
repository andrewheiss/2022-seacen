library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)


# Malaysian unemployment from econdb API ----------------------------------

# Build the URL query
api_url <- modify_url("https://www.econdb.com/",
                      path = "api/series/URATEMY",
                      query = list(format = "json"))

# Submit the query
r <- GET(api_url)

# See the HTTP headers, for fun
headers(r)

# See the HTTP content, for fun
content(r)

# See the raw HTTP content as text, for fun
content(r, "text")

# Extract the data from the "data" slot in the JSON from the API
api_data <- data.frame(fromJSON(content(r, "text"))$data)
api_data

# Plot the data
api_data_clean <- api_data %>% 
  mutate(dates = ymd(dates)) %>% 
  filter(dates > ymd("2010-01-01"))

ggplot(api_data_clean, aes(x = dates, y = values)) +
  geom_line() +
  labs(x = NULL, y = "Unemployment rate\nin Malaysia")


# Your turn #5: Something from econdb -------------------------------------

# Go to https://www.econdb.com, find an indicator, take note of its URL, build a
# query and use GET() to pull JSON data directly from the API, and do something
# with the data


# Do stuff here


# _________________________________________________________________________
# _________________________________________________________________________
# _________________________________________________________________________


# Bonus: Polygon.io -------------------------------------------------------

# Go to https://polygon.io/, create a free account, and generate an API key. Put
# that API key where it currently says SUPER_SECRET_THING in the code

# Build URL
polygon_url <- modify_url(
  "https://api.polygon.io",
  path = "v2/aggs/ticker/AAPL/range/1/day/2022-08-01/2022-09-09",
  query = list(apiKey = "SUPER_SECRET_THING")
)

# Get data
r <- GET(polygon_url)

# Clean data
aapl <- data.frame(fromJSON(content(r, "text"))$results) %>% 
  mutate(ts = as_datetime(t / 1000))

# Plot data
ggplot(aapl, aes(x = ts, y = c)) +
  geom_line() +
  labs(x = NULL, y = "Closing stock price", title = "AAPL stock price", 
       subtitle = "August 1-September 9, 2022", caption = "Source: Polygon.io")
