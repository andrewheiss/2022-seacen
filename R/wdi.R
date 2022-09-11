library(tidyverse)
library(WDI)


wdi_raw <- read_csv(here::here("R/API_NY.GDP.MKTP.KD_DS2_en_csv_v2_4487681.csv"),
                    skip = 3)

wdi_clean <- wdi_raw %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  pivot_longer(-c(`Country Name`, `Country Code`)) %>%
  mutate(year = as.numeric(name)) %>%
  filter(year >= 2010, `Country Code` %in% c("MYS", "IDN", "SGP"))

ggplot(wdi_clean, aes(x = year, y = value, color = `Country Name`)) +
  geom_line() +
  scale_x_continuous(breaks = 2010:2020) +
  labs(x = NULL, y = "GDP", color = NULL)

data <- WDI(country = c("MY", "ID", "SG"),
            indicator = c("NY.GDP.MKTP.KD",      # GDP, 2015 USD
                          "NY.GDP.MKTP.KD.ZG"),  # GDP growth, annual %
            extra = TRUE,  # Population, region, and other helpful columns
            start = 2010,
            end = 2020)

saveRDS(tibble(data), here::here("R/wdi_data.rds"))


data1 <- WDI(country = c("MY", "ID", "SG"),
             indicator = c(gdp = "NY.GDP.MKTP.KD",      # GDP, 2015 USD
                           gdp_growth = "NY.GDP.MKTP.KD.ZG"),  # GDP growth, annual %
             extra = TRUE,  # Population, region, and other helpful columns
             start = 2010,
             end = 2020)

saveRDS(tibble(data1), here::here("R/wdi_data1.rds"))


library(tidyquant)
data <- tq_get(x = c("CPILFESL",  # CPI
                     "RSXFSN",  # Advance retail sales
                     "USREC"),  # US recessions
               get = "economic.data",  # Use FRED
               from = "2000-01-01",
               to = "2022-09-01")

saveRDS(data, here::here("R/fred_data.rds"))


library(httr)
library(jsonlite)
r <- GET("https://www.econdb.com/api/series/URATEMY/?format=json")
r$content

headers(r)
asdf <- data.frame(fromJSON(content(r, "text"))$data)

parse_url("https://www.econdb.com/api/series/URATEMY/?format=json")

modify_url()

# Build the URL query
api_url <- modify_url("https://www.econdb.com/",
                     path = "api/series/URATEMY",
                     query = list(format = "json"))

# Submit the query
r <- GET(api_url)

saveRDS(r, here::here("R/econdb-json-r.rds"))

# See the results
headers(r) %>% head()
content(r) %>% head()


1626926400000

1648565400

"https://api.polygon.io/v2/aggs/ticker/AAPL/range/1/day/2020-06-01/2020-06-17?apiKey=ItjViWxsrsa5Da05vVxhG2eU3XfnU2RR"

library(httr)
polygon_url <- modify_url("https://api.polygon.io",
                          path = "v2/aggs/ticker/AAPL/range/1/day/2022-08-01/2022-09-09",
                          query = list(apiKey = "ItjViWxsrsa5Da05vVxhG2eU3XfnU2RR"))
r <- GET(polygon_url)

saveRDS(r, here::here("R/polygon-json-r.rds"))

aapl <- data.frame(fromJSON(content(r, "text"))$results)



content(r)



library(lubridate)
"ItjViWxsrsa5Da05vVxhG2eU3XfnU2RR"



library(rvest)

wiki_url <- "https://en.wikipedia.org/wiki/Chair_of_the_Federal_Reserve"

wiki_raw <- read_html(wiki_url)

xml2::write_html(wiki_raw, here::here("R/wiki-fed-raw.html"))

saveRDS(wiki_raw, here::here("R/wiki-fed-raw.rds"))

wiki_raw <- read_html(wiki_url) %>%
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>%
  html_table() %>%
  bind_rows()

wiki_raw %>%
  slice(-1) %>%
  separate(`Name(birth–death)`, into = c("Name", "birth-death"), sep = "\\(")

wiki_clean <- wiki_raw %>%
  # Remove first row
  slice(-1) %>%
  # Extract name
  separate(`Name(birth–death)`, into = c("Name", "birth-death"), sep = "\\(") %>%
  mutate(Name = str_remove(Name, "\\[.\\]")) %>%
  # Calculate duration in office
  mutate(tenure_length = as.period(`Tenure length`)) %>%
  mutate(seconds = as.numeric(tenure_length)) %>%
  mutate(years = seconds / 60 / 60 / 24 / 365.25) %>%
  # Put name in order of duration
  arrange(tenure_length) %>%
  mutate(Name = fct_inorder(Name))

ggplot(wiki_clean, aes(x = years, y = Name)) +
  geom_col()
