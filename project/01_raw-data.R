library(tidyverse)

# World Bank GDP from CSV file --------------------------------------------

gdp_raw <- read_csv("data/API_NY.GDP.MKTP.KD_DS2_en_csv_v2_4487681.csv",
                    skip = 3)
gdp_raw

# oooof this is a mess
gdp_clean <- gdp_raw %>% 
  select(-`Indicator Name`, -`Indicator Code`) %>% 
  pivot_longer(-c(`Country Name`, `Country Code`)) %>% 
  mutate(year = as.numeric(name)) %>% 
  filter(year >= 2010, `Country Code` %in% c("MYS", "IDN", "SGP"))
gdp_clean

ggplot(gdp_clean, aes(x = year, y = value, color = `Country Name`)) +
  geom_line() +
  scale_x_continuous(breaks = 2010:2020) +
  labs(x = NULL, y = "GDP", color = NULL)


# Your turn #1: Something from the World Bank -----------------------------

# Go to https://data.worldbank.org/, find an indicator, download its CSV file,
# put it in the data folder here, and do something with the data

# Do stuff here


# _________________________________________________________________________
# _________________________________________________________________________
# _________________________________________________________________________


# FRED CPI from CSV -------------------------------------------------------

cpi_raw <- read_csv("data/CPILFESL.csv")
cpi_raw

ggplot(cpi_raw, aes(x = DATE, y = CPILFESL)) +
  geom_line() +
  labs(x = NULL, y = "CPI")


# Your turn #2: Something from FRED ---------------------------------------

# Go to https://fred.stlouisfed.org/, find an indicator, download its CSV file,
# put it in the data folder here, and do something with the data

# Do stuff here
