library(tidyverse)
library(WDI)
library(tidyquant)

# World Bank GDP from API -------------------------------------------------

data <- WDI(country = c("MY", "ID", "SG"),
            indicator = c(gdp = "NY.GDP.MKTP.KD",      # GDP, 2015 USD
                          gdp_growth = "NY.GDP.MKTP.KD.ZG"),  # GDP growth, annual %
            extra = TRUE,  # Population, region, and other helpful columns
            start = 2010,
            end = 2020)

ggplot(data, aes(x = year, y = gdp_growth, color = country)) +
  geom_line() +
  scale_x_continuous(breaks = 2010:2020) +
  labs(x = NULL, y = "Annual GDP growth (%)", color = NULL)


# Your turn #3: Something from the World Bank -----------------------------

# Go to https://data.worldbank.org/, find an indicator, take note of its
# indicator code in the URL, use WDI() to download the data directly from the
# internet, and do something with the data.

# Do stuff here


# _________________________________________________________________________
# _________________________________________________________________________
# _________________________________________________________________________


# FRED retail sales from API ----------------------------------------------

data <- tq_get(x = c("CPILFESL",  # CPI
                     "RSXFSN",  # Advance retail sales
                     "USREC"),  # US recessions
               get = "economic.data",  # Use FRED
               from = "2000-01-01",
               to = "2022-09-01")

retail <- data %>% filter(symbol == "RSXFSN")

ggplot(retail, aes(x = date, y = price)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(x = NULL, y = "Retail sales\n(millions of dollars)")


# Create recession bars
recessions_start_end <- data %>% 
  filter(symbol == "USREC") %>% 
  mutate(recession_change = price - lag(price)) %>% 
  filter(recession_change != 0)

recessions <- tibble(start = filter(recessions_start_end, recession_change == 1)$date,
                     end = filter(recessions_start_end, recession_change == -1)$date)


# Plot recession bars too
ggplot(retail, aes(x = date, y = price)) +
  geom_rect(data = recessions, 
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "orange", alpha = 0.5) +
  geom_line() +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(x = NULL, y = "Retail sales\n(millions of dollars)")


# Your turn #4: Something from FRED ---------------------------------------

# Go to https://fred.stlouisfed.org/, find an indicator, take note of its
# indicator code in the URL, use tq_get() to download the data directly from the
# internet, and do something with the data.

# Do stuff here
