library(tidyverse)
library(lubridate)
library(rvest)


# Federal Reserve chairs from Wikipedia -----------------------------------

wiki_url <- "https://en.wikipedia.org/wiki/Chair_of_the_Federal_Reserve"

# Even better to use the Internet Archive since web pages change over time
wiki_url <- "https://web.archive.org/web/20220908211042/https://en.wikipedia.org/wiki/Chair_of_the_Federal_Reserve"

wiki_raw <- read_html(wiki_url)
wiki_raw

wiki_extracted <- wiki_raw %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]") %>% 
  html_table() %>% 
  bind_rows()
wiki_extracted

wiki_clean <- wiki_extracted %>% 
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
wiki_clean

# Plot the data
ggplot(wiki_clean, aes(x = years, y = Name)) +
  geom_col(fill = "darkgreen") +
  labs(x = "Years in office", y = NULL)


# _________________________________________________________________________
# _________________________________________________________________________
# _________________________________________________________________________


# Demonstration together: Kuala Lumpur historical demographics ------------

# https://en.wikipedia.org/wiki/Kuala_Lumpur#Historical_demographics


# _________________________________________________________________________
# _________________________________________________________________________
# _________________________________________________________________________


# Your turn #6: Something from Wikipedia ----------------------------------

# Find a table on some Wikipedia article somewhere and use rvest to scrape and
# parse it in R
