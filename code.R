# load packages -----------------------------------------------------
library(tidyverse)
library(rvest)
library(glue)

# read data ---------------------------------------------------------
jsm2018_html <- read_html("data/jsm2018.html")

# scrape session schedule -------------------------------------------

dates_times <- jsm2018_html %>%
  html_nodes("td tr:nth-child(1) td:nth-child(2) b") %>%
  html_text() %>%
  trim() %>%
  str_replace_all("\n", " ") %>%
  str_trim()

locations <- jsm2018_html %>%
  html_nodes("td~ td+ td b") %>%
  html_text()

sessions_types <- jsm2018_html %>%
  html_nodes("tr:nth-child(2) b") %>%
  html_text()

ids <- jsm2018_html %>%
  html_nodes("#bottom br+ a") %>%
  html_text() %>%
  str_remove("!") %>%
  str_remove("!$") %>%
  str_remove("\\*") %>%
  str_trim()

slugs <- jsm2018_html %>%
  html_nodes("#bottom br+ a") %>%
  html_attr("href")

sponsors <- jsm2018_html %>%
  html_nodes("tr:nth-child(3) td") %>%
  html_text() %>%
  str_trim() %>%
  str_replace_all("  ,|   ,| ,", ",")
  
jsm_sessions_raw <- tibble(
  date_time = dates_times,
  location = locations,
  id = ids,
  session_type = sessions_types,
  slug = slugs,
  sponsor = sponsors
)

jsm_sessions <- jsm_sessions_raw %>%
  # separate columns
  separate(date_time, into = c("day", "date", "time"), sep = ", ") %>%
  separate(time, into = c("beg_time", "end_time"), sep = " - ", remove = FALSE) %>%
  separate(session_type, into = c("session", "type"), sep = " \u2014 ") %>%
  # manual fixes
  mutate(
    # data error
    end_time = ifelse(id == "581", "3:50 PM", end_time),
    # compose URLs
    url = ifelse(
      type == "Professional Development Continuing Education Course", slug,
      glue("http://ww2.amstat.org/meetings/jsm/2018/onlineprogram/{slug}")
    ),
    # reduce type levels
    type = case_when(
      str_detect(type, "Roundtable")               ~ "Roundtable",
      str_detect(type, "Professional Development") ~ "Professional Development",
      str_detect(type, "Other")                    ~ "Other",
      TRUE                                         ~ type
    ),
    # civilian to military time
    beg_time_round = format(strptime(beg_time, "%I:%M %p"), format="%H:%M:%S") %>% str_remove("\\:.+") %>% as.numeric(),
    end_time_round = format(strptime(end_time, "%I:%M %p"), format="%H:%M:%S") %>% str_remove("\\:.+") %>% as.numeric(),
    end_time_round = ifelse(str_detect(end_time, "\\:[1-5]"), end_time_round+1, end_time_round),
    # for convenience
    end_time_round = ifelse(id == "216596", 23, end_time_round)
  ) %>%
  select(-slug)

write_csv(jsm_sessions, path = "data/jsm2018_sessions.csv")

# scrape talk info -------------------------------------------------

titles <- jsm2018_html %>%
  html_nodes("tr+ tr td+ td a") %>%
  html_text()

urls <- jsm2018_html %>%
  html_nodes("tr+ tr td+ td a") %>%
  html_attr("href")

jsm_talks_raw <- tibble(
  title = titles,
  url = glue("http://ww2.amstat.org/meetings/jsm/2018/onlineprogram/{urls}")
)

jsm_talks <- jsm_talks_raw %>%
  mutate(
    has_R       = str_detect(title, " R | R$"),
    has_tidy    = str_detect(title, "[tT]idy"),
    has_shiny   = str_detect(title, "[sS]hiny"),
    has_python  = str_detect(title, "[pP]ython"),
    has_rstudio = str_detect(title, "RStudio|R Studio"),
    has_fee     = str_detect(title, "(ADDED FEE)")
  )

write_csv(jsm_talks, "data/jsm2018_talks.csv")
