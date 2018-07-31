# Load packages -----------------------------------------------------
library(shiny)
library(tidyverse)
library(DT)
library(glue)
library(lubridate)
library(shinythemes)

# Load data ---------------------------------------------------------
jsm_sessions <- read_csv("data/jsm2018_sessions.csv")
jsm_talks <- read_csv("data/jsm2018_talks.csv")

# Create lists for use later ----------------------------------------
sponsors <- glue_collapse(jsm_sessions$sponsor, sep = ", ") %>%
  str_split(", ") %>%
  pluck(1) %>%
  str_trim() %>%
  unique() %>%
  sort()

types <- jsm_sessions %>%
  distinct(type) %>%
  arrange() %>%
  pull()

types <- c(
  keep(types, str_detect, pattern = "^Invited") %>% sort(),
  keep(types, str_detect, pattern = "^Topic") %>% sort(),
  keep(types, str_detect, pattern = "^Contributed") %>% sort(),
  discard(types, str_detect, pattern = "^Invited|^Topic|^Contributed|^Other") %>% sort(),
  "Other"
)

# UI ----------------------------------------------------------------
ui <- navbarPage(
  theme = shinytheme("cosmo"),
  "JSM 2018",
  
  # Tab 1: Session schedule -----------------------------------------
  tabPanel("Session Schedule",
           sidebarLayout(
             sidebarPanel(
               # Instructions ---------------------------------------
               h4("Select date/time, sponsors, and type of session."),
               br(),
               
               # Select day(s) --------------------------------------
               checkboxGroupInput(
                 "day",
                 "Day",
                 choices = c(
                   "Fri, Jul 27" = "Fri",
                   "Sat, Jul 28" = "Sat",
                   "Sun, Jul 29" = "Sun",
                   "Mon, Jul 30" = "Mon",
                   "Tue, Jul 31" = "Tue",
                   "Wed, Aug 1"  = "Wed",
                   "Thu, Aug 2"  = "Thu"
                 ),
                 selected = wday(Sys.Date(), label = TRUE, abbr = TRUE)
               ),
               
               # Select times ---------------------------------------
               sliderInput(
                 "time",
                 "Time",
                 min = 7,
                 max = 23,
                 value = c(8, 18),
                 step = 1
               ),
               
               # Select sponsor(s) ----------------------------------
               selectInput(
                 "sponsor",
                 "Session sponsor",
                 choices = sponsors,
                 selected = c(
                   "Section on Statistical Education",
                   "Section on Statistical Computing",
                   "Section on Statistical Graphics"
                 ),
                 multiple = TRUE,
                 selectize = TRUE
               ),
               
               # Select typess ------------------------------------
               selectInput(
                 "type",
                 "Type of session",
                 choices = types,
                 multiple = TRUE,
                 selectize = TRUE
               ),
               
               br(),
               
               # Excluded fee events --------------------------------
               checkboxInput("exclude_fee",
                             "Exclude added fee events"),
               
               width = 3
             ),
             
             # Output -----------------------------------------------
             mainPanel(DT::dataTableOutput(outputId = "schedule"), width = 9)
             
           )),
  
  # Tab 2: Talk finder
  tabPanel("Talk Finder",
           sidebarLayout(
             sidebarPanel(
               # Instructions ---------------------------------------
               h4("Search for keywords in talk/workshop titles."),
               br(),
               
               # Keyword selection ----------------------------------
               checkboxGroupInput(
                 "keyword_choice",
                 "Select keywords you're interested in",
                 choices = c(
                   "R"       = "( R | R$)",
                   "tidy"    = "tidy",
                   "Shiny"   = "shiny",
                   "RStudio" = "(RStudio|R Studio)",
                   "Python"  = "python"
                 ),
                 selected = "( R | R$)"
               ),
               
               # Other ----------------------------------------------
               textInput(
                 "keyword_text",
                 "Add additional keywords or phrases separated by commas"
               ),
               
               br(),
               
               # Excluded fee events --------------------------------
               checkboxInput("exclude_fee",
                             "Exclude added fee events")
               
             ),
             
             # Output -----------------------------------------------
             mainPanel(DT::dataTableOutput(outputId = "talks"))
             
           ))
)

# Server ------------------------------------------------------------
server <- function(input, output) {
  
  # Sessions --------------------------------------------------------
  output$schedule <- DT::renderDataTable({

    # Require inputs ------------------------------------------------
    req(input$day)
    
    # Wrangle sponsor text ------------------------------------------
    sponsor_string <- glue_collapse(input$sponsor, sep = "|")
    if (length(sponsor_string) == 0)
      sponsor_string <- ".*"
    
    session_type <- input$type
    if (length(session_type) == 0)
      session_type <- types
    
    # Exclude fee events --------------------------------------------
    if (input$exclude_fee) {
      jsm_sessions <- jsm_sessions %>% filter(has_fee == FALSE)
    }
    
    # Filter and tabulate data --------------------------------------
    jsm_sessions %>%
      filter(
        day %in% input$day,
        type %in% session_type,
        beg_time_round >= input$time[1],
        end_time_round <= input$time[2],
        str_detect(sponsor, sponsor_string)
      ) %>%
      mutate(
        date_time = glue("{day}, {date}<br/>{time}"),
        session = glue('<a href="{url}" target="_blank">{session}</a>')
      ) %>%
      select(date_time, session, location, type, sponsor) %>%
      DT::datatable(rownames = FALSE, escape = FALSE) %>%
      formatStyle(columns = "date_time",
                  fontSize = "80%",
                  width = "100px") %>%
      formatStyle(columns = "session", width = "450px") %>%
      formatStyle(columns = c("location", "type"), width = "100px") %>%
      formatStyle(columns = "sponsor",
                  fontSize = "80%",
                  width = "200px")
    
  })
  
  # Talks -----------------------------------------------------------
  output$talks <- DT::renderDataTable({
    # Exclude fee events
    if (input$exclude_fee) {
      jsm_talks <- jsm_talks %>% filter(has_fee == FALSE)
    }
    
    # Create pattern
    keywords <- input$keyword_text %>%
      str_split(",") %>%
      pluck(1) %>%
      str_trim() %>%
      discard( ~ .x == "")
    
    keyword_regex <- c(input$keyword_choice, keywords)
    
    if (length(keyword_regex) == 0) {
      keyword_regex = ""
    }
    
    matching_titles <- keyword_regex %>%
      tolower() %>%
      map(str_detect, string = tolower(jsm_talks$title)) %>%
      reduce(`&`)
    
    # Subset for pattern
    jsm_talks %>%
      filter(matching_titles) %>%
      mutate(title = glue('<a href="{url}" target="_blank">{title}</a>')) %>%
      select(title) %>%
      DT::datatable(
        rownames = FALSE,
        escape = FALSE,
        options = list(dom = "ltp")
      )
    
  })
}

# Create the app object ---------------------------------------------
shinyApp(ui, server)
