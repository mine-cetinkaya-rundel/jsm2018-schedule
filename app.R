# Load packages -----------------------------------------------------
library(shiny)
library(tidyverse)
library(DT)
library(glue)
library(lubridate)

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

# UI ----------------------------------------------------------------
ui <- navbarPage(
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
                 "Day(s)",
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
               
               sliderInput(
                 "time",
                 "Time range",
                 min = 7,
                 max = 23,
                 value = c(7, 23),
                 step = 1
               ),
               
               # Select sponsor(s) ----------------------------------
               selectInput(
                 "sponsors",
                 "Select sponsors",
                 choices = sponsors,
                 selected = "Section on Statistical Education",
                 multiple = TRUE,
                 selectize = TRUE
               ),
               
               # Select typess ------------------------------------
               selectInput(
                 "type",
                 "Type(s)",
                 choices = types,
                 selected = str_subset(types, "Invited"),
                 multiple = TRUE,
                 selectize = TRUE
               ),
               
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
    req(input$type)
    # Wrangle sponsor text ------------------------------------------
    sponsor_string <- glue_collapse(input$sponsors, sep = "|")
    cat(paste(sponsor_string, "\n"))
    # Filter and tabulate data --------------------------------------
    jsm_sessions %>%
      filter(
        day %in% input$day,
        type %in% input$type,
        beg_time_round >= input$time[1],
        end_time_round <= input$time[2],
        str_detect(sponsor, sponsor_string)
      ) %>%
      mutate(
        date_time = glue("{day}, {date}<br/>{time}"),
        session = glue('<a href="{url}">{session}</a>')
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
      mutate(title = glue('<a href="{url}">{title}</a>')) %>%
      select(title) %>%
      DT::datatable(
        rownames = FALSE,
        escape = FALSE,
        options = list(dom = "tp")
      )
    
  })
}

# Create the app object ---------------------------------------------
shinyApp(ui, server)
