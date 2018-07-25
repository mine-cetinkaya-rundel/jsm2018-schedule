# Load packages -----------------------------------------------------
library(shiny)
library(tidyverse)
library(DT)

# Load data ---------------------------------------------------------
jsm_sessions <- read_csv("data/jsm2018_sessions.csv")
jsm_talks <- read_csv("data/jsm2018_talks.csv")

ui <- navbarPage(
  "JSM 2018",
  
  # Tab 1: Session schedule -----------------------------------------
  tabPanel("Session Schedule",
           sidebarLayout(
             sidebarPanel(
               HTML(
                 "Select day(s) and sponsor(s) to get started. Scroll
                 down to limit session types."
               ),
               br(),
               br(),
               
               # Select day(s) -----------------------
               checkboxGroupInput(
                 "day",
                 "Day(s)",
                 choices = c(
                   "Fri, Jun 27" = "Fri",
                   "Sat, Jun 28" = "Sat",
                   "Sun, Jun 29" = "Sun",
                   "Mon, Jun 30" = "Mon",
                   "Tue, Jun 31" = "Tue",
                   "Wed, Aug 1"  = "Wed",
                   "Thu, Aug 2"  = "Thu"
                 )
               ),
               
               # Select start and end time -----------
               sliderInput(
                 "beg_time",
                 "Start time",
                 min = 7,
                 max = 23,
                 value = 8,
                 step = 1
               ),
               
               sliderInput(
                 "end_time",
                 "End time",
                 min = 7,
                 max = 23,
                 value = 17,
                 step = 1
               ),
               
               # Select sponsor(s) -------------------
               checkboxGroupInput(
                 "sponsor_check",
                 "Sponsor(s)",
                 choices = c(
                   "Statistical Education",
                   "Statistical Computing",
                   "Statistical Graphics",
                   "Statistical Learning and Data Science"
                 )
               ),
               
               # Select other sponsor(s) -------------
               textInput("sponsor_text",
                         "For other sponsor(s) type keyword(s)"),
               
               # Select types(s) ---------------------
               checkboxGroupInput(
                 "type",
                 "Type(s)",
                 choices = sort(unique(jsm_sessions$type)),
                 selected = unique(jsm_sessions$type)
               )
               
               ),
             
             # Output --------------------------------
             mainPanel(DT::dataTableOutput(outputId = "schedule"))
             
           )),
  
  # Tab 2: Talk finder
  tabPanel("Talk Finder",
           sidebarLayout(
             sidebarPanel(
               # Has R -------------------------------------------------------
               checkboxInput("has_R",
                             "R in title"),
               
               # Has tidy ----------------------------------------------------
               checkboxInput("has_tidy",
                             "tidy in title"),
               
               # Has Shiny ---------------------------------------------------
               checkboxInput("has_shiny",
                             "Shiny in title"),
               
               # Has RStudio -------------------------------------------------
               checkboxInput("has_rstudio",
                             "RStudio in title"),               
               
               # Has Python --------------------------------------------------
               checkboxInput("has_python",
                             "Python in title"),
               
               # Other -------------------------------------------------------
               textInput("has_something",
                         "Other keywords"),
               
               # Excluded fee events -----------------------------------------
               checkboxInput("exclude_fee",
                             "Exclude added fee events")
               
             ),
             
             # Output --------------------------------------------------------
             mainPanel(DT::dataTableOutput(outputId = "talks"))
             
           ))
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  # Sessions --------------------------------------------------------
  output$schedule <- DT::renderDataTable({
    # Require inputs ------------------------------------------------
    req(input$day)
    req(input$type)
    req(input$sponsor_check)
    # Wrangle sponsor text ------------------------------------------
    sponsor_check_string <- unlist(input$sponsor_check) %>%
      as.character() %>%
      paste0(collapse = "|")
    sponsor_text_string <- as.character(input$sponsor_text)
    sponsor_string <-
      paste(sponsor_check_string, sponsor_text_string, sep = "|") %>%
      str_replace_all(" ", "|") %>%
      tolower()
    # Filter and tabulate data --------------------------------------
    jsm_sessions %>%
      filter(
        day %in% input$day,
        type %in% input$type,
        beg_time_round >= input$beg_time,
        end_time_round <= input$end_time,
        str_detect(tolower(sponsor), sponsor_string)
      ) %>%
      mutate(
        date_time = glue("{day}, {date}, {time}"),
        session = glue('<a href="{url}">{session}</a>')
      ) %>%
      select(date_time, session, type, sponsor, location, id) %>%
      DT::datatable(rownames = FALSE, escape = FALSE)
    
  })
  
  # Talks -----------------------------------------------------------
  output$talks <- DT::renderDataTable({
    if (input$has_R)     {
      jsm_talks <- jsm_talks %>% filter(has_R)
    }
    if (input$has_tidy)  {
      jsm_talks <- jsm_talks %>% filter(has_tidy)
    }
    if (input$has_shiny) {
      jsm_talks <- jsm_talks %>% filter(has_shiny)
    }
    if (input$has_rstudio) {
      jsm_talks <- jsm_talks %>% filter(has_rstudio)
    }
    if (input$has_python) {
      jsm_talks <- jsm_talks %>% filter(has_python)
    }
    if (input$exclude_fee) {
      jsm_talks <- jsm_talks %>% filter(has_fee == FALSE)
    }
    
    jsm_talks %>%
      filter(str_detect(title, input$has_something)) %>%
      mutate(title = glue('<a href="{url}">{title}</a>')) %>%
      select(title) %>%
      DT::datatable(rownames = FALSE, escape = FALSE)
    
  })
}

# Create the app object ---------------------------------------------
shinyApp(ui, server)
