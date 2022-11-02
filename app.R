# load packages ----
library(tidyverse)
library(lubridate)
library(shiny)
library(dplyr)
library(bslib)
library(thematic)
library(shinyjs)

# apply thematic over ggplot in server code ---- 
thematic_shiny(
    bg = "auto",
    fg = "auto",
    accent = "auto",
    font = "auto",
    sequential = sequential_gradient(),
    qualitative = okabe_ito(),
    inherit = FALSE,
    session = shiny::getDefaultReactiveDomain()
    )
light <- bs_theme(version = 4, bootswatch = "lux", primary = "#75B3CE")

# define UI ----- 
ui <- fluidPage(
  
    # enable shinyjs functions
    useShinyjs(),
  
    # load theme
    theme = light, 
    
    # leave title blank for cleaner look
    titlePanel(""),
    
    # define inputs options on sidebar
    sidebarLayout(
        sidebarPanel(
          h6("Edit view"),
          sliderInput(
              inputId = "hours_per_day",
              label = "Hours per day",
              value = 3,
              min = 2,
              max = 10
              ),
          # sliderInput(
          #     inputId = "max_daily_task_hours",
          #     label = "Max daily hours per task",
          #     value = 1,
          #     min = 1,
          #     max = 10
          #     ),
          dateRangeInput(
              inputId = "dates", 
              label = "Date range",
              format = "yyyy-mm-dd",
              weekstart = 1,
              start = today(tzone = "US/Central"),
              end = ceiling_date(today(tzone = "US/Central"), 'week'),
              min = floor_date(today(tzone = "US/Central"), 'week'),
              max = ceiling_date(today(tzone = "US/Central"), 'week') + 21
              ),
          checkboxInput("weekend", "Include weekend"),
          br(),
          p(h6("Manage tasks")),
          selectInput(
              inputId = "add_remove",
              label = NULL,
              choices = c(" ", "Add" = 1, "Remove" = 2, "Hide"),
              selected = NULL
          ),
          conditionalPanel(
              condition = "input.add_remove == 2",
              textInput(
                inputId = "name_2",
                label = "Name"
              ),
              numericInput(
                inputId = "task_hour", 
                label = "Hours", 
                value = NULL, 
                min = 0, 
                max = 10, 
                step = 0.5
              ),
              actionButton(
                  inputId = "remove",
                  label = "Remove",
                  icon = NULL
              ),
              actionButton(
                  inputId = "clear",
                  label = "Clear",
                  icon = NULL
              )
          ),
          conditionalPanel(
              condition = "input.add_remove == 1",
              textInput(
                  inputId = "name",
                  label = "Name"
              ),
              numericInput(
                  inputId = "priority_level", 
                  label = "Priority level (0-10)", 
                  value = NULL, 
                  min = 0, 
                  max = 10, 
                  step = 1
                  ),
              numericInput(
                  inputId = "estimated_duration",
                  label = "Estimated duration (hrs)",
                  value = NULL,
                  min = 0.5,
                  max = 24,
                  step = 0.5
                  ),
              dateInput(
                inputId = "deadline",
                label = "Deadline",
                value = "none",
                min = NULL,
                max = NULL,
                format = "yyyy-mm-dd",
                startview = "month",
                weekstart = 1,
                language = "en"
                ),
              actionButton(
                  inputId = "add",
                  label = "Add",
                  icon = NULL
              )
          ),
          br(),
          p(h6("Export calendar")),
          downloadButton("export", "CSV"),
          height = 5,
          width = 3
          ),
      mainPanel(
        plotOutput("distPlot"),
        # DT::dataTableOutput("mytable")
          )
    )
)

# define server ----
server <- function(input, output, session) {
    
  # limit number of characters in textInput 
  shinyjs::runjs("$('#name').attr('maxlength', 15)")
  shinyjs::runjs("$('#name_2').attr('maxlength', 15)")
  
  # load initial data that contains tasks as rows and characteristics as columns
  task_dat <- tibble(
    task = character(),
    deadline = as.Date(character()),
    priority_level = numeric(),
    hours_per_task = numeric(),
    interval = interval(),
    cum_hours_per_task = numeric()
    ) %>% 
    arrange(desc(priority_level)) %>% 
    arrange(deadline)
  
    values <- reactiveValues(task_dat = NULL)
 
    # load previous data if exists    
    if (file.exists("task_dat.rds")) 
      {values$task_dat <- read_rds("task_dat.rds")} 
    else 
      {values$task_dat <- task_dat}
    
  # add tasks to calendar when "Add" is clicked
    observeEvent(input$add, {
      req(input$name)
      req(input$priority_level)
      req(between(input$priority_level, 0, 10))
      req(input$estimated_duration)
      req(input$deadline)
      temp <- values$task_dat %>% 
        add_row(
          task = rep(input$name, input$estimated_duration/0.5),
          priority_level = rep(input$priority_level, input$estimated_duration/0.5),
          deadline = rep(input$deadline, input$estimated_duration/0.5)
        ) %>%
        arrange(desc(priority_level)) %>% 
        arrange(deadline) 
      values$task_dat <- temp
    })
    
    # remove tasks from calendar when "Remove" is clicked
    observeEvent(input$remove, {
      req(input$name_2)
      req(input$task_hour)
      temp <- values$task_dat %>%
        group_by(task) %>% 
        mutate(index = 1:n()) %>% 
        filter(!(task == input$name_2 & index %in% 1:(input$task_hour/0.5))) %>%
        ungroup() %>%
        select(-index)
      values$task_dat <- temp
    })
    
    # clear tasks from calendar when "Clear" is clicked
    observeEvent(input$clear, {
      temp <- values$task_dat %>% 
        slice(0)
      values$task_dat <- temp
    })
    
    # save calendar automatically when app closes
    save_function <- session$onSessionEnded(function() {
      isolate(write_rds(values$task_dat, "task_dat.rds"))
    })
    
    # process data
    processed_dat <- reactive({
      # create a dataset with date information
      date_dat <- tibble(
        day = today(tzone = "US/Central") + days(0:364),
        week = isoweek(day),
        month = month(day, label = TRUE, abbr = TRUE),
        day_of_week = wday(day, label = TRUE, abbr = TRUE)
        ) %>%
        filter(if (input$weekend == F) (day_of_week != "Sun" & day_of_week != "Sat") else day_of_week != "nothing") %>%
        select(month, week, day, day_of_week) %>% 
        mutate(
          day_of_week = factor(day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
          id = row_number(),
          day_label = str_replace(str_sub(day, 6, 10), "-", "/")
        ) 
      # combine task dataset with date dataset and assign dates to tasks
      values$task_dat %>% 
          mutate(
            hours_per_task = 0.5,
            interval = cut_interval(cumsum(hours_per_task), length = input$hours_per_day)
          ) %>% 
          group_by(interval) %>%
          mutate(id = cur_group_id()) %>%
          ungroup() %>%
          left_join(date_dat, by = "id") %>% 
          filter(between(day, input$dates[1], input$dates[2])) %>%
          mutate(
            flag = case_when(
              ymd(day) < ymd(deadline) ~ 1,
              ymd(day) == ymd(deadline) ~ 0.5,
              TRUE ~ 0
              )
            )
    })
    
    # output table for debugging
    output$mytable = DT::renderDataTable({
      req(length(values$task_dat$task) > 0)
      processed_dat()
    })
    
    # output calendar
    output$distPlot <- renderPlot({
      req(length(values$task_dat$task) > 0)
      processed_dat() %>% 
          ggplot(aes(x = day_of_week, y = hours_per_task, fill = priority_level)) +
              geom_col(
                  position = "stack",
                  aes(color = flag)
                  ) +
              geom_text(
                  aes(label = task),
                  position = position_stack(vjust = 0.5),
                  color = "white",
                  hjust = "center",
              ) +
              geom_text(
                aes(label = day_label),
                nudge_y = 0.7,
                size = 3,
                color = "#9D9F9F"
              ) +
              scale_x_discrete(position = "top") +
              scale_color_gradientn(
                name = "Deadline",
                limits = c(0, 1),
                colors = c("red", "orange", "green"),
                breaks = c(0, 0.5, 1),
                labels = c("Overdue", "Due", "On time")
              ) +
              labs(
                fill = "Priority level",
                y = "Hours"
                ) +
              scale_y_reverse() +
              facet_wrap(
                ~week,
                ncol = 1
                ) +
              theme(
                plot.title = element_text(face = "bold", size = 20),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(size = 15, color = "#9D9F9F"),
                axis.text.y = element_text(color = "#9D9F9F"),
                legend.position = "right",
                legend.justification = "top",
                strip.text = element_blank()
              )
    },
    height = 400,
    width = 900
    )
    
    # further process processed data to make it ready for export
    export_dat <- reactive({
      processed_dat() %>%
        select(task, day, hours_per_task) %>%
        group_by(day) %>%
        mutate(
          `Start Time` = str_replace(ymd_hms(str_c(day, "10:00:00")) + minutes((cumsum(hours_per_task) - 0.5) * 60), "^\\S* ", ""),
          `End Time` = str_replace(ymd_hms(str_c(day, "10:00:00")) + minutes((cumsum(hours_per_task)) * 60), "^\\S* ", "")
          ) %>%
        ungroup() %>%
        select(-hours_per_task) %>%
        mutate(
          `All Day Event` = FALSE
        ) %>%
        rename(
          Subject = task,
          `Start Date` = day
        )
    })

    # export data as CSV
    output$export <- downloadHandler(
      filename = function(file) {
        "clarify.csv"
      },
      content = function(file) {
        write.csv(export_dat(), file, row.names = FALSE)
      }
    )
}


# run app ----
shinyApp(ui, server)
