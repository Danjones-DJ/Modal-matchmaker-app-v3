# Packages ----------------------------------------------------------------
pacman::p_load(tidyverse, shiny, bslib, jsonlite, RColorBrewer)
options(scipen = 999)
# df = read_csv("Version3-CAH-ALLUNI.csv")
# jobsdf = read_csv("JobsDataModal.csv")

df      <- readRDS("TESTINGCOMP.Version3-CAH-ALLUNI.rds")
jobsdf  <- readRDS("TESTINGCOMP.JobsDataModal.rds")

df_long <- df %>%
  select(kiscourseid, starts_with("JOB_")) %>%
  pivot_longer(
    cols = starts_with("JOB_"),
    names_to = "Job",
    values_to = "Proportion"
  )

grade_map = list(
  "None" = c("No A Levels", "Less than EEE"),
  "EEE+" = c("EEE", "DEE"),  
  "DDE+" = c("DDE", "DDD"),  
  "CDD+" = c("CDD", "CCD"),  
  "CCC+" = c("CCC", "BCC"),  
  "BBC+" = c("BBC", "BBB"),  
  "ABB+" = c("ABB", "AAB"), 
  "AAA+" = c("AAA", "A*AA"),  
  "A*A*A+" = c("A*A*A", "A*A*A*", "A*A*A*E", "A*A*A*C"), 
  "A*A*A*A+" = c("A*A*A*A", "A*A*A*A*", "A*A*A*A*")
)

band_to_col <- c(
  "None"      = "None",
  "EEE+"      = "EEE",
  "DDE+"      = "DDE",
  "CDD+"      = "CDD",
  "CCC+"      = "CCC",
  "BBC+"      = "BBC",
  "ABB+"      = "ABB",
  "AAA+"      = "AAA",
  "A*A*A+"    = "A*A*A",
  "A*A*A*A+"  = "A*A*A*A"
)

potGrades = c("EEE", "DEE", "DDE", "DDD", "CDD", "CCD", "CCC", "BCC", 
              "BBC", "BBB", "ABB", "AAB", "AAA", "A*AA", "A*A*A", "A*A*A*", 
              "A*A*A*E", "A*A*A*C", "A*A*A*A", "A*A*A*A*", "A*A*A*A*")

find_band <- function(grade) {
  gradeband = names(grade_map)[sapply(grade_map, function(x) grade %in% x)]
  return(gradeband)
}

# UI ----------------------------------------------------------------------
ui <- page_navbar(
  
  # Title
  title = "Degree Matchmaker VERSION 3.1.1",
  
  # Colour Scheme
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  # Panel with Modals
  nav_panel(
    # Title
    "Degree Modals",
    
    # Use sidebar with a filter
    page_sidebar(
      sidebar = sidebar(
        
        # Let user input text to filter degrees
        textInput(
          "degree_search", "Search Degrees",
          placeholder = "e.g. Computer Science, Law..."
        ),
        helpText('Quick search by key word in degree name.'),
        selectInput(
          "jobselected",
          "Select Job Outcome",
          choices = c(
            "Any" = "ANY",
            setNames(
              names(df)[grepl("^JOB_", names(df))],
              gsub("^JOB_", "", names(df)[grepl("^JOB_", names(df))])
            )
          )
        ),
        helpText('Use this job filter to arrange by highest proportion of 
                 graduates employed in this area. Keep in mind, the classifications 
                 are broad, and do not speak of prestige or reputation of the firm within the sector.'),
        checkboxInput(
          "strict_filter",
          "Strict filtering (match all selected interests)",
          value = FALSE
        ),
        selectInput(
          "grades_selected",
          "Input your A Level Grades",
          choices = (potGrades),
          selected = "AAA"
        ),
        helpText("When you input your A Level grades, we filter the degrees shown.
                 This works by checking if there have been previous students
                 who have been admitted onto the program with similar grades to yours.
                 If so, we see this as a positive sign."),
        selectInput(
          "subject_selected",
          "Select Subject Area",
          choices = sort(unique(unlist(strsplit(df$label, " \\| ")))),
          multiple = TRUE
        ),
        helpText('We use "CAH" subject classifications to help you filter by interests.
                 These are not the most "detailed" - this is intentional. When selecting,
                 broad categories are ideal for finding degrees of interest!'),
        selectInput(
          "uni_selected",
          "Select any University Preferences",
          choices = sort(unique(df$legal_name)),
          multiple = TRUE
        ),
        helpText('Use this to filter by universities of interest.'),
        selectInput(
          "type_selected",
          "Select any Degree-Type Preferences",
          choices = sort(unique(df$degreetype)),
          multiple = TRUE
        ),
        helpText("If you are looking for a specialisation, it may be quicker to search using Degree types."),
        textOutput("card_count"),
        helpText("Use the search bar to filter by degree title of interest, 
                 and use the job filter to sort the degrees by proportion of 
                 graduates in the respective field."),
        
        width = 400
      ),
      uiOutput("degree_cards")
    )
  )
)

# Server ------------------------------------------------------------------



server <- function(input, output, session) {
  search_term <- reactive(input$degree_search) %>%
    debounce(400)
  
  filtered_degree_list <- reactive({
    
    term <- trimws(search_term() %||% "")
    data <- df
    
    # Grades filter
    if (!is.na(input$grades_selected) && length(input$grades_selected) > 0) {
      
      band <- find_band(input$grades_selected)
      
      eligible_ids <- df |>
        filter(Grades == band, Count > 0) |>
        pull(kiscourseid) |>
        unique()
      
      data <- data |>
        filter(kiscourseid %in% eligible_ids)
    }
    
    # Uni filter
    if (!is.null(input$uni_selected) && length(input$uni_selected) >0) {
      data = data %>%
        filter(legal_name %in% input$uni_selected)
    }
    
    # Degree type filter
    if (!is.null(input$type_selected) && length(input$type_selected) >0) {
      data = data %>%
        filter(degreetype %in% input$type_selected)
    }
    
    # Subject filter
    # if (!is.null(input$subject_selected) && length(input$subject_selected) > 0) {
    #   
    #   if (isTRUE(input$strict_filter)) {
    #     
    #     # STRICT (AND logic)
    #     data <- data %>%
    #       filter(label %in% input$subject_selected) %>%
    #       group_by(kiscourseid) %>%
    #       filter(n_distinct(label) == length(input$subject_selected)) %>%
    #       ungroup()
    #     
    #   } else {
    #     
    #     # NON-STRICT (OR logic)
    #     data <- data %>%
    #       filter(label %in% input$subject_selected)
    #   }
    # }
    if (!is.null(input$subject_selected) && length(input$subject_selected) > 0) {
      
      if (isTRUE(input$strict_filter)) {
        
        # STRICT (AND logic) - degree must contain ALL selected subjects
        data <- data %>%
          filter(Reduce(`&`, lapply(input$subject_selected, function(s) str_detect(label, fixed(s))))) 
        
      } else {
        
        # NON-STRICT (OR logic) - degree must contain ANY selected subject
        data <- data %>%
          filter(Reduce(`|`, lapply(input$subject_selected, function(s) str_detect(label, fixed(s)))))
      }
    }
    
    # Text search
    if (nchar(term) > 0) {
      data <- data %>%
        filter(grepl(term, title, ignore.case = TRUE))
    }
    
    # Always reduce to one row per course
    data %>%
      distinct(kiscourseid, .keep_all = TRUE)
  })
  
  output$card_count = renderText({
    paste(nrow(data))
  })
  
  prop_in_joblist <- reactive({
    df = df_long
    
    if (input$jobselected == "ANY") {
      df_long
    } else {
      df_long %>% filter(Job == input$jobselected)
    }
  })
  
  output$degree_cards <- renderUI({
    
    base_df  <- filtered_degree_list()
    
    base_df <- base_df |> slice_head(n = 50)
    
    output$card_count <- renderText({
      if (nrow(filtered_degree_list()) < 50){
        print.num = nrow(filtered_degree_list())
      } else {
        print.num = 50
      }
      paste0("Showing ", print.num, " of ", nrow(filtered_degree_list()), " courses")
    })
    
    jobprops <- prop_in_joblist()
    
    # Always reduce to one row per course FIRST
    base_df <- base_df %>%
      distinct(kiscourseid, .keep_all = TRUE)
    
    # If specific job selected, attach proportion and sort
    if (input$jobselected != "ANY") {
      
      job_sorted <- jobprops %>%
        group_by(kiscourseid) %>%
        summarise(Proportion = max(Proportion, na.rm = TRUE), .groups = "drop")
      
      base_df <- base_df %>%
        left_join(job_sorted, by = "kiscourseid") %>%
        mutate(Proportion = replace_na(Proportion, 0)) %>%
        arrange(desc(Proportion))
      
    } else {
      base_df$Proportion <- NA
    }
    
    cards <- lapply(seq_len(nrow(base_df)), function(i) {
      
      deg     <- paste(base_df$degreetype[[i]], base_df$title[[i]])
      title   <- base_df$title[[i]]
      uni     <- base_df$legal_name[[i]]
      address <- base_df$provaddress[[i]]
      url     <- base_df$crseurl[[i]]
      
      # Add % next to title only if job selected
      header_text <- if (!is.na(base_df$Proportion[[i]])) {
        paste0(deg, " (", round(base_df$Proportion[[i]], 1), "%)")
      } else {
        deg
      }
      
      card(
        style = "cursor: pointer;",
        onclick = sprintf(
          "Shiny.setInputValue('open_modal', {deg: %s, title: %s, uni: %s}, {priority: 'event'})",
          jsonlite::toJSON(deg,   auto_unbox = TRUE),
          jsonlite::toJSON(title, auto_unbox = TRUE),
          jsonlite::toJSON(uni,   auto_unbox = TRUE)
        ),
        card_header(header_text),
        p(class = "text-muted", uni),
        p(address),
        a("Visit Course Page", href = url, target = "_blank",
          class = "btn btn-primary btn-sm",
          onclick = "event.stopPropagation()")
      )
    })
    
    rows <- lapply(
      split(cards, ceiling(seq_along(cards) / 2)),
      function(row_cards) {
        fluidRow(lapply(row_cards, function(crd) column(6, crd)))
      }
    )
    
    tagList(rows)
  })
  
  observeEvent(input$open_modal, {
    
    base_df <- filtered_degree_list()
    my_cols <- colorRampPalette(brewer.pal(9, "Blues"))(10)
    
    output$plot1 <- renderPlot({
      
      grade_order <- (c("None", "EEE+", "DDE+", "CDD+", "CCC+", 
                        "BBC+", "ABB+", "AAA+", "A*A*A+", "A*A*A*A+"))
      
      df %>%
        filter(title == input$open_modal$title, legal_name == input$open_modal$uni) |>
        mutate(Grades = factor(Grades, levels = grade_order)) |>
        ggplot(aes(x = Grades, y = Count, fill = Grades)) +
        geom_col() +
        coord_flip() +
        scale_fill_manual(values = my_cols) +
        theme_classic() +
        theme(
          legend.position = "none",
          axis.text = element_text(size = 4),
          plot.title = element_text(size = 5, face = "bold"),
          plot.subtitle = element_text(size = 4)
        ) +
        labs(x = NULL, y = NULL,
             title = "Average Entrant Grades",
             subtitle = "*Note, results are positively skewed due to tariff points including AS Levels, and EPQ")
      
    }, res = 192, bg = "transparent")
    
    output$plot2 = renderPlot({
      
      JobsLong = jobsdf %>%
        filter(title == input$open_modal$title, legal_name == input$open_modal$uni) 
      
      nonZeroJobs = JobsLong %>% 
        filter(Proportion != 0) 
      
      my_cols2 <- colorRampPalette(brewer.pal(9, "PuBu"))(nrow(nonZeroJobs))
      
      JobsLong %>% 
        filter(Proportion != 0) %>%
        mutate(Job = fct_reorder(Job, Proportion, .desc = TRUE)) %>%
        ggplot(aes(x = Job, y = Proportion, fill = Job)) +
        geom_col(colour = "black", linewidth = 0.3) +
        scale_fill_manual(values = my_cols2) +
        labs(x = NULL, y = NULL) +
        theme_classic() +
        theme(
          axis.text = element_text(size = 4),
          plot.title = element_text(size = 5, face = "bold"),
          plot.subtitle = element_text(size = 4),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      
    })
    
    showModal(modalDialog(
      title = tagList(
        div(input$open_modal$deg, style = "font-weight:bold;"),
        div(input$open_modal$uni, style = "font-size:0.85rem; color:#888;")
      ),
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$style(".modal-dialog { max-width: 90vw; }"),
      
      fluidRow(
        column(6,
               plotOutput("plot1", height = "400px"),
        ),
        column(6,
               plotOutput("plot2", height = "400px"))
      )
    ))
  })
}

shinyApp(ui, server)
