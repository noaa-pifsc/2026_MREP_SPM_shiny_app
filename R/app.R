library(shiny)
library(DT)
library(dplyr)
library(magrittr)
library(bslib)
library(bsicons)
library(shinyjs)
library(markdown)
# https://excalidraw.com/

source("./sim_data_funs.R") 
source("./fit_spm_funs.R")

workshop_species <- "Opakapaka"
number_of_drops <- 5

empty_workshop_data <- function(n_rows = 0, drops = number_of_drops) {
  data.frame(
    Species = rep(workshop_species, n_rows),
    Year = rep(NA_real_, n_rows),
    "Number of Drops" = rep(drops, n_rows),
    Count = rep(NA_real_, n_rows),
    check.names = FALSE
  )
}


ui <- page_sidebar(
  title = "WP-MREP Stock Assessment Simulation Tool",
  theme = bs_theme(bootswatch = "lumen", "navbar-bg" = "#158CBA"),
  
  useShinyjs(),  # Initialize shinyjs

  sidebar = sidebar(
    tags$style(HTML("
      .sidebar-nav-link {
        display: block;
        padding: 12px 15px;
        margin: 5px 0;
        border-radius: 5px;
        color: #333;
        text-decoration: none;
        transition: background-color 0.2s;
        cursor: pointer;
      }
      .sidebar-nav-link:hover {
        background-color: #e9ecef;
      }
      .sidebar-nav-link.active {
        background-color: var(--bs-primary);
        color: white;
      }
    ")),
    
    actionLink("nav_welcome", 
               tagList(bsicons::bs_icon("house"), " Welcome"),
               class = "sidebar-nav-link active"),
    
    actionLink("nav_data_entry", 
               tagList(bsicons::bs_icon("table"), " Data Entry"),
               class = "sidebar-nav-link"),
    
    actionLink("nav_model_results", 
               tagList(bsicons::bs_icon("graph-up"), " Run Model"),
               class = "sidebar-nav-link"),
    
    actionLink("nav_summary", 
               tagList(bsicons::bs_icon("clipboard-data"), " Summary"),
               class = "sidebar-nav-link")
  ),
  
  # Just put the UI output here - don't define it!
  uiOutput("page_content")
)

server <- function(input, output, session) {
  
  current_page <- reactiveVal("welcome")
  fish_data <- reactiveVal(empty_workshop_data())
  model_results <- reactiveVal(NULL)
  # A place to store saved plots and results
  saved_results <- reactiveVal(list())

  # Function to update active class
  update_nav <- function(active_page) {
    pages <- c("welcome", "data_entry", "model_results", "summary")
    for (page in pages) {
      if (page == active_page) {
        runjs(sprintf("$('#nav_%s').addClass('active');", page))
      } else {
        runjs(sprintf("$('#nav_%s').removeClass('active');", page))
      }
    }
  }
  
  # Navigation observers
  observeEvent(input$nav_welcome, {
    current_page("welcome")
    update_nav("welcome")
  })
  
  observeEvent(input$nav_data_entry, {
    current_page("data_entry")
    update_nav("data_entry")
  })
  
  observeEvent(input$nav_model_results, {
    current_page("model_results")
    update_nav("model_results")
  })
  
  observeEvent(input$nav_summary, {
    current_page("summary")
    update_nav("summary")
  })
  
  # Render page content - ONLY in server, not in UI!
  output$page_content <- renderUI({
    switch(current_page(),
      
      "welcome" = div(
        shiny::includeMarkdown("./welcome.md")
      ),
      
      "data_entry" = div(
        h3("Catch Data"),
        
        fluidRow(
          column(3,
            selectInput("filter_species", 
                       "Select Species:", 
                       choices = workshop_species,
                       selected = workshop_species,
                       width = "100%")
          ),
          column(3,
            numericInput("number_of_drops", "Number of Drops:", value = number_of_drops,
                         min = 1, step = 1, width = "100%")
          ),
          column(3,
            numericInput("start_year", "Start Year:", value = 2012,
                         min = 1, step = 1, width = "100%")
          ),
          column(3,
            numericInput("end_year", "End Year:", value = 2023,
                         min = 1, step = 1, width = "100%")
          )
        ),
        div(style = "margin-top: 12px;",
          actionButton("create_year_rows", "Create Year Rows", class = "btn-primary"),
          actionButton("delete_rows", "Delete Selected Rows")
        ),
        fluidRow(
          column(6,
            div(style = "margin-top: 12px;",
              downloadButton("download_data", "Download CSV")
            )
          ),
          column(6,
            fileInput("upload_data", "Upload combined CSV", accept = ".csv",
                      buttonLabel = "Choose CSV")
          )
        ),
        br(),
        
        # Data table
        DTOutput("data_table"),
        
        br(),
        
        uiOutput("data_plots_ui")
      ),
      
      "model_results" = div(
        h2("Run Surplus Production Model"),
        
        fluidRow(
          column(12,
            wellPanel(
              p("Review the Opakapaka data on the Data Entry page, then click the button below to run the model."),
              actionButton("run_model_btn", "Run Model", class = "btn-primary btn-lg", width = "100%")
            )
          )
        ),
        
        fluidRow(
          column(12,
            uiOutput("model_output_ui")
          )
        )
      ),
      
      "summary" = div(
        h2("Summary"),
        p("Let's wrap up what we learned and compare some of our results!"),
        # Show saved plots
        uiOutput("saved_plots_ui"),
        
        hr(),
        p(strong("What were your fishing motivations?")),
        p(strong("Based on what you know about the life history strategy of your species, 
        how do you think your fishing strategy impacted the population?"))
      )
    )
  })
  
 # Render directly editable workshop data
  output$data_table <- renderDT({
    datatable(
      fish_data(),
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1, 2))),
      selection = list(mode = "multiple", target = "row"),
      options = list(dom = "t", paging = FALSE, scrollY = "360px", scrollCollapse = TRUE)
    )
  })
  
  # Convert the workshop table to the columns required by the model.
  filtered_data_table <- reactive({
    fish_data()
  })

  model_data <- reactive({
    fish_data() %>%
      filter(!is.na(Year), !is.na(Count)) %>%
      transmute(
        Date = Year,
        Count = Count,
        Effort = `Number of Drops`
      ) %>%
      arrange(Date)
  })

  output$download_data <- downloadHandler(
    filename = function() {
      sprintf("opakapaka_workshop_data_%s.csv", Sys.Date())
    },
    content = function(file) {
      utils::write.csv(fish_data(), file, row.names = FALSE)
    }
  )

  output$data_plots_ui <- renderUI({
    fluidRow(
      column(6,
        plotOutput("catch_plot", height = "400px")
      ),
      column(6,
        plotOutput("cpue_plot", height = "400px")
      )
    )
  })
  
  # Render catch plot
  output$catch_plot <- renderPlot({
    data <- model_data()
    req(nrow(data) > 0)
    
    plot_catch_data(data)
  })
  
  # Render CPUE plot
  output$cpue_plot <- renderPlot({
    data <- model_data()
    req(nrow(data) > 0)
    
    plot_cpue(data)
  })

  # Display saved plots
  output$saved_plots_ui <- renderUI({
    saved <- saved_results()
    
    if (length(saved) == 0) {
      return(
        div(
          style = "padding: 20px; text-align: center; color: #666;",
          icon("chart-bar", style = "font-size: 48px;"),
          h4("No saved plots yet"),
          p("Run models and click 'Send to Summary' to save plots here.")
        )
      )
    }
    
    # Create a card for each saved plot
    plot_cards <- lapply(names(saved), function(result_id) {
      result <- saved[[result_id]]
      
      # Create unique output ID
      plot_output_id <- paste0("saved_plot_", gsub("[^[:alnum:]]", "_", result_id))
      
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(
            strong(result$title),
            br(),
            span(style = "font-size: 0.9em; color: #666;",
                format(result$timestamp, "%Y-%m-%d %H:%M:%S"))
          ),
          actionButton(
            inputId = paste0("delete_", result_id),
            label = icon("trash"),
            class = "btn-danger btn-sm",
            onclick = sprintf("Shiny.setInputValue('delete_plot', '%s', {priority: 'event'})", result_id)
          )
        ),
        card_body(
          plotOutput(plot_output_id, height = "500px")
        )
      )
    })
    
    # Arrange in a grid
    layout_column_wrap(
      width = 1/2,  # Two columns
      !!!plot_cards
    )
  })

  # Render each saved plot
  observe({
    saved <- saved_results()
    
    lapply(names(saved), function(result_id) {
      result <- saved[[result_id]]
      plot_output_id <- paste0("saved_plot_", gsub("[^[:alnum:]]", "_", result_id))
      
      output[[plot_output_id]] <- renderPlot({
        if (result$plot_type == "kobe") {
          plot_kobe(result$model_fit)
        } else if (result$plot_type == "biomass") {
          plot_biomass_trajectory(result$model_fit)
        }
      })
    })
  })

  # Delete saved plot
  observeEvent(input$delete_plot, {
    result_id <- input$delete_plot
    
    current_saved <- saved_results()
    current_saved[[result_id]] <- NULL
    saved_results(current_saved)
    
    showNotification("Plot removed from Summary", type = "message", duration = 2)
  })

   # Run model when button is clicked
  observeEvent(input$run_model_btn, {
    # Show progress notification
    showNotification("Running model...", id = "model_run", duration = NULL, type = "message")
    
    species_data <- model_data()
    
    # Check if there's data
    if (nrow(species_data) < 3) {
      removeNotification("model_run")
      showNotification("Enter at least three Year and Count rows before running the model.", type = "error")
      return()
    }
    # Run the surplus production model
    tryCatch({
      model_fit <- fit_schaefer_model(
        data = species_data,
        use_cpue = TRUE
      )
      
      results <- list(
        species = workshop_species,
        n_observations = nrow(species_data),
        data = species_data,
        model_fit = model_fit  
      )
      
      model_results(results)
      
      removeNotification("model_run")
      showNotification("Model run complete!", type = "message", duration = 3)
      
    }, error = function(e) {
      removeNotification("model_run")
      showNotification(paste("Error running model:", e$message), type = "error")
    })
  })
  
  # Display model results
  output$model_output_ui <- renderUI({
    results <- model_results()
    
    if (is.null(results)) {
      return(
        div(
          style = "padding: 20px; text-align: center; color: #666;",
          icon("info-circle", style = "font-size: 48px;"),
          h4("No model results yet"),
          p("Select a species and click 'Run Model' to see results here.")
        )
      )
    }

    title <- paste("Model Results:", results$species)
    
    # Display results
    div(
      wellPanel(
        h3(title),
        hr(),
        
        hr(),
        h4("Model Parameters & Stock Status"),
        verbatimTextOutput("model_summary"),
        
      hr(),
      h4("CPUE Indices: Observed and Predicted"),
      plotOutput("cpue_indices_plot", height = "450px"),

      hr(),
      fluidRow(
        column(10,
          h4("Kobe Plot - Stock Status")
        ),
        column(2,
          actionButton("save_kobe", "Send to Summary", 
                      class = "btn-success btn-sm",
                      style = "margin-top: 10px;")
        )
      ),
      plotOutput("kobe_plot", height = "600px"),
        
      hr(),
      fluidRow(
        column(10,
          h4("Biomass Trajectory")
        ),
        column(2,
          actionButton("save_biomass", "Send to Summary", 
                      class = "btn-success btn-sm",
                      style = "margin-top: 10px;")
        )
      ),
      plotOutput("biomass_plot", height = "600px")
      )
    )
  })
  
  # Show the filtered data used in model
  output$model_data_table <- renderDT({
    results <- model_results()
    req(results)
    
    datatable(
      results$data,
      options = list(pageLength = 5)
    )
  })
  
  # Show model summary
  output$model_summary <- renderPrint({
    results <- model_results()
    req(results)
    req(results$model_fit)
    
    model_fit <- results$model_fit
    
    cat("=== MODEL FIT SUMMARY ===\n\n")
    cat("Species:", results$species, "\n")
    cat("Data points:", results$n_observations, "\n\n")
    
    cat("--- Estimated Parameters ---\n")
    cat(sprintf("  r (intrinsic growth rate): %.4f\n", model_fit$parameters$r))
    cat(sprintf("  K (carrying capacity): %.2f\n", model_fit$parameters$K))
    cat(sprintf("  q (catchability): %.6f\n\n", model_fit$parameters$q))
    
    cat("--- Reference Points ---\n")
    cat(sprintf("  B_MSY: %.2f\n", model_fit$reference_points$Bmsy))
    cat(sprintf("  MSY: %.2f\n", model_fit$reference_points$MSY))
    cat(sprintf("  F_MSY: %.4f\n\n", model_fit$reference_points$Fmsy))
    
    cat("--- Current Stock Status ---\n")
    cat(sprintf("  Current Biomass: %.2f\n", model_fit$current_status$B_current))
    cat(sprintf("  B/B_MSY: %.3f\n", model_fit$current_status$B_Bmsy))
    cat(sprintf("  Current Fishing Mortality: %.4f\n", model_fit$current_status$F_current))
    cat(sprintf("  F/F_MSY: %.3f\n", model_fit$current_status$F_Fmsy))
    cat(sprintf("  Stock Status: %s\n\n", model_fit$current_status$status))
    
  })

  output$kobe_plot <- renderPlot({
    results <- model_results()
    req(results)
    req(results$model_fit)
    
    model_fit <- results$model_fit

    plot_kobe(model_fit)
  })

  output$cpue_indices_plot <- renderPlot({
    results <- model_results()
    req(results)
    req(results$model_fit)

    plot_model_fit(results$model_fit)
  })

  output$biomass_plot <- renderPlot({
    results <- model_results()
    req(results)
    req(results$model_fit)
    
    model_fit <- results$model_fit

    plot_biomass_trajectory(model_fit)
  })

  observeEvent(input$create_year_rows, {
    drops <- input$number_of_drops
    if (is.na(drops) || drops < 1 || drops != floor(drops)) {
      showNotification("Number of Drops must be a whole number of at least one.", type = "warning")
      return()
    }

    start_year <- input$start_year
    end_year <- input$end_year
    if (is.na(start_year) || is.na(end_year) || start_year < 1 ||
        end_year < start_year || start_year != floor(start_year) ||
        end_year != floor(end_year)) {
      showNotification("Enter whole-number years with an end year no earlier than the start year.", type = "warning")
      return()
    }

    years <- seq(start_year, end_year, by = 1)
    current_data <- fish_data()
    new_rows <- empty_workshop_data(length(years))
    new_rows$Year <- years
    new_rows[["Number of Drops"]] <- drops
    matching_rows <- match(years, current_data$Year)
    new_rows$Count <- current_data$Count[matching_rows]
    fish_data(new_rows)
    model_results(NULL)
  })

  observeEvent(input$number_of_drops, {
    drops <- input$number_of_drops
    if (is.na(drops) || drops < 1 || drops != floor(drops)) return()

    current_data <- fish_data()
    if (nrow(current_data) > 0) {
      current_data[["Number of Drops"]] <- drops
      fish_data(current_data)
    }
    model_results(NULL)
  })

  observeEvent(input$upload_data, {
    uploaded_data <- tryCatch(
      utils::read.csv(input$upload_data$datapath, check.names = FALSE,
                      stringsAsFactors = FALSE),
      error = function(error) error
    )

    if (inherits(uploaded_data, "error")) {
      showNotification("The selected file could not be read as a CSV.", type = "error")
      return()
    }

    required_columns <- c("Species", "Year", "Number of Drops", "Count")
    if (!all(required_columns %in% names(uploaded_data))) {
      showNotification(
        "The CSV must include Species, Year, Number of Drops, and Count columns.",
        type = "error"
      )
      return()
    }

    uploaded_data <- uploaded_data[, required_columns, drop = FALSE]
    uploaded_data$Species <- trimws(as.character(uploaded_data$Species))
    uploaded_data$Year <- suppressWarnings(as.numeric(uploaded_data$Year))
    uploaded_data[["Number of Drops"]] <- suppressWarnings(
      as.numeric(uploaded_data[["Number of Drops"]])
    )
    uploaded_data$Count <- suppressWarnings(as.numeric(uploaded_data$Count))

    invalid_data <- nrow(uploaded_data) == 0 ||
      any(uploaded_data$Species != workshop_species) ||
      any(is.na(uploaded_data$Year) | uploaded_data$Year < 1 |
            uploaded_data$Year != floor(uploaded_data$Year)) ||
      any(is.na(uploaded_data[["Number of Drops"]]) |
            uploaded_data[["Number of Drops"]] < 1 |
            uploaded_data[["Number of Drops"]] != floor(uploaded_data[["Number of Drops"]])) ||
      any(is.na(uploaded_data$Count) | uploaded_data$Count < 0)

    if (invalid_data) {
      showNotification(
        "Upload a non-empty Opakapaka CSV with whole-number Year and Number of Drops values, and non-negative Count values.",
        type = "error"
      )
      return()
    }

    uploaded_data <- uploaded_data %>% arrange(Year)
    fish_data(uploaded_data)
    model_results(NULL)

    updateNumericInput(session, "start_year", value = min(uploaded_data$Year))
    updateNumericInput(session, "end_year", value = max(uploaded_data$Year))
    if (dplyr::n_distinct(uploaded_data[["Number of Drops"]]) == 1) {
      updateNumericInput(session, "number_of_drops",
                         value = uploaded_data[["Number of Drops"]][1])
    }

    showNotification("CSV uploaded. The table has been replaced with the uploaded data.",
                     type = "message")
  })

  observeEvent(input$data_table_cell_edit, {
    edit <- input$data_table_cell_edit
    if (edit$col != 3) return()

    value <- suppressWarnings(as.numeric(edit$value))
    if (is.na(value) || value < 0) {
      showNotification("Count must be a non-negative number.", type = "warning")
      return()
    }

    current_data <- fish_data()
    current_data[edit$row, edit$col + 1] <- value
    fish_data(current_data)
    model_results(NULL)
  })

  observeEvent(input$delete_rows, {
    selected <- input$data_table_rows_selected
    if (is.null(selected) || length(selected) == 0) {
      showNotification("Select one or more rows to delete.", type = "warning")
      return()
    }

    current_data <- fish_data()
    fish_data(current_data[-selected, , drop = FALSE])
    model_results(NULL)
  })

  # Save Kobe plot to summary
observeEvent(input$save_kobe, {
  results <- model_results()
  req(results)
  
  # Create a unique ID for this result
  result_id <- paste0(results$species, "_kobe_", 
                      format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  # Store the plot information
  new_result <- list(
    id = result_id,
    species = results$species,
    plot_type = "kobe",
    model_fit = results$model_fit,
    timestamp = Sys.time(),
    title = paste("Kobe Plot:", results$species)
  )
  
  # Add to saved results
  current_saved <- saved_results()
  current_saved[[result_id]] <- new_result
  saved_results(current_saved)
  
  showNotification("Kobe plot saved to Summary page!", 
                  type = "message", duration = 3)
})

  # Save Biomass plot to summary
  observeEvent(input$save_biomass, {
    results <- model_results()
    req(results)
    
    # Create a unique ID for this result
    result_id <- paste0(results$species, "_biomass_", 
                        format(Sys.time(), "%Y%m%d_%H%M%S"))
    
    # Store the plot information
    new_result <- list(
      id = result_id,
      species = results$species,
      plot_type = "biomass",
      model_fit = results$model_fit,
      timestamp = Sys.time(),
      title = paste("Biomass Trajectory:", results$species)
    )
    
    # Add to saved results
    current_saved <- saved_results()
    current_saved[[result_id]] <- new_result
    saved_results(current_saved)
    
    showNotification("Biomass plot saved to Summary page!", 
                    type = "message", duration = 3)
  })

}

shinyApp(ui = ui, server = server)
