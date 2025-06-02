# Data Module - Handle data loading and selection
# modules/data_module.R

library(shiny)
library(DT)
library(shinycssloaders)

# Load required libraries for example datasets
suppressPackageStartupMessages({
  library(moveHMM, quietly = TRUE)
  library(mHMMbayes, quietly = TRUE)
})

dataModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6,
      # Data source selection
      box(
        title = "Choose Data Source", 
        status = "primary", 
        solidHeader = TRUE,
        width = NULL,
        
        radioButtons(
          ns("data_source"),
          "What data would you like to analyze?",
          choices = list(
            "Example: Animal Movement (Haggis Data)" = "haggis",
            "Example: Muskox Movement Data" = "muskox",
            "Example: Fitbit Heart Rate Data" = "fitbit",
            "Upload my own CSV file" = "upload"
          ),
          selected = "haggis"
        ),
        
        # Conditional file upload
        conditionalPanel(
          condition = paste0("input['", ns("data_source"), "'] == 'upload'"),
          br(),
          fileInput(
            ns("file_upload"),
            "Choose CSV File",
            accept = c(".csv", ".CSV"),
            placeholder = "No file selected"
          ),
          helpText("CSV files should have a column that identifies different individuals/time series (like 'ID' or 'subject').")
        ),
        
        br(),
        actionButton(
          ns("load_data"), 
          "Load Data", 
          class = "btn-primary",
          icon = icon("upload")
        )
      )
    ),    
    column(6,
      # Data configuration
      box(
        title = "Data Configuration", 
        status = "info", 
        solidHeader = TRUE,
        width = NULL,
        
        conditionalPanel(
          condition = paste0("output['", ns("data_loaded"), "']"),
          
          h4("Configure Data Columns"),
          
          selectInput(
            ns("id_column"),
            "Which column identifies different individuals/groups?",
            choices = NULL,
            width = "100%"
          ),
          helpText("This column groups your data by individual subjects or time series."),
          
          br(),
          checkboxGroupInput(
            ns("observable_vars"),
            "Which variables do you want to analyze? (Choose 1-3)",
            choices = NULL,
            width = "100%"
          ),
          helpText("These are the measurements you want to find hidden patterns in."),
          
          br(),
          checkboxGroupInput(
            ns("covariate_vars"),
            "Which variables might influence the patterns? (Optional)",
            choices = NULL,
            width = "100%"
          ),
          helpText("These are factors that might affect when patterns change (like time, weather, etc.)."),
          
          br(),
          actionButton(
            ns("confirm_config"),
            "Confirm Configuration",
            class = "btn-success",
            icon = icon("check")
          )
        )
      )
    ),    
    # Data preview
    column(12,
      conditionalPanel(
        condition = paste0("output['", ns("data_loaded"), "']"),
        box(
          title = "Data Preview", 
          status = "success", 
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          
          withSpinner(
            DTOutput(ns("data_preview")),
            color = "#3c8dbc"
          ),
          
          br(),
          verbatimTextOutput(ns("data_summary"))
        )
      )
    )
  )
}

dataModuleServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for this module
    local_data <- reactiveVal(NULL)
    data_columns <- reactiveVal(NULL)
    
    # Data loading logic
    observeEvent(input$load_data, {
      
      if (input$data_source == "upload") {
        req(input$file_upload)
        
        tryCatch({
          data <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
          local_data(data)
          values$data_name <- input$file_upload$name
          showNotification("File uploaded successfully!", type = "message")
        }, error = function(e) {
          showNotification(paste("Error loading file:", e$message), type = "error")
          return()
        })
        
      } else {
        # Load example datasets
        data <- switch(input$data_source,          "haggis" = {
            values$data_name <- "Haggis Movement Data"
            if (requireNamespace("moveHMM", quietly = TRUE)) {
              tryCatch({
                # Load and preprocess haggis data with moveHMM
                cat("Loading haggis_data...\n")
                haggis_raw <- moveHMM::haggis_data
                cat("Raw haggis data loaded. Columns:", colnames(haggis_raw), "\n")
                cat("Data dimensions:", dim(haggis_raw), "\n")
                
                # Check the data structure
                cat("First few rows:\n")
                print(head(haggis_raw, 3))
                
                cat("Calling prepData...\n")
                haggis_processed <- moveHMM::prepData(haggis_raw, type = 'UTM')
                cat("prepData completed successfully!\n")
                cat("Processed data columns:", colnames(haggis_processed), "\n")
                
                haggis_processed
              }, error = function(e) {
                # More informative error message
                cat("Error processing haggis data:", e$message, "\n")
                showNotification(paste("Error processing haggis data:", e$message, "Using fallback data."), 
                               type = "warning", duration = 5)
                
                # Fallback: create simple movement data with step and angle
                n_points <- 100
                data.frame(
                  ID = factor(rep(1:2, each = n_points)),
                  x = c(cumsum(c(0, rnorm(n_points-1, 0, 2))), cumsum(c(5, rnorm(n_points-1, 0, 1.5)))),
                  y = c(cumsum(c(0, rnorm(n_points-1, 0, 2))), cumsum(c(3, rnorm(n_points-1, 0, 1.5)))),
                  step = c(abs(rnorm(n_points, 2, 1)), abs(rnorm(n_points, 1.5, 0.8))),
                  angle = c(runif(n_points, -pi, pi), runif(n_points, -pi, pi))
                )
              })
            } else {
              cat("moveHMM package not available, using fallback data\n")
              showNotification("moveHMM package not available. Using synthetic movement data.", 
                             type = "warning", duration = 3)
              
              # Fallback: create simple movement data with step and angle
              n_points <- 100
              data.frame(
                ID = factor(rep(1:2, each = n_points)),
                x = c(cumsum(c(0, rnorm(n_points-1, 0, 2))), cumsum(c(5, rnorm(n_points-1, 0, 1.5)))),
                y = c(cumsum(c(0, rnorm(n_points-1, 0, 2))), cumsum(c(3, rnorm(n_points-1, 0, 1.5)))),
                step = c(abs(rnorm(n_points, 2, 1)), abs(rnorm(n_points, 1.5, 0.8))),
                angle = c(runif(n_points, -pi, pi), runif(n_points, -pi, pi))
              )
            }
          },
          "muskox" = {
            values$data_name <- "Muskox Movement Data"
            # Read the muskox data from the provided CSV
            read.csv("muskox_summer_2016.csv", stringsAsFactors = FALSE)
          },
          "fitbit" = {
            values$data_name <- "Fitbit Heart Rate Data"
            tryCatch({
              # Read the fitbit heart rate CSV
              fitbit_raw <- read.csv("fitbit_heartrate.csv", stringsAsFactors = FALSE)
              
              # Preprocess following the example workflow
              # Take the last 73,000 rows
              data <- tail(fitbit_raw, 73000)
              
              # Create numeric TimeOfDay column
              data$TimeOfDay <- as.numeric(data$Time.of.Day)
              
              # Take every 100th row to reduce data size for demo
              every_100th_row <- data[seq(1, nrow(data), by = 100), ]
              
              # Clean the final dataset
              processed_data <- every_100th_row[!is.na(every_100th_row$TimeOfDay), ]
              
              # Ensure ID is properly formatted
              processed_data$ID <- as.factor(processed_data$ID)
              
              cat("Fitbit data processed successfully.\n")
              cat("Final dimensions:", dim(processed_data), "\n")
              cat("Columns:", colnames(processed_data), "\n")
              
              processed_data
              
            }, error = function(e) {
              cat("Error loading fitbit data:", e$message, "\n")
              showNotification(paste("Error loading fitbit data:", e$message, "Check that fitbit_heartrate.csv exists."), 
                             type = "warning", duration = 5)
              
              # Fallback: create synthetic heart rate data
              n_points <- 730  # About a day's worth at 100-row intervals
              time_seq <- seq(0, 24, length.out = n_points)
              
              # Simulate realistic heart rate with daily pattern
              base_hr <- 70 + 20 * cos(2 * pi * time_seq / 24)  # Daily rhythm
              noise_hr <- base_hr + rnorm(n_points, 0, 10)  # Add noise
              noise_hr <- pmax(noise_hr, 50)  # Ensure realistic minimum
              
              data.frame(
                ID = factor(rep(1, n_points)),
                Time = seq(as.POSIXct("2016-04-01 00:00:00"), by = "2 min", length.out = n_points),
                Value = round(noise_hr),
                Hour = floor(time_seq),
                TimeOfDay = time_seq,
                stringsAsFactors = FALSE
              )
            })
          }
        )
        
        local_data(data)
        showNotification("Example data loaded successfully!", type = "message")
      }
      
      # Update column choices
      if (!is.null(local_data())) {
        cols <- colnames(local_data())
        data_columns(cols)
        
        # Smart defaults for ID column
        id_candidates <- cols[grepl("id|ID|subject|individual|burst", cols, ignore.case = TRUE)]
        if (length(id_candidates) > 0) {
          default_id <- id_candidates[1]
        } else {
          default_id <- cols[1]
        }
        
        updateSelectInput(session, "id_column", 
                         choices = cols, 
                         selected = default_id)
        
        # Update observable variable choices (numeric columns only)
        numeric_cols <- cols[sapply(local_data(), function(x) is.numeric(x) || is.integer(x))]
        non_id_numeric <- numeric_cols[!numeric_cols %in% c(default_id)]
        
        updateCheckboxGroupInput(session, "observable_vars", 
                                choices = non_id_numeric)
        
        # Update covariate choices (exclude ID and selected observables)
        updateCheckboxGroupInput(session, "covariate_vars", 
                                choices = cols[!cols %in% c(default_id)])
      }
    })    
    # Update covariate choices when observable vars change
    observeEvent(input$observable_vars, {
      if (!is.null(data_columns()) && !is.null(input$id_column)) {
        available_covs <- data_columns()[!data_columns() %in% c(input$id_column, input$observable_vars)]
        updateCheckboxGroupInput(session, "covariate_vars", 
                                choices = available_covs)
      }
    })
    
    # Confirm configuration
    observeEvent(input$confirm_config, {
      req(input$id_column, input$observable_vars)
      
      if (length(input$observable_vars) == 0) {
        showNotification("Please select at least one variable to analyze.", type = "warning")
        return()
      }
      
      if (length(input$observable_vars) > 3) {
        showNotification("Please select no more than 3 variables for this demo.", type = "warning")
        return()
      }
      
      # Prepare data for analysis
      analysis_data <- local_data()
      
      # Rename ID column to "ID" as required by hmmTMB
      if (input$id_column != "ID") {
        colnames(analysis_data)[colnames(analysis_data) == input$id_column] <- "ID"
      }
      
      # Ensure ID is a factor
      analysis_data$ID <- as.factor(analysis_data$ID)
      
      # Store in global values
      values$data <- analysis_data
      values$id_column <- "ID"
      values$observable_vars <- input$observable_vars
      values$covariate_vars <- input$covariate_vars
      values$step_completed[1] <- TRUE
      
      showNotification("Data configuration confirmed! Ready for model setup.", type = "message")
    })    
    # Data loaded flag for conditional panels
    output$data_loaded <- reactive({
      !is.null(local_data())
    })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    # Data preview table
    output$data_preview <- renderDT({
      req(local_data())
      
      datatable(
        local_data(),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'tp'
        )
      ) %>% formatRound(columns = which(sapply(local_data(), is.numeric)), digits = 3)
    })
    
    # Data summary
    output$data_summary <- renderText({
      req(local_data())
      
      data <- local_data()
      n_rows <- nrow(data)
      n_cols <- ncol(data)
      n_numeric <- sum(sapply(data, is.numeric))
      
      paste0(
        "Dataset Summary:\n",
        "• ", n_rows, " rows and ", n_cols, " columns\n",
        "• ", n_numeric, " numeric columns available for analysis\n",
        "• ", length(unique(data[[if (is.null(input$id_column)) colnames(data)[1] else input$id_column]])), 
        " unique individuals/groups"
      )
    })
    
  })
}