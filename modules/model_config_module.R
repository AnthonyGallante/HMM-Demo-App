# Model Configuration Module
# modules/model_config_module.R

library(shiny)
library(shinycssloaders)

modelConfigModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6,
      # States configuration
      box(
        title = "Hidden States", 
        status = "primary", 
        solidHeader = TRUE,
        width = NULL,
        
        div(
          style = "margin-bottom: 15px;",
          h4("How many hidden states do you think exist?"),
          helpText("Hidden states represent different 'modes' or 'behaviors' in your data. For example, an animal might have 'resting', 'foraging', and 'traveling' states.")
        ),
        
        sliderInput(
          ns("n_states"),
          "Number of States:",
          min = 2,
          max = 5,
          value = 2,
          step = 1,
          width = "100%"
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("n_states"), "'] == 2"),
          div(class = "help-text", "💭 2 states: Good for simple on/off or high/low patterns")
        ),
        conditionalPanel(
          condition = paste0("input['", ns("n_states"), "'] == 3"),
          div(class = "help-text", "💭 3 states: Common for low/medium/high or rest/forage/travel patterns")
        ),
        conditionalPanel(
          condition = paste0("input['", ns("n_states"), "'] >= 4"),
          div(class = "help-text", "💭 4+ states: For complex multi-level patterns")
        )
      )
    ),    
    column(6,
      # Variables summary
      box(
        title = "Analysis Summary", 
        status = "info", 
        solidHeader = TRUE,
        width = NULL,
        
        conditionalPanel(
          condition = paste0("output['", ns("config_ready"), "']"),
          
          h4("📊 Your Analysis Setup"),
          verbatimTextOutput(ns("analysis_summary")),
          
          br(),
          actionButton(
            ns("auto_configure"),
            "Auto-Configure Model",
            class = "btn-warning",
            icon = icon("magic"),
            width = "100%"
          ),
          helpText("Let us automatically set up the model with smart defaults."),
          
          br(),
          hr(),
          br(),
          
          actionButton(
            ns("confirm_model"),
            "Confirm Model Setup",
            class = "btn-success",
            icon = icon("check-circle"),
            width = "100%"
          )
        )
      )
    ),
    
    # Distribution configuration (appears after auto-configure)
    column(12,
      conditionalPanel(
        condition = paste0("output['", ns("distributions_ready"), "']"),
        
        box(
          title = "Distribution Configuration", 
          status = "warning", 
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          
          h4("📈 How are your variables distributed?"),
          helpText("We've automatically detected the best distributions for your data. You can adjust these if needed."),
          
          withSpinner(
            uiOutput(ns("distribution_config")),
            color = "#f39c12"
          ),
          
          br(),
          div(
            style = "text-align: center;",
            actionButton(
              ns("reset_params"),
              "Reset to Auto-Detected Values",
              class = "btn-outline-secondary",
              icon = icon("refresh")
            )
          )
        )
      )
    )
  )
}
modelConfigModuleServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Local reactive values
    distributions_configured <- reactiveVal(FALSE)
    suggested_distributions <- reactiveVal(NULL)
    suggested_parameters <- reactiveVal(NULL)
    
    # Check if configuration is ready
    output$config_ready <- reactive({
      !is.null(values$data) && !is.null(values$observable_vars) && length(values$observable_vars) > 0
    })
    outputOptions(output, "config_ready", suspendWhenHidden = FALSE)
    
    # Analysis summary
    output$analysis_summary <- renderText({
      req(values$data, values$observable_vars)
      
      n_obs <- length(values$observable_vars)
      n_cov <- length(if (is.null(values$covariate_vars)) character(0) else values$covariate_vars)
      n_individuals <- length(unique(values$data$ID))
      n_rows <- nrow(values$data)
      
      paste0(
        "Dataset: ", values$data_name, "\n",
        "Variables to analyze: ", n_obs, " (", paste(values$observable_vars, collapse = ", "), ")\n",
        "Covariates: ", if(n_cov > 0) paste0(n_cov, " (", paste(values$covariate_vars, collapse = ", "), ")") else "None", "\n",
        "Individuals: ", n_individuals, "\n",
        "Observations: ", n_rows, "\n",
        "Hidden states: ", if (is.null(input$n_states)) 2 else input$n_states
      )
    })    
    # Auto-configure model
    observeEvent(input$auto_configure, {
      req(values$data, values$observable_vars, input$n_states)
      
      showNotification("Auto-configuring model... This may take a moment.", type = "message", duration = 3)
      
      tryCatch({
        # Detect distributions for each observable variable
        dists <- list()
        params <- list()
        
        # Special handling for Fitbit heart rate data
        is_fitbit_data <- !is.null(values$data_name) && grepl("Fitbit", values$data_name, ignore.case = TRUE)
        
        for (var in values$observable_vars) {
          var_data <- values$data[[var]]
          var_data <- var_data[!is.na(var_data)]
          
          # Ensure we have enough data
          if (length(var_data) < 10) {
            stop("Not enough data points for variable: ", var)
          }
          
          # Special configuration for Fitbit heart rate data
          if (is_fitbit_data && var == "Value") {
            # Heart rate data - use normal distribution with physiologically realistic parameters
            dists[[var]] <- "norm"
            
            # Set realistic heart rate parameters for resting vs active states
            # State 1: Resting heart rate (lower mean, smaller variance)
            # State 2: Active heart rate (higher mean, larger variance)
            params[[var]] <- list(
              mean = c(70, 100),  # Resting: ~70 bpm, Active: ~100 bpm
              sd = c(8, 15)       # Less variation when resting, more when active
            )
            
            cat("Configured Fitbit heart rate data with specialized parameters\n")
            next
          }
          
          # Standard distribution detection logic for other variables
          if (all(var_data >= 0) && all(var_data == round(var_data))) {
            # Non-negative integers - use Poisson
            dists[[var]] <- "pois"
            # Create well-separated lambda values
            data_mean <- max(mean(var_data, na.rm = TRUE), 0.1)
            lambda_values <- seq(data_mean * 0.3, data_mean * 1.7, length.out = input$n_states)
            lambda_values <- pmax(lambda_values, 0.1)  # Ensure all positive
            params[[var]] <- list(lambda = lambda_values)
            
          } else if (all(var_data >= 0)) {
            # Non-negative continuous - use gamma
            dists[[var]] <- "gamma"
            # Use method of moments for gamma parameters with better separation
            var_mean <- mean(var_data, na.rm = TRUE)
            var_var <- var(var_data, na.rm = TRUE)
            
            if (var_var <= 0 || var_mean <= 0) {
              # Fallback if variance is zero or mean is negative
              shape <- 1
              scale <- var_mean
            } else {
              shape <- var_mean^2 / var_var
              scale <- var_var / var_mean
            }
            
            # Create well-separated parameters
            shape_values <- seq(max(shape * 0.5, 0.1), shape * 2, length.out = input$n_states)
            scale_values <- rep(max(scale, 0.1), input$n_states)
            
            params[[var]] <- list(
              shape = shape_values,
              scale = scale_values
            )
          } else {
            # Can be negative - use normal
            dists[[var]] <- "norm"
            data_mean <- mean(var_data, na.rm = TRUE)
            data_sd <- sd(var_data, na.rm = TRUE)
            
            if (data_sd <= 0) {
              data_sd <- 1  # Fallback if no variation
            }
            
            # Create well-separated means
            mean_range <- seq(data_mean - data_sd, data_mean + data_sd, length.out = input$n_states)
            sd_values <- rep(data_sd * 0.7, input$n_states)  # Slightly smaller SD for each state
            
            params[[var]] <- list(
              mean = mean_range,
              sd = pmax(sd_values, 0.01)  # Ensure positive SD
            )
          }
        }
        
        suggested_distributions(dists)
        suggested_parameters(params)
        distributions_configured(TRUE)
        
        # Update global values
        values$n_states <- input$n_states
        values$distributions <- dists
        
        showNotification("Model auto-configured successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error in auto-configuration:", e$message), type = "error")
      })
    })    
    # Distributions ready flag
    output$distributions_ready <- reactive({
      distributions_configured()
    })
    outputOptions(output, "distributions_ready", suspendWhenHidden = FALSE)
    
    # Distribution configuration UI
    output$distribution_config <- renderUI({
      req(suggested_distributions(), suggested_parameters())
      
      dist_configs <- list()
      
      for (var in names(suggested_distributions())) {
        dist_type <- suggested_distributions()[[var]]
        params <- suggested_parameters()[[var]]
        
        # Create UI for this variable
        var_config <- div(
          style = "border: 1px solid #ddd; padding: 15px; margin: 10px 0; border-radius: 5px;",
          
          h5(paste("📊", var), style = "color: #3c8dbc; margin-bottom: 10px;"),
          
          fluidRow(
            column(4,
              selectInput(
                session$ns(paste0("dist_", var)),
                "Distribution Type:",
                choices = list(
                  "Normal (can be negative)" = "norm",
                  "Gamma (positive only)" = "gamma", 
                  "Poisson (counting data)" = "pois",
                  "Beta (between 0 and 1)" = "beta"
                ),
                selected = dist_type,
                width = "100%"
              )
            ),
            column(8,
              uiOutput(session$ns(paste0("params_", var)))
            )
          )
        )
        
        dist_configs[[var]] <- var_config
      }
      
      do.call(tagList, dist_configs)
    })    
    # Dynamic parameter inputs for each variable
    observe({
      req(suggested_distributions(), suggested_parameters())
      
      for (var in names(suggested_distributions())) {
        local({
          current_var <- var
          
          output[[paste0("params_", current_var)]] <- renderUI({
            dist_type <- if (is.null(input[[paste0("dist_", current_var)]])) suggested_distributions()[[current_var]] else input[[paste0("dist_", current_var)]]
            params <- suggested_parameters()[[current_var]]
            n_states <- input$n_states
            
            param_inputs <- switch(dist_type,
              "norm" = list(
                div(
                  style = "margin-bottom: 10px;",
                  strong("Mean values for each state:"),
                  fluidRow(
                    lapply(1:n_states, function(i) {
                      column(12/n_states,
                        numericInput(
                          session$ns(paste0(current_var, "_mean_", i)),
                          paste("State", i, "Mean:"),
                          value = if (is.null(params$mean[i])) 0 else params$mean[i],
                          step = 0.1,
                          width = "100%"
                        )
                      )
                    })
                  )
                ),
                div(
                  strong("Standard deviation values for each state:"),
                  fluidRow(
                    lapply(1:n_states, function(i) {
                      column(12/n_states,
                        numericInput(
                          session$ns(paste0(current_var, "_sd_", i)),
                          paste("State", i, "SD:"),
                          value = if (is.null(params$sd[i])) 1 else params$sd[i],
                          min = 0.01,
                          step = 0.1,
                          width = "100%"
                        )
                      )
                    })
                  )
                )
              ),              
              "gamma" = list(
                div(
                  style = "margin-bottom: 10px;",
                  strong("Shape values for each state:"),
                  fluidRow(
                    lapply(1:n_states, function(i) {
                      column(12/n_states,
                        numericInput(
                          session$ns(paste0(current_var, "_shape_", i)),
                          paste("State", i, "Shape:"),
                          value = if (is.null(params$shape[i])) 1 else params$shape[i],
                          min = 0.01,
                          step = 0.1,
                          width = "100%"
                        )
                      )
                    })
                  )
                ),
                div(
                  strong("Scale values for each state:"),
                  fluidRow(
                    lapply(1:n_states, function(i) {
                      column(12/n_states,
                        numericInput(
                          session$ns(paste0(current_var, "_scale_", i)),
                          paste("State", i, "Scale:"),
                          value = if (is.null(params$scale[i])) 1 else params$scale[i],
                          min = 0.01,
                          step = 0.1,
                          width = "100%"
                        )
                      )
                    })
                  )
                )
              ),
              
              "pois" = list(
                div(
                  strong("Lambda (rate) values for each state:"),
                  fluidRow(
                    lapply(1:n_states, function(i) {
                      column(12/n_states,
                        numericInput(
                          session$ns(paste0(current_var, "_lambda_", i)),
                          paste("State", i, "Lambda:"),
                          value = if (is.null(params$lambda[i])) 1 else params$lambda[i],
                          min = 0.01,
                          step = 0.1,
                          width = "100%"
                        )
                      )
                    })
                  )
                )
              )
            )
            
            do.call(tagList, param_inputs)
          })
        })
      }
    })    
    # Reset parameters
    observeEvent(input$reset_params, {
      req(suggested_parameters())
      
      for (var in names(suggested_parameters())) {
        params <- suggested_parameters()[[var]]
        dist_type <- suggested_distributions()[[var]]
        
        # Reset parameter inputs
        if (dist_type == "norm") {
          for (i in 1:input$n_states) {
            updateNumericInput(session, paste0(var, "_mean_", i), value = params$mean[i])
            updateNumericInput(session, paste0(var, "_sd_", i), value = params$sd[i])
          }
        } else if (dist_type == "gamma") {
          for (i in 1:input$n_states) {
            updateNumericInput(session, paste0(var, "_shape_", i), value = params$shape[i])
            updateNumericInput(session, paste0(var, "_scale_", i), value = params$scale[i])
          }
        } else if (dist_type == "pois") {
          for (i in 1:input$n_states) {
            updateNumericInput(session, paste0(var, "_lambda_", i), value = params$lambda[i])
          }
        }
      }
      
      showNotification("Parameters reset to auto-detected values.", type = "message")
    })
    
    # Confirm model configuration
    observeEvent(input$confirm_model, {
      req(distributions_configured(), suggested_distributions())
      
      tryCatch({
        # Collect current distribution selections and parameters
        final_dists <- list()
        final_params <- list()
        
        for (var in names(suggested_distributions())) {
          dist_type <- if (is.null(input[[paste0("dist_", var)]])) suggested_distributions()[[var]] else input[[paste0("dist_", var)]]
          final_dists[[var]] <- dist_type
          
          # Collect parameters based on distribution type
          if (dist_type == "norm") {
            means <- sapply(1:input$n_states, function(i) {
              if (is.null(input[[paste0(var, "_mean_", i)]])) 0 else input[[paste0(var, "_mean_", i)]]
            })
            sds <- sapply(1:input$n_states, function(i) {
              if (is.null(input[[paste0(var, "_sd_", i)]])) 1 else input[[paste0(var, "_sd_", i)]]
            })
            final_params[[var]] <- list(mean = means, sd = sds)
            
          } else if (dist_type == "gamma") {
            shapes <- sapply(1:input$n_states, function(i) {
              if (is.null(input[[paste0(var, "_shape_", i)]])) 1 else input[[paste0(var, "_shape_", i)]]
            })
            scales <- sapply(1:input$n_states, function(i) {
              if (is.null(input[[paste0(var, "_scale_", i)]])) 1 else input[[paste0(var, "_scale_", i)]]
            })
            final_params[[var]] <- list(shape = shapes, scale = scales)
            
          } else if (dist_type == "pois") {
            lambdas <- sapply(1:input$n_states, function(i) {
              if (is.null(input[[paste0(var, "_lambda_", i)]])) 1 else input[[paste0(var, "_lambda_", i)]]
            })
            final_params[[var]] <- list(lambda = lambdas)
          }
        }
        
        # Store in global values
        values$n_states <- input$n_states
        values$distributions <- final_dists
        values$parameters <- final_params
        values$step_completed[2] <- TRUE
        
        showNotification("Model configuration confirmed! Ready to fit the model.", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error confirming model:", e$message), type = "error")
      })
    })
    
  })
}