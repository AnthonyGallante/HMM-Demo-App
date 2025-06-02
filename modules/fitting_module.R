# Model Fitting Module
# modules/fitting_module.R

library(shiny)
library(shinycssloaders)
library(hmmTMB)

fittingModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6,
      # Model summary and fitting controls
      box(
        title = "Model Summary & Fitting", 
        status = "primary", 
        solidHeader = TRUE,
        width = NULL,
        
        conditionalPanel(
          condition = paste0("output['", ns("ready_to_fit"), "']"),
          
          h4("🎯 Your Model Setup"),
          verbatimTextOutput(ns("model_summary")),
          
          br(),
          div(
            style = "text-align: center;",
            actionButton(
              ns("fit_model"),
              "🚀 Fit Model",
              class = "btn-primary btn-lg",
              icon = icon("play-circle"),
              style = "font-size: 18px; padding: 15px 30px;"
            )
          ),
          
          br(),
          helpText("This process analyzes your data to find the hidden patterns. It may take 30 seconds to a few minutes depending on your data size."),
          
          # Progress indicator
          conditionalPanel(
            condition = paste0("input['", ns("fit_model"), "'] > 0 && !output['", ns("model_fitted"), "']"),
            br(),
            div(
              style = "text-align: center; padding: 20px;",
              withSpinner(
                div(
                  h4("🔄 Fitting Model..."),
                  p("Please wait while we analyze your data patterns.", style = "color: #666;")
                ),
                color = "#3c8dbc",
                size = 1
              )
            )
          )
        ),        
        conditionalPanel(
          condition = paste0("!output['", ns("ready_to_fit"), "']"),
          div(
            style = "text-align: center; color: #999; padding: 30px;",
            icon("exclamation-triangle", style = "font-size: 48px;"),
            h4("Configuration Incomplete"),
            p("Please complete the previous steps to configure your model before fitting.")
          )
        )
      )
    ),
    
    column(6,
      # Model results
      conditionalPanel(
        condition = paste0("output['", ns("model_fitted"), "']"),
        
        box(
          title = "✅ Model Results", 
          status = "success", 
          solidHeader = TRUE,
          width = NULL,
          
          h4("Model Performance Metrics"),
          verbatimTextOutput(ns("model_metrics")),
          
          br(),
          h4("State Probabilities"),
          helpText("These show the estimated probability of being in each hidden state over time."),
          withSpinner(
            plotOutput(ns("state_probs_plot"), height = "300px"),
            color = "#28a745"
          ),
          
          br(),
          div(
            style = "text-align: center;",
            actionButton(
              ns("view_results"),
              "🔍 Explore Detailed Results",
              class = "btn-success btn-lg",
              icon = icon("chart-line")
            )
          )
        )
      )
    ),    
    # Error handling
    column(12,
      conditionalPanel(
        condition = paste0("output['", ns("fitting_error"), "']"),
        
        box(
          title = "⚠️ Fitting Error", 
          status = "danger", 
          solidHeader = TRUE,
          width = NULL,
          
          h4("Something went wrong during model fitting"),
          verbatimTextOutput(ns("error_message")),
          
          br(),
          p("Suggestions:"),
          tags$ul(
            tags$li("Try reducing the number of states"),
            tags$li("Check that your data doesn't have too many missing values"),
            tags$li("Consider using different initial parameter values"),
            tags$li("Make sure your observable variables have sufficient variation")
          ),
          
          br(),
          actionButton(
            ns("retry_fit"),
            "🔄 Try Again",
            class = "btn-warning",
            icon = icon("refresh")
          )
        )
      )
    )
  )
}

fittingModuleServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Local reactive values
    fitting_error <- reactiveVal(FALSE)
    error_msg <- reactiveVal("")
    model_fitted <- reactiveVal(FALSE)
    
    # Check if ready to fit
    output$ready_to_fit <- reactive({
      !is.null(values$data) && 
      !is.null(values$distributions) && 
      !is.null(values$parameters) &&
      !is.null(values$n_states) &&
      values$step_completed[2]
    })
    outputOptions(output, "ready_to_fit", suspendWhenHidden = FALSE)    
    # Model summary
    output$model_summary <- renderText({
      req(values$data, values$distributions, values$n_states)
      
      n_obs <- length(values$observable_vars)
      n_cov <- length(if (is.null(values$covariate_vars)) character(0) else values$covariate_vars)
      n_individuals <- length(unique(values$data$ID))
      n_observations <- nrow(values$data)
      
      dist_summary <- paste(
        sapply(names(values$distributions), function(var) {
          dist_name <- switch(values$distributions[[var]],
            "norm" = "Normal",
            "gamma" = "Gamma", 
            "pois" = "Poisson",
            "beta" = "Beta",
            values$distributions[[var]]
          )
          paste0("  • ", var, ": ", dist_name, " distribution")
        }),
        collapse = "\n"
      )
      
      paste0(
        "Dataset: ", values$data_name, "\n",
        "Observations: ", n_observations, " (from ", n_individuals, " individuals)\n",
        "Hidden States: ", values$n_states, "\n",
        "Observable Variables: ", n_obs, "\n",
        dist_summary, "\n",
        "Covariates: ", if(n_cov > 0) paste(values$covariate_vars, collapse = ", ") else "None"
      )
    })
    
    # Fit model
    observeEvent(input$fit_model, {
      req(values$data, values$distributions, values$parameters, values$n_states)
      
      # Reset states
      fitting_error(FALSE)
      model_fitted(FALSE)
      
      tryCatch({
        
        # Validate data before fitting
        for (var in values$observable_vars) {
          var_data <- values$data[[var]]
          var_data <- var_data[!is.na(var_data)]
          
          if (length(var_data) < 10) {
            stop("Variable '", var, "' has too few observations (", length(var_data), "). Need at least 10.")
          }
          
          if (var(var_data, na.rm = TRUE) == 0) {
            stop("Variable '", var, "' has no variation. All values are the same.")
          }
          
          # Check distribution-specific constraints
          dist_type <- values$distributions[[var]]
          if (dist_type == "gamma" && any(var_data <= 0)) {
            stop("Variable '", var, "' contains non-positive values but is set to Gamma distribution.")
          }
          if (dist_type == "pois" && (!all(var_data >= 0) || !all(var_data == round(var_data)))) {
            stop("Variable '", var, "' contains non-integer or negative values but is set to Poisson distribution.")
          }
        }
        
        # Validate parameters
        for (var in names(values$parameters)) {
          params <- values$parameters[[var]]
          dist_type <- values$distributions[[var]]
          
          if (dist_type == "norm") {
            if (any(params$sd <= 0)) {
              stop("Standard deviation must be positive for variable '", var, "'.")
            }
          } else if (dist_type == "gamma") {
            if (any(params$shape <= 0) || any(params$scale <= 0)) {
              stop("Gamma parameters must be positive for variable '", var, "'.")
            }
          } else if (dist_type == "pois") {
            if (any(params$lambda <= 0)) {
              stop("Lambda must be positive for Poisson variable '", var, "'.")
            }
          }
        }
        
        # Special case: Fitbit heart rate data with TimeOfDay covariate
        is_fitbit_data <- !is.null(values$data_name) && grepl("Fitbit", values$data_name, ignore.case = TRUE)
        has_timeofday <- !is.null(values$covariate_vars) && "TimeOfDay" %in% values$covariate_vars
        
        if (is_fitbit_data && has_timeofday && "Value" %in% values$observable_vars) {
          cat("Detected Fitbit heart rate data with TimeOfDay - using advanced observation formulas\n")
          
          # Create special trigonometric formula for daily patterns
          obs_formula <- ~ cos(TimeOfDay / 4)
          
          # Set up observation formulas for heart rate parameters
          obs_formulas <- list(
            Value = list(
              mean = obs_formula,
              sd = obs_formula
            )
          )
          
          # For Fitbit data, create MarkovChain WITHOUT covariate formula
          # (covariates go to observation parameters instead)
          hid <- MarkovChain$new(
            data = values$data,
            n_states = values$n_states,
            initial_state = "stationary"
          )
          
          # Create Observation object WITH formulas
          obs <- Observation$new(
            data = values$data,
            dists = values$distributions,
            par = values$parameters,
            n_states = values$n_states,
            formulas = obs_formulas
          )
          
        } else {
          # Standard case: Create Markov Chain object
          if (length(values$covariate_vars) > 0) {
            # With covariates on transition matrix
            formula_str <- paste("~", paste(values$covariate_vars, collapse = " + "))
            cov_formula <- as.formula(formula_str)
            
            hid <- MarkovChain$new(
              data = values$data,
              n_states = values$n_states,
              initial_state = "stationary",
              formula = cov_formula
            )
          } else {
            # Without covariates
            hid <- MarkovChain$new(
              data = values$data,
              n_states = values$n_states,
              initial_state = "stationary"
            )
          }
          
          # Create standard Observation object
          obs <- Observation$new(
            data = values$data,
            dists = values$distributions,
            par = values$parameters,
            n_states = values$n_states
          )
        }
        
        # Create HMM object
        model <- HMM$new(
          obs = obs,
          hid = hid
        )
        
        # Fit the model
        model$fit()
        
        # Store results
        values$model <- model
        model_fitted(TRUE)
        values$fitted <- TRUE
        values$step_completed[3] <- TRUE
        
        showNotification("🎉 Model fitted successfully!", type = "message", duration = 5)
        
      }, error = function(e) {
        fitting_error(TRUE)
        error_msg(as.character(e))
        showNotification("❌ Model fitting failed. Please check the error details.", type = "error")
      })
    })
    
    # Retry fitting
    observeEvent(input$retry_fit, {
      fitting_error(FALSE)
      error_msg("")
    })
    
    # Model fitted flag
    output$model_fitted <- reactive({
      model_fitted() && !fitting_error()
    })
    outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)
    
    # Fitting error flag
    output$fitting_error <- reactive({
      fitting_error()
    })
    outputOptions(output, "fitting_error", suspendWhenHidden = FALSE)    
    # Model metrics
    output$model_metrics <- renderText({
      req(values$model, model_fitted())
      
      tryCatch({
        marginal_aic <- values$model$AIC_marginal()
        conditional_aic <- values$model$AIC_conditional()
        
        paste0(
          "✅ Model fitted successfully!\n\n",
          "Model Quality Metrics:\n",
          "• Marginal AIC: ", round(marginal_aic, 2), "\n",
          "• Conditional AIC: ", round(conditional_aic, 2), "\n\n",
          "Lower AIC values indicate better model fit.\n",
          "Use these metrics to compare different model configurations."
        )
      }, error = function(e) {
        "Model fitted successfully!\nMetrics calculation in progress..."
      })
    })
    
    # State probabilities plot
    output$state_probs_plot <- renderPlot({
      req(values$model, model_fitted())
      
      tryCatch({
        # Get state probabilities
        state_probs <- values$model$state_probs()
        
        # Create a simple visualization
        if (is.matrix(state_probs)) {
          n_obs <- nrow(state_probs)
          n_states <- ncol(state_probs)
          
          # Create time index
          time_idx <- 1:n_obs
          
          # Plot
          par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
          plot(time_idx, state_probs[, 1], type = "l", col = rainbow(n_states)[1], 
               ylim = c(0, 1), xlab = "Time", ylab = "State Probability",
               main = "Hidden State Probabilities Over Time", lwd = 2)
          
          if (n_states > 1) {
            for (i in 2:n_states) {
              lines(time_idx, state_probs[, i], col = rainbow(n_states)[i], lwd = 2)
            }
          }
          
          # Add legend
          legend("topright", legend = paste("State", 1:n_states), 
                 col = rainbow(n_states), lty = 1, lwd = 2, cex = 0.8)
          
        } else {
          plot(1, 1, type = "n", main = "State Probabilities", 
               xlab = "", ylab = "", axes = FALSE)
          text(1, 1, "State probabilities not available", cex = 1.2)
        }
        
      }, error = function(e) {
        plot(1, 1, type = "n", main = "State Probabilities", 
             xlab = "", ylab = "", axes = FALSE)
        text(1, 1, "Unable to generate plot", cex = 1.2)
      })
    })
    
    # Error message
    output$error_message <- renderText({
      error_msg()
    })
    
    # View results button
    observeEvent(input$view_results, {
      values$step_completed[4] <- TRUE
      showNotification("Navigate to the 'Explore Results' tab to see detailed visualizations!", 
                      type = "message", duration = 3)
    })
    
  })
}