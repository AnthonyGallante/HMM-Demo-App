# Visualization Module
# modules/visualization_module.R

library(shiny)
library(shinycssloaders)
library(plotly)

visualizationModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Plot controls
    column(3,
      box(
        title = "📊 Plot Controls", 
        status = "primary", 
        solidHeader = TRUE,
        width = NULL,
        
        conditionalPanel(
          condition = paste0("output['", ns("model_available"), "']"),
          
          h4("Distribution Plots"),
          helpText("See how each variable is distributed across different hidden states."),
          
          uiOutput(ns("dist_plot_buttons")),
          
          hr(),
          
          h4("Time Series Plots"),
          helpText("Visualize your data over time, colored by hidden states."),
          
          selectInput(
            ns("ts_var1"),
            "Primary Variable:",
            choices = NULL,
            width = "100%"
          ),
          
          selectInput(
            ns("ts_var2"),
            "Secondary Variable (Optional):",
            choices = c("None" = "none"),
            selected = "none",
            width = "100%"
          ),
          
          actionButton(
            ns("plot_timeseries"),
            "📈 Time Series Plot",
            class = "btn-info plot-button",
            icon = icon("chart-line"),
            width = "100%"
          ),          
          # Covariate plots (conditional)
          conditionalPanel(
            condition = paste0("output['", ns("has_covariates"), "']"),
            
            hr(),
            h4("Covariate Effects"),
            helpText("See how covariates influence state transitions and probabilities."),
            
            selectInput(
              ns("covariate_select"),
              "Select Covariate:",
              choices = NULL,
              width = "100%"
            ),
            
            div(
              style = "margin: 5px 0;",
              actionButton(
                ns("plot_delta"),
                "📊 State Probabilities",
                class = "btn-warning plot-button",
                style = "width: 100%; margin-bottom: 5px;"
              ),
              actionButton(
                ns("plot_tpm"),
                "🔄 Transition Probabilities", 
                class = "btn-warning plot-button",
                style = "width: 100%; margin-bottom: 5px;"
              ),
              actionButton(
                ns("plot_obspar"),
                "📈 Observation Parameters",
                class = "btn-warning plot-button",
                style = "width: 100%;"
              )
            )
          ),
          
          hr(),
          
          h4("Model Diagnostics"),
          helpText("Check how well your model fits the data."),
          
          actionButton(
            ns("plot_residuals"),
            "🔍 Residual Analysis",
            class = "btn-secondary plot-button",
            width = "100%"
          )
        ),
        
        conditionalPanel(
          condition = paste0("!output['", ns("model_available"), "']"),
          div(
            style = "text-align: center; color: #999; padding: 20px;",
            icon("info-circle", style = "font-size: 36px;"),
            h4("No Model Available"),
            p("Please fit a model first to generate visualizations.")
          )
        )
      )
    ),    
    # Main plot area
    column(9,
      box(
        title = "📈 Visualization Area", 
        status = "success", 
        solidHeader = TRUE,
        width = NULL,
        
        conditionalPanel(
          condition = paste0("output['", ns("plot_ready"), "']"),
          
          # Plot title and description
          uiOutput(ns("plot_header")),
          
          # Main plot output
          withSpinner(
            uiOutput(ns("main_plot_output")),
            color = "#28a745",
            size = 1
          ),
          
          br(),
          
          # Plot explanation
          uiOutput(ns("plot_explanation"))
        ),
        
        conditionalPanel(
          condition = paste0("!output['", ns("plot_ready"), "']"),
          div(
            style = "text-align: center; color: #999; padding: 50px;",
            icon("chart-bar", style = "font-size: 64px;"),
            h3("Ready for Visualization"),
            p("Click any plot button on the left to generate visualizations of your model results.", 
              style = "font-size: 16px;")
          )
        )
      )
    ),    
    # Summary statistics
    column(12,
      conditionalPanel(
        condition = paste0("output['", ns("model_available"), "']"),
        
        box(
          title = "📋 Model Summary & Insights", 
          status = "info", 
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          
          fluidRow(
            column(6,
              h4("🎯 Hidden States Discovered"),
              verbatimTextOutput(ns("state_summary"))
            ),
            column(6,
              h4("📊 Variable Insights"),
              verbatimTextOutput(ns("variable_insights"))
            )
          ),
          
          br(),
          h4("💡 What do these results mean?"),
          uiOutput(ns("results_interpretation"))
        )
      )
    )
  )
}

visualizationModuleServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Local reactive values
    current_plot <- reactiveVal(NULL)
    current_plot_type <- reactiveVal("")
    current_plot_description <- reactiveVal("")
    
    # Check if model is available
    output$model_available <- reactive({
      !is.null(values$model) && values$fitted
    })
    outputOptions(output, "model_available", suspendWhenHidden = FALSE)
    
    # Check if covariates are available
    output$has_covariates <- reactive({
      !is.null(values$covariate_vars) && length(values$covariate_vars) > 0
    })
    outputOptions(output, "has_covariates", suspendWhenHidden = FALSE)    
    # Initialize UI elements when model is available
    observe({
      req(values$model, values$observable_vars)
      
      # Update variable choices for time series plots
      var_choices <- setNames(values$observable_vars, values$observable_vars)
      updateSelectInput(session, "ts_var1", choices = var_choices, selected = var_choices[1])
      
      ts_var2_choices <- c("None" = "none", var_choices)
      updateSelectInput(session, "ts_var2", choices = ts_var2_choices, selected = "none")
      
      # Update covariate choices if available
      if (!is.null(values$covariate_vars) && length(values$covariate_vars) > 0) {
        cov_choices <- setNames(values$covariate_vars, values$covariate_vars)
        updateSelectInput(session, "covariate_select", choices = cov_choices, selected = cov_choices[1])
      }
    })
    
    # Generate distribution plot buttons
    output$dist_plot_buttons <- renderUI({
      req(values$observable_vars)
      
      buttons <- lapply(values$observable_vars, function(var) {
        actionButton(
          session$ns(paste0("plot_dist_", gsub("[^A-Za-z0-9]", "_", var))),
          paste("📊", var),
          class = "btn-success plot-button",
          style = "width: 100%; margin-bottom: 5px;"
        )
      })
      
      do.call(tagList, buttons)
    })    
    # Distribution plot observers
    observe({
      req(values$observable_vars)
      
      for (var in values$observable_vars) {
        local({
          current_var <- var
          button_id <- paste0("plot_dist_", gsub("[^A-Za-z0-9]", "_", current_var))
          
          observeEvent(input[[button_id]], {
            req(values$model)
            
            tryCatch({
              plot_obj <- values$model$plot_dist(current_var)
              current_plot(plot_obj)
              current_plot_type(paste("Distribution Plot:", current_var))
              current_plot_description(paste(
                "This plot shows how the variable", current_var, 
                "is distributed across different hidden states. Each state should have a distinct distribution pattern."
              ))
              
              showNotification(paste("Generated distribution plot for", current_var), type = "message")
              
            }, error = function(e) {
              showNotification(paste("Error generating plot:", e$message), type = "error")
            })
          })
        })
      }
    })
    
    # Time series plot
    observeEvent(input$plot_timeseries, {
      req(values$model, input$ts_var1)
      
      tryCatch({
        if (input$ts_var2 != "none") {
          plot_obj <- values$model$plot_ts(var = input$ts_var1, var2 = input$ts_var2)
          current_plot_type(paste("Time Series Plot:", input$ts_var1, "vs", input$ts_var2))
          current_plot_description(paste(
            "This plot shows your data over time, with colors indicating which hidden state",
            "the model thinks each observation belongs to. Look for patterns in when states change."
          ))
        } else {
          plot_obj <- values$model$plot_ts(var = input$ts_var1)
          current_plot_type(paste("Time Series Plot:", input$ts_var1))
          current_plot_description(paste(
            "This plot shows", input$ts_var1, "over time, colored by hidden states.",
            "Different colors represent different behavioral states or patterns."
          ))
        }
        
        current_plot(plot_obj)
        showNotification("Generated time series plot", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error generating time series plot:", e$message), type = "error")
      })
    })    
    # Delta plot (state probabilities)
    observeEvent(input$plot_delta, {
      req(values$model, input$covariate_select)
      
      tryCatch({
        plot_obj <- values$model$plot("delta", input$covariate_select)
        current_plot(plot_obj)
        current_plot_type(paste("State Probabilities vs", input$covariate_select))
        current_plot_description(paste(
          "This plot shows how the probability of being in each state changes with",
          input$covariate_select, ". It helps understand what factors influence state membership."
        ))
        
        showNotification("Generated state probabilities plot", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error generating delta plot:", e$message), type = "error")
      })
    })
    
    # TPM plot (transition probabilities)
    observeEvent(input$plot_tpm, {
      req(values$model, input$covariate_select)
      
      tryCatch({
        plot_obj <- values$model$plot("tpm", input$covariate_select)
        current_plot(plot_obj)
        current_plot_type(paste("Transition Probabilities vs", input$covariate_select))
        current_plot_description(paste(
          "This plot shows how the probability of switching between states changes with",
          input$covariate_select, ". It reveals what triggers state transitions."
        ))
        
        showNotification("Generated transition probabilities plot", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error generating TPM plot:", e$message), type = "error")
      })
    })
    
    # Observation Parameters plot
    observeEvent(input$plot_obspar, {
      req(values$model, input$covariate_select)
      
      tryCatch({
        plot_obj <- values$model$plot("obspar", input$covariate_select)
        current_plot(plot_obj)
        current_plot_type(paste("Observation Parameters vs", input$covariate_select))
        current_plot_description(paste(
          "This plot shows how the observation parameters (mean and variance) change with",
          input$covariate_select, ". This is particularly useful for time-varying effects like daily rhythms."
        ))
        
        showNotification("Generated observation parameters plot", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error generating observation parameters plot:", e$message), type = "error")
      })
    })    
    # Residual analysis
    observeEvent(input$plot_residuals, {
      req(values$model, values$observable_vars)
      
      tryCatch({
        # Generate pseudo residuals
        pseudo_residuals <- values$model$pseudores()
        
        current_plot(TRUE)  # Set to TRUE to indicate a plot is ready
        current_plot_type("Residual Analysis")
        current_plot_description(
          "Residual plots help assess model fit. QQ plots should follow the diagonal line if residuals are normally distributed. Autocorrelation plots should show no significant correlations at non-zero lags if the model captures temporal dependencies well."
        )
        
        showNotification("Generated residual analysis plots", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error generating residual plots:", e$message), type = "error")
      })
    })
    
    # Plot ready flag
    output$plot_ready <- reactive({
      !is.null(current_plot())
    })
    outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
    
    # Plot header
    output$plot_header <- renderUI({
      req(current_plot_type())
      
      div(
        h3(current_plot_type(), style = "color: #28a745; margin-bottom: 5px;"),
        p(current_plot_description(), class = "help-text", style = "margin-bottom: 15px;")
      )
    })
    
    # Main plot output
    output$main_plot_output <- renderUI({
      req(current_plot())
      
      if (current_plot_type() == "Residual Analysis") {
        # Special handling for residual plots
        plotOutput(session$ns("residual_plots"), height = "600px")
      } else {
        # Regular ggplot objects
        plotOutput(session$ns("main_plot"), height = "500px")
      }
    })    
    # Render main plot
    output$main_plot <- renderPlot({
      req(current_plot())
      if (current_plot_type() != "Residual Analysis") {
        print(current_plot())
      }
    })
    
    # Render residual plots
    output$residual_plots <- renderPlot({
      req(current_plot_type() == "Residual Analysis", values$model, values$observable_vars)
      
      tryCatch({
        pseudo_residuals <- values$model$pseudores()
        n_vars <- length(values$observable_vars)
        
        par(mfrow = c(n_vars, 2), mar = c(4, 4, 3, 2))
        
        for (var in values$observable_vars) {
          if (var %in% names(pseudo_residuals)) {
            residuals <- pseudo_residuals[[var]]
            
            # QQ plot
            qqnorm(residuals, main = paste("QQ Plot -", var), cex.main = 0.9)
            qqline(residuals, col = "red", lwd = 2)
            
            # ACF plot
            acf(residuals, main = paste("Autocorrelation -", var), cex.main = 0.9)
          }
        }
        
      }, error = function(e) {
        plot(1, 1, type = "n", main = "Error generating residual plots")
        text(1, 1, paste("Error:", e$message), cex = 0.8)
      })
    })
    
    # Plot explanation
    output$plot_explanation <- renderUI({
      req(current_plot())
      
      explanation <- switch(current_plot_type(),
        "Distribution Plot" = "Look for clear separation between states. Well-separated distributions suggest the model successfully identified distinct behavioral modes.",
        "Time Series Plot" = "Different colors show when the model predicts different states are active. Look for meaningful patterns in state switches.",
        "State Probabilities" = "This shows how external factors influence which state is most likely. Steep changes indicate strong covariate effects.",
        "Transition Probabilities" = "This reveals what causes animals/subjects to switch between states. Look for clear relationships between covariates and transitions.",
        "Observation Parameters" = "This shows how the mean and variance of observations change with covariates. For heart rate data, this reveals daily rhythms in both average heart rate and variability.",
        "Residual Analysis" = "Good model fit is indicated by: (1) QQ plots following the diagonal line, and (2) ACF plots showing no significant autocorrelation."
      )
      
      if (!is.null(explanation)) {
        div(
          class = "info-box",
          style = "background-color: #f8f9fa; border-left: 4px solid #28a745;",
          h5("💡 How to interpret this plot:"),
          p(explanation)
        )
      }
    })    
    # State summary
    output$state_summary <- renderText({
      req(values$model, values$n_states)
      
      tryCatch({
        # Get basic state information
        state_probs <- values$model$state_probs()
        
        if (is.matrix(state_probs)) {
          avg_state_probs <- colMeans(state_probs, na.rm = TRUE)
          
          summary_text <- paste0(
            "Number of hidden states: ", values$n_states, "\n\n",
            "Average time spent in each state:\n"
          )
          
          for (i in 1:values$n_states) {
            percentage <- round(avg_state_probs[i] * 100, 1)
            summary_text <- paste0(summary_text, "• State ", i, ": ", percentage, "%\n")
          }
          
          summary_text
        } else {
          paste("Number of hidden states:", values$n_states, "\nDetailed statistics computing...")
        }
        
      }, error = function(e) {
        paste("Number of hidden states:", values$n_states, "\nStatistics not available")
      })
    })
    
    # Variable insights
    output$variable_insights <- renderText({
      req(values$observable_vars, values$distributions)
      
      insights <- paste0("Variables analyzed: ", length(values$observable_vars), "\n\n")
      
      for (var in values$observable_vars) {
        dist_name <- switch(values$distributions[[var]],
          "norm" = "Normal",
          "gamma" = "Gamma",
          "pois" = "Poisson", 
          "beta" = "Beta",
          values$distributions[[var]]
        )
        
        insights <- paste0(insights, "• ", var, ": ", dist_name, " distribution\n")
      }
      
      if (length(values$covariate_vars) > 0) {
        insights <- paste0(insights, "\nCovariates included: ", paste(values$covariate_vars, collapse = ", "))
      }
      
      insights
    })    
    # Results interpretation
    output$results_interpretation <- renderUI({
      req(values$model, values$n_states, values$observable_vars)
      
      interpretation_text <- paste(
        "Your Hidden Markov Model has identified", values$n_states, "distinct hidden states in your data.",
        "Each state represents a different 'mode' or 'behavior' that generates the observed patterns.",
        "The model estimates when your system switches between these states and what triggers the transitions."
      )
      
      if (length(values$covariate_vars) > 0) {
        interpretation_text <- paste(
          interpretation_text,
          "Since you included covariates, the model can also tell you how external factors",
          "influence both the probability of being in each state and the likelihood of transitioning between states."
        )
      }
      
      recommendations <- tags$div(
        tags$h5("🔍 Next Steps:"),
        tags$ul(
          tags$li("Generate distribution plots to see how states differ"),
          tags$li("Create time series plots to visualize state sequences"),
          if (length(values$covariate_vars) > 0) tags$li("Examine covariate effects to understand what drives state changes"),
          tags$li("Check residual plots to validate model assumptions")
        )
      )
      
      tagList(
        p(interpretation_text),
        br(),
        recommendations
      )
    })
    
  })
}