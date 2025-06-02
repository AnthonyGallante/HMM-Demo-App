# HMM Demo Application - Main App
# A user-friendly interface for building Hidden Markov Models with hmmTMB

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(hmmTMB)
library(shinyjs)

# Source module files
source("modules/data_module.R")
source("modules/model_config_module.R")
source("modules/fitting_module.R")
source("modules/visualization_module.R")
source("utils/helpers.R")

# Define UI
ui <- dashboardPage(
  
  # Header
  dashboardHeader(
    title = "Hidden Markov Model Demo",
    titleWidth = 250,
    tags$li(
      class = "dropdown",
      style = "margin-top: 8px; margin-right: 10px;",
      actionButton(
        "reset_app",
        "🔄 Reset",
        class = "btn-danger",
        style = "color: white; background-color: #d9534f; border-color: #d43f3a;"
      )
    )
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("1. Load Data", tabName = "data", icon = icon("upload")),
      menuItem("2. Configure Model", tabName = "config", icon = icon("cogs")),
      menuItem("3. Fit Model", tabName = "fitting", icon = icon("play-circle")),
      menuItem("4. Explore Results", tabName = "results", icon = icon("chart-line"))
    ),
    
    # Progress indicator
    div(
      style = "padding: 20px;",
      h4("Progress", style = "color: white; margin-bottom: 10px;"),
      div(id = "progress-indicator", style = "color: white; font-size: 12px;",
        div(id = "step1", "📁 Load Data"),
        div(id = "step2", style = "opacity: 0.5;", "⚙️ Configure Model"),
        div(id = "step3", style = "opacity: 0.5;", "🔧 Fit Model"),
        div(id = "step4", style = "opacity: 0.5;", "📊 Explore Results")
      )
    )
  ),  
  # Body
  dashboardBody(
    
    # Initialize shinyjs
    useShinyjs(),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .info-box {
          background: white;
          border-left: 3px solid #3c8dbc;
          margin-bottom: 15px;
          padding: 15px;
          border-radius: 5px;
        }
        .help-text {
          color: #666;
          font-style: italic;
          margin-bottom: 10px;
        }
        .step-completed {
          opacity: 1 !important;
          color: #28a745 !important;
        }
        .step-active {
          opacity: 1 !important;
          color: #ffc107 !important;
          font-weight: bold;
        }
        .plot-button {
          margin: 5px;
          min-width: 150px;
        }
      "))
    ),
    
    tabItems(
      
      # Data Loading Tab
      tabItem(
        tabName = "data",
        fluidRow(
          column(12,
            div(class = "info-box",
              h3("📁 Step 1: Load Your Data"),
              p("Choose a dataset to analyze. You can use our example datasets or upload your own CSV file.",
                class = "help-text")
            )
          )
        ),
        dataModuleUI("data_module")
      ),      
      # Model Configuration Tab
      tabItem(
        tabName = "config",
        fluidRow(
          column(12,
            div(class = "info-box",
              h3("⚙️ Step 2: Configure Your Model"),
              p("Tell us what you want to analyze and how many hidden states you think exist in your data.",
                class = "help-text")
            )
          )
        ),
        modelConfigModuleUI("config_module")
      ),
      
      # Model Fitting Tab
      tabItem(
        tabName = "fitting",
        fluidRow(
          column(12,
            div(class = "info-box",
              h3("🔧 Step 3: Fit Your Model"),
              p("Now we'll train the model to find patterns in your data. This might take a moment.",
                class = "help-text")
            )
          )
        ),
        fittingModuleUI("fitting_module")
      ),
      
      # Results Tab
      tabItem(
        tabName = "results",
        fluidRow(
          column(12,
            div(class = "info-box",
              h3("📊 Step 4: Explore Your Results"),
              p("Discover what patterns the model found and visualize your results.",
                class = "help-text")
            )
          )
        ),
        visualizationModuleUI("viz_module")
      )
    )
  )
)
# Define server
server <- function(input, output, session) {
  
  # Create reactive values to store state across modules
  values <- reactiveValues(
    data = NULL,
    data_name = NULL,
    id_column = NULL,
    observable_vars = NULL,
    covariate_vars = NULL,
    n_states = 2,
    distributions = NULL,
    model = NULL,
    fitted = FALSE,
    step_completed = c(FALSE, FALSE, FALSE, FALSE)
  )
  
  # Reset application functionality
  observeEvent(input$reset_app, {
    showModal(modalDialog(
      title = "Reset Application",
      "Are you sure you want to reset the application? This will clear all data and start over.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Reset", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_reset, {
    # Reset all reactive values to initial state
    values$data <- NULL
    values$data_name <- NULL
    values$id_column <- NULL
    values$observable_vars <- NULL
    values$covariate_vars <- NULL
    values$n_states <- 2
    values$distributions <- NULL
    values$model <- NULL
    values$fitted <- FALSE
    values$step_completed <- c(FALSE, FALSE, FALSE, FALSE)
    
    # Close the modal
    removeModal()
    
    # Navigate back to the first tab
    updateTabItems(session, "sidebar", "data")
    
    # Reset progress indicators
    shinyjs::runjs("$('#step1, #step2, #step3, #step4').removeClass('step-completed step-active');")
    shinyjs::runjs("$('#step1').css('opacity', '1').css('color', '#ffc107').css('font-weight', 'bold');")
    shinyjs::runjs("$('#step2, #step3, #step4').css('opacity', '0.5').css('color', '').css('font-weight', '');")
    
    showNotification("Application reset successfully!", type = "message", duration = 3)
  })
  
  # Module servers
  dataModuleServer("data_module", values)
  modelConfigModuleServer("config_module", values)
  fittingModuleServer("fitting_module", values)
  visualizationModuleServer("viz_module", values)
  
  # Progress indicator updates
  observe({
    if (values$step_completed[1]) {
      shinyjs::runjs("$('#step1').addClass('step-completed');")
      shinyjs::runjs("$('#step2').removeClass('step-active').addClass('step-active');")
    }
    if (values$step_completed[2]) {
      shinyjs::runjs("$('#step2').addClass('step-completed');")
      shinyjs::runjs("$('#step3').removeClass('step-active').addClass('step-active');")
    }
    if (values$step_completed[3]) {
      shinyjs::runjs("$('#step3').addClass('step-completed');")
      shinyjs::runjs("$('#step4').removeClass('step-active').addClass('step-active');")
    }
    if (values$step_completed[4]) {
      shinyjs::runjs("$('#step4').addClass('step-completed');")
    }
  })
  
  # Auto-advance to next step when current step is completed
  observeEvent(values$step_completed[1], {
    if (values$step_completed[1]) {
      updateTabItems(session, "sidebar", "config")
    }
  })
  
  observeEvent(values$step_completed[2], {
    if (values$step_completed[2]) {
      updateTabItems(session, "sidebar", "fitting")
    }
  })
  
  observeEvent(values$step_completed[3], {
    if (values$step_completed[3]) {
      updateTabItems(session, "sidebar", "results")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)