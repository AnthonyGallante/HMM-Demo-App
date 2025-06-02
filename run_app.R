# HMM Demo Application Launcher
# run_app.R

# Check and install required packages
check_and_install_packages <- function() {
  required_packages <- c(
    "shiny",
    "shinydashboard", 
    "shinyWidgets",
    "shinycssloaders",
    "DT",
    "plotly",
    "hmmTMB",
    "shinyjs"
  )
  
  # Optional packages for example datasets
  optional_packages <- c(
    "moveHMM",
    "mHMMbayes"
  )
  
  # Check required packages
  missing_required <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_required) > 0) {
    cat("Installing required packages:", paste(missing_required, collapse = ", "), "\n")
    install.packages(missing_required)
  }
  
  # Check optional packages
  missing_optional <- optional_packages[!sapply(optional_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_optional) > 0) {
    cat("Note: Some example datasets may not be available without these packages:\n")
    cat(paste(missing_optional, collapse = ", "), "\n")
    cat("You can install them with: install.packages(c('", paste(missing_optional, collapse = "', '"), "'))\n\n")
  }
  
  # Verify hmmTMB is properly installed
  if (!requireNamespace("hmmTMB", quietly = TRUE)) {
    stop("hmmTMB package is required but not available. Please install it first.")
  }
  
  cat("✅ Package check complete!\n\n")
}

# Create necessary directories
setup_directories <- function() {
  if (!dir.exists("modules")) {
    dir.create("modules")
    cat("Created modules/ directory\n")
  }
  
  if (!dir.exists("utils")) {
    dir.create("utils")
    cat("Created utils/ directory\n")
  }
}
# Print startup message
print_startup_message <- function() {
  cat("\n")
  cat("🎯 Hidden Markov Model Demo Application\n")
  cat("=====================================\n\n")
  cat("This application provides a user-friendly interface for:\n")
  cat("• Loading and exploring data\n")
  cat("• Configuring Hidden Markov Models\n") 
  cat("• Fitting models with hmmTMB\n")
  cat("• Generating comprehensive visualizations\n\n")
  cat("📁 Make sure the following files are in your working directory:\n")
  cat("   - app.R (main application)\n")
  cat("   - modules/data_module.R\n")
  cat("   - modules/model_config_module.R\n")
  cat("   - modules/fitting_module.R\n")
  cat("   - modules/visualization_module.R\n")
  cat("   - utils/helpers.R\n")
  cat("   - muskox_summer_2016.csv (optional example dataset)\n\n")
}

# Main function to run the app
run_hmm_demo <- function(launch_browser = TRUE, port = NULL) {
  
  print_startup_message()
  
  # Check packages
  tryCatch({
    check_and_install_packages()
  }, error = function(e) {
    cat("❌ Error checking packages:", e$message, "\n")
    cat("Please install required packages manually and try again.\n")
    return(invisible())
  })
  
  # Setup directories
  setup_directories()
  
  # Check if main app file exists
  if (!file.exists("app.R")) {
    cat("❌ Error: app.R not found in current directory.\n")
    cat("Please make sure all application files are in the working directory.\n")
    return(invisible())
  }
  
  # Launch the application
  tryCatch({
    cat("🚀 Launching HMM Demo App...\n\n")
    
    if (is.null(port)) {
      port <- 3838
    }
    
    shiny::runApp(
      appDir = ".",
      launch.browser = launch_browser,
      port = port,
      host = "127.0.0.1"
    )
    
  }, error = function(e) {
    cat("❌ Error launching app:", e$message, "\n")
    cat("\nTroubleshooting tips:\n")
    cat("1. Make sure all required files are present\n")
    cat("2. Check that the working directory is correct\n")
    cat("3. Verify all packages are properly installed\n")
    cat("4. Try running: source('app.R') directly\n")
  })
}
# If this script is run directly, launch the app
if (interactive()) {
  run_hmm_demo()
} else {
  cat("To run the HMM Demo App, use: run_hmm_demo()\n")
  cat("Or simply source this file in an interactive R session.\n")
}

# Additional utility functions for demo preparation

# Function to download example dataset if not present
download_muskox_data <- function() {
  if (!file.exists("muskox_summer_2016.csv")) {
    cat("Muskox dataset not found. You may need to provide this file manually.\n")
    cat("Expected filename: muskox_summer_2016.csv\n")
  } else {
    cat("✅ Muskox dataset found: muskox_summer_2016.csv\n")
  }
}

# Function to verify application structure
verify_app_structure <- function() {
  required_files <- c(
    "app.R",
    "modules/data_module.R",
    "modules/model_config_module.R", 
    "modules/fitting_module.R",
    "modules/visualization_module.R",
    "utils/helpers.R"
  )
  
  cat("📋 Checking application structure:\n")
  
  all_present <- TRUE
  for (file in required_files) {
    if (file.exists(file)) {
      cat("✅", file, "\n")
    } else {
      cat("❌", file, "(MISSING)\n")
      all_present <- FALSE
    }
  }
  
  # Check optional files
  optional_files <- c("muskox_summer_2016.csv")
  
  cat("\n📁 Optional files:\n")
  for (file in optional_files) {
    if (file.exists(file)) {
      cat("✅", file, "\n")
    } else {
      cat("⚠️ ", file, "(optional - some examples may not work)\n")
    }
  }
  
  if (all_present) {
    cat("\n🎉 All required files present! Ready to launch.\n")
  } else {
    cat("\n❌ Some required files are missing. Please ensure all files are in place.\n")
  }
  
  return(all_present)
}

# Function to create a quick demo script
create_demo_script <- function() {
  demo_script <- '
# Quick Demo Script for HMM Application
# ====================================

# 1. Load the application
source("run_app.R")

# 2. Verify everything is set up correctly
verify_app_structure()

# 3. Launch the demo app
run_hmm_demo()

# Alternative: Launch without opening browser (useful for RStudio Server)
# run_hmm_demo(launch_browser = FALSE)

# Alternative: Launch on specific port
# run_hmm_demo(port = 4000)
'
  
  writeLines(demo_script, "demo.R")
  cat("📝 Created demo.R script for easy launching\n")
}