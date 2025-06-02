# HMM Demo Application - Setup and Testing Script
# setup_and_test.R

# This script helps you set up and test the HMM demo application
# Run this before your presentation to ensure everything works correctly

cat("🔧 HMM Demo Application - Setup & Testing\n")
cat("==========================================\n\n")

# Function to create the complete directory structure
setup_complete_environment <- function() {
  cat("📁 Setting up directory structure...\n")
  
  # Create directories
  if (!dir.exists("modules")) dir.create("modules")
  if (!dir.exists("utils")) dir.create("utils")
  
  # Check if key files exist
  required_files <- c(
    "app.R",
    "modules/data_module.R",
    "modules/model_config_module.R",
    "modules/fitting_module.R", 
    "modules/visualization_module.R",
    "utils/helpers.R",
    "run_app.R"
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    cat("❌ Missing required files:\n")
    for (file in missing_files) {
      cat("   -", file, "\n")
    }
    cat("\nPlease ensure all application files are in the correct locations.\n")
    return(FALSE)
  }
  
  cat("✅ Directory structure complete!\n\n")
  return(TRUE)
}

# Function to install and check all required packages
setup_packages <- function() {
  cat("📦 Checking and installing required packages...\n")
  
  # Core packages (required)
  core_packages <- c(
    "shiny",
    "shinydashboard", 
    "shinyWidgets",
    "shinycssloaders", 
    "DT",
    "plotly"
  )
  
  # HMM package (critical)
  hmm_packages <- c("hmmTMB")
  
  # Example data packages (optional but recommended)
  data_packages <- c("moveHMM", "mHMMbayes")  
  # Install core packages
  cat("Installing core Shiny packages...\n")
  missing_core <- core_packages[!sapply(core_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_core) > 0) {
    install.packages(missing_core, dependencies = TRUE)
  }
  
  # Install HMM package
  cat("Checking hmmTMB package...\n")
  if (!requireNamespace("hmmTMB", quietly = TRUE)) {
    cat("Installing hmmTMB...\n")
    install.packages("hmmTMB", dependencies = TRUE)
  }
  
  # Install data packages (with error handling)
  cat("Installing example data packages...\n")
  for (pkg in data_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      tryCatch({
        install.packages(pkg, dependencies = TRUE)
        cat("✅ Installed", pkg, "\n")
      }, error = function(e) {
        cat("⚠️  Could not install", pkg, "- some examples may not work\n")
      })
    } else {
      cat("✅", pkg, "already installed\n")
    }
  }
  
  # Verify critical packages
  cat("\nVerifying critical packages...\n")
  critical_packages <- c(core_packages, hmm_packages)
  
  all_good <- TRUE
  for (pkg in critical_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      cat("✅", pkg, "\n")
    } else {
      cat("❌", pkg, "- REQUIRED\n")
      all_good <- FALSE
    }
  }
  
  if (!all_good) {
    cat("\n❌ Some required packages are missing. Please install them manually.\n")
    return(FALSE)
  }
  
  cat("\n✅ All required packages installed!\n\n")
  return(TRUE)
}

# Function to test the application with sample data
test_application <- function() {
  cat("🧪 Testing application functionality...\n")
  
  # Test 1: Load required libraries
  cat("Test 1: Loading libraries...\n")
  tryCatch({
    library(shiny, quietly = TRUE)
    library(hmmTMB, quietly = TRUE)
    cat("✅ Libraries loaded successfully\n")
  }, error = function(e) {
    cat("❌ Error loading libraries:", e$message, "\n")
    return(FALSE)
  })  
  # Test 2: Check example datasets
  cat("\nTest 2: Checking example datasets...\n")
  
  # Test built-in data
  tryCatch({
    data(airquality)
    cat("✅ Built-in airquality data available\n")
  }, error = function(e) {
    cat("⚠️  Built-in data issue:", e$message, "\n")
  })
  
  # Test moveHMM data
  if (requireNamespace("moveHMM", quietly = TRUE)) {
    tryCatch({
      data("haggis_data", package = "moveHMM")
      cat("✅ moveHMM haggis data available\n")
    }, error = function(e) {
      cat("⚠️  moveHMM data issue:", e$message, "\n")
    })
  } else {
    cat("⚠️  moveHMM package not available - haggis example won't work\n")
  }
  
  # Test mHMMbayes data
  if (requireNamespace("mHMMbayes", quietly = TRUE)) {
    tryCatch({
      data("nonverbal", package = "mHMMbayes")
      cat("✅ mHMMbayes nonverbal data available\n")
    }, error = function(e) {
      cat("⚠️  mHMMbayes data issue:", e$message, "\n")
    })
  } else {
    cat("⚠️  mHMMbayes package not available - nonverbal example won't work\n")
  }
  
  # Test CSV data
  if (file.exists("muskox_summer_2016.csv")) {
    tryCatch({
      muskox_test <- read.csv("muskox_summer_2016.csv", nrows = 5)
      cat("✅ Muskox CSV data available\n")
    }, error = function(e) {
      cat("⚠️  Muskox CSV issue:", e$message, "\n")
    })
  } else {
    cat("⚠️  muskox_summer_2016.csv not found - muskox example won't work\n")
  }
  
  # Test 3: Basic HMM functionality
  cat("\nTest 3: Testing basic HMM functionality...\n")
  tryCatch({
    # Create simple test data
    test_data <- data.frame(
      ID = factor(rep(1:2, each = 50)),
      x = c(rnorm(50, 0, 1), rnorm(50, 3, 1)),
      y = c(rnorm(50, 0, 1), rnorm(50, 3, 1))
    )
    
    # Test MarkovChain creation
    hid <- MarkovChain$new(
      data = test_data,
      n_states = 2,
      initial_state = "stationary"
    )
    
    # Test Observation creation
    obs <- Observation$new(
      data = test_data,
      dists = list(x = "norm", y = "norm"),
      par = list(
        x = list(mean = c(0, 3), sd = c(1, 1)),
        y = list(mean = c(0, 3), sd = c(1, 1))
      ),
      n_states = 2
    )
    
    # Test HMM creation
    model <- HMM$new(obs = obs, hid = hid)
    
    cat("✅ Basic HMM objects created successfully\n")
    
    # Quick fit test (this might take a moment)
    cat("   Testing model fitting (this may take 30 seconds)...\n")
    model$fit()
    cat("✅ Model fitting successful\n")
    
  }, error = function(e) {
    cat("❌ HMM functionality test failed:", e$message, "\n")
    cat("   This might indicate issues with hmmTMB installation\n")
    return(FALSE)
  })
  
  cat("\n✅ Application functionality tests passed!\n\n")
  return(TRUE)
}
# Function to create a test session log
create_test_session <- function() {
  cat("📝 Creating test session documentation...\n")
  
  session_info <- sessionInfo()
  
  test_log <- paste0(
    "HMM Demo Application - Test Session Log\n",
    "======================================\n",
    "Date: ", Sys.time(), "\n",
    "R Version: ", R.version.string, "\n",
    "Platform: ", R.version$platform, "\n\n",
    "Package Versions:\n",
    "-----------------\n"
  )
  
  # Add key package versions
  key_packages <- c("shiny", "hmmTMB", "moveHMM", "mHMMbayes")
  
  for (pkg in key_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      version <- as.character(packageVersion(pkg))
      test_log <- paste0(test_log, pkg, ": ", version, "\n")
    } else {
      test_log <- paste0(test_log, pkg, ": NOT INSTALLED\n")
    }
  }
  
  test_log <- paste0(
    test_log,
    "\nFile Structure Check:\n",
    "--------------------\n"
  )
  
  required_files <- c(
    "app.R",
    "modules/data_module.R", 
    "modules/model_config_module.R",
    "modules/fitting_module.R",
    "modules/visualization_module.R",
    "utils/helpers.R",
    "run_app.R"
  )
  
  for (file in required_files) {
    status <- if (file.exists(file)) "✅ PRESENT" else "❌ MISSING"
    test_log <- paste0(test_log, file, ": ", status, "\n")
  }
  
  # Write log file
  writeLines(test_log, "test_session_log.txt")
  cat("✅ Test log saved to: test_session_log.txt\n\n")
}

# Main setup function
main_setup <- function() {
  cat("🚀 Starting complete setup process...\n\n")
  
  success <- TRUE
  
  # Step 1: Directory structure
  if (!setup_complete_environment()) {
    success <- FALSE
  }
  
  # Step 2: Packages
  if (success && !setup_packages()) {
    success <- FALSE
  }
  
  # Step 3: Testing (only if everything else succeeded)
  if (success) {
    if (!test_application()) {
      cat("⚠️  Some tests failed, but basic setup is complete.\n")
      cat("   You may still be able to run the application.\n")
    }
  }
  
  # Step 4: Create documentation
  create_test_session()
  
  if (success) {
    cat("🎉 Setup completed successfully!\n")
    cat("   You can now launch the app with: source('run_app.R')\n")
  } else {
    cat("❌ Setup encountered issues. Check the messages above.\n")
    cat("   Try installing missing packages manually.\n")
  }
  
  return(success)
}
# Utility function for quick demo preparation
prepare_for_demo <- function() {
  cat("🎯 Preparing for demonstration...\n\n")
  
  # Quick checks
  cat("Quick pre-demo checklist:\n")
  
  # Check app files
  if (file.exists("app.R")) {
    cat("✅ Main app file present\n")
  } else {
    cat("❌ app.R missing - CRITICAL\n")
    return(FALSE)
  }
  
  # Check modules
  module_files <- c(
    "modules/data_module.R",
    "modules/model_config_module.R", 
    "modules/fitting_module.R",
    "modules/visualization_module.R"
  )
  
  all_modules <- all(file.exists(module_files))
  if (all_modules) {
    cat("✅ All module files present\n")
  } else {
    cat("❌ Some module files missing\n")
    return(FALSE)
  }
  
  # Check hmmTMB
  if (requireNamespace("hmmTMB", quietly = TRUE)) {
    cat("✅ hmmTMB package available\n")
  } else {
    cat("❌ hmmTMB package missing - CRITICAL\n")
    return(FALSE)
  }
  
  # Test quick model fit
  cat("✅ Testing quick model fit...\n")
  tryCatch({
    # Very simple test
    test_data <- data.frame(
      ID = factor(rep(1, 20)),
      x = rnorm(20)
    )
    
    hid <- MarkovChain$new(data = test_data, n_states = 2, initial_state = "stationary")
    obs <- Observation$new(
      data = test_data,
      dists = list(x = "norm"),
      par = list(x = list(mean = c(-1, 1), sd = c(1, 1))),
      n_states = 2
    )
    
    model <- HMM$new(obs = obs, hid = hid)
    model$fit()
    
    cat("✅ Model fitting works\n")
    
  }, error = function(e) {
    cat("⚠️  Model fitting issue - check during demo\n")
  })
  
  cat("\n🎉 Demo preparation complete!\n")
  cat("   Launch with: source('run_app.R')\n")
  
  return(TRUE)
}

# If running this script directly
if (interactive()) {
  cat("Choose an option:\n")
  cat("1. Full setup (run once)\n") 
  cat("2. Quick demo preparation (run before each demo)\n")
  cat("3. Just test the application\n")
  
  choice <- readline(prompt = "Enter choice (1-3): ")
  
  switch(choice,
    "1" = main_setup(),
    "2" = prepare_for_demo(), 
    "3" = test_application(),
    cat("Invalid choice. Run manually:\n- main_setup()\n- prepare_for_demo()\n- test_application()\n")
  )
} else {
  cat("Available functions:\n")
  cat("- main_setup(): Complete setup process\n")
  cat("- prepare_for_demo(): Quick pre-demo checks\n") 
  cat("- test_application(): Test HMM functionality\n")
}