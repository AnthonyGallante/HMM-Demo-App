# Helper Functions
# utils/helpers.R

# Load required libraries
library(shiny)

# Function to detect appropriate distribution for a variable
detect_distribution <- function(data, var_name) {
  var_data <- data[[var_name]]
  var_data <- var_data[!is.na(var_data)]
  
  # Check if all values are non-negative integers
  if (all(var_data >= 0) && all(var_data == round(var_data))) {
    return("pois")  # Poisson for count data
  }
  
  # Check if all values are between 0 and 1
  if (all(var_data >= 0) && all(var_data <= 1)) {
    return("beta")  # Beta for proportions
  }
  
  # Check if all values are non-negative
  if (all(var_data >= 0)) {
    return("gamma")  # Gamma for positive continuous data
  }
  
  # Default to normal for data that can be negative
  return("norm")
}

# Function to suggest initial parameters based on data and distribution
suggest_initial_parameters <- function(data, var_name, dist_type, n_states) {
  var_data <- data[[var_name]]
  var_data <- var_data[!is.na(var_data)]
  
  if (length(var_data) == 0) {
    # Fallback if no data
    switch(dist_type,
      "norm" = list(mean = rep(0, n_states), sd = rep(1, n_states)),
      "gamma" = list(shape = rep(1, n_states), scale = rep(1, n_states)),
      "pois" = list(lambda = rep(1, n_states)),
      "beta" = list(shape1 = rep(1, n_states), shape2 = rep(1, n_states))
    )
  } else {
    switch(dist_type,
      "norm" = {
        data_mean <- mean(var_data, na.rm = TRUE)
        data_sd <- sd(var_data, na.rm = TRUE)
        
        # Create different means for different states
        mean_range <- seq(data_mean - data_sd, data_mean + data_sd, length.out = n_states)
        
        list(
          mean = mean_range,
          sd = rep(data_sd / 2, n_states)  # Smaller SD for each state
        )
      },      
      "gamma" = {
        data_mean <- mean(var_data, na.rm = TRUE)
        data_var <- var(var_data, na.rm = TRUE)
        
        # Method of moments estimation
        shape <- data_mean^2 / data_var
        scale <- data_var / data_mean
        
        # Create different shapes for different states
        shape_range <- seq(shape * 0.5, shape * 1.5, length.out = n_states)
        
        list(
          shape = pmax(shape_range, 0.1),  # Ensure positive
          scale = rep(scale, n_states)
        )
      },
      
      "pois" = {
        data_mean <- mean(var_data, na.rm = TRUE)
        
        # Create different lambda values for different states
        lambda_range <- seq(data_mean * 0.5, data_mean * 1.5, length.out = n_states)
        
        list(
          lambda = pmax(lambda_range, 0.1)  # Ensure positive
        )
      },
      
      "beta" = {
        data_mean <- mean(var_data, na.rm = TRUE)
        data_var <- var(var_data, na.rm = TRUE)
        
        # Method of moments for beta distribution
        if (data_var > 0 && data_mean > 0 && data_mean < 1) {
          common_factor <- (data_mean * (1 - data_mean)) / data_var - 1
          shape1 <- data_mean * common_factor
          shape2 <- (1 - data_mean) * common_factor
          
          list(
            shape1 = rep(pmax(shape1, 0.1), n_states),
            shape2 = rep(pmax(shape2, 0.1), n_states)
          )
        } else {
          list(
            shape1 = rep(1, n_states),
            shape2 = rep(1, n_states)
          )
        }
      }
    )
  }
}
# Function to validate model configuration
validate_model_config <- function(data, observable_vars, distributions, parameters, n_states) {
  errors <- character(0)
  
  # Check data
  if (is.null(data) || nrow(data) == 0) {
    errors <- c(errors, "No data provided")
  }
  
  # Check observable variables
  if (is.null(observable_vars) || length(observable_vars) == 0) {
    errors <- c(errors, "No observable variables selected")
  }
  
  # Check if observable variables exist in data
  missing_vars <- observable_vars[!observable_vars %in% colnames(data)]
  if (length(missing_vars) > 0) {
    errors <- c(errors, paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
  }
  
  # Check distributions
  if (is.null(distributions) || length(distributions) != length(observable_vars)) {
    errors <- c(errors, "Distribution configuration incomplete")
  }
  
  # Check parameters
  if (is.null(parameters) || length(parameters) != length(observable_vars)) {
    errors <- c(errors, "Parameter configuration incomplete")
  }
  
  # Check number of states
  if (is.null(n_states) || n_states < 2 || n_states > 10) {
    errors <- c(errors, "Number of states must be between 2 and 10")
  }
  
  return(errors)
}

# Function to format model summary
format_model_summary <- function(model, data_name, n_states, observable_vars, covariate_vars = NULL) {
  if (is.null(model)) {
    return("No model available")
  }
  
  tryCatch({
    n_obs <- nrow(model$obs$data)
    n_individuals <- length(unique(model$obs$data$ID))    
    summary_text <- paste0(
      "Model Summary\n",
      "=============\n",
      "Dataset: ", data_name, "\n",
      "Observations: ", n_obs, "\n",
      "Individuals: ", n_individuals, "\n",
      "Hidden States: ", n_states, "\n",
      "Observable Variables: ", length(observable_vars), " (", paste(observable_vars, collapse = ", "), ")\n"
    )
    
    if (!is.null(covariate_vars) && length(covariate_vars) > 0) {
      summary_text <- paste0(summary_text, "Covariates: ", paste(covariate_vars, collapse = ", "), "\n")
    }
    
    # Add model metrics if available
    tryCatch({
      marginal_aic <- model$AIC_marginal()
      conditional_aic <- model$AIC_conditional()
      
      summary_text <- paste0(
        summary_text,
        "\nModel Metrics:\n",
        "Marginal AIC: ", round(marginal_aic, 2), "\n",
        "Conditional AIC: ", round(conditional_aic, 2), "\n"
      )
    }, error = function(e) {
      # AIC calculation failed, skip
    })
    
    return(summary_text)
    
  }, error = function(e) {
    return(paste("Error generating model summary:", e$message))
  })
}

# Function to check if required packages are available
check_required_packages <- function() {
  required_packages <- c("hmmTMB", "moveHMM", "mHMMbayes")
  missing_packages <- character(0)
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  if (length(missing_packages) > 0) {
    warning("Missing required packages: ", paste(missing_packages, collapse = ", "))
    return(FALSE)
  }
  
  return(TRUE)
}
# Function to safely load example datasets
load_example_dataset <- function(dataset_name) {
  tryCatch({
    switch(dataset_name,
      "haggis" = {
        if (requireNamespace("moveHMM", quietly = TRUE)) {
          return(moveHMM::haggis_data)
        } else {
          stop("moveHMM package not available")
        }
      },
      "airquality" = {
        return(datasets::airquality)
      },
      "nonverbal" = {
        if (requireNamespace("mHMMbayes", quietly = TRUE)) {
          data("nonverbal", package = "mHMMbayes", envir = environment())
          df <- as.data.frame(nonverbal)
          colnames(df)[1] <- "subject_id"
          return(df)
        } else {
          stop("mHMMbayes package not available")
        }
      },
      "muskox" = {
        if (file.exists("muskox_summer_2016.csv")) {
          return(read.csv("muskox_summer_2016.csv", stringsAsFactors = FALSE))
        } else {
          stop("Muskox data file not found")
        }
      },
      stop("Unknown dataset")
    )
  }, error = function(e) {
    stop(paste("Error loading dataset", dataset_name, ":", e$message))
  })
}

# Function to sanitize column names for UI IDs
sanitize_id <- function(text) {
  # Replace non-alphanumeric characters with underscores
  gsub("[^A-Za-z0-9]", "_", text)
}

# Function to generate informative error messages
format_error_message <- function(error, context = "operation") {
  error_msg <- as.character(error)
  
  # Common error patterns and user-friendly explanations
  if (grepl("optimization", error_msg, ignore.case = TRUE)) {
    return(paste(
      "The model optimization failed to converge. This might happen when:\n",
      "• The data doesn't fit the chosen distributions well\n",
      "• Initial parameter values are too far from optimal\n",
      "• The number of states is too high for the data complexity\n\n",
      "Try reducing the number of states or checking your data for outliers."
    ))
  }
  
  if (grepl("singular", error_msg, ignore.case = TRUE)) {
    return(paste(
      "The model encountered numerical issues (singular matrix). This suggests:\n",
      "• States might be too similar to distinguish\n",
      "• Some parameters might be redundant\n",
      "• The data might not support the chosen model complexity\n\n",
      "Try using fewer states or different initial parameters."
    ))
  }
  
  if (grepl("parameter|bound", error_msg, ignore.case = TRUE)) {
    return(paste(
      "There's an issue with the model parameters:\n",
      "• Some parameters might be outside valid ranges\n",
      "• Initial values might be inappropriate for your data\n\n",
      "Try using the auto-configuration feature or adjusting parameter values."
    ))
  }
  
  # Default error message
  return(paste(
    "An error occurred during", context, ":\n\n",
    error_msg, "\n\n",
    "Try simplifying your model or checking your data for issues."
  ))
}