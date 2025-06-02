# Hidden Markov Model Demo Application

A user-friendly Shiny application for building and exploring Hidden Markov Models using the `hmmTMB` package. Designed for presentations and demonstrations to audiences with no statistical modeling background.

## 🎯 Features

- **Intuitive Step-by-Step Workflow**: Guided process from data loading to results visualization
- **Multiple Data Sources**: Support for example datasets and CSV file uploads
- **Smart Auto-Configuration**: Automatic distribution detection and parameter initialization
- **Rich Visualizations**: Distribution plots, time series plots, covariate effects, and diagnostic plots
- **Plain Language Interface**: No statistical jargon - designed for general audiences
- **Real-time Progress Tracking**: Visual indicators for each step of the modeling process

## 📋 Requirements

### Required R Packages
```r
install.packages(c(
  "shiny",
  "shinydashboard", 
  "shinyWidgets",
  "shinycssloaders",
  "DT",
  "plotly",
  "hmmTMB"
))
```

### Optional Packages (for example datasets)
```r
install.packages(c(
  "moveHMM",      # For haggis movement data
  "mHMMbayes"     # For nonverbal behavior data
))
```

## 📁 File Structure

Organize your files as follows:

```
your-project-folder/
├── app.R                           # Main application file
├── run_app.R                       # Application launcher
├── demo.R                          # Quick demo script (auto-generated)
├── muskox_summer_2016.csv          # Example dataset (optional)
├── modules/
│   ├── data_module.R               # Data loading and configuration
│   ├── model_config_module.R       # Model setup and parameter configuration
│   ├── fitting_module.R            # Model fitting and basic results
│   └── visualization_module.R      # Advanced plotting and exploration
└── utils/
    └── helpers.R                   # Utility functions
```
## 🚀 Quick Start

### Method 1: Using the Launcher (Recommended)
```r
# Set your working directory to the project folder
setwd("path/to/your/hmm-demo-app")

# Run the launcher
source("run_app.R")
```

### Method 2: Direct Launch
```r
# Ensure all required packages are installed
# Then simply run:
shiny::runApp(".")
```

### Method 3: Using the Demo Script
```r
source("demo.R")
```

## 📊 Available Datasets

The application comes with several example datasets:

1. **Haggis Movement Data** (`moveHMM` package)
   - Animal movement tracking data
   - Variables: x, y coordinates, step, angle
   - Good for demonstrating basic 2-3 state models

2. **Muskox Movement Data** (CSV file)
   - Large movement dataset with environmental covariates
   - Variables: step length, turning angle, environmental factors
   - Good for complex models with multiple covariates

3. **Fitbit Heart Rate Data** (CSV file)
   - Physiological time series with daily patterns
   - Variables: heart rate values, time of day
   - Good for demonstrating daily rhythms and advanced time-varying effects

## 🎯 Demo Workflow

### Step 1: Data Loading
- Choose from example datasets or upload your own CSV
- Configure ID column and select variables to analyze
- Preview your data and get summary statistics

### Step 2: Model Configuration  
- Select number of hidden states (2-5 recommended)
- Auto-configure distributions and parameters
- Fine-tune parameters if needed

### Step 3: Model Fitting
- Review model summary
- Fit the model (30 seconds to a few minutes)
- View basic performance metrics

### Step 4: Results Exploration
- Generate distribution plots to see state differences
- Create time series plots to visualize state sequences
- Explore covariate effects (if applicable)
- Perform model diagnostics
## 🎨 Key Visualizations

### Distribution Plots
- Show how each variable differs across hidden states
- Help interpret what each state represents
- Critical for understanding model results

### Time Series Plots
- Display data over time colored by predicted states
- Show when and how often state switches occur
- Can plot 1 or 2 variables simultaneously

### Covariate Effect Plots
- **State Probabilities**: How covariates affect which state is most likely
- **Transition Probabilities**: What triggers switches between states
- Only available when covariates are included

### Diagnostic Plots
- **QQ Plots**: Check if residuals follow expected distributions
- **Autocorrelation Plots**: Verify model captures temporal dependencies
- Essential for validating model assumptions

## 🛠️ Troubleshooting

### Common Issues and Solutions

#### "Package not found" errors
```r
# Install missing packages
install.packages("package_name")

# For hmmTMB specifically:
install.packages("hmmTMB")
```

#### "Example dataset not available"
- Some datasets require optional packages (`moveHMM`, `mHMMbayes`)
- Use built-in datasets or upload your own CSV instead

#### "Model fitting failed"
- Try reducing the number of states
- Check for missing values in your data
- Use auto-configuration instead of manual parameters
- Ensure your data has sufficient variation

#### "File not found" errors
- Verify all files are in the correct directory structure
- Run `verify_app_structure()` from `run_app.R` to check

#### App doesn't launch
```r
# Check if required packages are installed
source("run_app.R")
check_and_install_packages()

# Verify file structure
verify_app_structure()

# Try launching manually
shiny::runApp(".", port = 3838)
```

### Performance Tips

- **Large datasets**: Start with fewer states and simpler models
- **Slow fitting**: Use built-in datasets for faster demos
- **Memory issues**: Close other R sessions and restart if needed
## 📝 Preparing Your Demo

### Before Your Presentation

1. **Test Run**: Launch the app and walk through the entire workflow
2. **Prepare Data**: Have your example datasets ready and tested
3. **Plan Examples**: Decide which models you'll demonstrate
4. **Check Performance**: Ensure models fit quickly enough for live demo

### Suggested Demo Flow

1. **Start Simple**: Use haggis data with 2 states, no covariates
2. **Show Auto-Config**: Let the audience see the automatic setup
3. **Visualize Results**: Focus on distribution and time series plots
4. **Add Complexity**: Demonstrate covariates with air quality data
5. **Interactive Exploration**: Let audience suggest parameters to try

### Tips for Success

- **Keep it Visual**: Focus on plots rather than technical details
- **Tell a Story**: Explain what the hidden states might represent
- **Handle Errors Gracefully**: Have backup plans if models fail
- **Engage the Audience**: Ask them to interpret the plots

## 🔧 Customization

### Adding New Datasets
1. Place CSV files in the main directory
2. Add dataset option in `data_module.R`
3. Update the `load_example_dataset()` function in `helpers.R`

### Modifying the Interface
- Edit module UI functions to change layouts
- Modify CSS in the main `app.R` file for styling
- Add new plot types in `visualization_module.R`

### Adding Distributions
- Extend the auto-detection logic in `helpers.R`
- Add new distribution options in `model_config_module.R`
- Update parameter input generators accordingly

## 📚 References

- [hmmTMB Package Documentation](https://cran.r-project.org/package=hmmTMB)
- [Shiny Framework](https://shiny.rstudio.com/)
- [Hidden Markov Models Overview](https://en.wikipedia.org/wiki/Hidden_Markov_model)

## 🤝 Support

For issues specific to this demo application:
1. Check the troubleshooting section above
2. Verify all files are present and packages installed
3. Test with the provided example datasets first

For hmmTMB-specific questions, refer to the package documentation and examples.

---

**Good luck with your presentation! 🎉**