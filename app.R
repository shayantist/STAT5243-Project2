# app.R
library(base)
library(shiny)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(jsonlite)
library(shinydashboard)
library(corrplot)

# Load built-in datasets
data(iris)
data(economics)
data(USArrests)
builtin_datasets <- list(
  "Iris Dataset" = iris,
  "US Economic Time Series by Month" = economics,
  "US Crime Statistics by State" = USArrests
)

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Project 2, Team 11: Interactive Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
      menuItem("Data Loading", tabName = "load", icon = icon("upload")),
      menuItem("Data Cleaning & Preprocessing", tabName = "clean", icon = icon("broom")),
      menuItem("Feature Engineering", tabName = "feature", icon = icon("tools")),
      menuItem("Exploratory Data Analysis (EDA)", tabName = "eda", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box {
          border-radius: 5px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
        .selectize-input {
          min-height: 34px;
        }
        .tooltip-inner {
          max-width: 300px;
          text-align: left;
        }
      "))
    ),
    tabItems(
      # User Instructions Tab
      tabItem(
        tabName = "instructions",
        fluidRow(
          box(
            width = 12,
            title = "STAT 5453 Project 2: Interactive Data Analysis Web Application",
            status = "primary",
            solidHeader = TRUE,
            p("Made by Team 11 for STAT 5453 at Columbia University for Spring 2025. This application allows you to interactively analyze and process your data through an intuitive interface. Follow the steps below to get started:"),
            tags$ol(
              tags$li(strong("Load Data:"), "Upload your dataset (multiple file types supported) or choose from built-in examples"),
              tags$li(strong("Data Cleaning:"), "Handle missing values, remove duplicates, and preprocess your data"),
              tags$li(strong("Feature Engineering:"), "Create new features, transform variables, and enhance your dataset"),
              tags$li(strong("Exploratory Analysis:"), "Visualize your data with interactive plots and statistical summaries")
            )
          )
        ),
        # fluidRow(
        #   box(
        #     width = 12,
        #     title = "Detailed Instructions",
        #     status = "info",
        #     solidHeader = TRUE,
        #     collapsible = TRUE,
            
        #     h4("Loading Data"),
        #     p("In the 'Load Data' tab, you can:"),
        #     tags$ul(
        #       tags$li("Upload your own data (csv file) OR select from built-in datasets to explore the app's functionality"),
        #       tags$li("Preview your data before proceeding to analysis")
        #     ),
            
        #     h4("Data Cleaning"),
        #     p("In the 'Data Cleaning' tab, you can:"),
        #     tags$ul(
        #       tags$li("Handle missing values through various imputation methods or removal"),
        #       tags$li("Remove duplicate entries from your dataset"),
        #       tags$li("Transform categorical variables into numerical format"),
        #       tags$li("Normalize or standardize numerical variables"),
        #       tags$li("Handle outliers through removal or transformation")
        #     ),
            
        #     h4("Feature Engineering"),
        #     p("In the 'Feature Engineering' tab, you can:"),
        #     tags$ul(
        #       tags$li("Create new features based on existing ones"),
        #       tags$li("Apply transformations to improve variable distributions"),
        #       tags$li("Create bins from continuous variables")
        #     ),
            
        #     h4("Exploratory Analysis"),
        #     p("In the 'Exploratory Analysis' tab, you can:"),
        #     tags$ul(
        #       tags$li("Generate summary statistics for your data"),
        #       tags$li("Create interactive visualizations (histograms, scatterplots, boxplots, etc.)"),
        #       tags$li("Examine correlations between variables"),
        #       tags$li("Filter and group your data for more targeted analysis")
        #     )
        #   )
        # )
      ),
      
      # Data Loading Tab
      tabItem(
        tabName = "load",
        fluidRow(
          box(
            width = 12,
            title = "Data Source Selection",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            radioButtons(
              "dataSource", 
              "Select Data Source:",
              choices = list("Upload File" = "upload", "Use Built-in Dataset" = "builtin"),
              selected = "builtin"
            ),
            
            conditionalPanel(
              condition = "input.dataSource == 'upload'",
              fileInput("file", "Choose File to Upload (.csv, .xlsx, .xls, .json, .rds):", accept = c(".csv", ".xlsx", ".xls", ".json", ".rds")),
              checkboxInput("header", "File has header", TRUE)
            ),
    
            conditionalPanel(
              condition = "input.dataSource == 'builtin'",
              selectInput("builtinDataset", "Select Dataset:", choices = names(builtin_datasets))
            ),
            
            actionButton("loadData", "Load Data")
          )
        ),

        conditionalPanel(
          condition = "output.dataLoaded",
          fluidRow(
            box(
              width = 12,
              title = "Data Preview",
              status = "primary",
              solidHeader = TRUE, 
              collapsible = TRUE,

              DTOutput("dataPreview")
            )
          )
        )
      ),
      
      # Data Cleaning Tab
      tabItem(
        tabName = "clean",
        fluidRow(
          box(
            width = 12,
            title = "Missing Value Summary (By Column)",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            DTOutput("missingValueTable"),
            
            h4("Handle Missing Values"),
            selectInput("missingColumns", "Select Columns:", choices = NULL, multiple = TRUE),
            selectInput("missingMethod", "Treatment Method:",
                        choices = list(
                          "Remove rows with missing values" = "remove",
                          "Fill with constant value" = "constant"
                        ),
                        selected = "remove"
            ),
            conditionalPanel(
              condition = "input.missingMethod == 'constant'",
              textInput("constantValue", "Enter Value:", "0")
            ),
            actionButton("applyMissing", "Apply")
          ),
        ),
        fluidRow(
          box(
            width = 6,
            title = "Duplicate Handling",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            verbatimTextOutput("duplicateSummary"),
            
            selectInput("duplicateColumns", "Check Duplicates Based On Column(s):", choices = NULL, multiple = TRUE),
            checkboxInput("keepFirst", "Keep First Occurrence", TRUE),
            actionButton("removeDuplicates", "Remove Duplicates")
          ),
          
          box(
            width = 6,
            title = "Variable Transformations",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput("transformColumn", "Select Column:", choices = NULL),
            selectInput("transformMethod", "Transformation Method:",
                        choices = list(
                          "Standardize (z-score)" = "standardize",
                          "Normalize (0-1)" = "normalize",
                          "Log transformation" = "log",
                          "One-hot encode (categorical)" = "onehot",
                          "Remove outliers" = "removeOutliers"
                        )
            ),
            conditionalPanel(
              condition = "input.transformMethod == 'removeOutliers'",
              numericInput("outlierThreshold", "Outlier Threshold (z-score):", 3, min = 1, max = 10)
            ),
            actionButton("applyTransform", "Apply Transformation")
          )
        ),
        
        conditionalPanel(
          condition = "output.dataLoaded",
          fluidRow(
            box(
              width = 12,
              title = "Cleaned Data Preview",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,

              DTOutput("cleanedDataPreview")
            )
          )
        )
      ),
      
      # Feature Engineering Tab
      tabItem(
        tabName = "feature",
        fluidRow(
          box(
            width = 6,
            title = "Create New Features",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            selectInput("featureType", "Feature Creation Method:",
                        choices = list(
                          "Mathematical Operation" = "math",
                          "Binning" = "bin",
                          "Date Processing" = "date"
                        )
            ),
            
            conditionalPanel(
              condition = "input.featureType == 'math'",
              textInput("newFeatureName", "New Feature Name:", "new_feature"),
              selectInput("mathCol1", "Column 1:", choices = NULL),
              selectInput("mathOperation", "Operation:",
                          choices = list(
                            "+ (Add Column 1 to Column 2)" = "add",
                            "- (Subtract Column 2 from Column 1)" = "subtract",
                            "* (Multiply Column 1 by Column 2)" = "multiply",
                            "/ (Divide Column 1 by Column 2)" = "divide"
                          )
              ),
              selectInput("mathCol2", "Column 2:", choices = NULL)
            ),
            
            conditionalPanel(
              condition = "input.featureType == 'bin'",
              selectInput("binColumn", "Column to Bin:", choices = NULL),
              textInput("binFeatureName", "New Feature Name:", "binned_feature"),
              numericInput("binCount", "Number of Bins:", 5, min = 2, max = 20),
              checkboxInput("binEqual", "Equal Width Bins", TRUE)
            ),
            
            conditionalPanel(
              condition = "input.featureType == 'date'",
              selectInput("dateColumn", "Date Column:", choices = NULL),
              selectInput("dateOperation", "Date Operation:",
                          choices = list(
                            "Extract Full Date" = "date",
                            "Extract Year Only" = "year",
                            "Extract Month Only" = "month",
                            "Extract Day Only" = "day"
                          )
              ),
              textInput("dateFeatureName", "New Feature Name:", "date_feature")
            ),
            
            actionButton("createFeature", "Create Feature")
          ),
          
          box(
            width = 6,
            title = "Feature Impact Visualization",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            plotlyOutput("featureImpactPlot", height = "300px"),
            selectInput("impactPlotType", "Plot Type:",
                        choices = list(
                          "Histogram" = "histogram",
                          "Density" = "density",
                          "Boxplot" = "boxplot"
                        ),
                        selected = "histogram"
            ),
            selectInput("impactFeature", "Select Feature:", choices = NULL)
          )
        ),
        
        conditionalPanel(
          condition = "output.dataLoaded",
          fluidRow(
            box(
              width = 12,
              title = "Feature Engineering Results",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              DTOutput("engineeredDataPreview"),
              downloadButton("downloadData", label = "Download Cleaned and/or Feature Engineered Data")
            )
          )
        )
      ),
      
      # EDA Tab
      tabItem(
        tabName = "eda",
        fluidRow(
          box(
            width = 12,
            title = "Data Summary",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            # verbatimTextOutput("dataSummary"),
            verbatimTextOutput("dataStructure")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Visualizations",
            status = "primary",
            solidHeader = TRUE,
            
            tabBox(
              width = 12,
              tabPanel(
              "Univariate Analysis",
              fluidRow(
                column(
                  width = 3,
                  selectInput("univarPlotType", "Plot Type:", 
                              choices = list(
                                "Histogram" = "histogram",
                                "Box Plot" = "boxplot",
                                "Density Plot" = "density",
                                "Bar Chart" = "bar"
                              )
                  ),
                  selectInput("univarColumn", "Select Column:", choices = NULL),
                  actionButton("generateUnivar", "Generate Plot")
                ),
                column(
                  width = 9,
                  plotlyOutput("univarPlot", height = "500px")
                )
              )
            ),
            tabPanel(
              "Bivariate Analysis",
              fluidRow(
                column(
                  width = 3,
                  selectInput("bivarPlotType", "Plot Type:", 
                              choices = list(
                                "Scatter Plot" = "scatter",
                                "Line Chart" = "line",
                                "Box Plot" = "boxplot",
                                "Bar Chart" = "bar",
                                "Heatmap" = "heatmap"
                              )
                  ),
                  selectInput("bivarXColumn", "X Column:", choices = NULL),
                  selectInput("bivarYColumn", "Y Column:", choices = NULL),
                  conditionalPanel(
                    condition = "input.bivarPlotType == 'scatter'",
                    selectInput("bivarColorBy", "Color By:", choices = NULL, selected = NULL)
                  ),
                  actionButton("generateBivar", "Generate Plot")
                ),
                column(
                  width = 9,
                  plotlyOutput("bivarPlot", height = "500px")
                )
              )
            ),
            tabPanel(
              "Correlation Analysis",
              fluidRow(
                column(
                  width = 3,
                  selectInput("corrColumns", "Select Columns:", choices = NULL, multiple = TRUE),
                  selectInput("corrMethod", "Correlation Method:", 
                              choices = list(
                                "Pearson" = "pearson",
                                "Spearman" = "spearman",
                                "Kendall" = "kendall"
                              )
                  ),
                  actionButton("generateCorr", "Generate Correlation")
                ),
                column(
                  width = 9,
                  plotOutput("corrPlot", height = "500px")
                )
              )
            )
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Filtered Data View",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            fluidRow(
              column(
                width = 3,
                selectInput("filterColumn", "Filter By:", choices = NULL),
                uiOutput("filterControls")
              ),
              column(
                width = 9,
                DTOutput("filteredDataView")
              )
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store data at different stages
  values <- reactiveValues(
    rawData = NULL, # original data
    cleanedData = NULL, # data after missing value handling
    engineeredData = NULL, # data after feature engineering
    currentData = NULL, # data after filtering
    columnTypes = NULL, # column types of the original data
    dataLoaded = FALSE # whether the data is loaded
  )
  
  table_options <- list(pageLength = 10, scrollX = TRUE)
  # Data loading function
  observeEvent(input$loadData, {
    if (input$dataSource == "builtin") {
      values$rawData <- builtin_datasets[[input$builtinDataset]]
      values$dataLoaded <- TRUE
      showNotification("Built-in dataset loaded successfully!", type = "message")
    } else if (input$dataSource == "upload" && !is.null(input$file)) {
      ext <- tools::file_ext(input$file$name)
      
      tryCatch({
        if (ext == "csv") {
          values$rawData <- read.csv(input$file$datapath, header = input$header, sep = ',')
        } else if (ext == "xlsx") {
          values$rawData <- read.xlsx(input$file$datapath, header = input$header)
        } else if (ext == "xls") {
          values$rawData <- read.xls(input$file$datapath, header = input$header)
        } else if (ext == "json") {
          values$rawData <- fromJSON(input$file$datapath)
        } else if (ext == "rds") {
          values$rawData <- readRDS(input$file$datapath)
        } else {
          showNotification("Unsupported file format", type = "error")
          return(NULL)
        }
        values$dataLoaded <- TRUE
        showNotification("File uploaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error loading file:", e$message), type = "error")
      })
    }
    
    # Initialize other datasets
    if (values$dataLoaded) {
      # Edge case: if dataset is the USArrests dataset, set the rownames to a new column called "State"
      if (input$builtinDataset == "US Crime Statistics by State") {
        values$rawData <- cbind(State = rownames(values$rawData), values$rawData)
        rownames(values$rawData) <- NULL
      }

      # Set all data to the raw data (for now)
      values$cleanedData <- values$rawData
      values$engineeredData <- values$rawData
      values$currentData <- values$rawData

      # Determine column types
      values$columnTypes <- sapply(values$rawData, class)
      
      # Update UI components that depend on column names
      components <- c(
        "missingColumns", "duplicateColumns", 
        "transformColumn", 
        "mathCol1", "mathCol2", 
        "binColumn", 
        "univarColumn", 
        "bivarXColumn", "bivarYColumn", "bivarColorBy", 
        "corrColumns", "filterColumn", 
        "impactFeature", "dateColumn"
      )
      for (component in components) {
        updateSelectInput(session, component, choices = names(values$rawData))
      }
    }
  })
  
  # Data Preview
  output$dataPreview <- renderDT({
    req(values$rawData)
    datatable(values$rawData, options = table_options)
  })
  
  # Download current data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("processed_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$currentData, file, row.names = FALSE)
    }
  )
  
  output$dataLoaded <- reactive({
    return(values$dataLoaded)
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  # Missing value summary per column
  output$missingValueTable <- renderDT({
    req(values$cleanedData)
    missing_values <- sapply(values$cleanedData, function(y) sum(length(which(is.na(y) | y == ""))))
    missing_values_table <- data.frame(missing_values)
    missing_values_table$Column <- rownames(missing_values_table)
    missing_values_table <- missing_values_table[, c(2, 1)]
    rownames(missing_values_table) <- NULL
    datatable(missing_values_table, options = table_options)
  })
  
  # Handle missing values
  observeEvent(input$applyMissing, {
    req(values$cleanedData, input$missingColumns)
    
    # Make a copy of the current data
    tempData <- values$cleanedData
    
    # Apply missing value treatment to selected columns
    for (col in input$missingColumns) {
      if (input$missingMethod == "remove") {
        # Remove rows with missing values in selected columns
        tempData <- tempData[!is.na(tempData[[col]]), ]
      } else if (input$missingMethod == "constant") {
        # Fill with constant value
        if (is.numeric(tempData[[col]])) { # if the column is numeric, fill with numeric constant value
          tempData[[col]][is.na(tempData[[col]])] <- as.numeric(input$constantValue)
        } else { # if the column is not numeric, fill with character constant value
          tempData[[col]][is.na(tempData[[col]])] <- input$constantValue
        }
      }
    }
    
    values$cleanedData <- tempData
    values$currentData <- tempData
    values$engineeredData <- tempData
    
    showNotification("Missing values handled successfully!", type = "message")
  })
  
  # Duplicate summary
  output$duplicateSummary <- renderPrint({
    req(values$cleanedData, input$duplicateColumns)
    if (length(input$duplicateColumns) > 0) {
      dup_count <- sum(duplicated(values$cleanedData[input$duplicateColumns]))
      cat("Columns selected for duplicate check: ", input$duplicateColumns, "\n")
      cat("Number of duplicate rows based on selected columns:", dup_count, "\n")
      cat("Total rows:", nrow(values$cleanedData), "\n")
      cat("Unique rows:", nrow(values$cleanedData) - dup_count, " (", round((nrow(values$cleanedData) - dup_count) / nrow(values$cleanedData) * 100, 2), "% of total)", "\n")
    } else {
      cat("Select columns to check for duplicates")
    }
  })
  
  # Remove duplicates
  observeEvent(input$removeDuplicates, {
    req(values$cleanedData, input$duplicateColumns)
    
    if (length(input$duplicateColumns) > 0) {
      if (input$keepFirst) { # keep first occurrence of duplicates
        values$cleanedData <- values$cleanedData[!duplicated(values$cleanedData[input$duplicateColumns]), ]
      } else { # remove all occurrences of duplicates
        dups <- duplicated(values$cleanedData[input$duplicateColumns]) | 
          duplicated(values$cleanedData[input$duplicateColumns], fromLast = TRUE)
        values$cleanedData <- values$cleanedData[!dups, ]
      }
      
      values$currentData <- values$cleanedData
      values$engineeredData <- values$cleanedData
      
      showNotification("Duplicates removed successfully!", type = "message")
    } else {
      showNotification("Please select columns to check for duplicates", type = "warning")
    }
  })
  
  # Apply transformations
  observeEvent(input$applyTransform, {
    req(values$cleanedData, input$transformColumn)
    
    # Make a copy of the current data
    tempData <- values$cleanedData
    col <- input$transformColumn
    
    if (input$transformMethod == "standardize" && is.numeric(tempData[[col]])) { # standardize numeric columns (subtract mean and divide by standard deviation)
      tempData[[col]] <- scale(tempData[[col]])
      showNotification(paste("Standardized column:", col), type = "message")
      
    } else if (input$transformMethod == "normalize" && is.numeric(tempData[[col]])) { # normalize numeric columns (subtract min and divide by range)
      min_val <- min(tempData[[col]], na.rm = TRUE)
      max_val <- max(tempData[[col]], na.rm = TRUE)
      tempData[[col]] <- (tempData[[col]] - min_val) / (max_val - min_val)
      showNotification(paste("Normalized column:", col), type = "message")
      
    } else if (input$transformMethod == "log" && is.numeric(tempData[[col]])) { # log transform numeric columns (take natural logarithm)
      # Check for non-positive values
      if (min(tempData[[col]], na.rm = TRUE) <= 0) {
        # Add a small constant to make all values positive
        tempData[[col]] <- log(tempData[[col]] - min(tempData[[col]], na.rm = TRUE) + 1)
      } else {
        tempData[[col]] <- log(tempData[[col]])
      }
      showNotification(paste("Applied log transformation to column:", col), type = "message")      
    } else if (input$transformMethod == "onehot" && (is.factor(tempData[[col]]) || is.character(tempData[[col]]))) { # one-hot encode categorical columns
      # One-hot encoding for categorical variables
      dummies <- model.matrix(~ tempData[[col]] - 1)
      colnames(dummies) <- paste0(col, "_", levels(factor(tempData[[col]])))
      tempData <- cbind(tempData, dummies)
      showNotification(paste("One-hot encoded column:", col), type = "message")
      
    } else if (input$transformMethod == "removeOutliers" && is.numeric(tempData[[col]])) { # remove outliers based on z-score
      # Remove outliers based on z-score
      z_scores <- abs(scale(tempData[[col]]))
      tempData <- tempData[z_scores <= input$outlierThreshold, ]
      showNotification(paste("Removed outliers from column:", col), type = "message")
      
    } else {
      showNotification("Transformation not applicable to selected column type", type = "warning")
      return()
    }
    
    values$cleanedData <- tempData
    values$currentData <- tempData
    values$engineeredData <- tempData
    
    # Update column lists
    components <- c("missingColumns", "duplicateColumns", "transformColumn", "mathCol1", "mathCol2", "binColumn")
    for (component in components) {
      updateSelectInput(session, component, choices = names(values$cleanedData))
    }
  })
  
  # Cleaned data preview
  output$cleanedDataPreview <- renderDT({
    req(values$cleanedData)
    datatable(values$cleanedData, options = table_options)
  })
  
  # Create new features
  observeEvent(input$createFeature, {
    req(values$engineeredData)
    
    # Make a copy of the current data
    tempData <- values$engineeredData
    
    if (input$featureType == "math") { # create new feature by performing mathematical operation on two existing features
      req(input$mathCol1, input$mathOperation, input$mathCol2)
      
      # Check if both columns are numeric
      if (!is.numeric(tempData[[input$mathCol1]]) || !is.numeric(tempData[[input$mathCol2]])) {
        showNotification("Both columns must be numeric for mathematical operations", type = "error")
        return()
      }
      
      # Apply the selected operation
      if (input$mathOperation == "add") {
        tempData[[input$newFeatureName]] <- tempData[[input$mathCol1]] + tempData[[input$mathCol2]]
      } else if (input$mathOperation == "subtract") {
        tempData[[input$newFeatureName]] <- tempData[[input$mathCol1]] - tempData[[input$mathCol2]]
      } else if (input$mathOperation == "multiply") {
        tempData[[input$newFeatureName]] <- tempData[[input$mathCol1]] * tempData[[input$mathCol2]]
      } else if (input$mathOperation == "divide") {
        # Check for division by zero
        if (any(tempData[[input$mathCol2]] == 0, na.rm = TRUE)) {
          tempData[[input$newFeatureName]] <- tempData[[input$mathCol1]] / ifelse(tempData[[input$mathCol2]] == 0, NA, tempData[[input$mathCol2]])
          showNotification("Warning: Division by zero replaced with NA", type = "warning")
        } else {
          tempData[[input$newFeatureName]] <- tempData[[input$mathCol1]] / tempData[[input$mathCol2]]
        }
      }
      
      showNotification(paste("Created new feature:", input$newFeatureName), type = "message")
      
    } else if (input$featureType == "bin") {
      req(input$binColumn, input$binCount)
      
      # Check if column is numeric
      if (!is.numeric(tempData[[input$binColumn]])) {
        showNotification("Column must be numeric for binning", type = "error")
        return()
      }
      
      # Create bins
      if (input$binEqual) {
        # Equal width bins
        breaks <- seq(min(tempData[[input$binColumn]], na.rm = TRUE),
                     max(tempData[[input$binColumn]], na.rm = TRUE),
                     length.out = input$binCount + 1)
        tempData[[input$binFeatureName]] <- cut(tempData[[input$binColumn]], 
                                              breaks = breaks,
                                              include.lowest = TRUE,
                                              dig.lab = 3)
      } else {
        # Equal frequency bins (quantiles)
        breaks <- quantile(tempData[[input$binColumn]], 
                         probs = seq(0, 1, length.out = input$binCount + 1), 
                         na.rm = TRUE)
        tempData[[input$binFeatureName]] <- cut(tempData[[input$binColumn]], 
                                              breaks = breaks,
                                              include.lowest = TRUE,
                                              dig.lab = 3)
      }
      
      showNotification(paste("Created binned feature:", input$binFeatureName), type = "message")
    } else if (input$featureType == "date") { 
      # create new feature by extracting date components from existing date column
      req(input$dateColumn, input$dateOperation)
      
      # Try to convert to Date if not already
      if (!inherits(tempData[[input$dateColumn]], "Date")) {
        tryCatch({
          datCol <- as.Date(tempData[[input$dateColumn]])
          if (all(is.na(datCol))) {
            showNotification("Column couldn't be converted to Date format", type = "error")
            return()
          }
          tempData[[input$dateColumn]] <- datCol
        }, error = function(e) {
          showNotification("Error converting to Date format", type = "error")
          return()
        })
      }
      
      # Apply date operation
      if (input$dateOperation == "date") {
        tempData[[input$dateFeatureName]] <- tempData[[input$dateColumn]]
      } else if (input$dateOperation == "year") {
        tempData[[input$dateFeatureName]] <- as.numeric(format(tempData[[input$dateColumn]], "%Y"))
      } else if (input$dateOperation == "month") {
        tempData[[input$dateFeatureName]] <- as.numeric(format(tempData[[input$dateColumn]], "%m"))
      } else if (input$dateOperation == "day") {
        tempData[[input$dateFeatureName]] <- as.numeric(format(tempData[[input$dateColumn]], "%d"))
      }
      
      showNotification(paste("Created date feature:", input$dateFeatureName), type = "message")
    }
    
    values$engineeredData <- tempData
    values$currentData <- tempData
    
    # Update column lists
    components <- c("univarColumn", "bivarXColumn", "bivarYColumn", "bivarColorBy", "corrColumns", "filterColumn", "impactFeature")
    for (component in components) {
      updateSelectInput(session, component, choices = names(tempData))
    }
  })
  
  # Feature impact visualization
  output$featureImpactPlot <- renderPlotly({
    req(values$engineeredData, input$impactFeature)
    
    # Check if selected feature exists
    if (!input$impactFeature %in% names(values$engineeredData)) {
      return(NULL)
    }
    
    feature_data <- values$engineeredData[[input$impactFeature]]
    
    if (is.numeric(feature_data)) {
      if (input$impactPlotType == "histogram") {
        p <- plot_ly(x = feature_data, type = "histogram", 
                     marker = list(color = "#75AADB", line = list(color = "white", width = 0.4)))
        p <- p %>% layout(title = paste("Histogram of", input$impactFeature),
                          xaxis = list(title = input$impactFeature),
                          yaxis = list(title = "Frequency"))
      } else if (input$impactPlotType == "density") {
        dens <- density(feature_data, na.rm = TRUE)
        p <- plot_ly(x = dens$x, y = dens$y, type = "scatter", mode = "lines", 
                     fill = "tozeroy", line = list(color = "#75AADB"))
        p <- p %>% layout(title = paste("Density Plot of", input$impactFeature),
                          xaxis = list(title = input$impactFeature),
                          yaxis = list(title = "Density"))
      } else if (input$impactPlotType == "boxplot") {
        p <- plot_ly(y = feature_data, type = "box", boxmean = TRUE,
                     marker = list(color = "#75AADB"))
        p <- p %>% layout(title = paste("Boxplot of", input$impactFeature),
                          yaxis = list(title = input$impactFeature))
      }
    } else if (is.factor(feature_data) || is.character(feature_data)) {
      # For categorical data, always use bar chart
      if (!is.factor(feature_data)) {
        feature_data <- as.factor(feature_data)
      }
      
      counts <- table(feature_data)
      p <- plot_ly(x = names(counts), y = as.numeric(counts), type = "bar",
                   marker = list(color = "#75AADB"))
      p <- p %>% layout(title = paste("Bar Chart of", input$impactFeature),
                        xaxis = list(title = input$impactFeature),
                        yaxis = list(title = "Count"))
    } else {
      return(NULL)
    }
    
    return(p)
  })
  
  # Engineered data preview
  output$engineeredDataPreview <- renderDT({
    req(values$engineeredData)
    datatable(values$engineeredData, options = table_options)
  })
  
  # Data summary and structure for EDA
  output$dataSummary <- renderPrint({
    req(values$currentData)
    summary(values$currentData)
  })
  output$dataStructure <- renderPrint({
    req(values$currentData)
    str(values$currentData)
  })
  
  # Univariate analysis
  observeEvent(input$generateUnivar, {
    req(values$currentData, input$univarColumn)
    
    # Check if column exists
    if (!input$univarColumn %in% names(values$currentData)) {
      showNotification("Selected column not found", type = "error")
      return()
    }
  })
  
  # TODO: combine with feature impact plot (somewhat duplicate code)
  output$univarPlot <- renderPlotly({
    req(values$currentData, input$univarColumn, input$generateUnivar)
    
    # Create base ggplot
    col_data <- values$currentData[[input$univarColumn]]
    
    if (is.numeric(col_data)) {
      if (input$univarPlotType == "histogram") {
        p <- ggplot(values$currentData, aes_string(x = input$univarColumn)) +
          geom_histogram(fill = "#75AADB", color = "white", bins = 30) +
          theme_minimal() +
          labs(title = paste("Histogram of", input$univarColumn))
        
      } else if (input$univarPlotType == "boxplot") {
        p <- ggplot(values$currentData, aes_string(y = input$univarColumn)) +
          geom_boxplot(fill = "#75AADB") +
          theme_minimal() +
          labs(title = paste("Boxplot of", input$univarColumn))
        
      } else if (input$univarPlotType == "density") {
        p <- ggplot(values$currentData, aes_string(x = input$univarColumn)) +
          geom_density(fill = "#75AADB", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Density Plot of", input$univarColumn))
      }
    } else {
      # For categorical variables, use bar chart
      p <- ggplot(values$currentData, aes_string(x = input$univarColumn)) +
        geom_bar(fill = "#75AADB") +
        theme_minimal() +
        labs(title = paste("Bar Chart of", input$univarColumn)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    # Convert to plotly
    ggplotly(p)
  })
  
  # Bivariate analysis
  observeEvent(input$generateBivar, {
    req(values$currentData, input$bivarXColumn, input$bivarYColumn)
    
    # Check if columns exist
    if (!input$bivarXColumn %in% names(values$currentData) || 
        !input$bivarYColumn %in% names(values$currentData)) {
      showNotification("Selected column(s) not found", type = "error")
      return()
    }
    
    # If color by is selected, check that too
    if (input$bivarColorBy != "None" && !input$bivarColorBy %in% names(values$currentData)) {
      showNotification("Selected color column not found", type = "error")
      return()
    }
  })
  
  output$bivarPlot <- renderPlotly({
    req(values$currentData, input$bivarXColumn, input$bivarYColumn, input$generateBivar)
    
    # Base ggplot aesthetics
    if (input$bivarColorBy != "None") {
      aes_mapping <- aes_string(x = input$bivarXColumn, 
                                y = input$bivarYColumn, 
                                color = input$bivarColorBy)
    } else {
      aes_mapping <- aes_string(x = input$bivarXColumn, 
                                y = input$bivarYColumn)
    }
    
    # Create appropriate plot based on variable types and selected plot type
    x_is_numeric <- is.numeric(values$currentData[[input$bivarXColumn]])
    y_is_numeric <- is.numeric(values$currentData[[input$bivarYColumn]])
    
    if (input$bivarPlotType == "scatter" && x_is_numeric && y_is_numeric) {
      p <- ggplot(values$currentData, aes_mapping) +
        geom_point(alpha = 0.7) +
        theme_minimal() +
        labs(title = paste(input$bivarYColumn, "vs", input$bivarXColumn))
      
    } else if (input$bivarPlotType == "line" && x_is_numeric && y_is_numeric) {
      p <- ggplot(values$currentData, aes_mapping) +
        geom_line() +
        theme_minimal() +
        labs(title = paste(input$bivarYColumn, "vs", input$bivarXColumn))
      
    } else if (input$bivarPlotType == "boxplot") {
      if (!x_is_numeric && y_is_numeric) {
        # Categorical X, numeric Y
        p <- ggplot(values$currentData, aes_mapping) +
          geom_boxplot() +
          theme_minimal() +
          labs(title = paste(input$bivarYColumn, "by", input$bivarXColumn)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else if (x_is_numeric && !y_is_numeric) {
        # Numeric X, categorical Y - switch axes
        if (input$bivarColorBy != "None") {
          new_aes <- aes_string(x = input$bivarYColumn, 
                                y = input$bivarXColumn, 
                                color = input$bivarColorBy)
        } else {
          new_aes <- aes_string(x = input$bivarYColumn, 
                                y = input$bivarXColumn)
        }
        
        p <- ggplot(values$currentData, new_aes) +
          geom_boxplot() +
          theme_minimal() +
          labs(title = paste(input$bivarXColumn, "by", input$bivarYColumn)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        showNotification("Boxplot requires one categorical and one numeric variable", type = "warning")
        return(NULL)
      }
    } else if (input$bivarPlotType == "bar") {
      if (!x_is_numeric && y_is_numeric) {
        # Categorical X, numeric Y - use stat_summary for mean
        p <- ggplot(values$currentData, aes_mapping) +
          stat_summary(fun = "mean", geom = "bar") +
          theme_minimal() +
          labs(title = paste("Mean", input$bivarYColumn, "by", input$bivarXColumn)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else if (!x_is_numeric && !y_is_numeric) {
        # Both categorical - use count
        p <- ggplot(values$currentData, aes_mapping) +
          geom_count() +
          theme_minimal() +
          labs(title = paste("Count of", input$bivarYColumn, "by", input$bivarXColumn)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        showNotification("Bar chart requires X to be categorical", type = "warning")
        return(NULL)
      }
    } else if (input$bivarPlotType == "heatmap" && x_is_numeric && y_is_numeric) {
      # Create a 2D bin heatmap for two numeric variables
      p <- ggplot(values$currentData, aes_string(x = input$bivarXColumn, y = input$bivarYColumn)) +
        geom_bin2d(bins = 30) +
        scale_fill_viridis_c() +
        theme_minimal() +
        labs(title = paste("Heatmap of", input$bivarYColumn, "vs", input$bivarXColumn))
    } else {
      showNotification("The selected plot type is not appropriate for the variable types", type = "warning")
      return(NULL)
    }
    
    # Convert to plotly
    ggplotly(p)
  })
  
  # Correlation analysis
  observeEvent(input$generateCorr, {
    req(values$currentData, input$corrColumns)
    
    # Check if we have at least 2 numeric columns
    numeric_cols <- names(which(sapply(values$currentData[input$corrColumns], is.numeric)))
    if (length(numeric_cols) < 2) {
      showNotification("Need at least 2 numeric columns for correlation analysis", type = "error")
      return()
    }
  })
  
  output$corrPlot <- renderPlot({
    req(values$currentData, input$corrColumns, input$generateCorr)
    
    # Filter numeric columns
    numeric_cols <- names(which(sapply(values$currentData[input$corrColumns], is.numeric)))
    
    if (length(numeric_cols) >= 2) {
      # Calculate correlation matrix
      corr_matrix <- cor(values$currentData[numeric_cols], 
                         method = input$corrMethod, 
                         use = "pairwise.complete.obs")
      
      # Plot correlation matrix
      corrplot(corr_matrix, 
               method = "color", 
               type = "upper", 
               order = "hclust", 
               tl.col = "black", 
               tl.srt = 45, 
               diag = FALSE, 
               addCoef.col = "black",
               number.cex = 0.7)
    } else {
      plot(NULL, xlim=c(0,1), ylim=c(0,1),  
           xlab="", ylab="", 
           axes=FALSE)
      text(0.5, 0.5, "Not enough numeric columns selected", cex=1.5)
    }
  })
  
  # Filtered data view
  output$filterControls <- renderUI({
    req(values$currentData, input$filterColumn)
    
    col_data <- values$currentData[[input$filterColumn]]
    
    if (is.numeric(col_data)) {
      # For numeric columns, provide min-max range slider
      min_val <- min(col_data, na.rm = TRUE)
      max_val <- max(col_data, na.rm = TRUE)
      
      sliderInput("numericFilter", 
                  label = paste("Filter", input$filterColumn),
                  min = min_val, 
                  max = max_val, 
                  value = c(min_val, max_val),
                  step = (max_val - min_val) / 100)
    } else {
      # For categorical columns, provide checkboxes
      unique_vals <- unique(col_data)
      checkboxGroupInput("categoricalFilter",
                         label = paste("Filter", input$filterColumn),
                         choices = unique_vals,
                         selected = unique_vals)
    }
  })
  
  output$filteredDataView <- renderDT({
    req(values$currentData, input$filterColumn)
    
    # Apply filters
    filtered_data <- values$currentData
    
    col_data <- values$currentData[[input$filterColumn]]
    
    if (is.numeric(col_data) && !is.null(input$numericFilter)) {
      # Apply numeric filter
      filtered_data <- filtered_data[filtered_data[[input$filterColumn]] >= input$numericFilter[1] &
                                       filtered_data[[input$filterColumn]] <= input$numericFilter[2], ]
    } else if (!is.null(input$categoricalFilter)) {
      # Apply categorical filter
      filtered_data <- filtered_data[filtered_data[[input$filterColumn]] %in% input$categoricalFilter, ]
    }
    
    # Return filtered data
    datatable(filtered_data, options = table_options)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

