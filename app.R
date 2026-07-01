# app.R
# Shiny front-end for the linear-model diagnostics. The statistics live in
# the R/ scripts; this file only wires the UI controls to those functions
# and shows the results. Data can either be simulated or read from a CSV the
# user uploads.

library(shiny)
library(gt)
library(ggplot2)

# Load the simulator and the diag_lm class.
source("R/diag_lm_class.R")
source("R/simulate_data.R")

# Allow CSV uploads up to 30 MB (the default is 5 MB).
options(shiny.maxRequestSize = 30 * 1024^2)

# ---- User interface --------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Topic 4: Interactive Linear Model Diagnostics"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Data Source"),
      radioButtons("data_source", NULL,
                   choices = c("Simulate data"  = "simulate",
                               "Upload a CSV"    = "upload"),
                   selected = "simulate"),
      
      # Controls for the simulator. Only shown when "Simulate data" is picked.
      conditionalPanel(
        condition = "input.data_source == 'simulate'",
        helpText("Simulate a dataset to test OLS assumptions."),
        sliderInput("n_obs", "Sample Size (n):", min = 50, max = 1000, value = 500),
        checkboxInput("het_viol", "Inject Heteroskedasticity", value = FALSE),
        checkboxInput("coll_viol", "Inject Multicollinearity", value = FALSE),
        numericInput("seed", "Random seed (leave blank for a new draw):", value = NULL),
        # Nothing recomputes until this button is pressed (see eventReactive).
        actionButton("sim_btn", "Simulate & Fit Model", class = "btn-primary", width = "100%")
      ),
      
      # Controls for an uploaded CSV. The column pickers are built on the fly
      # once a file is read (see output$var_selectors in the server).
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        helpText("Upload a CSV, then choose the outcome and predictors."),
        fileInput("csv_file", "CSV file:", accept = c(".csv", "text/csv")),
        checkboxInput("header", "File has a header row", value = TRUE),
        uiOutput("var_selectors")
      ),
      
      hr(),
      
      h4("2. Plot Options"),
      selectInput("plot_type", "Select Diagnostic Plot:", 
                  choices = c("Residuals vs Fitted" = "residuals",
                              "Normal Q-Q"          = "qq",
                              "Scale-Location"      = "scale_location",
                              "Residual Histogram"  = "histogram"))
    ),
    
    mainPanel(
      # One tab for the table, one for the plot.
      tabsetPanel(
        tabPanel("Model Summary & Tests", 
                 br(),
                 gt_output("summary_table")),
        
        tabPanel("Diagnostic Plots", 
                 br(),
                 plotOutput("diag_plot"))
      )
    )
  )
)

# ---- Server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Simulated data -------------------------------------------------------
  # Only regenerate the data when the button is clicked, not on every slider
  # move. ignoreNULL = FALSE makes it also run once when the app starts.
  sim_data <- eventReactive(input$sim_btn, {
    # A blank seed box comes through as NULL/NA; use NULL for a fresh draw.
    seed_val <- if (is.null(input$seed) || is.na(input$seed)) NULL else input$seed
    simulate_ols_data(n = input$n_obs, 
                      heteroskedastic = input$het_viol, 
                      multicollinear = input$coll_viol,
                      seed = seed_val)
  }, ignoreNULL = FALSE)
  
  
  # --- Uploaded data --------------------------------------------------------
  # Read the CSV once a file is chosen. read.csv is wrapped so a malformed
  # file shows a friendly message instead of crashing the app.
  uploaded_data <- reactive({
    req(input$csv_file)
    tryCatch(
      read.csv(input$csv_file$datapath, header = input$header),
      error = function(e) {
        validate(need(FALSE, paste("Could not read the file:", conditionMessage(e))))
      }
    )
  })
  
  # Build the outcome / predictor pickers from the uploaded columns. Only the
  # numeric columns are offered, since the linear model needs numeric inputs.
  output$var_selectors <- renderUI({
    df <- uploaded_data()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    validate(need(length(numeric_cols) >= 2,
                  "The CSV needs at least two numeric columns."))
    
    tagList(
      selectInput("y_var", "Outcome (Y):", choices = numeric_cols),
      selectInput("x_vars", "Predictors (X):", choices = numeric_cols,
                  selected = numeric_cols[2], multiple = TRUE)
    )
  })
  
  
  # --- Fit the model --------------------------------------------------------
  # Picks the right data source, builds the model, and wraps it in a diag_lm
  # object. validate()/req() keep the UI calm while inputs are still missing.
  diag_model <- reactive({
    
    if (input$data_source == "simulate") {
      req(sim_data())
      fit <- lm(Y ~ X1 + X2, data = sim_data())
      
    } else {
      df <- uploaded_data()
      req(input$y_var, input$x_vars)
      
      # The outcome cannot also be a predictor.
      preds <- setdiff(input$x_vars, input$y_var)
      validate(need(length(preds) >= 1,
                    "Choose at least one predictor that is not the outcome."))
      
      model_formula <- as.formula(
        paste(input$y_var, "~", paste(preds, collapse = " + "))
      )
      fit <- lm(model_formula, data = df)
    }
    
    new_diag_lm(fit)
  })
  
  
  # --- Outputs --------------------------------------------------------------
  # The summary() and plot() calls below are our own S3 methods, so the
  # server stays short - all the work happens inside diag_lm_class.R.
  output$summary_table <- render_gt({
    summary(diag_model())
  })
  
  output$diag_plot <- renderPlot({
    plot(diag_model(), type = input$plot_type)
  })
}

shinyApp(ui = ui, server = server)
