# app.R
# Shiny front-end for the diagnostics workflow.
# Core stats stay in R/; this file handles inputs and display.

library(shiny)
library(shinydashboard)
library(gt)
library(ggplot2)

# Load helper scripts.
source("R/diag_lm_class.R")
source("R/simulate_data.R")

# Let users upload larger CSV files.
options(shiny.maxRequestSize = 30 * 1024^2)

# ---- User interface --------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Advanced Econometric Diagnostics Hub"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Setup & Specifications", tabName = "setup", icon = icon("sliders")),
      menuItem("2. Diagnostics", tabName = "diagnostics", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "setup",
        fluidRow(
          box(
            title = "Data Setup",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            radioButtons("data_source", NULL,
                         choices = c("Simulate data" = "simulate",
                                     "Upload a CSV" = "upload"),
                         selected = "simulate"),
            uiOutput("data_config_controls"),
            
            hr(),
            
            selectInput("plot_type", "Select Diagnostic Plot:",
                        choices = c("Residuals vs Fitted" = "residuals",
                                    "Normal Q-Q" = "qq",
                                    "Scale-Location" = "scale_location",
                                    "Residual Histogram" = "histogram",
                                    "Autocorrelation (ACF)" = "acf"))
          )
        )
      ),
      tabItem(
        tabName = "diagnostics",
        fluidRow(
          box(
            title = "Model Summary & Tests",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            gt_output("summary_table"),
            br(),
            textOutput("simulator_used_note"),
            br(),
            textOutput("time_series_note"),
            br(),
            htmlOutput("remediation")
          )
        ),
        fluidRow(
          box(
            title = "Diagnostic Plots",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("diag_plot")
          )
        )
      )
    )
  )
)

# ---- Server ----------------------------------------------------------------
server <- function(input, output, session) {
  output$data_config_controls <- renderUI({
    req(input$data_source)
    
    if (identical(input$data_source, "simulate")) {
      tagList(
        selectInput(
          "data_structure_sim",
          "Select Econometric Data Structure:",
          choices = c(
            "Cross-Sectional" = "cs",
            "Time Series" = "ts"
          ),
          selected = if (!is.null(input$data_structure_sim)) input$data_structure_sim else "cs"
        ),
        helpText("Simulate a dataset to test OLS assumptions."),
        sliderInput("n_obs", "Sample Size (n):", min = 50, max = 1000, value = 500),
        checkboxInput("het_viol", "Inject Heteroskedasticity", value = FALSE),
        checkboxInput("coll_viol", "Inject Multicollinearity", value = FALSE),
        numericInput("seed", "Random seed (leave blank for a new draw):", value = NULL),
        actionButton("sim_btn", "Simulate & Fit Model", class = "btn-primary", width = "100%")
      )
    } else {
      panel_selected <- identical(input$data_structure_upload, "panel")
      tagList(
        selectInput(
          "data_structure_upload",
          "Select Econometric Data Structure:",
          choices = c(
            "Cross-Sectional" = "cs",
            "Time Series" = "ts",
            "Panel Data" = "panel"
          ),
          selected = if (!is.null(input$data_structure_upload)) input$data_structure_upload else "cs"
        ),
        if (panel_selected) {
          tagList(
            textInput("p_idx_i", "Cross-Section Index:", value = ""),
            textInput("p_idx_t", "Time Index:", value = "")
          )
        },
        helpText("Upload a CSV, then choose the outcome and predictors."),
        fileInput("csv_file", "CSV file:", accept = c(".csv", "text/csv")),
        checkboxInput("header", "File has a header row", value = TRUE),
        uiOutput("var_selectors")
      )
    }
  })
  
  data_structure <- reactive({
    req(input$data_source)
    if (identical(input$data_source, "simulate")) {
      req(input$data_structure_sim)
      input$data_structure_sim
    } else {
      req(input$data_structure_upload)
      input$data_structure_upload
    }
  })
  
  # Simulated data: only refresh on button click.
  sim_data <- eventReactive(input$sim_btn, {
    # Empty seed means draw a new random sample.
    seed_val <- if (is.null(input$seed) || is.na(input$seed)) NULL else input$seed
    if (identical(data_structure(), "ts")) {
      get("simulate_ts_data", mode = "function")(n = input$n_obs,
                                                  heteroskedastic = input$het_viol,
                                                  multicollinear = input$coll_viol,
                                                  seed = seed_val)
    } else {
      get("simulate_ols_data", mode = "function")(n = input$n_obs,
                                                   heteroskedastic = input$het_viol,
                                                   multicollinear = input$coll_viol,
                                                   seed = seed_val)
    }
  }, ignoreNULL = FALSE)
  
  
  # Uploaded data with a friendly read error.
  uploaded_data <- reactive({
    req(input$csv_file)
    tryCatch(
      read.csv(input$csv_file$datapath, header = input$header),
      error = function(e) {
        validate(need(FALSE, paste("Could not read the file:", conditionMessage(e))))
      }
    )
  })
  
  # Build Y/X selectors from numeric columns.
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
  
  
  # Fit either lm or plm, then wrap in diag_lm.
  diag_model <- reactive({
    req(data_structure())
    
    # Build formula/data first, then choose lm vs plm.
    if (input$data_source == "simulate") {
      req(sim_data())
      df <- sim_data()
      model_formula <- Y ~ X1 + X2
      
    } else {
      df <- uploaded_data()
      req(input$y_var, input$x_vars)
      
      # Do not allow Y inside X.
      preds <- setdiff(input$x_vars, input$y_var)
      validate(need(length(preds) >= 1,
                    "Choose at least one predictor that is not the outcome."))
      
      model_formula <- as.formula(
        paste(input$y_var, "~", paste(preds, collapse = " + "))
      )
    }
    
    if (data_structure() == "panel") {
      validate(need(input$data_source == "upload",
                    "Panel Data requires an uploaded CSV with explicit panel index columns."))
      req(input$p_idx_i, input$p_idx_t)
      validate(need(nzchar(input$p_idx_i) && nzchar(input$p_idx_t),
                    "Provide both panel index column names."))
      validate(need(input$p_idx_i %in% names(df),
                    "Cross-Section Index column was not found in the uploaded data."))
      validate(need(input$p_idx_t %in% names(df),
                    "Time Index column was not found in the uploaded data."))
      
      fit <- plm::plm(
        formula = model_formula,
        data = df,
        model = "pooling",
        index = c(input$p_idx_i, input$p_idx_t)
      )
    } else {
      fit <- lm(model_formula, data = df)
    }
    
    get("new_diag_lm", mode = "function")(fit, data_type = data_structure())
  })
  
  
  # Outputs come from S3 summary/plot methods.
  output$summary_table <- render_gt({
    summary(diag_model())
  })

  output$simulator_used_note <- renderText({
    req(diag_model())

    if (identical(input$data_source, "simulate")) {
      if (identical(data_structure(), "ts")) {
        "Simulator used: Time-Series simulator (simulate_ts_data)."
      } else {
        "Simulator used: Cross-Section simulator (simulate_ols_data)."
      }
    } else {
      "Data source used: Uploaded CSV file."
    }
  })
  
  output$time_series_note <- renderText({
    req(data_structure())
    req(diag_model())
    
    if (data_structure() == "ts") {
      "Time Series Selected: Check serial-correlation diagnostics in the summary output."
    }
  })
  
  output$remediation <- renderUI({
    HTML(get("remediation_advice", mode = "function")(diag_model()))
  })
  
  output$diag_plot <- renderPlot({
    plot(diag_model(), type = input$plot_type)
  })
}

shinyApp(ui = ui, server = server)
