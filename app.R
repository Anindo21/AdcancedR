# app.R
# Shiny front-end for the diagnostics workflow with a custom color palette.
# Load Packages

library(shiny)
library(shinydashboard)
library(gt)
library(ggplot2)

# Load helper scripts.
source("R/diag_lm_class.R")
source("R/simulate_data.R")
source("R/llm_interpret.R")

# Let users upload larger CSV files.
options(shiny.maxRequestSize = 30 * 1024^2)

# ---- Pre-defined Path for System Data ---------------------------------------
AVAILABLE_DATA_PATH <- "data/wage.csv"

# Crash-proof dynamic date parsing helper
parse_date_dynamically <- function(x) {
  x_clean <- sub("\\.0+$", "", trimws(as.character(x)))
  
  if (!any(is.na(suppressWarnings(as.numeric(x_clean))))) {
    num_vals <- as.numeric(x_clean)
    try_date <- tryCatch(as.Date(num_vals, origin = "1970-01-01"), error = function(e) NULL)
    if (!is.null(try_date)) return(try_date)
  }
  
  parsed <- as.Date(x_clean, format = "%Y-%m-%d")
  if (all(is.na(parsed))) parsed <- as.Date(x_clean, format = "%Y/%m/%d")
  if (all(is.na(parsed))) parsed <- as.Date(x_clean, format = "%d-%m-%Y")
  if (all(is.na(parsed))) parsed <- as.Date(x_clean, format = "%d.%m.%Y")
  if (all(is.na(parsed))) parsed <- as.Date(x_clean, format = "%d%m%y")
  if (all(is.na(parsed))) parsed <- as.Date(x_clean, format = "%d%m%Y")
  
  if (all(is.na(parsed))) {
    return(x)
  }
  return(parsed)
}

# ---- User interface --------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "RegressionDiagnostics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Setup & Specifications", tabName = "setup", icon = icon("sliders")),
      menuItem("2. Diagnostics", tabName = "diagnostics", icon = icon("chart-line")),
      menuItem("3. Transformation", tabName = "transformation", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* --- CUSTOM PALETTE SKINNING OVERRIDES --- */
        
        /* 1. Main Header/Navbar background (#3d5a80) */
        .main-header .navbar, .main-header .logo {
          background-color: #3d5a80 !important;
          color: #ffffff !important;
        }
        .main-header .navbar .sidebar-toggle:hover {
          background-color: #293241 !important;
        }
        
        /* 2. Sidebar background (#293241) and Active Menu (#3d5a80 with #98c1d9 accent) */
        .main-sidebar {
          background-color: #293241 !important;
        }
        .sidebar-menu > li.active > a, .sidebar-menu > li:hover > a {
          background-color: #3d5a80 !important;
          border-left-color: #98c1d9 !important;
          color: #ffffff !important;
        }
        .sidebar-menu > li > a {
          color: #98c1d9 !important;
        }
        
        /* 3. Box Headers Overrides (Primary = #3d5a80, Info = #293241, Warning = #ee6c4d) */
        .box.box-solid.box-primary > .box-header {
          background-color: #3d5a80 !important;
          color: #ffffff !important;
        }
        .box.box-solid.box-info > .box-header {
          background-color: #293241 !important;
          color: #ffffff !important;
        }
        .box.box-solid.box-warning > .box-header {
          background-color: #ee6c4d !important;
          color: #ffffff !important;
        }
        .box {
          border-top-color: #98c1d9 !important;
          box-shadow: 0 2px 5px rgba(0,0,0,0.05);
        }
        
        /* 4. Custom Button Theme System Matching CSS Specs */
        .btn-custom-primary {
          background-color: #3d5a80 !important;
          color: white !important;
          border: none;
        }
        .btn-custom-primary:hover { background-color: #293241 !important; }
        
        .btn-custom-success {
          background-color: #3d5a80 !important;
          color: white !important;
          border: none;
          font-weight: bold;
        }
        .btn-custom-success:hover { background-color: #293241 !important; }
        
        .btn-custom-warning {
          background-color: #ee6c4d !important;
          color: white !important;
          border: none;
        }
        .btn-custom-warning:hover { background-color: #ee6c4d !important; }
        
        .btn-custom-danger {
          background-color: #293241 !important;
          color: #98c1d9 !important;
          border: 1px solid #98c1d9;
        }
        .btn-custom-danger:hover { background-color: #ee6c4d !important; color: white !important; }
        
        /* Slider component active track accent override */
        .irs-bar, .irs-bar-edge, .irs-single {
          background: #3d5a80 !important;
        }
        
        /* --- ORIGINAL SCROLLING & STRUCTURE CLASSES --- */
        .scrollable-table {
          max-height: 400px;
          overflow-y: auto;
          overflow-x: auto;
          border: 1px solid #ddd;
          background-color: white;
        }
        .scrollable-table table {
          width: 100%;
          border-collapse: collapse;
        }
        .scrollable-table th {
          position: sticky;
          top: 0;
          background-color: #f5f5f5;
          z-index: 10;
          box-shadow: inset 0 -2px 0 #ccc;
        }
        .codebook-text {
          white-space: pre-wrap;
          font-family: monospace;
          font-size: 12px;
          line-height: 1.4;
        }
        .formula-preview-box {
          background-color: #f0f4f7;
          border: 1px solid #d3dfe4;
          border-radius: 4px;
          padding: 15px;
          margin-bottom: 20px;
          font-family: 'Courier New', Courier, monospace;
          color: #555;
          font-size: 14px;
          line-height: 1.6;
        }
        .orange-heading {
          color: #ee6c4d;
          font-weight: bold;
          margin-bottom: 8px;
        }
        /* AI Interpretation panel: render the LLM's Markdown-derived HTML clearly */
        .ai-interpretation {
          background-color: #f8fafb;
          border: 1px solid #d3dfe4;
          border-left: 4px solid #3d5a80;
          border-radius: 4px;
          padding: 15px 18px;
          line-height: 1.5;
        }
        .ai-interpretation h2, .ai-interpretation h3 {
          color: #293241;
          font-size: 16px;
          margin: 12px 0 6px 0;
          border-bottom: 1px solid #98c1d9;
          padding-bottom: 4px;
        }
        .ai-interpretation h2:first-child, .ai-interpretation h3:first-child {
          margin-top: 0;
        }
        .ai-interpretation ul, .ai-interpretation ol {
          padding-left: 20px;
          margin-bottom: 10px;
        }
        .ai-interpretation li { margin-bottom: 4px; }
        .ai-interpretation strong { color: #ee6c4d; }
        .ai-interpretation table {
          width: 100%;
          border-collapse: collapse;
          margin-bottom: 10px;
        }
        .ai-interpretation th, .ai-interpretation td {
          border: 1px solid #d3dfe4;
          padding: 6px 8px;
          text-align: left;
        }
        .ai-interpretation th { background-color: #eef3f6; }
      "))
    ),
    tabItems(
      # --- TAB 1: DATA SETUP ---
      tabItem(
        tabName = "setup",
        fluidRow(
          box(
            title = "Data Setup",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            radioButtons("model_family", "Model Family:",
                         choices = c("Linear (OLS)" = "linear",
                                     "Logistic (Binomial)" = "logistic"),
                         selected = "linear", inline = TRUE),
            radioButtons("data_source", NULL,
                         choices = c("Simulate data" = "simulate",
                                     "Choose available data" = "available",
                                     "Upload a CSV" = "upload"),
                         selected = "simulate"),
            uiOutput("data_config_controls"),
            
            hr(),
            
            h4(tags$strong("Modify Variable Types")),
            uiOutput("var_to_edit_ui"),
            selectInput("new_data_type", "New data type:",
                        choices = c("Factor" = "factor",
                                    "Numeric" = "numeric",
                                    "Integer" = "integer",
                                    "Character" = "character",
                                    "Smart Date (Auto-Detect)" = "date")),
            actionButton("change_type_btn", "Convert Data Type", class = "btn-custom-warning btn-sm w-100")
          ),
          box(
            title = "Data Preview & Statistical Properties", 
            width = 8, 
            status = "info", 
            solidHeader = TRUE,
            h4("System Metadata & Structural Profile"),
            uiOutput("metadata_display_ui"),
            hr(),
            h4("Raw Data Grid"),
            div(class = "scrollable-table", tableOutput("data_preview")),
            hr(),
            h4("Data Column Profiles"),
            div(class = "scrollable-table", gt_output("data_summary_table")),
            br(),
            h4("Pearson Correlation Matrix (Numeric Fields)"),
            gt_output("correlation_matrix_table")
          )
        )
      ),
      # --- TAB 2: DIAGNOSTICS & MODEL FIT ---
      tabItem(
        tabName = "diagnostics",
        fluidRow(
          box(
            title = "Define Statistical Model Layout",
            width = 4,
            status = "warning",
            solidHeader = TRUE,
            uiOutput("diagnostics_var_selectors"),
            br(),
            fluidRow(
              column(6, actionButton("run_analysis_btn", "Run analysis", class = "btn-custom-success w-100")),
              column(6, actionButton("reset_model_btn", "Reset model", class = "btn-custom-danger w-100"))
            )
          ),
          box(
            title = "Statistical Analysis Output Profile",
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            
            h3(tags$strong("Statistical Analysis")),
            div(class = "orange-heading", "Selected variables for analysis"),
            uiOutput("formula_summary_preview_ui"),
            
            div(class = "orange-heading", "The table shows the results from the fitted linear regression model"),
            gt_output("summary_table"),
            br(),
            textOutput("simulator_used_note"),
            br(),
            textOutput("time_series_note"),
            br(),
            htmlOutput("remediation")
          )
        ),
        
        # --- SEPARATE DIAGNOSTIC TEST BOX  ---
        fluidRow(
          box(
            title = "Assumptions Check",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            uiOutput("assumptions_valueboxes_ui")
          )
        ),
        
        # --- DIAGNOSTIC PLOTS ---
        uiOutput("diagnostic_plots_ui"),
        
        # --- AI INTERPRETATION (ADDITIVE, OPT-IN) ---
        fluidRow(
          box(
            title = "AI Interpretation (Groq)",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            
            actionButton("ai_interpret_btn", "Generate AI Interpretation", class = "btn-custom-success w-100"),
            br(), br(),
            uiOutput("ai_interpretation_ui")
          )
        )
      ),
      # --- TAB 3: TRANSFORMATION ---
      tabItem(
        tabName = "transformation",
        fluidRow(
          box(
            title = "Configure Transformation",
            width = 4,
            status = "warning",
            solidHeader = TRUE,
            uiOutput("transform_var_selector_ui"),
            selectInput("transform_type", "Select Operator:",
                        choices = c("Logarithmic (log)" = "log",
                                    "Exponential (exp)" = "exp",
                                    "Square (x^2)" = "square",
                                    "Standardize (z-score)" = "standardize",
                                    "Log Return" = "log_return")),
            textInput("new_var_name", "Custom Column Name:", value = ""),
            helpText("Provide a unique identifier or leave blank for a default label auto-generation."),
            br(),
            actionButton("apply_transform_btn", "Execute Transformation", class = "btn-custom-success w-100")
          ),
          box(
            title = "Transformed Structural Workspace Summary",
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            h4("Transformed Grid View (Top 25 Records Displayed)"),
            div(class = "scrollable-table", tableOutput("transformed_data_preview"))
          )
        )
      )
    )
  )
)

# ---- Server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  modified_df <- reactiveVal(NULL)
  active_interactions <- reactiveVal(character(0))
  reset_trigger <- reactiveVal(0)
  
  observeEvent(list(input$csv_file, input$sim_btn, input$data_source), {
    modified_df(NULL)
    active_interactions(character(0))
  })
  
  observeEvent(input$reset_model_btn, {
    active_interactions(character(0))
    reset_trigger(reset_trigger() + 1)
  })
  
  output$data_config_controls <- renderUI({
    req(input$data_source, input$model_family)
    is_logistic <- identical(input$model_family, "logistic")
    
    if (identical(input$data_source, "simulate")) {
      if (is_logistic) {
        tagList(
          helpText("Simulate a binary outcome to test logistic-regression assumptions."),
          sliderInput("n_obs", "Sample Size (n):", min = 50, max = 1000, value = 500),
          checkboxInput("coll_viol", "Inject Multicollinearity", value = FALSE),
          checkboxInput("nonlinear_viol", "Inject Omitted Non-linearity", value = FALSE),
          checkboxInput("imbalance_viol", "Inject Class Imbalance", value = FALSE),
          numericInput("seed", "Random seed (leave blank for a new draw):", value = NULL),
          actionButton("sim_btn", "Simulate & Load Framework", class = "btn-custom-primary", width = "100%")
        )
      } else {
        tagList(
          selectInput(
            "data_structure_sim",
            "Select Econometric Data Structure:",
            choices = c("Cross-Sectional" = "cs", "Time Series" = "ts"),
            selected = if (!is.null(input$data_structure_sim)) input$data_structure_sim else "cs"
          ),
          helpText("Simulate a dataset to test OLS assumptions."),
          sliderInput("n_obs", "Sample Size (n):", min = 50, max = 1000, value = 500),
          checkboxInput("het_viol", "Inject Heteroskedasticity", value = FALSE),
          checkboxInput("coll_viol", "Inject Multicollinearity", value = FALSE),
          numericInput("seed", "Random seed (leave blank for a new draw):", value = NULL),
          actionButton("sim_btn", "Simulate & Load Framework", class = "btn-custom-primary", width = "100%")
        )
      }
    } else if (identical(input$data_source, "available")) {
      if (file.exists(AVAILABLE_DATA_PATH)) {
        tagList(
          helpText(paste("Successfully loaded system file:", AVAILABLE_DATA_PATH)),
          p(tags$strong("Structure assumed:"), " Cross-Sectional structure profile."),
          if (is_logistic) helpText("For logistic mode, pick a 0/1 (or two-level) column as the outcome, e.g. 'female'.")
        )
      } else {
        tagList(
          p(tags$span(style="color:red; font-weight:bold;", paste("Missing File Error: '", AVAILABLE_DATA_PATH, "' was not found inside the app directory."))),
          helpText(paste("Please ensure the dataset exists at '", AVAILABLE_DATA_PATH, "' relative to this app file to utilize this option."))
        )
      }
    } else if (is_logistic) {
      tagList(
        helpText("Upload a CSV file with a 0/1 outcome to inspect logistic-regression assumptions."),
        fileInput("csv_file", "CSV file:", accept = c(".csv", "text/csv")),
        checkboxInput("header", "File has a header row", value = TRUE)
      )
    } else {
      panel_selected <- identical(input$data_structure_upload, "panel")
      tagList(
        selectInput(
          "data_structure_upload",
          "Select Econometric Data Structure:",
          choices = c("Cross-Sectional" = "cs", "Time Series" = "ts", "Panel Data" = "panel"),
          selected = if (!is.null(input$data_structure_upload)) input$data_structure_upload else "cs"
        ),
        if (panel_selected) {
          tagList(
            textInput("p_idx_i", "Cross-Section Index:", value = ""),
            textInput("p_idx_t", "Time Index:", value = "")
          )
        },
        helpText("Upload a CSV file to inspect assumptions."),
        fileInput("csv_file", "CSV file:", accept = c(".csv", "text/csv")),
        checkboxInput("header", "File has a header row", value = TRUE)
      )
    }
  })
  
  data_structure <- reactive({
    req(input$data_source, input$model_family)
    if (identical(input$model_family, "logistic")) {
      "cs"
    } else if (identical(input$data_source, "simulate")) {
      req(input$data_structure_sim)
      input$data_structure_sim
    } else if (identical(input$data_source, "available")) {
      "cs"
    } else {
      req(input$data_structure_upload)
      input$data_structure_upload
    }
  })
  
  sim_data <- eventReactive(input$sim_btn, {
    seed_val <- if (is.null(input$seed) || is.na(input$seed)) NULL else input$seed
    if (identical(input$model_family, "logistic")) {
      get("simulate_logit_data", mode = "function")(n = input$n_obs, multicollinear = input$coll_viol, nonlinear = input$nonlinear_viol, imbalance = input$imbalance_viol, seed = seed_val)
    } else if (identical(data_structure(), "ts")) {
      get("simulate_ts_data", mode = "function")(n = input$n_obs, heteroskedastic = input$het_viol, multicollinear = input$coll_viol, seed = seed_val)
    } else {
      get("simulate_ols_data", mode = "function")(n = input$n_obs, heteroskedastic = input$het_viol, multicollinear = input$coll_viol, seed = seed_val)
    }
  }, ignoreNULL = FALSE)
  
  uploaded_data <- reactive({
    req(input$csv_file)
    tryCatch(
      read.csv(input$csv_file$datapath, header = input$header, stringsAsFactors = FALSE),
      error = function(e) {
        validate(need(FALSE, paste("Could not read the file:", conditionMessage(e))))
      }
    )
  })
  
  system_available_data <- reactive({
    validate(need(file.exists(AVAILABLE_DATA_PATH), paste("Please ensure standard local system file exists:", AVAILABLE_DATA_PATH)))
    tryCatch(
      read.csv(AVAILABLE_DATA_PATH, stringsAsFactors = FALSE),
      error = function(e) {
        validate(need(FALSE, paste("Could not read systemic available file:", conditionMessage(e))))
      }
    )
  })
  
  current_data <- reactive({
    if (!is.null(modified_df())) {
      modified_df()
    } else if (identical(input$data_source, "simulate")) {
      req(sim_data())
      sim_data()
    } else if (identical(input$data_source, "available")) {
      system_available_data()
    } else {
      req(uploaded_data())
      uploaded_data()
    }
  })
  
  output$var_to_edit_ui <- renderUI({
    req(current_data())
    selectInput("var_to_edit", "Select variable to edit:", choices = names(current_data()))
  })
  
  observeEvent(input$change_type_btn, {
    req(current_data(), input$var_to_edit, input$new_data_type)
    df <- current_data()
    target_col <- input$var_to_edit
    validate(need(target_col %in% names(df),
                  paste0("Column '", target_col, "' was not found in the current dataset.")))
    
    df[[target_col]] <- switch(input$new_data_type,
                               "factor"    = as.factor(df[[target_col]]),
                               "numeric"   = suppressWarnings(as.numeric(as.character(df[[target_col]]))),
                               "integer"   = suppressWarnings(as.integer(as.character(df[[target_col]]))),
                               "character" = as.character(df[[target_col]]),
                               "date"      = parse_date_dynamically(df[[target_col]])
    )
    modified_df(df)
  })
  
  # ---- Tab 3: Transformation UI Selector & Execution Server Logic ----
  output$transform_var_selector_ui <- renderUI({
    req(current_data())
    selectInput("transform_target_col", "Select target column:", choices = names(current_data()))
  })
  
  observeEvent(input$apply_transform_btn, {
    req(current_data(), input$transform_target_col, input$transform_type)
    df <- current_data()
    
    raw_vector <- suppressWarnings(as.numeric(as.character(df[[input$transform_target_col]])))
    validate(need(!all(is.na(raw_vector)), "Selected structural column must be entirely numeric to transform."))
    
    # Mathematical Calculations Guard
    transformed_values <- switch(input$transform_type,
                                 "log" = {
                                   validate(need(all(raw_vector > 0, na.rm = TRUE), "Log transformations require strictly positive (>0) values."))
                                   log(raw_vector)
                                 },
                                 "exp" = {
                                   res <- exp(raw_vector)
                                   validate(need(all(is.finite(res)), "Exponential overflow detected. Select smaller value vectors."))
                                   res
                                 },
                                 "square" = raw_vector ^ 2,
                                 "standardize" = {
                                   validate(need(sd(raw_vector, na.rm = TRUE) > 0, "Standard deviation must be non-zero to calculate z-scores."))
                                   (raw_vector - mean(raw_vector, na.rm = TRUE)) / sd(raw_vector, na.rm = TRUE)
                                 },
                                 "log_return" = {
                                   validate(need(length(raw_vector) > 1, "Insufficient observations to calculate continuous time periods."))
                                   validate(need(all(raw_vector > 0, na.rm = TRUE), "Log Return logic requires strictly positive (>0) values or asset prices."))
                                   c(NA, diff(log(raw_vector)))
                                 }
    )
    
    # Process column naming scheme
    final_col_name <- trimws(input$new_var_name)
    if (identical(final_col_name, "")) {
      final_col_name <- paste0(input$transform_target_col, "_", input$transform_type)
    }
    
    df[[final_col_name]] <- transformed_values
    modified_df(df)
    updateTextInput(session, "new_var_name", value = "") # clear text box input gracefully
  })
  
  output$transformed_data_preview <- renderTable({
    req(current_data())
    head(current_data(), 25)
  })
  
  output$diagnostics_var_selectors <- renderUI({
    reset_trigger()
    df <- current_data()
    all_cols <- names(df)
    
    if (identical(input$data_source, "simulate")) {
      tagList(p(tags$em("Simulated dataset locks standard defaults: Y ~ X1 + X2")))
    } else {
      # Smart pre-selection fallbacks targeted for labor economy variables
      y_sel <- if (identical(input$data_source, "available") && "wage" %in% all_cols) "wage" else ""
      x_sel <- if (identical(input$data_source, "available") && "experience" %in% all_cols) c("experience", "age", "female") else NULL
      
      tagList(
        selectInput("y_var", "Select outcome variable", choices = c("", all_cols), selected = y_sel),
        selectInput("x_vars", "Select predictor variable(s)", choices = all_cols, selected = x_sel, multiple = TRUE),
        selectInput("interaction_vars", "Select interaction term(s)", choices = all_cols, selected = NULL, multiple = TRUE),
        actionButton("add_interaction_btn", "Create and add term(s)", class = "btn-custom-primary btn-sm w-100"),
        br(), br(),
        uiOutput("interaction_checkboxes_ui")
      )
    }
  })
  
  observeEvent(input$add_interaction_btn, {
    req(input$interaction_vars)
    if (length(input$interaction_vars) >= 2) {
      new_term <- paste(input$interaction_vars, collapse = "*")
      current_list <- active_interactions()
      if (!(new_term %in% current_list)) {
        active_interactions(c(current_list, new_term))
      }
    }
  })
  
  output$interaction_checkboxes_ui <- renderUI({
    req(active_interactions())
    if (length(active_interactions()) > 0) {
      checkboxGroupInput("selected_interactions", "Active Model Interactions Toggle:",
                         choices = active_interactions(),
                         selected = active_interactions())
    }
  })
  
  output$formula_summary_preview_ui <- renderUI({
    if (identical(input$data_source, "simulate")) {
      outcome_txt <- "Y"
      predictors_txt <- "X1 + X2"
    } else {
      outcome_txt <- if (is.null(input$y_var) || input$y_var == "") "[Not Chosen]" else input$y_var
      
      preds <- setdiff(input$x_vars, input$y_var)
      if (length(preds) == 0) {
        predictors_txt <- "[Not Chosen]"
      } else {
        predictors_txt <- paste(preds, collapse = " + ")
      }
      
      if (!is.null(input$selected_interactions) && length(input$selected_interactions) > 0) {
        predictors_txt <- paste(predictors_txt, "+", paste(input$selected_interactions, collapse = " + "))
      }
    }
    
    div(class = "formula-preview-box",
        div(paste("Outcome variable:", outcome_txt)),
        div(paste("Predictor variable(s):", predictors_txt))
    )
  })
  
  diag_model <- eventReactive(input$run_analysis_btn, {
    req(data_structure())
    df <- current_data()
    
    if (input$data_source == "simulate") {
      model_formula <- Y ~ X1 + X2
    } else {
      req(input$y_var, input$x_vars)
      validate(need(input$y_var != "", "Please select a dependent outcome variable (Y)."))
      preds <- setdiff(input$x_vars, input$y_var)
      validate(need(length(preds) >= 1, "Select distinct valid predictor terms."))
      
      df[[input$y_var]] <- suppressWarnings(as.numeric(df[[input$y_var]]))
      validate(need(!all(is.na(df[[input$y_var]])),
                    paste0("Outcome variable '", input$y_var, "' could not be converted to numeric.")))
      for (p in preds) {
        df[[p]] <- suppressWarnings(as.numeric(df[[p]]))
        validate(need(!all(is.na(df[[p]])),
                      paste0("Predictor '", p, "' could not be converted to numeric.")))
      }
      
      # reformulate() automatically backtick-quotes non-syntactic column names
      # (e.g. "X (USD)"), unlike paste() + as.formula().
      term_labels <- preds
      if (!is.null(input$selected_interactions) && length(input$selected_interactions) > 0) {
        term_labels <- c(term_labels, input$selected_interactions)
      }
      model_formula <- reformulate(termlabels = term_labels, response = input$y_var)
    }
    
    if (identical(input$model_family, "logistic")) {
      fit <- glm(model_formula, data = df, family = binomial())
      return(get("new_diag_lm", mode = "function")(fit, data_type = "logistic"))
    }
    
    if (data_structure() == "panel") {
      validate(need(input$data_source == "upload", "Panel Data requires an uploaded CSV."))
      req(input$p_idx_i, input$p_idx_t)
      validate(need(nzchar(input$p_idx_i) && nzchar(input$p_idx_t),
                    "Provide both panel index column names."))
      validate(need(input$p_idx_i %in% names(df),
                    "Cross-Section Index column was not found in the uploaded data."))
      validate(need(input$p_idx_t %in% names(df),
                    "Time Index column was not found in the uploaded data."))
      fit <- plm::plm(formula = model_formula, data = df, model = "pooling", index = c(input$p_idx_i, input$p_idx_t))
    } else {
      fit <- lm(model_formula, data = df)
    }
    
    get("new_diag_lm", mode = "function")(fit, data_type = data_structure())
  }, ignoreNULL = FALSE)
  
  # ---- Data Preview Logic Renderers -----------------------------------------
  output$metadata_display_ui <- renderUI({
    df <- current_data()
    type_label <- switch(data_structure(), "cs" = "Cross-Sectional", "ts" = "Time Series", "panel" = "Panel Data")
    tags$p(HTML(paste0(
      "<strong>Data Structure Class:</strong> ", type_label, "<br>",
      "<strong>Observations Count (N):</strong> ", nrow(df), "<br>",
      "<strong>Total Variables Shown:</strong> ", ncol(df)
    )))
  })
  
  output$data_preview <- renderTable({ head(current_data(), 25) })
  
  output$data_summary_table <- render_gt({
    df <- current_data()
    total_rows <- nrow(df)
    codebook_list <- list()
    
    for (i in seq_along(names(df))) {
      var_name <- names(df)[i]
      col_data <- df[[var_name]]
      data_type <- class(col_data)[1]
      
      n_missing <- sum(is.na(col_data) | col_data == "" | col_data == "NA")
      pct_missing <- (n_missing / total_rows) * 100
      missing_str <- sprintf("%d\n(%.1f%%)", n_missing, pct_missing)
      
      suppressWarnings(numeric_test <- as.numeric(as.character(col_data)))
      is_col_numeric <- !all(is.na(numeric_test)) && (data_type %in% c("numeric", "integer", "double"))
      
      if (is_col_numeric) {
        v <- numeric_test[!is.na(numeric_test)]
        if (length(v) > 0) {
          avg <- mean(v); std <- sd(v); mn <- min(v); md <- median(v); mx <- max(v)
          stats_str <- sprintf("Mean (sd) : %.1f (%.1f)\nmin <= med <= max:\n%.1f <= %.1f <= %.1f\nIQR (CV) : %.1f (%.1f)", 
                               avg, std, mn, md, mx, IQR(v), if(avg != 0) std / abs(avg) else NA)
          freqs_str <- sprintf("%d distinct values", length(unique(v)))
        } else {
          stats_str <- "All values are missing"; freqs_str <- "-"
        }
      } else {
        v_chars <- as.character(col_data)
        v_chars[v_chars == "" | v_chars == "NA"] <- NA
        v_valid <- v_chars[!is.na(v_chars)]
        
        if (length(v_valid) > 0) {
          freq_table <- sort(table(v_valid), decreasing = TRUE)
          top_n <- min(10, length(freq_table))
          stats_lines <- c(); freqs_lines <- c()
          
          for (j in 1:top_n) {
            val_name <- names(freq_table)[j]
            val_count <- freq_table[j]
            if (nchar(val_name) > 25) val_name <- paste0(substr(val_name, 1, 22), "...")
            stats_lines <- c(stats_lines, sprintf("%d. %s", j, val_name))
            freqs_lines <- c(freqs_lines, sprintf("%d ( %.1f%%)", val_count, (val_count / length(v_valid)) * 100))
          }
          if (length(freq_table) > 10) {
            others_count <- sum(freq_table[(top_n + 1):length(freq_table)])
            stats_lines <- c(stats_lines, sprintf("[ %d others ]", length(freq_table) - 10))
            freqs_lines <- c(freqs_lines, sprintf("%d (%.1f%%)", others_count, (others_count / length(v_valid)) * 100))
          }
          stats_str <- paste(stats_lines, collapse = "\n")
          freqs_str <- paste(freqs_lines, collapse = "\n")
        } else {
          stats_str <- "Empty column"; freqs_str <- "-"
        }
      }
      
      codebook_list[[i]] <- data.frame(No = i, Variable = sprintf("%s\n[%s]", var_name, data_type),
                                       `Stats / Values` = stats_str, `Freqs (% of Valid)` = freqs_str,
                                       Missing = missing_str, check.names = FALSE, stringsAsFactors = FALSE)
    }
    
    gt(do.call(rbind, codebook_list)) %>%
      tab_options(table.width = pct(100), table.font.size = 13) %>%
      cols_align(align = "left", columns = c(Variable, `Stats / Values`, `Freqs (% of Valid)`)) %>%
      cols_align(align = "center", columns = c(No, Missing)) %>%
      text_transform(locations = cells_body(), fn = function(x) { paste0("<div class='codebook-text'>", gsub("\n", "<br>", x), "</div>") })
  })
  
  output$correlation_matrix_table <- render_gt({
    df <- current_data()
    num_cols <- names(df)[vapply(df, function(x) !all(is.na(suppressWarnings(as.numeric(x)))), logical(1))]
    validate(need(length(num_cols) >= 2, "Need at least two numeric fields to build a correlation matrix."))
    
    numeric_df <- as.data.frame(lapply(df[, num_cols, drop = FALSE], function(x) suppressWarnings(as.numeric(x))))
    cor_mat <- as.data.frame(cor(numeric_df, use = "pairwise.complete.obs", method = "pearson"))
    cor_mat <- cbind(Variable = rownames(cor_mat), cor_mat)
    
    gt(cor_mat) %>% 
      fmt_number(columns = -Variable, decimals = 3) %>% 
      tab_options(table.width = pct(100)) %>% 
      data_color(columns = -Variable, palette = c("#d73027", "#f7f7f7", "#4575b4"), domain = c(-1, 1))
  })
  
  # ---- Diagnostics Tab ValueBoxes & Summary Output Renderers ----------------
  output$summary_table <- render_gt({ summary(diag_model()) })
  
  # Breusch-Pagan diagnostic box
  output$bp_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$bp_pvalue
    
    if (is.na(p_value)) {
      valueBox(
        value = "N/A",
        subtitle = "Breusch-Pagan test unavailable",
        icon = icon("question-circle"),
        color = "yellow"
      )
    } else if (p_value >= 0.05) {
      valueBox(
        value = "PASS",
        subtitle = paste0("Constant error variance | p = ", format(round(p_value, 4), nsmall = 4)),
        icon = icon("check-circle"),
        color = "green"
      )
    } else {
      valueBox(
        value = "CHECK",
        subtitle = paste0("Unequal error variance | p = ", format(round(p_value, 4), nsmall = 4)),
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  # Durbin-Watson diagnostic box
  output$dw_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$dw_pvalue
    statistic <- model$diagnostics$dw_statistic
    
    if (is.na(p_value)) {
      valueBox(
        value = "N/A",
        subtitle = "Durbin-Watson test unavailable",
        icon = icon("question-circle"),
        color = "yellow"
      )
    } else if (p_value >= 0.05) {
      valueBox(
        value = "PASS",
        subtitle = paste0("Independent errors | DW = ", round(statistic, 2), " | p = ", format(round(p_value, 4), nsmall = 4)),
        icon = icon("check-circle"),
        color = "green"
      )
    } else {
      valueBox(
        value = "CHECK",
        subtitle = paste0("Serial correlation concern | DW = ", round(statistic, 2), " | p = ", format(round(p_value, 4), nsmall = 4)),
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  # Shapiro-Wilk diagnostic box
  output$sw_box <- renderValueBox({
    model <- diag_model()
    p_value <- model$diagnostics$shapiro_pvalue
    
    if (is.na(p_value)) {
      valueBox(
        value = "N/A",
        subtitle = "Shapiro-Wilk test unavailable",
        icon = icon("question-circle"),
        color = "yellow"
      )
    } else if (p_value >= 0.05) {
      valueBox(
        value = "PASS",
        subtitle = paste0("Approximately normal residuals | p = ", format(round(p_value, 4), nsmall = 4)),
        icon = icon("check-circle"),
        color = "green"
      )
    } else {
      valueBox(
        value = "CHECK",
        subtitle = paste0("Residual normality concern | p = ", format(round(p_value, 4), nsmall = 4)),
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })
  
  # VIF diagnostic box
  output$vif_box <- renderValueBox({
    model <- diag_model()
    vif_values <- model$diagnostics$vif_scores
    
    vif_numeric <- suppressWarnings(as.numeric(vif_values))
    vif_numeric <- vif_numeric[is.finite(vif_numeric)]
    
    if (length(vif_numeric) == 0) {
      valueBox(
        value = "N/A",
        subtitle = "VIF unavailable",
        icon = icon("question-circle"),
        color = "yellow"
      )
    } else {
      max_vif <- max(vif_numeric)
      
      if (max_vif < 5) {
        valueBox(
          value = "PASS",
          subtitle = paste0("Low multicollinearity | Max VIF = ", round(max_vif, 2)),
          icon = icon("check-circle"),
          color = "green"
        )
      } else if (max_vif < 10) {
        valueBox(
          value = "CAUTION",
          subtitle = paste0("Moderate multicollinearity | Max VIF = ", format(round(max_vif, 2), nsmall = 2)),
          icon = icon("exclamation-circle"),
          color = "yellow"
        )
      } else {
        valueBox(
          value = "CHECK",
          subtitle = paste0("High multicollinearity | Max VIF = ", format(round(max_vif, 2), nsmall = 2)),
          icon = icon("exclamation-triangle"),
          color = "red"
        )
      }
    }
  })
  
  # Overall diagnostic status box
  output$overall_box <- renderValueBox({
    model <- diag_model()
    
    bp_p <- model$diagnostics$bp_pvalue
    dw_p <- model$diagnostics$dw_pvalue
    sw_p <- model$diagnostics$shapiro_pvalue
    
    vif_values <- suppressWarnings(as.numeric(model$diagnostics$vif_scores))
    vif_values <- vif_values[is.finite(vif_values)]
    
    bp_pass <- !is.na(bp_p) && bp_p >= 0.05
    dw_pass <- !is.na(dw_p) && dw_p >= 0.05
    sw_pass <- !is.na(sw_p) && sw_p >= 0.05
    vif_pass <- length(vif_values) > 0 && max(vif_values) < 5
    
    results <- c(bp_pass, dw_pass, sw_pass, vif_pass)
    passed <- sum(results)
    total <- length(results)
    
    if (passed == total) {
      valueBox(
        value = paste0(passed, "/", total, " assumptions passed"),
        subtitle = "Overall status: No major diagnostic concerns",
        icon = icon("check-double"),
        color = "green",
        width = 12
      )
    } else if (passed >= 3) {
      valueBox(
        value = paste0(passed, "/", total, " assumptions passed"),
        subtitle = "Overall status: Review the highlighted diagnostic",
        icon = icon("exclamation-circle"),
        color = "yellow",
        width = 12
      )
    } else {
      valueBox(
        value = paste0(passed, "/", total, " assumptions passed"),
        subtitle = "Overall status: Multiple diagnostic concerns require attention",
        icon = icon("exclamation-triangle"),
        color = "red",
        width = 12
      )
    }
  })
  
  # Hosmer-Lemeshow diagnostic box (logistic only)
  output$hl_box <- renderValueBox({
    model <- diag_model()
    req(identical(model$data_type, "binary"))
    p_value <- model$diagnostics$logistic$hl_pvalue
    
    if (is.null(p_value) || is.na(p_value)) {
      valueBox(value = "N/A", subtitle = "Hosmer-Lemeshow test unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (p_value >= 0.05) {
      valueBox(value = "PASS", subtitle = paste0("Adequate fit | p = ", format(round(p_value, 4), nsmall = 4)), icon = icon("check-circle"), color = "green")
    } else {
      valueBox(value = "CHECK", subtitle = paste0("Poor fit | p = ", format(round(p_value, 4), nsmall = 4)), icon = icon("exclamation-triangle"), color = "red")
    }
  })
  
  # AUC diagnostic box (logistic only)
  output$auc_box <- renderValueBox({
    model <- diag_model()
    req(identical(model$data_type, "binary"))
    auc <- model$diagnostics$logistic$auc
    
    if (is.null(auc) || is.na(auc)) {
      valueBox(value = "N/A", subtitle = "AUC unavailable", icon = icon("question-circle"), color = "yellow")
    } else if (auc >= 0.8) {
      valueBox(value = "PASS", subtitle = paste0("Good discrimination | AUC = ", round(auc, 3)), icon = icon("check-circle"), color = "green")
    } else if (auc >= 0.7) {
      valueBox(value = "CAUTION", subtitle = paste0("Acceptable discrimination | AUC = ", round(auc, 3)), icon = icon("exclamation-circle"), color = "yellow")
    } else {
      valueBox(value = "CHECK", subtitle = paste0("Weak discrimination | AUC = ", round(auc, 3)), icon = icon("exclamation-triangle"), color = "red")
    }
  })
  
  # Accuracy diagnostic box (logistic only)
  output$accuracy_box <- renderValueBox({
    model <- diag_model()
    req(identical(model$data_type, "binary"))
    acc <- model$diagnostics$logistic$accuracy
    
    if (is.null(acc) || is.na(acc)) {
      valueBox(value = "N/A", subtitle = "Accuracy unavailable", icon = icon("question-circle"), color = "yellow")
    } else {
      valueBox(value = paste0(round(acc * 100, 1), "%"), subtitle = "Accuracy at 0.5 cutoff", icon = icon("bullseye"), color = if (acc >= 0.75) "green" else if (acc >= 0.6) "yellow" else "red")
    }
  })
  
  # Overall diagnostic status box (logistic only)
  output$overall_box_logistic <- renderValueBox({
    model <- diag_model()
    req(identical(model$data_type, "binary"))
    
    hl_p <- model$diagnostics$logistic$hl_pvalue
    auc  <- model$diagnostics$logistic$auc
    
    vif_values <- suppressWarnings(as.numeric(model$diagnostics$vif_scores))
    vif_values <- vif_values[is.finite(vif_values)]
    
    hl_pass  <- !is.na(hl_p) && hl_p >= 0.05
    auc_pass <- !is.na(auc) && auc >= 0.7
    vif_pass <- length(vif_values) == 0 || max(vif_values) < 5
    sep_pass <- !isTRUE(model$diagnostics$logistic$separation_flag)
    
    results <- c(hl_pass, auc_pass, vif_pass, sep_pass)
    passed <- sum(results)
    total <- length(results)
    
    if (passed == total) {
      valueBox(value = paste0(passed, "/", total, " assumptions passed"), subtitle = "Overall status: No major diagnostic concerns", icon = icon("check-double"), color = "green", width = 12)
    } else if (passed >= 3) {
      valueBox(value = paste0(passed, "/", total, " assumptions passed"), subtitle = "Overall status: Review the highlighted diagnostic", icon = icon("exclamation-circle"), color = "yellow", width = 12)
    } else {
      valueBox(value = paste0(passed, "/", total, " assumptions passed"), subtitle = "Overall status: Multiple diagnostic concerns require attention", icon = icon("exclamation-triangle"), color = "red", width = 12)
    }
  })
  
  output$assumptions_valueboxes_ui <- renderUI({
    req(input$model_family)
    
    if (identical(input$model_family, "logistic")) {
      tagList(
        fluidRow(
          valueBoxOutput("hl_box", width = 3),
          valueBoxOutput("auc_box", width = 3),
          valueBoxOutput("accuracy_box", width = 3),
          valueBoxOutput("vif_box", width = 3)
        ),
        fluidRow(
          valueBoxOutput("overall_box_logistic", width = 12)
        )
      )
    } else {
      tagList(
        fluidRow(
          valueBoxOutput("bp_box", width = 3),
          valueBoxOutput("dw_box", width = 3),
          valueBoxOutput("sw_box", width = 3),
          valueBoxOutput("vif_box", width = 3)
        ),
        fluidRow(
          valueBoxOutput("overall_box", width = 12)
        )
      )
    }
  })
  
  # ---- Diagnostics Tab Individual Plot Renderers ----------------------------
  output$plot_residuals      <- renderPlot({ plot(diag_model(), type = "residuals") })
  output$plot_qq             <- renderPlot({ plot(diag_model(), type = "qq") })
  output$plot_scale          <- renderPlot({ plot(diag_model(), type = "scale_location") })
  output$plot_hist           <- renderPlot({ plot(diag_model(), type = "histogram") })
  output$plot_acf            <- renderPlot({ plot(diag_model(), type = "acf") })
  output$plot_pacf           <- renderPlot({ plot(diag_model(), type = "pacf") })
  output$plot_residuals_time <- renderPlot({ plot(diag_model(), type = "residuals_time") })
  output$plot_binned         <- renderPlot({
    req(identical(diag_model()$data_type, "binary"))
    plot(diag_model(), type = "binned_residuals")
  })
  output$plot_roc            <- renderPlot({
    req(identical(diag_model()$data_type, "binary"))
    plot(diag_model(), type = "roc")
  })
  output$plot_calibration    <- renderPlot({
    req(identical(diag_model()$data_type, "binary"))
    plot(diag_model(), type = "calibration")
  })
  
  output$diagnostic_plots_ui <- renderUI({
    req(data_structure(), input$model_family)
    
    if (identical(input$model_family, "logistic")) {
      tagList(
        fluidRow(
          box(title = "Binned Residuals", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("plot_binned", height = "350px")),
          box(title = "ROC Curve", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("plot_roc", height = "350px"))
        ),
        fluidRow(
          box(title = "Calibration Plot", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("plot_calibration", height = "350px")),
          box(title = "Normal Q-Q", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("plot_qq", height = "350px"))
        )
      )
    } else if (identical(data_structure(), "ts")) {
      tagList(
        fluidRow(
          box(title = "Residuals over Time", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("plot_residuals_time", height = "350px")),
          box(title = "Autocorrelation (ACF)", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("plot_acf", height = "350px"))
        ),
        fluidRow(
          box(title = "Partial Autocorrelation (PACF)", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("plot_pacf", height = "350px")),
          box(title = "Normal Q-Q", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("plot_qq", height = "350px"))
        ),
        fluidRow(
          box(title = "Residual Histogram", width = 12, status = "primary", solidHeader = TRUE,
              plotOutput("plot_hist", height = "350px"))
        )
      )
    } else {
      tagList(
        fluidRow(
          box(title = "Residuals vs Fitted", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("plot_residuals", height = "350px")),
          box(title = "Normal Q-Q", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("plot_qq", height = "350px"))
        ),
        fluidRow(
          box(title = "Scale-Location", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("plot_scale", height = "350px")),
          box(title = "Residual Histogram", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("plot_hist", height = "350px"))
        )
      )
    }
  })
  
  output$simulator_used_note <- renderText({
    req(diag_model())
    if (identical(input$data_source, "simulate")) {
      if (identical(input$model_family, "logistic")) "Simulator used: Logistic simulator (simulate_logit_data)."
      else if (identical(data_structure(), "ts")) "Simulator used: Time-Series simulator." else "Simulator used: Cross-Section simulator."
    } else if (identical(input$data_source, "available")) {
      paste("Data source used: Local system available file (", AVAILABLE_DATA_PATH, ").")
    } else "Data source used: Uploaded CSV file."
  })
  
  output$time_series_note <- renderText({
    if (identical(data_structure(), "ts")) "Time Series Selected: Check serial-correlation diagnostics in the summary output."
  })
  
  output$remediation <- renderUI({ HTML(get("remediation_advice", mode = "function")(diag_model())) })
  
  # --- AI Interpretation (additive, opt-in): only calls out to Groq when the
  # user explicitly clicks the button, so existing reactivity is unaffected.
  ai_interpretation <- eventReactive(input$ai_interpret_btn, {
    req(diag_model())
    get_llm_interpretation(diag_model())
  })
  output$ai_interpretation_ui <- renderUI({ ai_interpretation() })
}

shinyApp(ui = ui, server = server)