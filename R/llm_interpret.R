# llm_interpret.R
# Optional, additive AI-generated plain-language interpretation of diag_lm
# diagnostics via the Groq API (OpenAI-compatible chat completions endpoint).
# Never called automatically -- app.R only invokes this when the user clicks
# "Generate AI Interpretation", and it never sends raw data, only the
# already-computed aggregate diagnostics.

library(httr2)
library(commonmark)

#' Build a compact, privacy-safe text summary of a diag_lm object's diagnostics
#'
#' Only aggregated statistics are included -- never raw data rows -- to keep
#' the prompt small and avoid sending potentially sensitive data to a
#' third-party API.
#'
#' @param object An object of class 'diag_lm'
#' @return A single character string describing the model and its diagnostics
#' @export
build_diag_summary <- function(object) {
  if (!inherits(object, "diag_lm")) {
    stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  }

  fmt <- function(v, digits = 4) {
    if (is.null(v) || length(v) == 0 || all(is.na(v))) return("NA")
    paste(round(v, digits), collapse = ", ")
  }

  lines <- c(
    paste0("Model type: ", object$data_type),
    paste0("Formula: ", deparse(object$formula)),
    paste0(
      "Coefficient estimates: ",
      paste(sprintf("%s = %s", rownames(object$coefficients),
                     fmt(object$coefficients[, 1])), collapse = "; ")
    )
  )

  d <- object$diagnostics

  if (identical(object$data_type, "binary")) {
    lg <- d$logistic
    lines <- c(
      lines,
      paste0("Hosmer-Lemeshow p-value: ", fmt(lg$hl_pvalue)),
      paste0("McFadden pseudo-R2: ", fmt(lg$mcfadden_r2)),
      paste0("AUC: ", fmt(lg$auc)),
      paste0("Accuracy (0.5 cutoff): ", fmt(lg$accuracy)),
      paste0("Separation flag: ", isTRUE(lg$separation_flag))
    )
  } else {
    lines <- c(
      lines,
      paste0("Breusch-Pagan p-value (heteroskedasticity): ", fmt(d$bp_pvalue)),
      paste0("Durbin-Watson p-value (serial correlation): ", fmt(d$dw_pvalue)),
      paste0("Shapiro-Wilk p-value (normality): ", fmt(d$shapiro_pvalue))
    )
  }

  if (!is.null(d$vif_scores) && !(length(d$vif_scores) == 1 && is.na(d$vif_scores))) {
    lines <- c(lines, paste0("VIF scores: ", fmt(d$vif_scores)))
  }

  if (!is.null(d$ts)) {
    lines <- c(
      lines,
      paste0("Breusch-Godfrey p-value: ", fmt(d$ts$bg_pvalue)),
      paste0("Ljung-Box p-value: ", fmt(d$ts$ljung_box_pvalue))
    )
  }

  if (!is.null(d$panel)) {
    lines <- c(
      lines,
      paste0("Panel Breusch-Pagan p-value: ", fmt(d$panel$bp_pvalue)),
      paste0("Hausman test p-value: ", fmt(d$panel$hausman_pvalue))
    )
  }

  paste(lines, collapse = "\n")
}

#' Get an AI-generated plain-language interpretation of diagnostics via Groq
#'
#' Additive, opt-in feature: returns a friendly fallback HTML message (never
#' raises an error) if no API key is configured or the request fails, so the
#' rest of the app is unaffected.
#'
#' @param object An object of class 'diag_lm'
#' @param api_key Groq API key. Defaults to the GROQ_API_KEY environment
#'   variable (set locally via .Renviron -- never hardcode a key in source).
#' @param model Groq model name.
#' @return An HTML string (shiny::HTML) suitable for uiOutput/renderUI
#' @export
get_llm_interpretation <- function(object,
                                    api_key = Sys.getenv("GROQ_API_KEY"),
                                    model = "openai/gpt-oss-120b") {
  if (!nzchar(api_key)) {
    return(shiny::HTML(paste0(
      "<div class='alert alert-info'>AI interpretation is not configured. ",
      "Set the <code>GROQ_API_KEY</code> environment variable (e.g. in a local ",
      "<code>.Renviron</code> file, see .Renviron.example) to enable this feature.</div>"
    )))
  }

  summary_text <- build_diag_summary(object)

  messages <- list(
    list(role = "system",
         content = paste(
           "You are an econometrics teaching assistant. Explain regression",
           "diagnostic output in plain, non-technical language for a student,",
           "using well-structured Markdown so it renders cleanly as HTML:",
           "a short '## Summary' paragraph (1-2 sentences on overall model health),",
           "then a '## Diagnostic Findings' bulleted list with one bullet per test",
           "(use the exact test name as given in the input, e.g. 'Breusch-Pagan',",
           "'Durbin-Watson', 'Shapiro-Wilk', 'Hosmer-Lemeshow' -- never invent or",
           "rename a test; state pass/fail in bold, then a one-line plain-language",
           "meaning), then a '## Recommended Actions' bulleted list with one concrete",
           "remedy per violated assumption (omit this section if nothing is violated).",
           "Keep the whole answer under 250 words. Do not repeat raw p-values verbatim",
           "beyond what is needed to justify the pass/fail call."
         )),
    list(role = "user", content = summary_text)
  )

  result <- tryCatch({
    resp <- httr2::request("https://api.groq.com/openai/v1/chat/completions") |>
      httr2::req_auth_bearer_token(api_key) |>
      httr2::req_body_json(list(
        model = model,
        messages = messages,
        temperature = 1,
        max_completion_tokens = 2048,
        top_p = 1,
        reasoning_effort = "medium",
        # Non-streaming here: renderUI needs one complete value, not chunks.
        stream = FALSE,
        stop = NULL
      )) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()

    body <- httr2::resp_body_json(resp)
    list(ok = TRUE, text = body$choices[[1]]$message$content)
  }, error = function(e) {
    list(ok = FALSE, text = conditionMessage(e))
  })

  if (!result$ok) {
    return(shiny::HTML(paste0(
      "<div class='alert alert-warning'>AI interpretation unavailable: ",
      htmltools::htmlEscape(result$text), "</div>"
    )))
  }

  # Escape first, then parse as Markdown: any stray HTML/script tags in the
  # model's response are neutralized as literal text rather than executed,
  # while Markdown syntax (#, *, -, |) survives untouched.
  body_html <- commonmark::markdown_html(
    htmltools::htmlEscape(result$text),
    extensions = c("table", "strikethrough")
  )

  shiny::HTML(paste0("<div class='ai-interpretation'>", body_html, "</div>"))
}
