## ============================================================
## AI-Assisted Anonymization Panel Controller
## ============================================================

# --- Helper functions ---

# Convert tool calls to readable R code for display and reproducibility
ai_calls_to_code <- function(calls, k = 3) {
  lines <- character(0)
  for (call in calls) {
    tool <- sub("^.*\\.", "", call$tool)
    line <- switch(tool,
      "groupAndRename" = {
        before <- as.character(unlist(call$before))
        after <- as.character(unlist(call$after))
        sprintf('sdcObj <- groupAndRename(sdcObj, var = "%s", before = c(%s), after = c(%s))',
          call$var,
          paste0('"', before, '"', collapse = ", "),
          paste0('"', after, '"', collapse = ", "))
      },
      "localSuppression" = {
        lk <- if (!is.null(call$k)) call$k else k
        sprintf("sdcObj <- localSuppression(sdcObj, k = %d)", lk)
      },
      "microaggregation" = {
        variables <- as.character(unlist(call$variables))
        method <- if (!is.null(call$method)) call$method else "mdav"
        sprintf('sdcObj <- microaggregation(sdcObj, variables = c(%s), method = "%s")',
          paste0('"', variables, '"', collapse = ", "), method)
      },
      "addNoise" = {
        variables <- as.character(unlist(call$variables))
        noise <- if (!is.null(call$noise)) call$noise else 150
        sprintf('sdcObj <- addNoise(sdcObj, variables = c(%s), noise = %s)',
          paste0('"', variables, '"', collapse = ", "), noise)
      },
      "pram" = "sdcObj <- pram(sdcObj)",
      "topBotCoding" = {
        sprintf('sdcObj <- topBotCoding(sdcObj, value = %s, replacement = %s, kind = "%s", column = "%s")',
          call$value, call$replacement, call$kind, call$column)
      },
      sprintf("# Unknown tool: %s", tool)
    )
    lines <- c(lines, line)
  }
  # Always include localSuppression at the end if not already present
  has_ls <- any(vapply(calls, function(c) sub("^.*\\.", "", c$tool) == "localSuppression", logical(1)))
  if (!has_ls) {
    lines <- c(lines, sprintf("sdcObj <- localSuppression(sdcObj, k = %d)", k))
  }
  lines
}

# Detect API key from environment variables for a given provider
ai_detect_api_key <- function(provider) {
  env_map <- list(
    openai = c("OPENAI_API_KEY", "LLM_API_KEY"),
    anthropic = c("ANTHROPIC_API_KEY", "LLM_API_KEY"),
    custom = c("LLM_API_KEY", "OPENAI_API_KEY")
  )
  vars <- env_map[[provider]]
  if (is.null(vars)) vars <- c("LLM_API_KEY")
  for (ev in vars) {
    val <- Sys.getenv(ev, "")
    if (nzchar(val)) return(val)
  }
  return("")
}

# Default model for a provider
ai_default_model <- function(provider) {
  switch(provider,
    openai = "gpt-4.1",
    anthropic = "claude-sonnet-4-20250514",
    custom = "",
    ""
  )
}

# Weight presets
ai_weight_presets <- list(
  "Balanced" = c(1/3, 1/3, 1/3),
  "Minimize suppressions" = c(0.6, 0.2, 0.2),
  "Preserve categories" = c(0.2, 0.6, 0.2),
  "Custom" = NULL
)

# Get weight vector from current inputs
ai_get_weights <- function() {
  preset <- input$ai_weight_preset
  if (is.null(preset)) preset <- "Balanced"
  if (preset == "Custom") {
    w <- c(
      if (!is.null(input$ai_w_suppression)) input$ai_w_suppression else 1,
      if (!is.null(input$ai_w_catloss)) input$ai_w_catloss else 1,
      if (!is.null(input$ai_w_il1)) input$ai_w_il1 else 1
    )
    s <- sum(w)
    if (s == 0) s <- 1
    return(w / s)
  }
  w <- ai_weight_presets[[preset]]
  if (is.null(w)) w <- c(1/3, 1/3, 1/3)
  return(w)
}

# Get API key: user override or env var
ai_get_api_key <- function() {
  user_key <- input$ai_api_key
  if (!is.null(user_key) && nzchar(user_key)) {
    return(user_key)
  }
  provider <- input$ai_provider
  if (is.null(provider)) provider <- "openai"
  detected <- ai_detect_api_key(provider)
  if (nzchar(detected)) return(detected)
  return("")
}


# --- Left sidebar ---
output$ui_ai_sidebar <- renderUI({
  # Provider selection
  sel_provider <- selectInput("ai_provider",
    label = p("LLM Provider"),
    choices = c("openai", "anthropic", "custom"),
    selected = if (!is.null(obj$ai_provider)) obj$ai_provider else "openai",
    width = "100%")

  # Model text input (default filled per provider)
  cur_provider <- input$ai_provider
  if (is.null(cur_provider)) cur_provider <- "openai"
  default_model <- ai_default_model(cur_provider)

  sel_model <- textInput("ai_model",
    label = p("Model"),
    value = if (!is.null(obj$ai_model) && nzchar(obj$ai_model)) obj$ai_model else default_model,
    width = "100%")

  # API key with status indicator
  detected_key <- ai_detect_api_key(cur_provider)
  has_key <- nzchar(detected_key)
  key_status <- if (has_key) {
    tags$span(icon("check"), "API key found in environment",
      style = "color: green; font-size: 0.85em;")
  } else {
    tags$span(icon("xmark"), "No API key in environment",
      style = "color: orange; font-size: 0.85em;")
  }

  sel_api_key <- passwordInput("ai_api_key",
    label = p("API Key (override)"),
    value = "",
    width = "100%")

  # Base URL (only for custom provider)
  sel_base_url <- NULL
  if (!is.null(cur_provider) && cur_provider == "custom") {
    sel_base_url <- textInput("ai_base_url",
      label = p("Base URL"),
      value = "",
      placeholder = "http://localhost:11434/v1",
      width = "100%")
  }

  # k-anonymity
  sel_k <- numericInput("ai_k",
    label = p("k-Anonymity level"),
    value = 3, min = 2, max = 20, step = 1,
    width = "100%")

  # Number of strategies
  sel_nstrat <- numericInput("ai_n_strategies",
    label = p("Number of strategies"),
    value = 3, min = 1, max = 10, step = 1,
    width = "100%")

  # Weight preset
  sel_weights <- selectInput("ai_weight_preset",
    label = p("Utility weight preset"),
    choices = names(ai_weight_presets),
    selected = "Balanced",
    width = "100%")

  # Custom weight sliders (conditional)
  custom_sliders <- NULL
  if (!is.null(input$ai_weight_preset) && input$ai_weight_preset == "Custom") {
    custom_sliders <- tagList(
      sliderInput("ai_w_suppression",
        label = "Suppression weight",
        min = 0, max = 1, value = 0.33, step = 0.01, width = "100%"),
      sliderInput("ai_w_catloss",
        label = "Category loss weight",
        min = 0, max = 1, value = 0.33, step = 0.01, width = "100%"),
      sliderInput("ai_w_il1",
        label = "IL1 weight",
        min = 0, max = 1, value = 0.33, step = 0.01, width = "100%")
    )
  }

  # Live progress checkbox
  sel_live <- checkboxInput("ai_show_live",
    label = "Show live progress",
    value = FALSE)

  # Run button
  btn_run <- myActionButton("btn_ai_run", label = "Run AI Anonymization", "primary")

  out <- list(
    sel_provider,
    sel_model,
    sel_api_key,
    key_status,
    sel_base_url,
    tags$hr(),
    sel_k,
    sel_nstrat,
    tags$hr(),
    sel_weights,
    custom_sliders,
    sel_live,
    tags$hr(),
    fluidRow(column(12, btn_run, align = "center"))
  )
  out
})

# --- Provider change observer: update model default ---
observeEvent(input$ai_provider, {
  new_model <- ai_default_model(input$ai_provider)
  updateTextInput(session, "ai_model", value = new_model)
  obj$ai_provider <- input$ai_provider
})


# --- Main area ---
output$ui_ai_main <- renderUI({
  # No data loaded
  if (is.null(obj$inputdata)) {
    btn_goto_micro <- myActionButton("btn_ai_goto_micro",
      label = "Load microdata", "primary")
    return(fluidRow(
      column(12, h3("No input data available!"), class = "wb-header"),
      column(12, p("Go to the Microdata tab to upload a dataset."), class = "wb-header-hint"),
      column(12, div(btn_goto_micro, align = "center"))
    ))
  }

  # No SDC problem
  if (is.null(sdcObj())) {
    btn_goto_anon <- myActionButton("btn_ai_goto_anon",
      label = "Create an SDC problem", "primary")
    return(fluidRow(
      column(12, h3("No SDC problem was specified"), class = "wb-header"),
      column(12, p("Set up an SDC problem in the Anonymize tab before using AI-assisted anonymization."), class = "wb-header-hint"),
      column(12, div(btn_goto_anon, align = "center"))
    ))
  }

  # SDC problem exists: show summary and results
  curObj <- sdcObj()
  summary_info <- sdcMicro:::summarize_sdcObj_structure(curObj, k = if (!is.null(input$ai_k)) input$ai_k else 3)

  kv_text <- paste(summary_info$keyVars, collapse = ", ")
  nv_text <- if (length(summary_info$numVars) > 0) paste(summary_info$numVars, collapse = ", ") else "(none)"
  pv_text <- if (length(summary_info$pramVars) > 0) paste(summary_info$pramVars, collapse = ", ") else "(none)"

  cur_k <- if (!is.null(input$ai_k)) input$ai_k else 3

  summary_panel <- fluidRow(
    column(12, h3("AI-Assisted Anonymization"), class = "wb-header"),
    column(12, p("Use a large language model to automatically propose and evaluate anonymization strategies."),
      class = "wb-header-hint"),
    column(12,
      tags$table(class = "table table-condensed",
        style = "width: auto; margin-top: 10px;",
        tags$tr(tags$td(tags$strong("Key variables:")), tags$td(code(kv_text))),
        tags$tr(tags$td(tags$strong("Numerical variables:")), tags$td(code(nv_text))),
        tags$tr(tags$td(tags$strong("PRAM variables:")), tags$td(code(pv_text))),
        tags$tr(tags$td(tags$strong(paste0("k-anonymity violations (k=", cur_k, "):"))),
          tags$td(code(as.character(summary_info$k_violations))))
      )
    ),
    tags$hr()
  )

  # Results area (rendered separately)
  results_panel <- uiOutput("ui_ai_results_area")

  list(summary_panel, results_panel)
})


# --- Results area ---
output$ui_ai_results_area <- renderUI({
  res <- obj$ai_results
  if (is.null(res) || length(res) == 0) {
    return(fluidRow(
      column(12, p("Click ", tags$strong("Run AI Anonymization"), " in the sidebar to generate strategies."),
        align = "center", style = "margin-top: 20px;")
    ))
  }

  # Build the DT output
  output$ai_results_table <- DT::renderDataTable({
    n <- length(res)
    sel_idx <- obj$ai_selected_idx
    if (is.null(sel_idx) || sel_idx < 1 || sel_idx > n) sel_idx <- 1

    df <- data.frame(
      Strategy = vapply(res, function(r) {
        if (!is.null(r$strategy$name)) r$strategy$name else "unnamed"
      }, character(1)),
      U_score = vapply(res, function(r) round(r$score$total, 4), numeric(1)),
      Suppression = vapply(res, function(r) round(r$score$suppression_rate, 4), numeric(1)),
      Cat.Loss = vapply(res, function(r) round(r$score$category_loss, 4), numeric(1)),
      IL1 = vapply(res, function(r) round(r$score$il1, 4), numeric(1)),
      stringsAsFactors = FALSE
    )

    # Determine the best row (lowest U)
    best_row <- which.min(df$U_score)

    DT::datatable(df,
      selection = list(mode = "single", selected = sel_idx),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        searching = FALSE,
        ordering = TRUE,
        dom = "t",
        columnDefs = list(list(className = "dt-center", targets = 1:4))
      )
    ) |>
      DT::formatStyle("U_score",
        backgroundColor = DT::styleEqual(df$U_score[best_row], "#d4edda")
      )
  })

  # Reasoning block for selected strategy
  output$ai_selected_reasoning <- renderUI({
    sel_idx <- obj$ai_selected_idx
    if (is.null(sel_idx) || sel_idx < 1 || sel_idx > length(res)) {
      return(NULL)
    }
    reasoning <- res[[sel_idx]]$strategy$reasoning
    if (is.null(reasoning) || !nzchar(reasoning)) {
      return(p(tags$em("No reasoning provided for this strategy.")))
    }
    tags$blockquote(style = "border-left: 3px solid #ccc; padding-left: 10px; color: #555; margin-top: 10px;",
      p(tags$strong("Reasoning:"), reasoning)
    )
  })

  # Methods applied for selected strategy
  output$ai_selected_methods <- renderUI({
    sel_idx <- obj$ai_selected_idx
    if (is.null(sel_idx) || sel_idx < 1 || sel_idx > length(res)) {
      return(NULL)
    }
    calls <- res[[sel_idx]]$strategy$calls
    if (is.null(calls) || length(calls) == 0) {
      return(p(tags$em("No method details available.")))
    }
    k <- if (!is.null(input$ai_k)) as.integer(input$ai_k) else 3L
    code_lines <- ai_calls_to_code(calls, k = k)
    tags$details(
      tags$summary(style = "cursor: pointer; color: #337ab7; margin-top: 5px;",
        tags$strong("Methods applied"), paste0(" (", length(calls), " steps)")),
      tags$pre(style = "background: #f8f8f8; padding: 10px; margin-top: 5px; font-size: 0.85em;",
        paste(code_lines, collapse = "\n"))
    )
  })

  # Action buttons
  btn_apply <- myActionButton("btn_ai_apply", label = "Apply selected", "primary")
  btn_refine <- myActionButton("btn_ai_refine", label = "Refine further", "default")
  btn_cancel <- myActionButton("btn_ai_cancel", label = "Cancel", "danger")

  out <- fluidRow(
    column(12, h4("Strategy Comparison"), align = "center"),
    column(12, DT::dataTableOutput("ai_results_table")),
    column(12, uiOutput("ai_selected_reasoning")),
    column(12, uiOutput("ai_selected_methods")),
    column(12, tags$br(),
      div(btn_apply, btn_refine, btn_cancel,
        style = "display: flex; gap: 10px; justify-content: center;"),
      align = "center")
  )
  out
})


# --- DT row selection observer ---
observeEvent(input$ai_results_table_rows_selected, {
  sel <- input$ai_results_table_rows_selected
  if (!is.null(sel) && length(sel) == 1) {
    obj$ai_selected_idx <- sel
  }
})


# --- Navigation observers ---
observeEvent(input$btn_ai_goto_micro, {
  updateNavbarPage(session, inputId = "mainnav", selected = "Microdata")
})
observeEvent(input$btn_ai_goto_anon, {
  updateNavbarPage(session, inputId = "mainnav", selected = "Anonymize")
})
observeEvent(input$btn_ai_goto_risk, {
  removeModal()
  updateNavbarPage(session, inputId = "mainnav", selected = "Risk/Utility")
})


# --- Run button handler ---
observeEvent(input$btn_ai_run, {
  # Check sdcObj exists
  curObj <- sdcObj()
  if (is.null(curObj)) {
    showModal(modalDialog(
      p("Please create an SDC problem first before running AI anonymization."),
      title = "No SDC Problem",
      footer = modalButton("OK"),
      easyClose = TRUE
    ))
    return(NULL)
  }

  # Check API key
  api_key <- ai_get_api_key()
  if (!nzchar(api_key)) {
    showModal(modalDialog(
      p("No API key available. Please enter an API key in the sidebar or set the appropriate environment variable (OPENAI_API_KEY, ANTHROPIC_API_KEY, or LLM_API_KEY)."),
      title = "API Key Required",
      footer = modalButton("OK"),
      easyClose = TRUE
    ))
    return(NULL)
  }

  # Gather inputs
  provider <- if (!is.null(input$ai_provider)) input$ai_provider else "openai"
  model <- if (!is.null(input$ai_model) && nzchar(input$ai_model)) input$ai_model else NULL
  base_url <- if (provider == "custom" && !is.null(input$ai_base_url) && nzchar(input$ai_base_url)) input$ai_base_url else NULL
  k <- if (!is.null(input$ai_k)) input$ai_k else 3
  n_strategies <- if (!is.null(input$ai_n_strategies)) input$ai_n_strategies else 3
  weights <- ai_get_weights()

  # Clear previous results
  obj$ai_results <- NULL
  obj$ai_selected_idx <- 1

  # Progress bar
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Querying LLM for anonymization strategies...", value = 0)

  # Build prompt and query
  summary_info <- sdcMicro:::summarize_sdcObj_structure(curObj, k)
  tool_schemas <- sdcMicro:::get_tool_schemas()
  prompt <- sdcMicro:::build_agentic_prompt(summary_info, k, n_strategies, tool_schemas)

  # Query LLM
  strategies <- tryCatch({
    use_native_tools <- provider %in% c("openai", "anthropic")

    if (use_native_tools) {
      response <- sdcMicro:::query_llm(
        prompt = prompt,
        system_prompt = "You are an expert in statistical disclosure control.",
        provider = provider, model = model, api_key = api_key,
        base_url = base_url, tools = tool_schemas
      )
      if (length(response$tool_calls) > 0) {
        list(list(
          name = "native",
          reasoning = if (!is.null(response$content)) response$content else "",
          calls = response$tool_calls
        ))
      } else if (!is.null(response$content)) {
        parse_strategies_json(response$content)
      } else {
        stop("LLM returned neither tool calls nor content.")
      }
    } else {
      content <- sdcMicro:::query_llm(
        prompt = prompt,
        system_prompt = "You are an expert in statistical disclosure control.",
        provider = provider, model = model, api_key = api_key,
        base_url = base_url
      )
      parse_strategies_json(content)
    }
  }, error = function(e) {
    showModal(modalDialog(
      p("Failed to get strategies from LLM:"),
      tags$pre(e$message),
      title = "LLM Error",
      footer = modalButton("OK"),
      easyClose = TRUE
    ))
    return(NULL)
  })

  if (is.null(strategies)) return(NULL)

  progress$set(message = "Evaluating strategies...", value = 0.3)

  # Evaluate each strategy
  results <- list()
  for (i in seq_along(strategies)) {
    strat <- strategies[[i]]
    progress$set(
      message = sprintf("Evaluating strategy %d/%d: %s",
        i, length(strategies),
        if (!is.null(strat$name)) strat$name else paste("Strategy", i)),
      value = 0.3 + (0.6 * (i - 1) / length(strategies))
    )

    sdcObj_copy <- curObj
    sdcObj_copy <- tryCatch({
      sdcObj_copy <- sdcMicro:::execute_tool_calls(sdcObj_copy, strat$calls)
      sdcObj_copy <- localSuppression(sdcObj_copy, k = k)
      sdcObj_copy
    }, error = function(e) {
      NULL
    })
    if (is.null(sdcObj_copy)) next

    score <- sdcMicro:::ai_utility_score(curObj, sdcObj_copy, weights)
    results[[length(results) + 1]] <- list(
      strategy = strat, sdcObj = sdcObj_copy, score = score
    )
  }

  progress$set(message = "Done.", value = 1)

  if (length(results) == 0) {
    showModal(modalDialog(
      p("All strategies failed during evaluation. Please try adjusting your SDC problem setup or try again."),
      title = "All Strategies Failed",
      footer = modalButton("OK"),
      easyClose = TRUE
    ))
    return(NULL)
  }

  # Find best
  best_idx <- which.min(vapply(results, function(r) r$score$total, numeric(1)))

  obj$ai_results <- results
  obj$ai_selected_idx <- best_idx
})


# --- Apply button handler ---
observeEvent(input$btn_ai_apply, {
  res <- obj$ai_results
  sel_idx <- obj$ai_selected_idx
  if (is.null(res) || is.null(sel_idx) || sel_idx < 1 || sel_idx > length(res)) {
    return(NULL)
  }

  selected <- res[[sel_idx]]

  # Generate reproducible R code for the script
  k <- if (!is.null(input$ai_k)) as.integer(input$ai_k) else 3L
  code_lines <- ai_calls_to_code(selected$strategy$calls, k = k)
  strat_name <- if (!is.null(selected$strategy$name)) selected$strategy$name else "AI strategy"
  code_block <- paste0(
    "## AI-assisted anonymization: ", strat_name, "\n",
    paste(code_lines, collapse = "\n"), "\n"
  )
  obj$code_anonymize <- c(obj$code_anonymize, code_block)

  obj$sdcObj <- selected$sdcObj
  obj$ai_results <- NULL
  obj$ai_selected_idx <- 1

  showModal(modalDialog(
    title = "AI Anonymization Applied",
    p(tags$strong(strat_name), "was successfully applied",
      paste0("(utility score: ", round(selected$score$total, 4), ").")),
    p("We recommend reviewing the results:"),
    tags$ul(
      tags$li("Go to ", tags$strong("Risk/Utility"), " to inspect disclosure risk and information loss."),
      tags$li("Use ", tags$strong("Obs. violating k-anon"), " (in the Risk/Utility sidebar) to check k-anonymity."),
      tags$li("Go to ", tags$strong("Reproducibility"), " to see the generated R script.")
    ),
    footer = tagList(
      myActionButton("btn_ai_goto_risk", "Go to Risk/Utility", "primary"),
      modalButton("Close")
    ),
    easyClose = TRUE
  ))
})


# --- Refine button handler ---
observeEvent(input$btn_ai_refine, {
  res <- obj$ai_results
  curObj <- sdcObj()
  if (is.null(res) || length(res) == 0 || is.null(curObj)) return(NULL)

  api_key <- ai_get_api_key()
  if (!nzchar(api_key)) {
    showModal(modalDialog(
      p("No API key available."),
      title = "API Key Required",
      footer = modalButton("OK"),
      easyClose = TRUE
    ))
    return(NULL)
  }

  provider <- if (!is.null(input$ai_provider)) input$ai_provider else "openai"
  model <- if (!is.null(input$ai_model) && nzchar(input$ai_model)) input$ai_model else NULL
  base_url <- if (provider == "custom" && !is.null(input$ai_base_url) && nzchar(input$ai_base_url)) input$ai_base_url else NULL
  k <- if (!is.null(input$ai_k)) input$ai_k else 3
  weights <- ai_get_weights()

  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Requesting refined strategy from LLM...", value = 0)

  summary_info <- sdcMicro:::summarize_sdcObj_structure(curObj, k)
  ref_prompt <- sdcMicro:::build_refinement_prompt(res, summary_info, k)

  ref_content <- tryCatch({
    sdcMicro:::query_llm(
      prompt = ref_prompt,
      system_prompt = "You are an expert in statistical disclosure control. Always respond with valid JSON only, no markdown or prose.",
      provider = provider, model = model, api_key = api_key,
      base_url = base_url
    )
  }, error = function(e) {
    showModal(modalDialog(
      p("Failed to get refinement from LLM:"),
      tags$pre(e$message),
      title = "LLM Error",
      footer = modalButton("OK"),
      easyClose = TRUE
    ))
    return(NULL)
  })

  if (is.null(ref_content)) return(NULL)

  # Handle query_llm returning a list (native mode) vs character (text mode)
  if (is.list(ref_content)) ref_content <- ref_content$content

  progress$set(message = "Parsing refined strategy...", value = 0.4)

  refined <- tryCatch(
    sdcMicro:::parse_single_strategy_json(ref_content),
    error = function(e) {
      showModal(modalDialog(
        p("Could not parse the refinement response from LLM:"),
        tags$pre(e$message),
        title = "Parse Error",
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
      NULL
    }
  )
  if (is.null(refined)) return(NULL)

  progress$set(message = "Evaluating refined strategy...", value = 0.6)

  sdcObj_copy <- curObj
  sdcObj_copy <- tryCatch({
    sdcObj_copy <- sdcMicro:::execute_tool_calls(sdcObj_copy, refined$calls)
    sdcObj_copy <- localSuppression(sdcObj_copy, k = k)
    sdcObj_copy
  }, error = function(e) {
    showModal(modalDialog(
      p("Refined strategy failed during evaluation:"),
      tags$pre(e$message),
      title = "Evaluation Error",
      footer = modalButton("OK"),
      easyClose = TRUE
    ))
    NULL
  })
  if (is.null(sdcObj_copy)) return(NULL)

  score <- sdcMicro:::ai_utility_score(curObj, sdcObj_copy, weights)
  result_entry <- list(strategy = refined, sdcObj = sdcObj_copy, score = score)

  # Append to results
  updated_results <- obj$ai_results
  updated_results[[length(updated_results) + 1]] <- result_entry
  obj$ai_results <- updated_results

  # Update selection to best
  scores <- vapply(obj$ai_results, function(r) r$score$total, numeric(1))
  obj$ai_selected_idx <- which.min(scores)

  progress$set(message = "Done.", value = 1)

  showNotification(
    paste0("Refined strategy added: ",
      if (!is.null(refined$name)) refined$name else "refined",
      " (U=", round(score$total, 4), ")"),
    type = "message",
    duration = 5
  )
})


# --- Cancel button handler ---
observeEvent(input$btn_ai_cancel, {
  obj$ai_results <- NULL
  obj$ai_selected_idx <- 1
})


# --- Panel assembly ---
output$ui_ai_panel <- renderUI({
  fluidRow(
    column(3, uiOutput("ui_ai_sidebar"), class = "wb_sidebar"),
    column(9, uiOutput("ui_ai_main"), class = "wb-maincolumn")
  )
})
