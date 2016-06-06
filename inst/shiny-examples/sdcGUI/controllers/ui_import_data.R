# specific (gui)-options for csv-import
output$ui_import_csv <- renderUI({
  rb1 <- radioButtons("import_csv_header", label=h4("First row contains variable names"), choices=c(TRUE,FALSE), inline=TRUE)
  rb2 <- radioButtons("import_csv_sep", label=h4("Separator"), choices=c(Semicolon=";", Tab="\t", Colon=","), inline=TRUE)
  out <- list(
    htmlTemplate("tpl_two_col.html",inp1=rb1, inp2=rb2)
  )
  return(out)
})

# specific (gui)-options for r-dataframe import
output$ui_import_rdf <- renderUI({
  selDF <- selectInput("sel_choose_df", label=NULL, choices=available_dfs,
    selected=input$sel_choose_df, width="100%")
  btn <- myActionButton("btn_chooose_df",label=("Use this data.frame"), "primary")
  out <- list(
    htmlTemplate("tpl_one_col.html",inp=h4("Select R-Dataset")),
    htmlTemplate("tpl_one_col.html",inp=selDF),
    htmlTemplate("tpl_one_col.html",inp=btn)
  )
  out
})

# specific (gui)-options for sas import
output$ui_import_sas <- renderUI({
  return(NULL)
})

# specific (gui)-options for spss import
output$ui_import_spss <- renderUI({
  return(NULL)
})

# specific (gui)-options for stata import
output$ui_import_stata <- renderUI({
  return(NULL)
})

output$ui_import_data <- renderUI({
  cur_error <- lastError()
  btn <- myActionButton("btn_reset_inputerror",label=("Try again!"), "primary")
  if (!is.null(lastError())) {
    return(list(
      htmlTemplate("tpl_one_col.html", inp=h4("Importing data resulted in an error!")),
      htmlTemplate("tpl_one_col.html", inp=verbatimTextOutput("ui_lasterror")),
      htmlTemplate("tpl_one_col.html", inp=btn)))
  }

  dat_type <- selectInput("dat_type", label=NULL,
    choices=c("Choose data.frame"="rdf","R-Dataset"="R",
      "SPSS-File"="spss","SAS-File"="sas","Comma-seperated File"="csv",
      "STATA-File"="stata"), selected=input$dat_type, width="100%")
  out <- list(
    htmlTemplate("tpl_one_col.html", inp=h2("Upload Data")),
    htmlTemplate("tpl_one_col.html", inp=h4("Select data source")),
    htmlTemplate("tpl_one_col.html", inp=dat_type)
  )

  if (!is.null(input$dat_type)) {
    if (input$dat_type == "csv") {
      allowed <- c(".txt",".csv")
      out <- list(out, uiOutput("ui_import_csv"))
    }
    if (input$dat_type == "spss") {
      allowed <- c(".sav")
      out <- list(out, uiOutput("ui_import_spss"))
    }
    if (input$dat_type == "sas") {
      allowed <- c(".sas7bdat")
      out <- list(out, uiOutput("ui_import_sas"))
    }
    if (input$dat_type == "R") {
      allowed <- c(".rdata")
    }
    if (input$dat_type == "stata") {
      allowed <- c(".dta")
      out <- list(out, uiOutput("ui_import_stata"))
    }

    if (input$dat_type %in% c("R","csv","spss","sas","rdata","stata")) {
      # convert characters automatically to factors
      rb1 <- radioButtons("rb_convert_c_to_f", label=h4("Convert character-vectors to factors?"), choices=c(TRUE, FALSE), inline=TRUE)
      rb2 <- radioButtons("rb_drop_all_missings", label=h4("Drop variables with only NA-values"), choices=c(TRUE, FALSE), inline=TRUE)
      out <- list(out, htmlTemplate("tpl_two_col.html", inp1=rb1, inp2=rb2))
      fI <- fileInput("file1", h4(paste0("Select File (allowed types are '",paste0(allowed, collapse="'"),"')")),
        width="100%", accept=allowed)
      out <- list(out, htmlTemplate("tpl_one_col.html", inp=fI))
    } else {
      out <- list(out, uiOutput("ui_import_rdf"))
    }
  }
  out
})


output$ui_inputdata <- renderUI({
  if ( is.null(obj$inputdata)) {
    uiOutput("ui_import_data")
  } else {
    uiOutput("ui_modify_data")
  }
})
