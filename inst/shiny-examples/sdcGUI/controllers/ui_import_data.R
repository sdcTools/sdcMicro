# specific (gui)-options for csv-import
output$ui_import_csv <- renderUI({
  rb1 <- radioButtons("import_csv_header", label=h5("First row contains variable names"), choices=c(TRUE,FALSE), inline=TRUE)
  rb2 <- radioButtons("import_csv_sep", label=h5("Separator"), choices=c(Semicolon=";", Tab="\t", Colon=","), inline=TRUE)
  return(fluidRow(
    column(6, p(rb1, align="center")),
    column(6, p(rb2, align="center"))))
})

# specific (gui)-options for r-dataframe import
output$ui_import_rdf <- renderUI({
  selDF <- selectInput("sel_choose_df", label=NULL, choices=available_dfs,
    selected=input$sel_choose_df, width="50%")
  btn <- myActionButton("btn_chooose_df",label=("Use this data.frame"), "primary")
  return(fluidRow(
    column(12, h5("Select an existing object from your workspace", align="center")),
    column(12, div(selDF, align="center")),
    column(12, p(btn, align="center"))))
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

output$ui_import_data_main <- renderUI({
  cur_error <- lastError()
  btn <- myActionButton("btn_reset_inputerror",label=("Try again!"), "primary")
  if (!is.null(lastError())) {
    return(fluidRow(
      column(12, h4("Importing data resulted in an error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror")),
      column(12, p(btn, align="center"))))
  }

  out <- fluidRow(
    column(12, h4("Uploading microdata", align="center")))
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
      rb1 <- radioButtons("rb_convert_c_to_f", label=h5("Convert character-vectors to factors?"), choices=c(TRUE, FALSE), inline=TRUE)
      rb2 <- radioButtons("rb_drop_all_missings", label=h5("Drop variables with only NA-values"), choices=c(TRUE, FALSE), inline=TRUE)

      out <- list(out, fluidRow(column(12, h5("Set additional options for the data import", align="center"))))

      out <- list(out, fluidRow(
        column(6, p(rb1, align="center")),
        column(6, p(rb2, align="center"))))

      out <- list(out, fluidRow(
        column(12, p("Note: the selected file file is loaded immediately. Set options before selecting the file."), align="center")
      ))

      fI <- fileInput("file1", h5(paste0("Select File (allowed types are '",paste0(allowed, collapse="'"),"')")),
        width="100%", accept=allowed)
      out <- list(out, fluidRow(column(12, p(fI, align="center"))))
    } else {
      out <- list(out, uiOutput("ui_import_rdf"))
    }
  }
  out
})

output$ui_import_data_sidebar_left <- renderUI({
  dat_type <- radioButtons("dat_type", label=NULL,
    choices=c("Use testdata/internal data"="rdf","R-Dataset (.rdata)"="R",
      "SPSS-File (.sav)"="spss","SAS-File (.sasb7dat)"="sas","Comma-seperated File (.csv)"="csv",
      "STATA-File (.dta)"="stata"), selected=input$dat_type, width="100%")
  fluidRow(
    column(12, h4("Select data source")),
    column(12, p(dat_type, align="center")))
})
output$ui_import_data <- renderUI({
  fluidRow(
    column(4, uiOutput("ui_import_data_sidebar_left")),
    column(8, uiOutput("ui_import_data_main")))
})

output$ui_inputdata <- renderUI({
  if ( is.null(obj$inputdata)) {
    uiOutput("ui_import_data")
  } else {
    uiOutput("ui_modify_data")
  }
})
