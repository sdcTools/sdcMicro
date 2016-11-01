# UI-Output for exporting the report
output$ui_export_report_btn <- renderUI({
  req(input$rb_simple_report, input$report_path)
  if (!dir.exists(input$report_path)) {
    return(myActionButton("myRepDownload_xxx", "Error: The specified directory does not exist!", btn.style="danger"))
  }
  if (file.access(input$report_path, mode=2)!=0) {
    return(myActionButton("myRepDownload_xxx", "Error: The specified directory is not writeable!", btn.style="danger"))
  }
  return(myActionButton("myRepDownload", "Save the report", btn.style="primary"))
})

output$ui_export_report <- renderUI({
  rb1 <- radioButtons("rb_simple_report", strong("Type of Report"),
    choices=c("internal (detailed)"="internal", "external (short overview)"="external"),
    inline=TRUE, selected=input$rb_simple_report)
  pp <- textInput("report_path", label=h5("Enter a directory where you want to write the file to"),
    placeholder=paste("e.g:",getwd()), width="75%")
  out <- fluidRow(
    column(12, h4("Export the anonymization report"), align="center"))

  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Trying to generate a report returned the following error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))))
  }
  out <- list(out, fluidRow(
    column(12, p("You can choose to create either an internal (detailed) or an external (non-detailed) report."), align="center")))

  out <- list(out, fluidRow(
    column(12, rb1, align="center"),
    column(12, pp, align="center"),
    column(12, uiOutput("ui_export_report_btn"), align="center")))

  if (!is.null(obj$lastreport)) {
    out <- list(out, fluidRow(
      column(12, tags$br(), p("Information: the last report you created was saved in", code(obj$lastreport)), align="center")))
    if (!rmarkdown::pandoc_available()) {
      out <- list(out, fluidRow(
      column(12, tags$br(), p("Note: The report was saved as a simple html-file with no stylesheet attached,
        because no current installation of",code(dQuote(pandoc)),"could be found."), align="center")))
    }
  }
  out
})

observeEvent(input$report_path, {
  obj$report_path <- input$report_path
})

# UI-Output for exporting the (anonymized) data
output$ui_export_data_btn <- renderUI({
  req(input$dat_exp_type, input$sel_export_randomizeorder, input$dataexport_path)
  if (!dir.exists(input$dataexport_path)) {
    return(myActionButton("btn_export_anon_data_xxx", "Error: The specified directory does not exist!", btn.style="danger"))
  }
  if (file.access(input$dataexport_path, mode=2)!=0) {
    return(myActionButton("btn_export_anon_data_xxx", "Error: The specified directory is not writeable!", btn.style="danger"))
  }
  return(myActionButton("btn_export_anon_data", "Save the anonymized dataset", btn.style="primary"))
})
output$ui_export_data <- renderUI({
  output$dt_exportData <- DT::renderDataTable({
    exportData()
  }, options=list(scrollX=TRUE, lengthMenu=list(c(10, 25, 100, -1), c('10', '20', '100', 'All')), pageLength=20), rownames=FALSE)

  # specific (gui)-options for csv-export
  output$ui_export_csv <- renderUI({
    rb1 <- radioButtons("export_csv_header", label=h4("First row contains variable names"), choices=c(TRUE,FALSE), inline=FALSE)
    rb2 <- radioButtons("export_csv_sep", label=h4("Separator"), choices=c("Semicolon (;)"=";", Tab="\t", "Comma (,)"=","), inline=FALSE)
    rb3 <- radioButtons("export_csv_dec", label=h4("Decimal-Character"), choices=c("Dot (.)"=".", "Comma (,)"=","), inline=FALSE)
    return(fluidRow(
      column(4, p(rb1, align="center")),
      column(4, p(rb2, align="center")),
      column(4, p(rb3, align="center"))))
  })
  output$ui_export_spss <- renderUI({
    txt <- list("The file will be written using", code("write_sav()"), "from package", code("haven"),".")
    fluidRow(column(12, p(txt, align="center")))
  })
  output$ui_export_stata <- renderUI({
    txt <- list("The file will be written using", code("write_dta()"), "from package", code("haven"),".")
    fluidRow(column(12, p(txt, align="center")))
  })
  rb_exptype <- radioButtons("dat_exp_type", label=NULL,
    choices=c("R-Dataset"="rdata","SPSS-File"="sav",
      "Comma-seperated File"="csv", "STATA-File"="dta"),
    selected=input$dat_exp_type, width="100%", inline=TRUE)

  out <- fluidRow(
    column(12, h4("Export the anonymized microdata"), align="center"))

  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Trying to export the anonymized data resulted in the following error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))))
  }
  out <- list(out, fluidRow(
    column(12, strong("Analyze the data", align="center")),
    column(12, dataTableOutput("dt_exportData")),
    column(12, strong("Select file-format", align="center")),
    column(12, rb_exptype, align="center")
  ))

  if (!is.null(input$dat_exp_type)) {
    if (input$dat_exp_type == "csv") {
      out <- list(out, uiOutput("ui_export_csv"))
    }
    if (input$dat_exp_type == "sav") {
      out <- list(out, uiOutput("ui_export_spss"))
    }
    if (input$dat_exp_type == "dta") {
      out <- list(out, uiOutput("ui_export_stata"))
    }

    output$sel_randomize_export <- renderUI({
      curObj <- sdcObj()
      if (is.null(curObj)) {
        return(NULL)
      }
      lab <- "Randomize Order of Observations"
      choices <- c("Do not randomize"="no","Perform random swapping of IDs"="simple",
        "Randomize by cluster/household id"="byHH", "Randomize within cluster/household id"="withinHH")
      if (!is.null(curObj@hhId)) {
        sel_randomize <- selectInput("sel_export_randomizeorder", label=h5(lab), choices=choices,
          selected=input$sel_export_randomizeorder)
      } else {
        sel_randomize <- selectInput("sel_export_randomizeorder", label=h5(lab), choices=choices[1:2],
          selected=input$sel_export_randomizeorder)
      }
      sel_randomize
    })
    help_randomize <- helpText("If you want to randomize the order of the observations, please specify",tags$i("yes"),".")
    pp <- textInput("dataexport_path", label=h5("Enter a directory in which you want to save the anonymized file"),
        placeholder=paste("e.g:",getwd()), width="75%")
    out <- list(out, fluidRow(
      column(6, uiOutput("sel_randomize_export"), align="center"),
      column(6, pp, align="center")))
    out <- list(out, fluidRow(
      column(6, help_randomize, align="center"),
      column(6, NULL, align="center")
    ))
    out <- list(out, fluidRow(column(12, uiOutput("ui_export_data_btn"), align="center")))

    if (!is.null(obj$lastdataexport)) {
      out <- list(out, fluidRow(
        column(12, tags$br(), p("Information: the last data you have exported was saved as", code(obj$lastdataexport)), align="center")))
    }
  }
  out
})

# update reactive variable
observeEvent(input$dataexport_path, {
  obj$dataexport_path <- input$dataexport_path
})

# UI-Output for Tab 'Export Data'
output$ui_export_main <- renderUI({
  out <- NULL
  if (is.null(sdcObj())) {
    return(list(out, noSdcProblem(uri="ui_export_data")))
  }

  if (!is.null(input$sel_what_export)) {
    if (input$sel_what_export=="exp_data") {
      out <- list(out, uiOutput("ui_export_data"))
    }
    if (input$sel_what_export=="report") {
      out <- list(out, uiOutput("ui_export_report"))
    }
  }
  out
})

output$ui_export_sidebar_left <- renderUI({
  rb <- radioButtons("sel_what_export", label=h4("What do you want to export?"),
    choices=c("Anonymized Data"="exp_data","Anonymization Report"="report"),
    selected=input$sel_what_export, width="100%")
  fluidRow(column(12, rb))
})

output$ui_export <- renderUI({
  if (is.null(sdcObj())) {
    return(noSdcProblem(uri="ui_export_data"))
  }
  return(fluidRow(
    column(2, uiOutput("ui_export_sidebar_left")),
    column(10, uiOutput("ui_export_main"))))
})
