# UI-Output for exporting the report
output$ui_export_report <- renderUI({
  rb1 <- radioButtons("rb_simple_report", h5("Type of Report"),
    choices=c("internal (detailed)"="internal", "external (short overview)"="external"),
    inline=TRUE, selected=input$rb_simple_report)
  out <- fluidRow(
    column(12, h4("Create anonymization report"), align="center"))

  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Trying to generate a report returned the following error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))))
  }
  out <- list(out, fluidRow(
    column(12, p("A report for internal use (more detailed) or a report for external use (less detailed) is saved to the export directory."), align="center")))

  out <- list(out, fluidRow(
    column(12, rb1, align="center"),
    column(12, myActionButton("myRepDownload", "Save report", btn.style="primary"), align="center")))

  if (!is.null(obj$lastreport)) {
    out <- list(out, fluidRow(
      column(12, tags$br(), p("The report was saved as", code(obj$lastreport)), align="center")))
    if (!rmarkdown::pandoc_available()) {
      out <- list(out, fluidRow(
      column(12, tags$br(), p("Note: The report was saved as a simple html-file with no stylesheet attached,
        because no current installation of",code(dQuote(pandoc)),"could be found."), align="center")))
    }
  }
  out
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
    req(input$rb_export_randomizeorder)
    exportData()
  }, options=list(scrollX=TRUE, lengthMenu=list(c(10, 20, 50, 100, -1), c('10', '20', '50', '100', 'All')), pageLength=10), rownames=FALSE)

  # specific (gui)-options for csv-export
  output$ui_export_csv <- renderUI({
    txt <- list("Set options for exporting a", code("csv"), "file")
    rb1 <- radioButtons("export_csv_header", label=h5("Include variable names in first row?"), choices=c("Yes"=TRUE,"No"=FALSE), inline=TRUE)
    rb2 <- radioButtons("export_csv_sep", label=h5("Field separator"), choices=c(Comma=",", Semicolon=";", Tab="\t"), inline=TRUE)
    rb3 <- radioButtons("export_csv_dec", label=h5("Decimal separator"), choices=c("Decimal point"=".", "Decimal comma"=","), inline=TRUE)
    return(fluidRow(
      column(12, p(txt, align="center")),
      column(4, rb1, align="center"),
      column(4, rb2, align="center"),
      column(4, rb3, align="center")))
  })
  output$ui_export_spss <- renderUI({
    txt <- list("The file will be exported using", code("write_sav()"), "from package", code("haven"),".")
    fluidRow(column(12, p(txt, align="center")))
  })
  output$ui_export_sas <- renderUI({
    txt <- list("The file will be exported using", code("write_sas()"), "from package", code("haven"),".")
    fluidRow(column(12, p(txt, align="center")))
  })
  output$ui_export_stata <- renderUI({
    txt <- list("The file will be exported using", code("write_dta()"), "from package", code("haven"),".")
    fluidRow(column(12, p(txt, align="center")))
  })
  rb_exptype <- radioButtons("dat_exp_type", label=h5("Select file format for export", align="center"),
    choices=c("R-dataset (.RData)"="rdata","SPSS-file (.sav)"="sav","CSV-file (.csv)"="csv", "STATA-file (.dta)"="dta", "SAS-file (.sas7bdat)"="sas"),
    width="100%", selected=input$dat_exp_type, inline=TRUE)

  out <- fluidRow(
    column(12, h4("Export anonymized microdata"), align="center"))

  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Trying to export the anonymized data resulted in the following error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))))
  }
  out <- list(out, fluidRow(
    column(12, h5("View anonymized data"), align="center"),
    column(12, DT::dataTableOutput("dt_exportData")),
    column(12, rb_exptype, align="center")
  ))

  if (!is.null(input$dat_exp_type)) {
    if (input$dat_exp_type == "csv") {
      out <- list(out, uiOutput("ui_export_csv"))
    }
    if (input$dat_exp_type == "sav") {
      out <- list(out, uiOutput("ui_export_spss"))
    }
    if (input$dat_exp_type == "sas") {
      out <- list(out, uiOutput("ui_export_sas"))
    }
    if (input$dat_exp_type == "dta") {
      out <- list(out, uiOutput("ui_export_stata"))
    }

    output$rb_randomize_export <- renderUI({
      curObj <- sdcObj()
      if (is.null(curObj)) {
        return(NULL)
      }
      txt_randomize     <- "Often the order of the records can be used to reconstruct the original data, such as suppressed values. To prevent this, it is advisable to randomize the order of records before releasing the data."
      txt_randomize     <- paste(txt_randomize, "In a subsequent step, the record IDs should be replaced, if any are available in the data.")
      txt_randomize_ind <- paste(txt_randomize, "The records are randomized at the record level.", tags$br(), tags$br(), "If this is the household level file to be merged with the individual level file, randomization should be postponed to the merged file.")
      txt_randomize_hh  <- paste(txt_randomize, "If a hierarchical structure is available in the data, randomization should occur by the hierarchcial identifier and, if necessary, within the hierachical units,")
      txt_randomize_hh  <- paste(txt_randomize_hh, " to preserve the hierarchical structure.", tags$br(), tags$br(), "Two options are available:", tags$br())
      txt_randomize_hh  <- paste(txt_randomize_hh, " - Randomize by hierarchical identifier: the order of the hierchical units is randomized and the order of the records within the hierarchical units is preserved", tags$br())
      txt_randomize_hh  <- paste(txt_randomize_hh, " - Randomize by hierarchical identifier and within hierarchical units: the order of the hierchical units is randomized as well as the order of the records within the hierarchcial units")
      choices <- c("No randomization"="no","Randomization at record level"="simple",
        "Randomize by hierarchical identifier"="byHH", "Randomize by hierarchical identifier and within hierarchical units"="withinHH")

      if (!is.null(curObj@hhId)) {
        rb <- radioButtons("rb_export_randomizeorder", label=h5("Randomize order of records", tipify(icon("question"), title=txt_randomize_hh, placement="top")), choices=choices[-2], inline=TRUE)
      } else {
        rb <- radioButtons("rb_export_randomizeorder", label=h5("Randomize order of records", tipify(icon("question"), title=txt_randomize_ind, placement="top")), choices=choices[1:2], inline=TRUE)
      }
      return(fluidRow(
        column(12, rb, align="center")
      ))
    })
    out <- list(out, uiOutput("rb_randomize_export"))
    out <- list(out, fluidRow(
      column(12, myActionButton("btn_export_anon_data", "Save dataset", btn.style="primary"), align="center")
    ))

    if (!is.null(obj$lastdataexport) & is.null(lastError())) {
      out <- list(out, fluidRow(
        column(12, tags$br(), p("The dataset was saved as", code(obj$lastdataexport)), align="center")))
    }
  }
  out
})

# modify stata variable labels
output$ui_modify_stata_labels <- renderUI({
  stataVarnames <- reactive({
    df <- obj$stata_varnames
    if (is.null(df)) {
      return(NULL)
    }
    df
  })
  output$statlab_table <- renderRHandsontable({
    df <- stataVarnames()
    if (is.null(df)) {
      return(NULL)
    }
    m <- rhandsontable(df, stretchH="all", rowHeaders=NULL) %>%
      hot_col("var.names", readOnly = TRUE)
    m
  })
  # update reactive value on table change
  observe({
    if (!is.null(input$statlab_table)) {
      tmpdf <- hot_to_r(input$statlab_table)
      obj$stata_varnames$var.label <- tmpdf$var.label
    }
  })

  out <- fluidRow(
    column(12, h4("Change Stata value labels"), align="center"),
    column(12, p("Modify variable labels for Stata here. The information you enter will be used if you save the anonymized data set
      as Stata-file."), align="center")
  )
  out <- fluidRow(
    column(12, rHandsontableOutput("statlab_table"))
  )
  out
})


# UI-Output for Tab 'Export Data'
output$ui_export_main <- renderUI({
  out <- NULL
  if (is.null(sdcObj())) {
    return(list(out, noSdcProblem(uri="ui_export_data")))
  }
  val <- obj$cur_selection_exports
  if (val=="btn_export_results_1") {
    return(uiOutput("ui_export_data"))
  }
  if (val=="btn_export_results_2") {
    return(uiOutput("ui_export_report"))
  }
  if (val=="btn_export_results_3") {
    return(uiOutput("ui_modify_stata_labels"))
  }
  out
})

output$ui_export_sidebar_left <- renderUI({
  choices_export <- reactive({
    cc <- c("Anonymized Data", "Anonymization Report")
    curLabs <- stataLabs()
    if (!is.null(curLabs)) {
      cc <- c(cc, "Change Stata Labels")
    }
    cc
  })
  output$ui_sel_export_btns <- renderUI({
    cc <- choices_export()
    out <- fluidRow(column(12, h4("What do you want to export?"), align="center"))
    for (i in 1:length(cc)) {
      id <- paste0("btn_export_results_", i)
      if (obj$cur_selection_exports==id) {
        style <- "primary"
      } else {
        style <- "default"
      }
      out <- list(out, fluidRow(
        column(12, bsButton(id, label=cc[i], block=TRUE, size="extra-small", style=style), tags$br())
      ))
    }
    return(out)
  })
  # required observers that update the color of the active button!
  eval(parse(text=genObserver_menus(pat="btn_export_results_", n=1:3, updateVal="cur_selection_exports")))
  return(uiOutput("ui_sel_export_btns"))
})

output$ui_export <- renderUI({
  if (is.null(sdcObj())) {
    return(list(
      noSdcProblem(uri="ui_export_data"),
      fluidRow(column(12, tags$br(), p("or go to Undo tab by clicking the button below and upload a previously saved problem instance"), align="center")),
      fluidRow(column(12, myActionButton("nodata_export_uploadproblem", label="Upload a previously saved problem", btn.style="primary"), align="center"))
    ))
  }
  return(fluidRow(
    column(2, uiOutput("ui_export_sidebar_left")),
    column(10, uiOutput("ui_export_main"))))
})
