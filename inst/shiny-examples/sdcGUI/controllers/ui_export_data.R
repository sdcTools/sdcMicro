# UI-Output for exporting the report
output$ui_export_report <- renderUI({
  rb1 <- radioButtons("rb_simple_report", strong("Type of Report"),
    choices=c("internal (detailed)"="internal", "external (short overview)"="external"),
    inline=TRUE, selected=input$rb_simple_report)
  db <- downloadButton('downloadReport', 'Download the Report')

  fluidRow(
    column(12, h4("Export the anonymization report", align="center")),
    column(12, p(rb1, align="center")),
    column(12, p(db, align="center")))
})

# UI-Output for exporting the (anonymized) data
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
    column(12, h4("Export the anonymized microdata"), align="center"),
    column(12, strong("View the data", align="center")),
    column(12, dataTableOutput("dt_exportData")),
    column(12, strong("Select file-format", align="center")),
    column(12, rb_exptype, align="center")
  )

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

    out <- list(out, fluidRow(
      column(12, uiOutput("sel_randomize_export"), align="center"),
      column(12, help_randomize, align="center")
    ))

    db <- downloadButton('downloadData', 'Download the anonymized dataset')
    out <- list(out, fluidRow(column(12, db, align="center")))
  }
  out
})

# UI-Output for Tab 'Export Data'
output$ui_export_main <- renderUI({
  output$downloadData <- downloadHandler(
    filename=function() {
      paste0("anon_data_",format(Sys.time(), "%Y%m%d_%H%M"),".",input$dat_exp_type)
    },
    content=function(file) {
      type <- input$dat_exp_type
      cmd <- paste0("writeSafeFile(obj=sdcObj, format=",shQuote(type), ", randomizeRecords=",shQuote(input$sel_export_randomizeorder))
      cmd <- paste0(cmd, ", fileOut=",shQuote(file))

      dat <- exportData()
      if (type=="rdata") {
        save(dat, file=file)
      }
      if (type=="sav") {
        write_sav(data=dat, path=file)
      }
      if (type=="dta") {
        write_dta(data=dat, path=file)
      }
      if (type=="csv") {
        write.table(dat, file=file, col.names=as.logical(input$export_csv_header),
          sep=input$export_csv_sep, dec=input$export_csv_dec)
        cmd <- paste0(cmd, ", col.names=",as.logical(input$export_csv_header))
        cmd <- paste0(cmd, ", sep=",shQuote(input$export_csv_sep))
        cmd <- paste0(cmd, ", dec=",shQuote(input$export_csv_dec))
      }
      cmd <- paste0("## export anonymized data file\n",cmd,")\n")
      #cmd <- paste0("## return anonymized microdata\nextractManipData(sdcObj, randomizeRecords=",shQuote(input$sel_export_randomizeorder),")")
      obj$code_anonymize <- c(obj$code_anonymize, cmd)
    }
  )
  output$downloadReport <- downloadHandler(
    filename=function() {
      paste0("report_",format(Sys.time(), "%Y%m%d_%H%M"),".html")
    },
    content=function(file) {
      internal <- ifelse(input$rb_simple_report=="internal", TRUE, FALSE)
      pout <- getwd()
      if (internal) {
        tmpF <- paste0("sdcReport_internal_",format(Sys.time(), "%Y%m%d_%H%M"))
      } else {
        tmpF <- paste0("sdcReport_external_",format(Sys.time(), "%Y%m%d_%H%M"))
      }
      curObj <- sdcObj()
      report(curObj, outdir=pout, filename=tmpF, title="SDC-Report", internal=internal)

      file.copy(paste0(pout,"/",tmpF,".html"), file)

      cmd <- paste0("report(sdcObj, outdir=",shQuote(pout),", filename=",shQuote(tmpF), ", title=",shQuote("SDC-Report"))
      cmd <- paste0(cmd, ", internal=",internal,")")
      cmd <- paste0("## Create Report\n",cmd,"\n")
      obj$code_anonymize <- c(obj$code_anonymize, cmd)
    }
  )

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

