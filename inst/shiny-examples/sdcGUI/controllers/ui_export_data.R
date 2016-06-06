# UI-Output for exporting the report
output$ui_export_report <- renderUI({
  rb1 <- radioButtons("rb_simple_report", h4("Type of Report"),
    choices=c("internal"="internal", "external"="external"),
    inline=TRUE, selected=input$rb_simple_report)
  db <- downloadButton('downloadReport', 'Download the Report')
  out <- list(
    htmlTemplate("tpl_one_col.html",inp=rb1),
    htmlTemplate("tpl_one_col.html",inp=db))
  out
})

# UI-Output for exporting the (anonymized) data
output$ui_export_data <- renderUI({
  output$dt_exportData <- renderDataTable({
    exportData()
  }, options=list(scrollX=TRUE, lengthMenu=c(5, 25, 50), pageLength=5))

  # specific (gui)-options for csv-export
  output$ui_export_csv <- renderUI({
    rb1 <- radioButtons("export_csv_header", label=h4("First row contains variable names"), choices=c(TRUE,FALSE), inline=TRUE)
    rb2 <- radioButtons("export_csv_sep", label=h4("Separator"), choices=c("Semicolon (;)"=";", Tab="\t", "Comma (,)"=","), inline=TRUE)
    rb3 <- radioButtons("export_csv_dec", label=h4("Decimal-Character"), choices=c("Dot (.)"=".", "Comma (,)"=","), inline=TRUE)
    out <- list(htmlTemplate("tpl_three_col.html",inp1=rb1, inp2=rb2, inp3=rb3))
    return(out)
  })
  output$ui_export_spss <- renderUI({
    txt <- list("The file will be written using", code("write_sav()"), "from package", code("haven"),".")
    htmlTemplate("tpl_one_col.html",inp=txt)
  })
  output$ui_export_stata <- renderUI({
    txt <- list("The file will be written using", code("write_dta()"), "from package", code("haven"),".")
    htmlTemplate("tpl_one_col.html",inp=txt)
  })
  dat_exptype <- selectInput("dat_exp_type", label=NULL,
    choices=c("R-Dataset"="rdata","SPSS-File"="sav",
      "Comma-seperated File"="csv", "STATA-File"="dta"),
    selected=input$dat_exp_type, width="100%")
  out <- list(
    htmlTemplate("tpl_one_col.html", inp=dataTableOutput("dt_exportData")),
    htmlTemplate("tpl_one_col.html", inp=h4("Select Export Format")),
    htmlTemplate("tpl_one_col.html", inp=dat_exptype)
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
    db <- downloadButton('downloadData', 'Download the anonymized dataset')
    out <- list(out, htmlTemplate("tpl_one_col.html", inp=db))
  }
  out
})

# UI-Output for Tab 'Export Data'
output$ui_export <- renderUI({
  output$downloadData <- downloadHandler(
    filename=function() {
      paste0("anon_data_",format(Sys.time(), "%Y%m%d_%H%M"),".",input$dat_exp_type)
    },
    content=function(file) {
      type <- input$dat_exp_type
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
      }
    }
  )
  output$downloadReport <- downloadHandler(
    filename=function() {
      paste0("report_",format(Sys.time(), "%Y%m%d_%H%M"),".html")
    },
    content=function(file) {
      internal <- ifelse(input$rb_simple_report=="internal", TRUE, FALSE)
      pout <- tempdir()
      if (internal) {
        tmpF <- paste0("sdcReport_internal",format(Sys.time(), "%Y%m%d_%H%M"))
      } else {
        tmpF <- paste0("sdcRepor_external",format(Sys.time(), "%Y%m%d_%H%M"))
      }
      report(obj$sdcObj, outdir=pout, filename=tmpF,
      format="HTML", title="SDC-Report", internal=internal)
      file.copy(paste0(pout,"/",tmpF,".html"), file)
    }
  )

  out <- htmlTemplate("tpl_one_col.html",inp=h2("Export anonymized Data"))
  if ( is.null(obj$sdcObj) ) {
    return(list(out, noSdcProblem(uri="ui_export_data")))
  }

  # what to export?
  sel1 <- selectInput("sel_what_export", label=h4("What do you want to export?"),
    choices=c("Anonymized Data"="exp_data","Anonymization Report"="report"),
    selected=input$sel_what_export, width="100%")
  out <- list(out, htmlTemplate("tpl_one_col.html",inp=sel1))

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
