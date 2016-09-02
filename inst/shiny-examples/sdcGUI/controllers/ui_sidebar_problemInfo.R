output$tabinfo_sb_results <- output$tabinfo_sb_anonymize <- output$tabinfo_sb_script <- output$tabinfo_sb_export <- renderDataTable({
  inp <- infodat()$df
  if (is.null(inp)) {
    return(NULL)
  }
  inp
}, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
  options = list(searching=FALSE, paging=FALSE, ordering=FALSE, bInfo = FALSE)
)

output$tabparam_sb_results <- output$tabparam_sb_anonymize <- output$tabparam_sb_script <- output$tabparam_sb_export <- renderDataTable({
  inp <- infodat()$params
  if (is.null(inp)) {
    return(NULL)
  }
  inp
}, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
options = list(searching=FALSE, paging=FALSE, ordering=FALSE, bInfo = FALSE)
)


output$sb_info_results <- renderUI({
  return(fluidRow(
    column(12, h4("Important Variables", align="center")),
    column(12, dataTableOutput("tabinfo_sb_results")),
    column(12, h4("Additional Parameters", align="center")),
    column(12, dataTableOutput("tabparam_sb_results")),
    column(12, h4("Risk", align="center")),
    column(12, p("Riskinfo", align="center"))
  ))
})
output$sb_info_anonymize <- renderUI({
  return(fluidRow(
    column(12, h4("Important Variables", align="center")),
    column(12, dataTableOutput("tabinfo_sb_anonymize")),
    column(12, h4("Additional Parameters", align="center")),
    column(12, dataTableOutput("tabparam_sb_anonymize")),
    column(12, h4("Risk", align="center")),
    column(12, p("Riskinfo", align="center"))
  ))
})
output$sb_info_script <- renderUI({
  return(fluidRow(
    column(12, h4("Important Variables", align="center")),
    column(12, dataTableOutput("tabinfo_sb_script")),
    column(12, h4("Additional Parameters", align="center")),
    column(12, dataTableOutput("tabparam_sb_script")),
    column(12, h4("Risk", align="center")),
    column(12, p("Riskinfo", align="center"))
  ))
})
output$sb_info_export <- renderUI({
  return(fluidRow(
    column(12, h4("Important Variables", align="center")),
    column(12, dataTableOutput("tabinfo_sb_export")),
    column(12, h4("Additional Parameters", align="center")),
    column(12, dataTableOutput("tabparam_sb_export")),
    column(12, h4("Risk", align="center")),
    column(12, p("Riskinfo", align="center"))
  ))
})
