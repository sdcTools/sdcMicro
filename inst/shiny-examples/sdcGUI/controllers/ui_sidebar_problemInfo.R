output$tabinfo_sb_results <- output$tabinfo_sb_anonymize <- output$tabinfo_sb_script <- output$tabinfo_sb_export <- renderDataTable({
  inp <- infodat()$df
  if (is.null(inp)) {
    return(NULL)
  }
  inp
}, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
  options = list(searching=FALSE, paging=FALSE, ordering=FALSE, bInfo = FALSE)
)

output$tabparam_sb_results <- output$tabparam_sb_anonymize <- output$tabparam_sb_script <-
  output$tabparam_sb_export <- renderDataTable({
  inp <- infodat()$params
  if (is.null(inp)) {
    return(NULL)
  }
  inp
}, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
options = list(searching=FALSE, paging=FALSE, ordering=FALSE, bInfo = FALSE)
)

# violating k-anon
output$risk_sb_results <- output$risk_sb_anonymize <- output$risk_sb_script <- output$risk_sb_export <- renderDataTable({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  risks <- get_risk()
  obs <- nrow(risks)
  n2 <- sum(risks$fk<2)
  n3 <- sum(risks$fk<3)
  n5 <- sum(risks$fk<5)
  v1 <- paste0(n2," (",formatC(100*(n2/obs), format="f", digits=2),"%)")
  v2 <- paste0(n3," (",formatC(100*(n3/obs), format="f", digits=2),"%)")
  v3 <- paste0(n5," (",formatC(100*(n5/obs), format="f", digits=2),"%)")
  df <- data.table(
    Measures=c("2-anonymity","3-anonymity","5-anonymity"),
    Value=c(v1,v2,v3))
  df
  }, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
  options = list(searching=FALSE, paging=FALSE, ordering=FALSE, bInfo = FALSE)
)
# information loss
output$loss_sb_results <- output$loss_sb_anonymize <- output$loss_sb_script <- output$loss_sb_export <- renderDataTable({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }

  utility <- get.sdcMicroObj(obj$sdcObj, type="utility")
  il1 <- formatC(utility$il1, format="f", digits=2)
  diff_eigen <- formatC(utility$eigen*100, format="f", digits=2)

  out <- data.frame(
    Measure=c("IL1","Difference of Eigenvalues"),
    Values=c(il1, diff_eigen))
  }, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
options = list(searching=FALSE, paging=FALSE, ordering=FALSE, bInfo = FALSE)
)


## 4 sidebars required, id's in shiny must be unique per page/tab
output$sb_info_results <- renderUI({
  return(fluidRow(
    column(12, h4("Important Variables", align="center")),
    column(12, dataTableOutput("tabinfo_sb_results")),
    column(12, h4("Additional Parameters", align="center")),
    column(12, dataTableOutput("tabparam_sb_results")),
    column(12, h4("Risk", align="center")),
    column(12, dataTableOutput("risk_sb_results")),
    column(12, h4("Current Information Loss", align="center")),
    column(12, dataTableOutput("loss_sb_results"))
  ))
})
output$sb_info_anonymize <- renderUI({
  return(fluidRow(
    column(12, h4("Important Variables", align="center")),
    column(12, dataTableOutput("tabinfo_sb_anonymize")),
    column(12, h4("Additional Parameters", align="center")),
    column(12, dataTableOutput("tabparam_sb_anonymize")),
    column(12, h4("Risk", align="center")),
    column(12, dataTableOutput("risk_sb_anonymize")),
    column(12, h4("Current Information Loss", align="center")),
    column(12, dataTableOutput("loss_sb_anonymize"))
  ))
})
output$sb_info_script <- renderUI({
  return(fluidRow(
    column(12, h4("Important Variables", align="center")),
    column(12, dataTableOutput("tabinfo_sb_script")),
    column(12, h4("Additional Parameters", align="center")),
    column(12, dataTableOutput("tabparam_sb_script")),
    column(12, h4("Risk", align="center")),
    column(12, dataTableOutput("risk_sb_script")),
    column(12, h4("Current Information Loss", align="center")),
    column(12, dataTableOutput("loss_sb_script"))
  ))
})
output$sb_info_export <- renderUI({
  return(fluidRow(
    column(12, h4("Important Variables", align="center")),
    column(12, dataTableOutput("tabinfo_sb_export")),
    column(12, h4("Additional Parameters", align="center")),
    column(12, dataTableOutput("tabparam_sb_export")),
    column(12, h4("Risk", align="center")),
    column(12, dataTableOutput("risk_sb_export")),
    column(12, h4("Current Information Loss", align="center")),
    column(12, dataTableOutput("loss_sb_export"))
  ))
})
