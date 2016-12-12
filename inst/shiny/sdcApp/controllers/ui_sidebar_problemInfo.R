output$tabinfo_sb_results <- output$tabinfo_sb_anonymize <- renderUI({
  inp <- infodat()$df
  if (is.null(inp)) {
    return(NULL)
  }

  fluidRow(
    column(12, h4("Important variables"), align="center"),
    column(12, DT::renderDataTable({
      inp
    }, rownames=FALSE, colnames = c("Variable name", "Type", "Suppressions"), selection='none', style='bootstrap', class='table-condensed',
    options = list(searching=FALSE, paging=FALSE, ordering=FALSE, bInfo=FALSE)), align="center")
  )
})

output$tabparam_sb_results <- output$tabparam_sb_anonymize <- renderUI({
  inp <- infodat()$params
  if (is.null(inp)) {
    return(NULL)
  }
  fluidRow(
    column(12, h4("Additional parameters"), align="center"),
    column(12, DT::renderDataTable({
      inp
    }, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
    options = list(searching=FALSE, paging=FALSE, ordering=FALSE, bInfo=FALSE)), align="center")
  )
})

# violating k-anon
output$risk_sb_anonymize <- renderUI({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }

  # current
  risks <- get_risk()
  obs <- nrow(risks)
  n2 <- sum(risks$fk<2)
  n3 <- sum(risks$fk<3)
  n5 <- sum(risks$fk<5)
  v1 <- paste0(n2," (",formatC(100*(n2/obs), format="f", digits=2),"%)")
  v2 <- paste0(n3," (",formatC(100*(n3/obs), format="f", digits=2),"%)")
  v3 <- paste0(n5," (",formatC(100*(n5/obs), format="f", digits=2),"%)")

  # original
  origrisks <- curObj@originalRisk$individual
  n2_o <- sum(origrisks[,2]<2)
  n3_o <- sum(origrisks[,2]<3)
  n5_o <- sum(origrisks[,2]<5)
  v1_o <- paste0(n2_o," (",formatC(100*(n2_o/obs), format="f", digits=2),"%)")
  v2_o <- paste0(n3_o," (",formatC(100*(n3_o/obs), format="f", digits=2),"%)")
  v3_o <- paste0(n5_o," (",formatC(100*(n5_o/obs), format="f", digits=2),"%)")

  df <- data.table(
    Measures=c("2-anonymity","3-anonymity","5-anonymity"),
    Original=c(v1_o, v2_o, v3_o),
    Modified=c(v1,v2,v3))

  fluidRow(
    column(12, h4("Risk (k-anonymity)"), align="center"),
    column(12, DT::renderDataTable({
      df
    }, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
    options = list(searching=FALSE, paging=FALSE, ordering=FALSE, bInfo=FALSE)), align="center")
  )
})

# numrisk
output$numrisk_sb_anonymize <- renderUI({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(invisible(NULL))
  }
  x <- print(curObj, type="numrisk", docat=FALSE)
  if (is.null(x)) {
    return(invisible(NULL))
  }
  dt <- data.table(data=c("orig","modified"), risk_min=paste0(c("100.00",x$risk_up),"%"), risk_max=paste0(c("100.00","100.00"),"%"))

  fluidRow(
    column(12, h4("Numeric Risk"), align="center"),
    column(12, DT::renderDataTable({
      dt
    }, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
    options = list(searching=FALSE, paging=FALSE, ordering=FALSE, bInfo=FALSE)))
  )
})

# information loss
output$loss_sb_anonymize <- renderUI({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }

  utility <- get.sdcMicroObj(curObj, type="utility")
  if (is.null(utility)) {
    return(invisible(NULL))
  }
  il1 <- formatC(utility$il1, format="f", digits=2)
  diff_eigen <- formatC(utility$eigen*100, format="f", digits=2)

  df <- data.frame(
    Measure=c("IL1s","Difference of Eigenvalues"),
    orig=c(0.00, 0.00),
    modified=c(il1, diff_eigen))

  fluidRow(
    column(12, h4("Information Loss"), align="center"),
    column(12, DT::renderDataTable({
      df
    }, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
    options = list(searching=FALSE, paging=FALSE, ordering=FALSE, bInfo=FALSE)),align="center")
  )
})

# postrandomization loss
output$pram_sb_anonymize <- renderUI({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  pI <- curObj@pram
  if (is.null(pI)) {
    return(NULL)
  }

  out <- fluidRow(column(12, h4("Postrandomization"), align="center"))

  # check warnings!
  wn <- curObj@additionalResults$sdcMicro_warnings
  if (!is.null(wn) && "pram" %in% wn$method) {
    out <- list(out, fluidRow(column(12, p("Note: Pram was applied on at least one categorical
        key variable. Risk measures and k-anonymity assessment are not useful anymore!", align="center"))))
  }
  out <- list(out, fluidRow(
    column(12, DT::renderDataTable({
      pI$summary
    }, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
    options = list(searching=FALSE, paging=FALSE, ordering=FALSE, bInfo=FALSE)), align="center")
  ))
})

# anonymization-methods applied
output$anonmeth_sb_risk <- renderUI({
  curMethods <- obj$anon_performed
  if (is.null(curMethods)) {
    return(NULL)
  }
  res <- tags$ul(
    lapply(1:length(curMethods), function(x) {
      tags$li(sub(" (see above) ","",curMethods[x]))
    }
  ))
  out <- fluidRow(column(12, h4("Anonymization Steps"), align="center"))
  out <- list(out, fluidRow(column(12, res)))
  return(out)
})


## 2 sidebars required, id's in shiny must be unique per page/tab
# sidebar for results-page
output$sb_info_results <- renderUI({
  out <- list(
    uiOutput("tabinfo_sb_results"),
    uiOutput("tabparam_sb_results"),
    uiOutput("anonmeth_sb_risk"))
  out
})

# sidebar for anonymize-page
output$sb_info_anonymize <- renderUI({
  out <- list(
    uiOutput("tabinfo_sb_anonymize"),
    uiOutput("tabparam_sb_anonymize"),
    uiOutput("risk_sb_anonymize"),
    uiOutput("numrisk_sb_anonymize"),
    uiOutput("loss_sb_anonymize"),
    uiOutput("pram_sb_anonymize"))
  out
})
