###################################################
# LLmodRisk Function
# Calculates two disclosure risk measures with 
# four possible log-linear models (standard, CE, PSE, weightedLLM)
# 1. estimates the number of sample uniques that are population unique
# 2. estimates the number of correct matches of sample uniques
###################################################

###################################################
#library(sdcMicro)

modRisk <- function (x, method = "standard", weights = NULL, formulaM = NULL, keyVars, bound = 5) 
{
  if(is.null(weights)){
    warning("Please provide the weights, \n
                  They are now set to 500 which is simple a wrong assumption.")
    weights <- 500
  }
  x <- data.frame(x, weights)
  if(is.null(formulaM)){
    warning("The formula for the glm() function is set to \n 
                  as.formula(paste(" ~ ", paste(c(as.character(keyVars)), collapse = "+")))")
    formulaM = as.formula(paste(" ~ ", paste(c(as.character(keyVars)), collapse = "+")))
  }
  risk1 <- function(l, p) {
    v = (1 - p) * l
    exp(-v)
  }
  risk2 <- function(l, p) {
    v = (1 - p) * l
    (1 - exp(-v))/v
  }
  file_risk <- function(freq, risk) {
    sum(as.numeric(freq == 1) * risk)
  }
  
  # all sample keys and their counts
  counts <- freqCalc(x, keyVars)$fk
  f1 <- as.formula(paste("counts ~ ", paste(c(as.character(keyVars)), collapse = "+")))
  u_c <- aggregate(f1, x, mean)
  f2 <- as.formula(paste("weights ~ ", paste(c(as.character(keyVars)), collapse = "+")))
  u_w <- aggregate(f2, x, sum) 
  x <- data.frame(u_c, weights=u_w[,"weights"])
  

  
  #################### 0er zum Modell hinzufuegen
  
  t <- 1:length(keyVars)
  lst <- lapply(t, function(t) levels(x[,keyVars[t]]))
  keys <- as.data.frame(expand.grid(lst)) 
  names(keys) <- keyVars
  
  # wie werden die Daten uebergeben schon mit Nullern und tabelliert? 
  keys$counts <- rep(0,nrow(keys))
  keys$weights <- rep(0,nrow(keys))
  xn <- rbind(x, keys)
  x <- xn[!duplicated(xn[,c(keyVars)]),]
  
  # model selection
  if(method == "standard") {
    form <- as.formula(paste(c("counts", as.character(formulaM)), collapse = ""))	
  } 
  else if(method == "CE") {
    EC <- x$counts/x$weights #offset term 
    EC[EC=="NaN"] <- 0
    EC <- log(EC + 0.1)
    form <- as.formula(paste(c("counts", c(as.character(formulaM)), "+ offset(EC)"), collapse = ""))  
  }
  else if(method == "PML") {
    f <- sum(x$counts)/sum(x$weights)
    weights_t <- round(x$weights*f) #round
    x <- data.frame(x, weights_t)
    form <- as.formula(paste(c("weights_t", as.character(formulaM)), collapse = ""))  
  }
  else if(method == "weightedLLM") {
    form_zw <- as.formula(paste(c(formulaM,"weights"),collapse="+"))
    form <- as.formula(paste(c("counts", as.character(form_zw)), collapse = ""))
  }
  else{ 
    form <- as.formula(paste(c("counts", as.character(formulaM)), collapse = ""))  
  }

  # calculationg the model with the chosen formula
  mod <- glm(form, data = x, family = poisson())
  lambda <- fitted(mod)
  #Risk
  x <- data.frame(x, Fk=lambda)
  x <- x[0 < x$counts & x$counts <= bound,]
  r1 <- risk1(x$Fk*x$weights, 1/x$weights)
  r2 <- risk2(x$Fk*x$weights, 1/x$weights)
  # 1. estimate of the number of sample uniques that are population unique
  # 2. estimate of the number of correct matches of sample uniques
  gr1 <- file_risk(x$counts, r1)
  gr2 <- file_risk(x$counts, r2)

  #return
  res <- list(popunique=gr1, matches=gr2)
}