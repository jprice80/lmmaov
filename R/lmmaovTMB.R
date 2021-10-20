#' Fit Models with TMB
#' @description Fit a linear mixed model using TMB including r-side covariance structures using containment Anova degrees of freedom tests.
#'
#' @param data a \code{\link{data.frame}} containing columns of variables.
#' @param fixed a two-sided linear formula object describing the fixed-effects part of the model, with the response on the left of a ~ operator and the terms, separated by + operators, on the right,
#' @param random a one-sided formula describing the random effects
#' @param repeated the variable name in the \code{\link{data.frame}} representing the repeated measure
#' @param units the variable name in the \code{\link{data.frame}}
#' @param rcov the R-side variance/covariance matrix diag (diagonal, heterogeneous variance)
#'    ar1 (autoregressive order-1, homogeneous variance)
#'    cs (compound symmetric, heterogeneous variance)
#'    ou (* Ornstein-Uhlenbeck, homogeneous variance)
#'    exp (* exponential autocorrelation)
#'    gau (* Gaussian autocorrelation)
#'    mat (* Matérn process correlation)
#'    toep (* Toeplitz)
#'    Structures marked with * are experimental/untested. See vignette("covstruct", package = "glmmTMB")< for more information.
#'
#' @param type type of test, "II", "III", 2, or 3. Roman numerals are equivalent to the corresponding Arabic numerals.
#' @param REML whether to use REML estimation rather than maximum likelihood.
#' @param control parameters, see glmmTMBControl.
#'
#' @return
#' @export
#'
#' @examples
#' data(heart)
#' heart<-allFactors(heart, vars=c("drug","time","patient"))
#' (m1<-lmmaovTMB(data=heart, fixed=HR~drug*time+basehr, random=~patient:drug, repeated="time", units="patient", rcov="ar1"))

lmmaovTMB<-function(data, fixed, random, repeated=NULL, units=NULL, rcov="vc", type=3, REML=TRUE, control = glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))){

  # if(control==1){
  #   control<-glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))
  # } else if (control==2) {
  #   control<-glmmTMBControl(
  #     optCtrl = list(iter.max = 100000,
  #                    eval.max = 100000,
  #                    x.tol=1E-8,
  #                    xf.tol = 1E-08,
  #                    step.min=.1,
  #                    step.max=1))
  # } else {
  # control = glmmTMBControl()
  # }

  #Sys.setenv(R_TEST = "JMP_TEST")
  usr_cont<-options('contrasts')$contrasts
  options(contrasts=c("contr.sum", "contr.poly"))
  options("scipen"=10, "digits"=4)

  forms<-formula_writer(data, fixed, random, repeated=repeated, units=units, rcov=rcov)

  full_form<-forms[[1]]
  form_no_repeated<-forms[[2]]
  form_qr_ready<-forms[[3]]
  noint<-forms[[4]]
  noint_full_form<-forms[[5]]

  #ANOVA calc

  if(rcov=="vc"){
    m1<-glmmTMB::glmmTMB(formula=full_form, dispformula = ~ 1, data=data, REML=REML, control = control)
  } else {
    m1<-glmmTMB::glmmTMB(formula=full_form, dispformula = ~ 0, data=data, REML=REML, control = control)
  }

  options(contrasts=c("contr.treatment", "contr.poly"))
  aov_out<-nlme_aov(m1, type=type, form_qr_ready, noint)

  #Replace correctly specify noint formula for summary table computation
  if(noint == TRUE){
    if(rcov=="vc"){
      m2<-glmmTMB::glmmTMB(formula=noint_full_form, dispformula = ~ 1, data=data, REML=REML, control = control)
    } else {
      m2<-glmmTMB::glmmTMB(formula=noint_full_form, dispformula = ~ 0, data=data, REML=REML, control = control)
    }

  } else {
    m2<-update(m1)
  }

  #Parameter estimates calc
  glmmTMB_summary<-summary(m2)
  params<-glmmTMB_summary$coefficients$cond[,1:3]

  sc<-round(summary_calc(params, aov_out, data),4)


  options(contrasts=usr_cont)

  out<-list("Model"=m2, "ANOVA"=aov_out, "Parameter estimates"=sc)
  class(out)<-c("lmm", "TMB")

  return(out)

}

#====================================== Formula Writer ===================================================

formula_writer<-function(data, fixed, random, repeated=NULL, units=NULL, rcov=NULL){

  if(rcov != "vc" & is.null(units)){
    stop("A units= is required for the specified rcoviance structure")
  } else if(rcov != "vc" & is.null(repeated)){
    stop("A repeated factor is required for the specified rcoviance structure")
  }

  #Fixed equation
  Y<-deparse(fixed[[2]])
  fixedform<-deparse(fixed[[3]])
  fixedtrms<-as.character(attr(terms.formula(fixed), "term.labels"))

  #random equation
  indivterms<-attr(terms.formula(random), "term.labels")
  randtrms<-vector()
  for(i in 1:length(indivterms)){
    randtrms[i]<-paste0("(1|", indivterms[i],")")
  }

  randomform<-paste(randtrms, collapse = "+")

  # Temporarily replace model noint model for ANOVA calc
  if(grepl("0", fixedform, fixed = TRUE) == TRUE){
    noint<-TRUE
  } else if (grepl("-1", fixedform, fixed = TRUE) == TRUE) {
    noint<-TRUE
  } else {
    noint<-FALSE
  }

  # Develop formulas
  if(rcov=="vc" || is.null(rcov)){

    if(noint ==TRUE){
      noint_full_form<-as.formula(paste0(Y, "~",fixedform, "+", randomform))
      fixedform<-gsub("0", "1", fixedform)
      fixedform<-gsub("-1", "1", fixedform)
    } else {
      noint_full_form<-NULL
    }

    full_form<-as.formula(paste0(Y, "~",fixedform, "+", randomform))
    form_no_repeated<-full_form
    form_qr_ready<-as.formula(paste0(Y, "~",fixedform, "+", paste(indivterms, collapse='+')))

  } else if (rcov %in% c("us", "cs", "ar1", "diag", "ou", "exp", "gau", "mat", "toep") & !is.null(repeated)){

    randtrms[length(randtrms)+1]<-paste0(rcov,"(",repeated,"+0|", units, ")")
    randomform2<-paste(randtrms, collapse = "+")

    if(noint ==TRUE){
      noint_full_form<-as.formula(paste0(Y, "~",fixedform, "+", randomform2))
      fixedform<-gsub("0", "1", fixedform)
      fixedform<-gsub("-1", "1", fixedform)
    } else {
      noint_full_form<-NULL
    }

    full_form<-as.formula(paste0(Y, "~",fixedform, "+", randomform2))
    form_no_repeated<-as.formula(paste0(Y, "~",fixedform, "+", randomform))
    form_qr_ready<-as.formula(paste0(Y, "~",fixedform, "+", paste(indivterms, collapse='+')))

  } else {

    cat("units or Repeated factor is not specified")
    break;
  }

  return(list(full_form, form_no_repeated, form_qr_ready, noint, noint_full_form))
}

#====================================== nlme_aov ===================================================

nlme_aov <- function(model = model, type = type, form_qr_ready, noint){

  dc <- dataClasses(model)

  #TMBaov <- suppressPackageStartupMessages(car::Anova(model, type=type))
  TMBaov <- suppressPackageStartupMessages(car::Anova(model, type=type))

  # Pull the DFs associated with each term
  base_dfs <- basic_aov_dfs(model, form_qr_ready)

  # Correct the terms if no intercept is estimated
  if(row.names(TMBaov)[1] == "(Intercept)") {
    fixed <- base_dfs[base_dfs$vartype=="fixed", ]
  } else {
    fixed <- base_dfs[base_dfs$vartype=="fixed", ][-1, ]
  }

  random <- base_dfs[base_dfs$vartype=="random", ]

  #sort the data by size to make sure the right df is chosen
  random<-random[order(random$df),]

  # Apply the nlme DFs
  fixed$vartype <- NULL
  fixed$denDf <- NA
  for(i in 1:nrow(fixed)){
    ftrm <- strsplit(fixed$terms[i], ":")[[1]]
    datacls <- dc[match(ftrm, dc$terms), "class"]

    if(!is.na(datacls[1])){
      for(j in 1:nrow(random)){
        rtrm<-strsplit(random$terms[j], ":")[[1]]

        if(rtrm[1] != "Residuals"){
          if(all(ftrm %in% rtrm)==TRUE){
            fixed$denDf[i]<-random$df[j]
            break;
          } else {
            fixed$denDf[i]<-random[nrow(random),2]
          }
        } else {
          fixed$denDf[i] <- random[nrow(random),2]
        }
      }
    } else {
      fixed$denDf[i] <- random[nrow(random),2]
    }
  }


  #Complete output
  chisq <- as.vector(TMBaov$Chisq)
  nDF <- as.vector(TMBaov$Df)
  Fval <- chisq/nDF
  dDF <- fixed$denDf
  Pval <- pf(Fval, nDF, dDF, lower.tail = FALSE)

  aod <- data.frame(numDF = nDF, denDF = dDF, Fvalue = round(Fval, 2), pvalue = round(Pval, 4))
  row.names(aod) <- fixed$terms
  class(aod) <- c("bdf", "nlme", "data.frame")
  y_name<- names(model$modelInfo$respCol)

  if (type == 3 || type == "III") {
    attr(aod, "heading") <-  paste("Analysis of Deviance Table (Type III F-tests)", "\n\nResponse: ", y_name)
  } else if (type == 2 || type == "II"){
    attr(aod, "heading") <-  paste("Analysis of Deviance Table (Type II F-tests)", "\n\nResponse: ", y_name)
  }

  if(noint==TRUE){
    aod<-aod[-1,]
  }

  return(aod)
}

#====================================== basic_aov_dfs ===================================================

basic_aov_dfs <- function(model, form_qr_ready) {
  fixed <- unique(attr(terms(model), "term.labels"))
  random <- unique(model$modelInfo$grpVar)
  intercept <- attr(model$modelInfo$terms$cond$fixed, "intercept")
  y_name <- names(model$modelInfo$respCol)

  #exclude units for qr decomp formula
  # if("units" %in% random){
  #   random<-random[length(random)-1]
  # }

  base_dfs_out <- data_aov_dfs(data = model$frame, form_qr_ready)

  base_dfs_out$vartype <- as.character(NA)
  for(i in 1:nrow(base_dfs_out)) {
    trm<-base_dfs_out$terms[i]

    if(trm %in% fixed) {
      base_dfs_out$vartype[i]<-"fixed"
    } else if(trm =="(Intercept)") {
      base_dfs_out$vartype[i]<-"fixed"
    } else {
      base_dfs_out$vartype[i]<-"random"
    }
  }

  return(base_dfs_out)
}

#====================================== data_aov_dfs ===================================================

data_aov_dfs <- function(data, form_qr_ready){

  #Define formula
  N <- nrow(data)

  # Define term numbers
  fullterms <- unique(attr(terms(form_qr_ready), "term.labels"))

  # Define model matrix
  mf1 <- model.frame(formula = form_qr_ready, data = data)
  mm1 <- model.matrix(form_qr_ready, mf1)

  intercept <- attr(formula, "Intercept")

  # Establish output dataframe and count the number of terms
  if(all(mm1[,1] == 1)){
    basic_dfs_out <- data.frame(terms=c("(Intercept)", fullterms))
  } else {
    basic_dfs_out <- data.frame(terms=fullterms)
  }

  basic_dfs_out$basic_dfs_effectnum <- seq(from = 0, to = (nrow(basic_dfs_out)-1))
  basic_dfs_out$df<-NA

  # QR decomposition information
  my_qr <- qr(mm1)
  pivot <- my_qr$pivot
  rank <- my_qr$rank
  resid_val <- N-rank

  # Identify the appropriate column indices and number of terms
  full_matrix_columns_index <- attr(mm1, "assign")
  full_matrix_names <- dimnames(mm1)[[2]]

  # Extract column terms corresponding to appropriate pivots to generate dfs
  valid_matrix_columns_index <- full_matrix_columns_index[pivot[1L:rank]]

  # calculate the appropriate basic aov degrees of freedom
  for (i in 1:nrow(basic_dfs_out)) {

    effnum <- basic_dfs_out$basic_dfs_effectnum[i]
    effname <- basic_dfs_out$terms[i]

    #identify the parameter estimates (effects) that this iteration corresponds to
    ai <- (valid_matrix_columns_index == effnum)

    # Sum the total number of effects for each term
    df <- sum(ai)

    if(!is.na(df) && df != 0){
      basic_dfs_out[which(basic_dfs_out$terms == effname),"df"] <- df
    } else {
      basic_dfs_out[which(basic_dfs_out$terms == effname),"df"] <- NA
    }
  }
  #
  basic_dfs_out$basic_dfs_effectnum <- NULL
  #basic_dfs_out[nrow(basic_dfs_out), "terms"] <- "Residuals"

  resids <- data.frame(terms="Residuals", df=resid_val)
  basic_dfs_out <- rbind(basic_dfs_out, resids)

  # if(any(is.na(basic_dfs_out$df))){
  #   warning("Unable to determine fully determine degrees of freedom for this model using the specified method.")
  # }

  return(basic_dfs_out)
}

#====================================== summary_calc ===================================================

summary_calc<-function(params, aov_out, data){

  attr(params, "dimnames")[[2]]<-c("Estimate","Std. Error","t value" )

  rw_names<-rownames(aov_out)
  #st<-summary_terms(data,rw_names)

  temp_aov_out<-cbind(trm=rw_names, aov_out)
  temp_params<-cbind(trm=rownames(params), data.frame(params))

  #iterate through identify appropriate dfs corresponding to summary term levels
  for(i in 1:length(rw_names)){
    rnam<-rw_names[i]
    trm<-strsplit(rnam,":")[[1]]

    if(length(trm)==1){
      lv<-levels(data[[rnam]])

      if(!is.null(lv)){
        for(j in 1:length(lv)){
          trm2<-paste0(rnam,lv[j])

          temp_params$trm<-stringr::str_replace(temp_params$trm, trm2, rnam)
        }
      }
    }
  }

  summary_tab<-dplyr::left_join(temp_params,temp_aov_out, by="trm") %>% dplyr::select(trm, Estimate, Std.Error=Std..Error, t.value, df=denDF)
  summary_tab$p.value<-2*(1-pt(abs(summary_tab$t.value), df=summary_tab$df))

  rownames(summary_tab)<-rownames(params)
  summary_tab$trm<-NULL

  return(summary_tab)
}

#====================================== summary_TMB ===================================================

summary_TMB <- function(object){

  cat("Model Information")
  cat("\n")
  print(object[["Model"]])
  cat("\n")

  cat("ANOVA")
  cat("\n")
  print(object[["ANOVA"]], row.names=TRUE)
  cat("\n")

  cat("Parameter Estimates")
  cat("\n")
  print(object[["Parameter estimates"]])
}








# glmmCtrl<-glmmTMBControl(
#   optCtrl = list(iter.max = 100000,
#                  eval.max = 100000,
#                  x.tol=1E-8,
#                  xf.tol = 1E-08,
#                  step.min=.1,
#                  step.max=1))
#
# data(heart)
# heart<-allFactors(heart, vars=c("drug","time","patient"))
# (m1<-lmmaovTMB(data=heart, fixed=HR~drug*time+basehr, random=~patient:drug, repeated="time", units="patient", rcov="ar1"))
#
# glmmCtrl<-glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))

