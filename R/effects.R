#====================================== allEffects ===================================================

#' Functions For Constructing Effect Displays
#'
#' @param mod a model fit by lmmaov model object
#'
#' @return a plot
#' @export
#'
#' @examples
allEffects.lmm<-function(mod){
  mod2<-mod[["Model"]]
  class(mod2)<-c("glmmTMB")
  effects::allEffects(mod2)
}

#====================================== effect ===================================================

#' Title
#'
#' @param term a lmmaov model
#' @param mod a model fit by lmmaov
#' @param ... not used
#'
#' @return a plot
#' @export
#'
#' @examples
# effect.lmm<-function(term, mod, ...){
#   mod2<-mod[["Model"]]
#   effects::effect(term, mod=mod2)
# }

#====================================== Effect ===================================================

#' Title
#'
#' @param focal.predictors a lmmaov model
#' @param mod  model fit by lmmaov
#' @param ... not used
#'
#' @return a plot
#' @export
#'
#' @examples
# Effect.lmm<-function(focal.predictors, mod, ...){
#   #Sys.getenv("JMP_TEST")
#   #mod2<-update(mod[["Model"]])
#   #glmmTMB:::Effect.glmmTMB(term, mod=mod2)
#
#   mod2<-mod[["Model"]]
#   glmmTMB::Effect.glmmTMB(focal.predictors, mod=mod2)
# }

#' ## modified from car::effectsmer.R
#'
#' ## effect.mer and effect.lme built from effect.lm by S. Weisberg 29 June 2011
#' ## 2012-03-08 to require() lme4 or nlme. J. Fox
#' ## 2012-10-05 effect.lme didn't work with 'weights', now corrected.  S. Weisberg
#' ## 2013-03-05: introduced merMod methods for development version of lme4. J. Fox
#' ## 2013-04-06: added support for lme4.0, J. Fox
#' ## 2013-07-30: added 'data' argument to lme.to.glm and mer.to.glm to allow
#' ##   calling effect from within a subroutine.
#' ## 2013-09-25:  removed the 'data' argument as it makes the functions fail with
#' ##   logs, splines and polynomials
#' ## 2014-09-24: added option for KR cov matrix to mer.to.glm(). J. Fox
#' ## 2014-12-07: don't assume that pbkrtest is installed. J. Fox
#' ## 2014-12-20: mer.to.glm failed for negative.binomial() because the link has an argument
#' ##   that was handled incorrectly by the family.glmResp function.  This function is no longer
#' ##   used by mer.to.glm.  The same error will recur in any link with an argument.
#' ## 2015-06-10: requireNamespace("pbkrtest") rather than require("pbkrtest)
#' ## 2015-07-02: fixed bug when the name of the data frame was the name of a function (e.g., sort, or lm)
#' ## 2015-12-13: make it work with pbkrtest 0.4-3. J. Fox
#' ## 2016-01-07: modified 'fixmod' to allow "||" in variance formulae
#' ## 2016-01-19: Fixed bug in glm.to.mer when 'poly' is used in a model.
#' ## 2016-06-08: Fixed bug handling the 'start' argument in glm.to.mer.  Fix by Ben Bolker, bug from Mariano Devoto
#' ## 2016-11-18: Change to mer.to.glm and lme.to.glm for stability in unusual glms. By Nate TeGrotenhuis.
#' ## 2017-03-28: in mer.to.glm, changed a name from m to .m.  The fake glm is created from the mer model's
#' ##             call slot, which included the name of the data.frame, if any.  A data.frame named 'm'
#' ##             therefore did not work.  Same bug fixed in lme.to.glm
#'
#' ## the function lm.wfit  gets the hessian wrong for mer's.  Get the variance
#' ## from the vcov method applied to the mer object.
#'
#' ## mer.to.glm evaluates a 'glm' model that is as similar to a given 'mer'
#' ## model as possible.  It is of class c("fakeglm", "glm", "lm")
#' ## several items are added to the created objects. Do not export
#'
#' #' @importFrom lme4 nobars
#' glmmTMB.to.glm <- function(mod, KR=FALSE) {
#'   if (KR) { ##  && !requireNamespace("pbkrtest", quietly=TRUE)){
#'     KR <- FALSE
#'     warning("pbkrtest is not compatible with glmmTMB, KR set to FALSE")
#'   }
#'   ## object$family$family doesn't work correctly with the negative binomial family because of the
#'   # argument in the family function, so the old line
#'   #   family <- family(mod)
#'   # returns an error message for these models.  The following kluge fixes this.
#'   # If this bug is fixed in lme4, this code may break because it expects resp$family$family
#'   # to return "Link Name(arg)" with ONE argument, and so spaces between Name and "(arg)"
#'   family1 <- function(object, ...){
#'     famname <- family(object)$family
#'     open.paren <- regexpr("\\(", famname)
#'     if(open.paren==-1) {
#'       name <- famname
#'       arg <- list()
#'     } else {
#'       name <- sub(" ", ".", tolower(substr(famname, 1, -1 + open.paren)))
#'       arg <- list(as.numeric(gsub("\\)", "", substr(famname, 1 + open.paren, 100))))
#'     }
#'     if(is.null(family(object)$initialize))
#'       do.call(name, arg) else family(object)
#'   }
#'   family <- family1(mod)
#'   # end
#'   link <- family$link
#'   family <- family$family
#'   cl <- getCall(mod)
#'   cl$control <- glm.control(epsilon=1) # suggested by Nate TeGrotenhuis
#'   ## if(cl[[1]] =="nlmer") stop("effects package does not support 'nlmer' objects")
#'   .m <- match(c("formula", "family", "data", "weights", "subset",
#'                 "na.action", "offset",
#'                 "model", "contrasts"), names(cl), 0L)
#'   cl <- cl[c(1L, .m)]
#'   cl[[1L]] <- as.name("glm")
#'   cl$formula <- lme4::nobars(as.formula(cl$formula))
#'   #    cl$data <- mod@frame # caused bug with a 'poly' in the formula
#'   cl$family <- gaussian
#'   mod2 <- eval(cl)
#'   cl$family <- family
#'   mod2$coefficients <- glmmTMB::fixef(mod)[["cond"]]
#'   ## mod2$vcov <- if (family == "gaussian" && link == "identity" && KR) as.matrix(pbkrtest::vcovAdj(mod)) else as.matrix(vcov(mod))
#'   mod2$vcov <- vcov(mod)[["cond"]]
#'   mod2$linear.predictors <- model.matrix(mod2) %*% mod2$coefficients
#'   mod2$fitted.values <- mod2$family$linkinv(mod2$linear.predictors)
#'   mod2$weights <- as.vector(with(mod2,
#'                                  prior.weights * (family$mu.eta(linear.predictors)^2 /
#'                                                     family$variance(fitted.values))))
#'   mod2$residuals <- with(mod2,
#'                          prior.weights * (y - fitted.values)/weights )
#'   class(mod2) <- c("fakeglm", class(mod2))
#'   mod2
#' }
#'
#' ##method for 'fakeglm' objects. Do not export
#' vcov.fakeglm <- function(object, ...) object$vcov
#'
#' ##The next six functions should be exported as S3 methods
#'
#' #' @export
#' effect.glmmTMB <- function(term, mod, vcov.=vcov, KR=FALSE, ...) {
#'   result <- effect(term, glmmTMB.to.glm(mod, KR=KR), vcov., ...)
#'   result$formula <- as.formula(formula(mod))
#'   result
#' }
#'
#' #' @export
#' allEffects.glmmTMB <- function(mod, KR=FALSE,...){
#'   allEffects(glmmTMB.to.glm(mod,KR=KR), ...)
#' }
#'
#' Effect.glmmTMB <- function (focal.predictors, mod, ...) {
#'   fam <- family(mod)
#'   ## code to make the 'truncated_*' families work
#'   if (grepl("^truncated", fam$family))
#'     fam <- c(fam, make.link(fam$link))
#'   ## dummy functions to make Effect.default work
#'   dummyfuns <- list(variance=function(mu) mu,
#'                     initialize=expression(mustart <- y + 0.1),
#'                     dev.resids=function(...) poisson()$dev.res(...)
#'   )
#'   for (i in names(dummyfuns)) {
#'     if (is.null(fam[[i]])) fam[[i]] <- dummyfuns[[i]]
#'   }
#'   ## allow calculation of effects ...
#'   if (length(formals(fam$variance))>1) {
#'     warning("overriding variance function for effects: ",
#'             "computed variances may be incorrect")
#'     fam$variance <- dummyfuns$variance
#'   }
#'   args <- list(call = getCall(mod),
#'                coefficients = lme4::fixef(mod)[["cond"]],
#'                vcov = vcov(mod)[["cond"]],
#'                family=fam)
#'   if (!requireNamespace("effects"))
#'     stop("please install the effects package")
#'   effects::Effect.default(focal.predictors, mod, ..., sources = args)
#' }
