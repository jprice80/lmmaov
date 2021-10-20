#====================================== allEffects ===================================================

#' Functions For Constructing Effect Displays
#'
#' @param mod a model fit by lmmaov model object
#'
#' @return a plot
#' @export
#'
#' @examples
allEffects.lmmaov<-function(mod){
  mod2<-mod[["Model"]]
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
effect.lmmaov<-function(term, mod, ...){
  mod2<-mod[["Model"]]
  effects::effect(term, mod=mod2)
}

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
Effect.lmmaov<-function(focal.predictors, mod, ...){
  #Sys.getenv("JMP_TEST")
  #mod2<-update(mod[["Model"]])
  #glmmTMB:::Effect.glmmTMB(term, mod=mod2)

  mod2<-mod[["Model"]]
  effects::Effect(focal.predictors, mod=mod2)
}
