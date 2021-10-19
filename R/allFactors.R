#' Title
#' @description Coerce variables to factors
#'
#' @param data a \code{\link{data.frame}} containing columns of variables.
#' @param vars a \code{\link{vector}} of variable names existing in the dataframe to perform normality tests on.
#'
#' @return
#' @export
#'
#' @examples Heart <- allFactors(Heart, vars=c("drug","time","patient"))
allFactors<-function(data, vars){
  data[vars]<-lapply(data[vars], factor)
  return(data)
}
