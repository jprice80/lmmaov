#' Produce Output using Pandoc tables
#'
#' @description The \code{report} function will produce ASCII formated output. This includes both custom and pandoc tables and figures.
#'
#' @param object a R object from an fpr function.
#' @param style a pandoc table style: \code{simple, multiline, grid, or rmarkdown}.
#' @param plots a TRUE/FALSE boolean value indicating whether or not to perform include plots in the report.
#' @param round The number of digits to round too when producing the report.
#' @param ... additional itemes to be passed to \code{\link{Pandoc}} for processing.
#'
#' @return a printed pander table and optional figures
#' @export
#'
#' @examples
#' m1<-descriptives(data=mtcars, vars=c("mpg","disp"), groupby=c("vs","am"))
#' report(m1)
report<-function(object, style="multiline", plots=FALSE, round=4, ...){

  c1<-class(object)[1]
  c2<-class(object)[2]

  extras <- list(...)

  if(c1 != "fpr"){
    stop(paste(object, "not an fpr object"))
  }

  if(c2 == "desc"){

    report_desc(object, style, ...)

    if(plots == TRUE){
      cat("There are no descriptive statistics plots available.")
    }

  } else if (c2 == "norm"){

    report_norm(object, style, ...)

    if(plots == TRUE){
      plot.fpr(object, conf.level = conf.level)
    }

  } else if(c2 == "eqvar"){

    report_eqvar(object, style, ...)

    if(plots == TRUE){
      plot.fpr(object)
    }

  } else {
    stop(paste(object, "not an fpr object"))
  }
}

#====================================== Combine Report ===================================================

#' Combine several fpr objects into one report
#'
#' @param objects a list of objects produced by fpr functions to combine into one report.
#' @param style a pandoc table style: \code{simple, multiline, grid, or rmarkdown}.
#' @param plots plots a TRUE/FALSE boolean value indicating whether or not to perform include plots in the report.
#' @param round the number of digits to round too when producing the report.
#' @param ... additional itemes to be passed to \code{\link{Pandoc}} for processing.
#'
#' @return a printed pander tables and optional figures.
#' @export
#'
#' @examples
combine.report<-function(objects, style="multiline", plots=FALSE, round=4, ...){

  if(class(objects) != "list"){
    stop(paste(object, "objects provided are not a list"))
  }

  extras <- list(...)

  for(i in 1:length(objects)){
    if(plots==TRUE){

      object<-objects$output[[i]]
      report(object, style, plots=TRUE, ...)

    } else {
      object<-objects$output[[i]]
      report(object, style, ...)
    }
  }
}
