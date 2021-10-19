######################################## Summary Function #####################################

#' @export
summary.lmm<-function(object, plots=FALSE){

  c1<-class(object)[1]
  c2<-class(object)[2]

  if(c1 != "lmm"){
    stop(paste(object, "not an lmm object"))
  }

  if(c2 == "desc"){

    summary_desc(object)

  } else if(c2 == "norm"){

    summary_norm(object, plots)

  } else if(c2 == "eqvar"){

    summary_eqvar(object, plots)

  }
}


######################################## Print Function #####################################

#' @export
print.lmm<-function(object, ...){

  summary.lmm(object, ...)

}


######################################## Plot Function #######################################

#' @export
plot.lmm<-function(object, ...){

  c1<-class(object)[1]
  c2<-class(object)[2]

  if(c2=="norm"){

    plot_normality(object, ...)

  } else if (c2=="eqvar"){

    plot_eqvar(object, ...)

  }
}
