#====================================== emmeans ===================================================

#' Estimated marginal means (Least-squares means)
#'
#' @param object a lmmaov fitted model object
#' @param specs A character vector specifying the names of the predictors over which EMMs are desired. specs may also be a formula or a list (optionally named) of valid specs.
#' @param ... Additional emmeans specifications
#'
#' @export
#'
#' @examples
emmeans<-function(object, specs, ...){
  mod<-object[["Model"]]
  attr(mod, "class")<-"glmmTMB"
  aov_out<-object[["ANOVA"]]

  specs_vec<-as.character(specs)

  if(specs_vec[2] == "pairwise"){
    trm<-as.character(specs_vec[3])
  } else {
    trm<-as.character(specs_vec[2])
  }

  trm<-gsub("*", ":", trm, fixed=TRUE)

  if(grepl("|", trm, fixed = TRUE) == TRUE){

    #Proceed if the term contains a |

    trm2<-strsplit(trm,"|", fixed=TRUE)[[1]]
    trm2<-trimws(trm2[length(trm2)], "both")

    if(length(trm2) > 2){
      stop("Cannont determine correct dfs with specs containing more than 1 '|' ")
    }

    rw_names<-rownames(aov_out)
    temp_aov_out<-cbind(trm=rw_names, aov_out)

  } else {
    trm2<-trimws(trm[length(trm)], "both")

    rw_names<-rownames(aov_out)
    temp_aov_out<-cbind(trm=rw_names, aov_out)
  }



  for(i in 1:nrow(temp_aov_out)){
    trm3<-strsplit(trm2,":", fixed=TRUE)[[1]]
    trm3<-trimws(trm3, "both")
    trmtest<-(strsplit(temp_aov_out$trm[i],":", fixed=TRUE)[[1]])

    len1<-length(trm3)
    len2<-length(trmtest)

    if(len1==len2 & all(trm3 %in% trmtest)){
      df<-temp_aov_out$denDF[i]
      emm_out<-emmeans::emmeans(mod, specs, df=df, ...)
    }
  }

  return(emm_out)
}
