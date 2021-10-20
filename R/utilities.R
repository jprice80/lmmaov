#Round all numeric variables in a dataframe
round_df <- function(object, digits=4) {

  if(!is.data.frame(object)){
    stop(paste("The object", object, "is not a data.frame"));
  }

  nums <- vapply(object, is.numeric, FUN.VALUE = logical(1))

  object[,nums] <- round(object[,nums], digits = digits)
  row.names(object)<-NULL

  return(object)
}

#Round all numeric variables in a list of dataframes
round_list <- function(object, digits=4) {

  if(!is.list(object)){
    stop(paste("The object", object, "is not a list"));
  }

  for(i in 1:length(object)){
    temp<-object[[i]]

    nums <- vapply(temp, is.numeric, FUN.VALUE = logical(1))

    temp[,nums] <- round(temp[,nums], digits = digits)
    row.names(temp)<-NULL
    object[[i]]<-temp
  }

  return(object)
}

#====================================== Data Classes ===================================================

dataClasses <- function(model){
  temp <- attr(model$modelInfo$terms$cond$fixed, "dataClasses")
  out <- data.frame(terms=names(temp), class=as.vector(temp))
  return(out)
}

