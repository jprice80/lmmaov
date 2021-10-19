#' Normality Tests for variables within a data frame
#'
#' @description The \code{normality} function is used to perform normality tests on a set of variables in a \code{\link{data.frame}}.
#' This can include by group normality tests when including a vector of categorical variables in the groupby= argument.
#' The Shapiro-Wilk Test for normality will be performed in all cases. If the sample size of all groups >= 20,
#' then the D'Agostino Omnibus normality test will also be performed.
#'
#' @param data a \code{\link{data.frame}} containing columns of variables.
#' @param vars a \code{\link{vector}} of variable names existing in the dataframe to perform normality tests on.
#' @param groupby an optional \code{\link{vector}} containing factor names within a dataframe used for by group processing.
#' @param energy a TRUE/FALSE boolean value indicating whether or not to perform the energy test (Székely and Rizzo) with bootstrap p-values.
#' @param R the number of boostrapped resamples.  Unused if energy=FALSE.
#'
#' @return A \code{\link{list}} containing normality tests results for each variable listed in the \code{vars} statement.
#'
#' @references
#' Szekely, G. J. and Rizzo, M. L. (2005) A New Test for Multivariate Normality, Journal of Multivariate Analysis, 93/1, 58-80, http://dx.doi.org/10.1016/j.jmva.2003.12.002.
#'
#' Rizzo, M. L. (2002). A New Rotation Invariant Goodness-of-Fit Test, Ph.D. dissertation, Bowling Green State University.
#'
#' J. P. Imhof (1961). Computing the Distribution of Quadratic Forms in Normal Variables, Biometrika, Volume 48, Issue 3/4, 419-426.
#'
#' D'Agostino R.B., Pearson E.S. (1973); Tests for Departure from Normality, Biometrika 60, 613–22.
#'
#' D'Agostino R.B., Rosman B. (1974); The Power of Geary's Test of Normality, Biometrika 61, 181–84.
#'
#' Shapiro S.S., Francia R.S. (1972); An Approximate Analysis of Variance Test for Normality, JASA 67, 215–216.
#'
#' Shapiro S.S., Wilk M.B., Chen V. (1968); A Comparative Study of Various Tests for Normality, JASA 63, 1343–72.
#' @export
#'
#' @examples
#' normality(data=mtcars, vars=c("mpg","disp"), groupby=c("vs"))
normality<-function(data, vars, groupby=NULL, energy=FALSE, R=2000, plots=FALSE, conf.level=0.95){

  #Check to insure data is a dataframe
  if(!is.data.frame(data)){
    stop(paste("The object", data, "is not a dataframe"));
  }

  # check to see if all vars are numeric
  temp <- data %>% select(vars)
  temp <- as.vector(sapply(temp,is.numeric))

  if(all(temp)==FALSE){
    stop(paste("All variables listed in the vars= statement are not numeric"));
  }

  #Setup output template
  #Start actual function
  out<-list()
  out$meta<-list()
  out$output<-list()

  out[["meta"]][["data"]]<-data
  out[["meta"]][["vars"]]<-vars
  out[["meta"]][["groupby"]]<-groupby
  out[["meta"]][["energy"]]<-energy
  out[["meta"]][["R"]]<-R

  tab<-data.frame()
  if(is.null(groupby)){

    for(i in 1:length(vars)){

      varname<-vars[i]

      #total number of complete cases
      tot<-sum(complete.cases(mtcars[[vars[i]]])==TRUE)

      if(energy == FALSE){
        if(tot >= 20){
          dat<-data %>%
            summarise(list(sw = fBasics::shapiroTest(!!sym(vars[i]))@test),
                      list(da = fBasics::dagoTest(!!sym(vars[i]))@test))

          W<-round(as.numeric(dat[[1]]$sw$statistic), 4)
          swpval<-round(as.numeric(dat[[1]]$sw$p.value), 4)
          dachisq<-round(as.numeric(dat[[2]]$da$statistic[1]), 4)
          dapval<-round(as.numeric(dat[[2]]$da$p.value[1]), 4)

        } else {
          dat<-data %>%
            summarise(list(sw = fBasics::shapiroTest(!!sym(vars[i]))@test))

          W<-round(as.numeric(dat[[1]]$sw$statistic), 4)
          swpval<-round(as.numeric(dat[[1]]$sw$p.value), 4)
          dachisq<-NA
          dapval<-NA
        }
      } else {
        if(tot >= 20){
          dat<-data %>%
            summarise(list(sw = fBasics::shapiroTest(!!sym(vars[i]))@test),
                      list(da = fBasics::dagoTest(!!sym(vars[i]))@test),
                      list(en = energy::normal.test(!!sym(vars[i]), R=R)))

          W<-round(as.numeric(dat[[1]]$sw$statistic), 4)
          swpval<-round(as.numeric(dat[[1]]$sw$p.value), 4)
          dachisq<-round(as.numeric(dat[[2]]$da$statistic[1]), 4)
          dapval<-round(as.numeric(dat[[2]]$da$p.value[1]), 4)
          enEstat<-round(as.numeric(dat[[3]]$en$statistic), 4)
          enEpval<-round(as.numeric(dat[[3]]$en$p.value), 4)


        } else {
          dat<-data %>%
            summarise(list(sw = fBasics::shapiroTest(!!sym(vars[i]))@test),
                      list(en = energy::normal.test(!!sym(vars[i]), R=R)))

          W<-round(as.numeric(dat[[1]]$sw$statistic), 4)
          swpval<-round(as.numeric(dat[[1]]$sw$p.value), 4)
          dachisq<-NA
          dapval<-NA
          enEstat<-round(as.numeric(dat[[3]]$en$statistic), 4)
          enEpval<-round(as.numeric(dat[[3]]$en$p.value), 4)
        }
      }

      if(energy == FALSE){

        currow<-data.frame(variable = varname, group="Overall", Shapiro.Wilk.W = W, Shapiro.Wilk.Pvalue = swpval,
                           DAgostino.Chisq = dachisq, DAgostino.Pvalue = dapval)

      } else {

        currow<-data.frame(variable = varname, group="Overall", Shapiro.Wilk.W = W, Shapiro.Wilk.Pvalue = swpval,
                           DAgostino.Chisq = dachisq, DAgostino.Pvalue = dapval, Energy.E = enEstat, Energy.Pvalue = enEpval)
      }

      tab<-rbind(currow, tab)
    }

    out$output$overall<-tab
    out$plots<-plots

  } else {

    for(i in 1:length(vars)){

      varname<-vars[i]

      ns<-data %>% group_by_at(groupby) %>%
        summarise(ntotal = n(),
                  nmiss = sum(is.na(!!sym(vars[i]))),
                  nvalid = ntotal-nmiss)

      tot<-min(ns$nvalid, na.rm = TRUE)

      if(energy == FALSE){

        if(tot >= 20){
          dat<-data %>% group_by_at(groupby) %>%
            summarise(list(sw = fBasics::shapiroTest(!!sym(vars[i]))@test),
                      list(da = fBasics::dagoTest(!!sym(vars[i]))@test))

          ns$ntotal<-NULL
          ns$nmiss<-NULL
          ns$nvalid<-NULL

          tab<-data.frame(variable=varname, ns, Shapiro.Wilk.W = NA, Shapiro.Wilk.Pvalue = NA, DAgostino.Chisq = NA, DAgostino.Pvalue = NA)

          for(j in 1:nrow(ns)){
            tab$Shapiro.Wilk.W[j]<-round(as.numeric(dat[[j]]$sw$statistic), 4)
            tab$Shapiro.Wilk.Pvalue[j]<-round(as.numeric(dat[[j]]$sw$p.value), 4)
            tab$DAgostino.Chisq[j]<-round(as.numeric(dat[[j]]$da$statistic[1]), 4)
            tab$DAgostino.Pvalue[j]<-round(as.numeric(dat[[j]]$da$p.value[1]), 4)
          }

          out$output[[varname]]<-tab

        } else {
          dat<-data %>% group_by_at(groupby) %>%
            summarise(list(sw = fBasics::shapiroTest(!!sym(vars[i]))@test))

          dat<-dat[[length(dat)]]

          ns$ntotal<-NULL
          ns$nmiss<-NULL
          ns$nvalid<-NULL

          tab<-data.frame(variable=varname, ns, Shapiro.Wilk.W = NA, Shapiro.Wilk.Pvalue = NA, DAgostino.Chisq = NA, DAgostino.Pvalue = NA)

          for(j in 1:nrow(ns)){
            tab$Shapiro.Wilk.W[j]<-round(as.numeric(dat[[j]]$statistic), 4)
            tab$Shapiro.Wilk.Pvalue[j]<-round(as.numeric(dat[[j]]$p.value), 4)
          }

          out$output[[varname]]<-tab
        }
      } else {

        #If energy == TRUE

        if(tot >= 20){
          dat<-data %>% group_by_at(groupby) %>%
            summarise(list(sw = fBasics::shapiroTest(!!sym(vars[i]))@test),
                      list(da = fBasics::dagoTest(!!sym(vars[i]))@test),
                      list(en = energy::normal.test(!!sym(vars[i]), R=R)))

          ns$ntotal<-NULL
          ns$nmiss<-NULL
          ns$nvalid<-NULL

          tab<-data.frame(variable=varname, ns, Shapiro.Wilk.W = NA, Shapiro.Wilk.Pvalue = NA, DAgostino.Chisq = NA, DAgostino.Pvalue = NA,
                          Energy.E = NA, Energy.Pvalue = NA)

          for(j in 1:nrow(ns)){
            tab$Shapiro.Wilk.W[j]<-round(as.numeric(dat[[j]]$sw$statistic), 4)
            tab$Shapiro.Wilk.Pvalue[j]<-round(as.numeric(dat[[j]]$sw$p.value), 4)
            tab$DAgostino.Chisq[j]<-round(as.numeric(dat[[j]]$da$statistic[1]), 4)
            tab$DAgostino.Pvalue[j]<-round(as.numeric(dat[[j]]$da$p.value[1]), 4)
            tab$Energy.E[j]<-round(as.numeric(dat[[j]]$statistic), 4)
            tab$Energy.Pvalue[j]<-round(as.numeric(dat[[j]]$p.value), 4)
          }

          out$output[[varname]]<-tab

        } else {
          dat<-data %>% group_by_at(groupby) %>%
            summarise(list(sw = fBasics::shapiroTest(!!sym(vars[i]))@test),
                      list(en = energy::normal.test(!!sym(vars[i]), R=R)))

          dat<-dat[[length(dat)]]

          ns$ntotal<-NULL
          ns$nmiss<-NULL
          ns$nvalid<-NULL

          tab<-data.frame(variable=varname, ns, Shapiro.Wilk.W = NA, Shapiro.Wilk.Pvalue = NA, DAgostino.Chisq = NA, DAgostino.Pvalue = NA,
                          Energy.E = NA, Energy.Pvalue = NA)

          for(j in 1:nrow(ns)){
            tab$Shapiro.Wilk.W[j]<-round(as.numeric(dat[[j]]$statistic), 4)
            tab$Shapiro.Wilk.Pvalue[j]<-round(as.numeric(dat[[j]]$p.value), 4)
            tab$Energy.E[j]<-round(as.numeric(dat[[j]]$statistic), 4)
            tab$Energy.Pvalue[j]<-round(as.numeric(dat[[j]]$p.value), 4)
          }

          out$output[[varname]]<-tab
        }
      }
    }
  }

  class(out)<-c("lmm", "norm")

  if(plots==TRUE){
    plot.lmm(out, conf.level = conf.level)
  }

  return(out)
}

#====================================== Plots ===================================================

plot_normality<-function(object, conf.level = 0.95){
  data<-object[["meta"]][["data"]]
  vars<-object[["meta"]][["vars"]]
  groupby<-object[["meta"]][["groupby"]]

  out<-list()
  out[["plots"]]<-list()
  plots<-list()

  if(is.null(groupby)){

    for(i in 1:length(vars)){

      varname<-vars[i]

      plt_hist<-ggplot(data, aes(x = !!sym(vars[i]))) +
        geom_histogram(color="darkblue", fill="lightblue",
                       binwidth = function(x) (max(x, na.rm = TRUE)-min(x, na.rm = TRUE))/grDevices::nclass.Sturges(x)) +
        #stat_function(fun = function(x)
        #dnorm(x, mean = mean(x), sd = sd(x)) * grDevices::nclass.scott(x) * nrow(data), color = "red", size = 1) +
        ggtitle(paste0("Histogram of ", vars[[i]])) + theme_bw() + labs(y = "Frequency") +
        theme(plot.title = element_text(size=10, hjust = 0.5, face="bold"),
              axis.title.x = element_text(size=10, color="black", face="bold"),
              axis.title.y = element_text(size=10, color="black", face="bold"))

      #print(plt_hist)

      plt_qq<-ggplot(data = data, mapping = aes(sample = !!sym(vars[i]))) +
        qqplotr::stat_qq_band(alpha=0.5, conf=0.95, qtype=1, bandType = "boot", fill="red") +
        qqplotr::stat_qq_line(identity=TRUE) +
        qqplotr::stat_qq_point(col="black") +
        ggtitle(paste0("Normal QQ Plot of ", vars[[i]], " with ", conf.level*100,"% Bootstrap CIs")) +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() +
        theme(plot.title = element_text(size=10, hjust = 0.5, face="bold"),
              axis.title.x = element_text(size=10, color="black", face="bold"),
              axis.title.y = element_text(size=10, color="black", face="bold"))

      #print(plt_qq)

      out$plots[[varname]][[1]]<-list(plt_hist, plt_qq)
    }

  } else {
    #create plots for each outcome

    for(i in 1:length(vars)){

      varname<-vars[i]

      plt_hist<-data %>% group_by_at(groupby) %>%
        do(plots=ggplot(data=.) + aes(x = !!sym(vars[i])) +
             geom_histogram(color="darkblue", fill="lightblue", binwidth = function(x) (max(x, na.rm = TRUE)-min(x, na.rm = TRUE))/grDevices::nclass.Sturges(x)) +
             ggtitle(paste0("Histogram of ", vars[[i]])) + theme_bw() + labs(y = "Frequency") +
             theme(plot.title = element_text(size=10, hjust = 0.5, face="bold"),
                   axis.title.x = element_text(size=10, color="black", face="bold"),
                   axis.title.y = element_text(size=10, color="black", face="bold")))

      plt_qq<-data %>% group_by_at(groupby) %>%
        do(plots=ggplot(data = ., mapping = aes(sample = !!sym(vars[i]))) +
             qqplotr::stat_qq_band(alpha=0.5, conf=conf.level, qtype=1, bandType = "boot", fill="red") +
             qqplotr::stat_qq_line(identity=TRUE) +
             qqplotr::stat_qq_point(col="black") +
             ggtitle(paste0("Normal QQ Plot of ", vars[[i]], " with ", conf.level*100,"% Boostrap CIs")) +
             labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() +
             theme(plot.title = element_text(size=10, hjust = 0.5, face="bold"),
                   axis.title.x = element_text(size=10, color="black", face="bold"),
                   axis.title.y = element_text(size=10, color="black", face="bold")))

      #define titles for use later
      nm<-names(plt_hist)
      nm<-nm[-length(nm)]
      nm<-cbind(varname, nm, plt_hist[,-ncol(plt_hist)])
      titles<-c()
      for(i in 1:nrow(nm)){
        titlei<-apply(nm[i,], 1, paste, collapse = "-")
        titles[i]<-titlei
      }


      #Reorder plots to correctly pair Histogram and QQs and identify levels
      for(i in 1:nrow(plt_hist)){

        one<-plt_hist[[ncol(plt_hist)]][[i]]
        one<-one+ggtitle(paste0("Histogram of ", titles[i])) +
          theme(plot.title = element_text(size=10, hjust = 0.5, face="bold"))

        two<-plt_qq[[ncol(plt_qq)]][[i]]
        two<-two+ggtitle(paste0("Normal QQ Plot of ", titles[i], " with ", conf.level*100,"% Bootstrap CIs")) +
          theme(plot.title = element_text(size=10, hjust = 0.5, face="bold"))
        #geom_text(x = min(data[[sym(varname)]], na.rm=TRUE), y = max(data[[sym(varname)]], na.rm=TRUE), label = lev, parse = TRUE)


        varname<-one$labels$x
        out$plots[[varname]][[i]]<-list(one,two)
      }
    }
  }

  plotout<-list()
  #print out the grid
  for(i in 1:length(out[["plots"]])){
    for(j in 1:length(out[["plots"]][[i]])){

      plot1<-out[["plots"]][[i]][[j]][[1]]
      plot2<-out[["plots"]][[i]][[j]][[2]]

      plotout[[length(plotout)+1]]<-gridExtra::grid.arrange(plot1,plot2,nrow=1)
      #plotout[[length(plotout)+1]]<-cowplot::plot_grid(plot1,plot2,nrow=1)

    }
  }

  return(invisible(plotout))
}

#====================================== Summary ===================================================

summary_norm<-function(object, plots=FALSE, conf.level=0.95){
  for(i in 1:length(object$output)){
    print(object$output[[i]], row.names=FALSE)
    cat("\n")

    if(plots==TRUE){
      plot.lmm(object, conf.level =  conf.level)
    }
  }

  return(object)
}

#====================================== Reports ===================================================

report_norm<-function(object, style="multiline", plots=FALSE, conf.level=0.95, split.tables=110, keep.trailing.zeros=TRUE,  ...){

  cat("\n")
  cat("=======================================================================================================================")
  cat("\n")
  cat("Normality Tests")
  cat("\n")
  cat("=======================================================================================================================")
  cat("\n")

  for(i in 1:length(object$output)){
    out<-object$output[[i]]
    pander::pandoc.table(out, style=style, split.tables=split.tables, keep.trailing.zeros=keep.trailing.zeros, ...)
  }

  if(plots==TRUE){
    plot.lmm(object, conf.level = conf.level)
  }
}
