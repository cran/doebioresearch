#' @title Analysis of Randomized Complete Block Design
#' @description The function gives ANOVA, R-square of the model, normality testing of residuals, SEm (standard error of mean), SEd (standard error of difference), interpretation of ANOVA results and multiple comparison test for means.
#' @param data dependent variables
#' @param treatmentvector vector containing treatments
#' @param replicationvector vector containing replications
#' @param MultipleComparisonTest 0 for no test, 1 for LSD test, 2 for Duncan test and 3 for HSD test
#' @importFrom agricolae LSD.test HSD.test duncan.test
#' @importFrom stats anova lm shapiro.test
#' @return ANOVA, interpretation of ANOVA, R-square, normality test result, SEm, SEd and multiple comparison test result
#' @export
#' @examples
#' data<-data.frame(GFY=c(16,13,14,16,16,17,16,17,16,16,17,16,15,15,15,13,15,14,
#' 16,14,15,14,15,17,18,15,15,15,14,14,14,14,15,15,13,15,14,14,13,13,13,12,15,12,15),
#' DMY=c(5,5,6,5,6,7,6,8,6,9,8,7,5,5,5,4,6,5,8,5,5,5,4,6,6,5,5,6,6,6,5,5,5,5,5,6,5,5,5,4,5,4,5,5,5),
#' Rep=rep(c("R1","R2","R3"),each=15),
#' Trt=rep(c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13","T14","T15"),3))
#' #' #RCBD analysis with duncan test for GFY only
#' rcbd(data[1],data$Trt,data$Rep,2)
#' #RCBD analysis with duncan test for both GFY and DMY
#' rcbd(data[1:2],data$Trt,data$Rep,2)
rcbd<-function(data,treatmentvector,replicationvector,MultipleComparisonTest){
  rbd.analysis<-function(data2,treatmentvector,replicationvector,mean.comparison.test){
    trt<-as.factor(treatmentvector)
    replication<-factor(replicationvector)
    r<-nlevels(replication)
    model<-lm(data2~replication+trt)
    n.test<-shapiro.test(model$residuals)
    n.result<-n.test$p.value
    if (n.result > 0.05){
      n.result<-"Normality assumption is not violated"
    } else {
      n.result<-"Normality assumption is violated"
    }
    anova.model<-anova(model)
    Sem<-sqrt(anova.model[3,3]/r)
    Sed<-sqrt(2)*Sem
    SEM<-paste("SEm",round(Sem,4),",","SEd",round(Sed,4))
    summary.model<-summary(model)
    R.square<-paste("R Square",round(summary.model$r.squared,digits = 3))
    if (anova.model[2,5]>0.05){
      multiple.comparison<-"All the treatment means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison<-"The treatment means of one or more treatments are not same, so go for multiple comparison test"
    }
    if (mean.comparison.test == 0){
      m.test<-"No multiple comparison test selected"
    }
    if (mean.comparison.test == 2){
      m.test1<-duncan.test(data2,trt,anova.model[3,1],anova.model[3,3])
      m.test<-list(m.test1$statistics,m.test1$duncan,m.test1$groups)
    }
    if (mean.comparison.test == 3){
      m.test1<-HSD.test(data2,trt,anova.model[3,1],anova.model[3,3])
      m.test<-list(m.test1$statistics,m.test1$parameters,m.test1$groups)
    }
    if (mean.comparison.test == 1){
      m.test1<-LSD.test(data2,trt,anova.model[3,1],anova.model[3,3])
      m.test<-list(m.test1$statistics,m.test1$groups)
    }
    my.list<-list(anova.model,R.square,n.test,n.result,SEM,multiple.comparison,m.test)
    return(my.list)
  }
  fiftn<-convert(data)
  colnumber<-ncol(data)
  output<-list()
  for (j in 1:colnumber){
    output[[j]]<-rbd.analysis(fiftn[[j]],treatmentvector,replicationvector,MultipleComparisonTest)
  }
  names(output)<-names(data)
  return(output)
}
