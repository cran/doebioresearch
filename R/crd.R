#' @title Analysis of Completely Randomized Design
#' @description The function gives ANOVA, R-square of the model, normality testing of residuals, SEm (standard error of mean), SEd (standard error of difference), interpretation of ANOVA results and multiple comparison test for means
#' @param data dependent variables
#' @param trt.vector vector containing treatments
#' @param MultipleComparisonTest 0 for no test, 1 for LSD test, 2 for Duncan test and 3 for HSD test
#' @importFrom agricolae LSD.test HSD.test duncan.test
#' @importFrom stats anova lm shapiro.test
#' @return ANOVA, interpretation of ANOVA, R-square, normality test result, SEm, SEd and multiple comparison test result
#' @export
#' @examples
#' data<-data.frame(Treatments=c("T1","T2","T3","T4","T5","T6","T7","T1","T2","T3","T4","T5","T6",
#' "T7","T1","T2","T3","T4","T5","T6","T7"),
#' yield=c(25,21,21,18,25,28,24,25,24,24,16,21,20,17,16,19,14,15,13,11,25),
#' height=c(130,120,125,135,139,140,145,136,129,135,150,152,140,148,130,135,145,160,145,130,160))
#' #CRD analysis with LSD test for yield only
#' crd(data[2],data$Treatments,1)
#' #CRD analysis with LSD test for both yield and height
#' crd(data[2:3],data$Treatments,1)
crd<-function(data,trt.vector,MultipleComparisonTest){
  crd.analysis<-function(data2,trt.vector,mean.comparison.test){
    trt<-as.factor(trt.vector)
    r<-length(data2)/nlevels(trt)
    model<-lm(data2~trt)
    n.test<-shapiro.test(model$residuals)
    n.result<-n.test$p.value
    if (n.result > 0.05){
      n.result<-"Normality assumption is not violated"
    } else {
      n.result<-"Normality assumption is violated"
    }
    anova.model<-anova(model)
    Sem<-sqrt(anova.model[2,3]/r)
    Sed<-sqrt(2)*Sem
    SEM<-paste("SEm",round(Sem,4),",","SEd",round(Sed,4))
    summary.model<-summary(model)
    R.square<-paste("R Square",round(summary.model$r.squared,digits = 3))
    if (anova.model[1,5]>0.05){
      multiple.comparison<-"All the treatment means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison<-"The treatment means of one or more treatments are not same, so go for multiple comparison test"
    }
    if (mean.comparison.test == 0){
      m.test<-"No multiple comparison test selected"
    }
    if (mean.comparison.test == 2){
      m.test1<-duncan.test(data2,trt,anova.model[2,1],anova.model[2,3])
      m.test<-list(m.test1$statistics,m.test1$duncan,m.test1$groups)
    }
    if (mean.comparison.test == 3){
      m.test1<-HSD.test(data2,trt,anova.model[2,1],anova.model[2,3])
      m.test<-list(m.test1$statistics,m.test1$parameters,m.test1$groups)
    }
    if (mean.comparison.test == 1){
      m.test1<-LSD.test(data2,trt,anova.model[2,1],anova.model[2,3])
      m.test<-list(m.test1$statistics,m.test1$groups)
    }
    my.list<-list(anova.model,multiple.comparison,R.square,n.test,n.result,SEM,m.test)
    done<-list(my.list)
    return(done)
  }
  fiftn<-convert(data)
  colnumber<-ncol(data)
  output<-list()
  for (j in 1:colnumber){
    output[[j]]<-crd.analysis(fiftn[[j]],trt.vector,MultipleComparisonTest)
  }
  names(output)<-names(data)
  return(output)
}
