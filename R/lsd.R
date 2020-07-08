#' @title Analysis of Latin Square Design
#' @description The function gives ANOVA, R-square of the model, normality testing of residuals, SEm (standard error of mean), SEd (standard error of difference), interpretation of ANOVA results and multiple comparison test for means.
#' @param data dependent variables
#' @param treatmentvector vector containing treatments
#' @param row vector for rows
#' @param column vector for columns
#' @param MultipleComparisonTest 0 for no test, 1 for LSD test, 2 for Duncan test and 3 for HSD test
#' @importFrom agricolae LSD.test HSD.test duncan.test
#' @importFrom stats anova lm shapiro.test
#' @return ANOVA, interpretation of ANOVA, R-square, normality test result, SEm, SEd and multiple comparison test result
#' @export
#' @examples
#' data(lsddata)
#' #LSD analysis with LSD test for Yield only
#' lsd(lsddata[4],lsddata$Treatment,lsddata$Row,lsddata$Column,1)
#' #LSD analysis with LSD test for Yield and Plant Height
#' lsd(lsddata[4:5],lsddata$Treatment,lsddata$Row,lsddata$Column,1)
lsd<-function(data,treatmentvector,row,column,MultipleComparisonTest){
  lsd.analysis<-function(data2,treatmentvector,row,column,mean.comparison.test){
    trt<-as.factor(treatmentvector)
    row<-factor(row)
    column<-factor(column)
    r<-length(data2)/nlevels(factor(treatmentvector))
    model<-lm(data2~row+column+trt)
    n.test<-shapiro.test(model$residuals)
    n.result<-n.test$p.value
    if (n.result > 0.05){
      n.result<-"Normality assumption is not violated"
    } else {
      n.result<-"Normality assumption is violated"
    }
    anova.model<-anova(model)
    Sem<-sqrt(anova.model[4,3]/r)
    Sed<-sqrt(2)*Sem
    SEM<-paste("SEm",round(Sem,4),",","SEd",round(Sed,4))
    summary.model<-summary(model)
    R.square<-paste("R Square",round(summary.model$r.squared,digits = 3))
    if (anova.model[3,5]>0.05){
      multiple.comparison<-"All the treatment means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison<-"The treatment means of one or more treatments are not same, so go for multiple comparison test"
    }
    if (mean.comparison.test == 0){
      m.test<-"No multiple comparison test selected"
    }
    if (mean.comparison.test == 2){
      m.test1<-duncan.test(data2,trt,anova.model[4,1],anova.model[4,3])
      m.test<-list(m.test1$statistics,m.test1$duncan,m.test1$groups)
    }
    if (mean.comparison.test == 3){
      m.test1<-HSD.test(data2,trt,anova.model[4,1],anova.model[4,3])
      m.test<-list(m.test1$statistics,m.test1$parameters,m.test1$groups)
    }
    if (mean.comparison.test == 1){
      m.test1<-LSD.test(data2,trt,anova.model[4,1],anova.model[4,3])
      m.test<-list(m.test1$statistics,m.test1$groups)
    }
    my.list<-list(anova.model,R.square,n.test,n.result,SEM,multiple.comparison,m.test)
    return(my.list)
  }
  fiftn<-convert(data)
  colnumber<-ncol(data)
  output<-list()
  for (j in 1:colnumber){
    output[[j]]<-lsd.analysis(fiftn[[j]],treatmentvector,row,column,MultipleComparisonTest)
  }
  names(output)<-names(data)
  return(output)
}
