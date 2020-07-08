#' @title Analysis of Factorial Randomized Block Design for 2 factors
#' @description The function gives ANOVA, R-square of the model, normality testing of residuals, SEm (standard error of mean), SEd (standard error of difference), interpretation of ANOVA results and multiple comparison test for means.
#' @param data dependent variables
#' @param replicationvector vector containing replications
#' @param fact.A vector containing levels of first factor
#' @param fact.B vector containing levels of second factor
#' @param Multiple.comparison.test 0 for no test, 1 for LSD test, 2 for Duncan test and 3 for HSD test
#' @importFrom agricolae LSD.test HSD.test duncan.test
#' @importFrom stats anova lm shapiro.test
#' @return ANOVA, interpretation of ANOVA, R-square, normality test result, SEm, SEd and multiple comparison test results for both the factors as well as interaction.
#' @export
#' @examples
#' data(factorialdata)
#' #FRBD analysis along with dunccan test for two dependent var.
#' frbd2fact(factorialdata[5:6],factorialdata$Replication,
#' factorialdata$Nitrogen,factorialdata$Phosphorus,2)
frbd2fact<-function(data,replicationvector,fact.A,fact.B,Multiple.comparison.test){
  frbd.2fact1<-function(dependent.var,replicationvector,fact.A,fact.B,mean.comparison.test){
    dependent.var<-as.numeric(dependent.var)
    fact.A<-as.factor(fact.A)
    fact.B<-as.factor(fact.B)
    replicationvector<-as.factor(replicationvector)
    r<-nlevels(replicationvector)
    model<-lm(dependent.var~replicationvector+fact.A+fact.B+fact.A:fact.B)
    n.test<-shapiro.test(model$residuals)
    n.result<-n.test$p.value
    if (n.result > 0.05){
      n.result<-"Normality assumption is not violated"
    } else {
      n.result<-"Normality assumption is violated"
    }
    anova.model<-anova(model)
    SEa<-sqrt(anova.model[5,3]/(r*nlevels(fact.B)))
    SEb<-sqrt(anova.model[5,3]/(r*nlevels(fact.A)))
    SEab<-sqrt(anova.model[5,3]/r)
    Sea<-sqrt(2)*SEa
    Seb<-sqrt(2)*SEb
    Seab<-sqrt(2)*SEab
    SEM<-paste("SEm of A:",round(SEa,3),",","SEd of A:",round(Sea,3),",","SEm of B:",round(SEb,3),",","SEd of B",round(Seb,3),",","SEm of AB:",round(SEab,3),",","SEd of AB:",round(Seab,3))
    summary.model<-summary(model)
    R.square<-paste("R Square",round(summary.model$r.squared,digits = 3))
    if (anova.model[2,5]>0.05){
      multiple.comparison.A<-"All the first factor level means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.A<-"The means of one or more levels of first factor are not same, so go for multiple comparison test"
    }
    if (anova.model[3,5]>0.05){
      multiple.comparison.B<-"All the second factor level means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.B<-"The means of one or more levels of second factor are not same, so go for multiple comparison test"
    }
    if (anova.model[4,5]>0.05){
      multiple.comparison.AB<-"The means of levels of interaction between two factors are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.AB<-"The means of levels of interaction between two factors are not same, so go for multiple comparison test"
    }
    if (mean.comparison.test == 2){
      m.test1A<-duncan.test(dependent.var,fact.A,anova.model[5,1],anova.model[5,3])
      m.testA<-list(m.test1A$statistics,m.test1A$duncan,m.test1A$groups)
      m.test1B<-duncan.test(dependent.var,fact.B,anova.model[5,1],anova.model[5,3])
      m.testB<-list(m.test1B$statistics,m.test1B$duncan,m.test1B$groups)
      m.test1AB<-duncan.test(dependent.var,fact.A:fact.B,anova.model[5,1],anova.model[5,3])
      m.testAB<-list(m.test1AB$statistics,m.test1AB$duncan,m.test1AB$groups)
    }
    if (mean.comparison.test == 3){
      m.test1A<-HSD.test(dependent.var,fact.A,anova.model[5,1],anova.model[5,3])
      m.testA<-list(m.test1A$statistics,m.test1A$parameters,m.test1A$groups)
      m.test1B<-HSD.test(dependent.var,fact.B,anova.model[5,1],anova.model[5,3])
      m.testB<-list(m.test1B$statistics,m.test1B$parameters,m.test1B$groups)
      m.test1AB<-HSD.test(dependent.var,fact.A:fact.B,anova.model[5,1],anova.model[5,3])
      m.testAB<-list(m.test1AB$statistics,m.test1AB$parameters,m.test1AB$groups)
    }
    if (mean.comparison.test == 1){
      m.test1A<-LSD.test(dependent.var,fact.A,anova.model[5,1],anova.model[5,3])
      m.testA<-list(m.test1A$statistics,m.test1A$groups)
      m.test1B<-LSD.test(dependent.var,fact.B,anova.model[5,1],anova.model[5,3])
      m.testB<-list(m.test1B$statistics,m.test1B$groups)
      m.test1AB<-LSD.test(dependent.var,fact.A:fact.B,anova.model[5,1],anova.model[5,3])
      m.testAB<-list(m.test1AB$statistics,m.test1AB$groups)
    }
    if (mean.comparison.test == 0){
      m.testA<-"No multiple comparison test selected"
      m.testB<-"No multiple comparison test selected"
      m.testAB<-"No multiple comparison test selected"
    }
    my.list<-list(anova.model,R.square,SEM,n.test,n.result,multiple.comparison.A,m.testA,multiple.comparison.B,m.testB,multiple.comparison.AB,m.testAB)
    return(my.list)
  }
  fiftn<-convert(data)
  colnumber<-ncol(data)
  output<-list()
  for (j in 1:colnumber){
    output[[j]]<-frbd.2fact1(fiftn[[j]],replicationvector,fact.A,fact.B,Multiple.comparison.test)
  }
  names(output)<-names(data)
  return(output)
}
