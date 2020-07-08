#' @title Analysis of Factorial Completely Randomized Design for 3 factors
#' @description The function gives ANOVA, R-square of the model, normality testing of residuals, SEm (standard error of mean), SEd (standard error of difference), interpretation of ANOVA results and multiple comparison test for means.
#' @param data dependent variables
#' @param fact.A vector containing levels of first factor
#' @param fact.B vector containing levels of second factor
#' @param fact.C vector containing levels of third factor
#' @importFrom agricolae LSD.test HSD.test duncan.test
#' @importFrom stats anova lm shapiro.test
#' @param Multiple.comparison.test 0 for no test, 1 for LSD test, 2 for Duncan test and 3 for HSD test
#' @return ANOVA, interpretation of ANOVA, R-square, normality test result, SEm, SEd and multiple comparison test result for both the factors as well as interaction.
#' @export
#' @examples
#' data(factorialdata)
#' #FCRD analysis along with dunccan test for two dependent var.
#' fcrd3fact(factorialdata[5:6],factorialdata$Nitrogen,
#' factorialdata$Phosphorus,factorialdata$Potassium,2)
fcrd3fact<-function(data,fact.A,fact.B,fact.C,Multiple.comparison.test){
  fcrd.3fact1<-function(dependent.var,fact.A,fact.B,fact.C,mean.comparison.test){
    dependent.var<-as.numeric(dependent.var)
    fact.A<-as.factor(fact.A)
    fact.B<-as.factor(fact.B)
    fact.C<-as.factor(fact.C)
    r<-(length(dependent.var)/(nlevels(fact.A)*nlevels(fact.B)*nlevels(fact.C)))
    model<-lm(dependent.var~fact.A+fact.B+fact.C+fact.A:fact.B+fact.A:fact.C+fact.B:fact.C+fact.A:fact.B:fact.C)
    n.test<-shapiro.test(model$residuals)
    n.result<-n.test$p.value
    if (n.result > 0.05){
      n.result<-"Normality assumption is not violated"
    } else {
      n.result<-"Normality assumption is violated"
    }
    anova.model<-anova(model)
    SEa<-sqrt(anova.model[8,3]/(r*nlevels(fact.B)*nlevels(fact.C)))
    SEb<-sqrt(anova.model[8,3]/(r*nlevels(fact.A)*nlevels(fact.C)))
    SEc<-sqrt(anova.model[8,3]/(r*nlevels(fact.A)*nlevels(fact.B)))
    SEab<-sqrt(anova.model[8,3]/(r*nlevels(fact.C)))
    SEac<-sqrt(anova.model[8,3]/(r*nlevels(fact.B)))
    SEbc<-sqrt(anova.model[8,3]/(r*nlevels(fact.A)))
    SEabc<-sqrt(anova.model[8,3]/r)
    Sea<-sqrt(2)*SEa
    Seb<-sqrt(2)*SEb
    Sec<-sqrt(2)*SEc
    Seab<-sqrt(2)*SEab
    Seac<-sqrt(2)*SEac
    Sebc<-sqrt(2)*SEbc
    Seabc<-sqrt(2)*SEabc
    SEM<-paste("SEm of A:",round(SEa,3),",","SEd of A:",round(Sea,3),",","SEm of B:",round(SEb,3),",","SEd of B",round(Seb,3),",","SEm of C:",round(SEc,3),",","SEd of C:",round(Sec,3),",","SEm of AB:",round(SEab,3),",","SEd of AB:",round(Seab,3),",","SEm of AC:",round(SEac,3),",","SEd of AC:",round(Seac,3),",","SEm of BC:",round(SEbc,3),",","SEd of BC:",round(Sebc,3),",","SEm of ABC:",round(SEabc,3),",","SEd of ABC:",round(Seabc,3))
    summary.model<-summary(model)
    R.square<-paste("R Square",round(summary.model$r.squared,digits = 3))
    if (anova.model[1,5]>0.05){
      multiple.comparison.A<-"All the factor A level means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.A<-"The means of one or more levels of factor A are not same, so go for multiple comparison test"
    }
    if (anova.model[2,5]>0.05){
      multiple.comparison.B<-"All the factor B level means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.B<-"The means of levels of factor B are not same, so go for multiple comparison test"
    }
    if (anova.model[3,5]>0.05){
      multiple.comparison.C<-"All the factor C level means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.C<-"The means of one or more levels of factor C are not same, so go for multiple comparison test"
    }
    if (anova.model[4,5]>0.05){
      multiple.comparison.AB<-"The means of levels of interaction between A and B factors are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.AB<-"The means of levels of interaction between A and B factors are not same, so go for multiple comparison test"
    }
    if (anova.model[5,5]>0.05){
      multiple.comparison.AC<-"The means of levels of interaction between A and C factors are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.AC<-"The means of levels of interaction between A and C factors are not same, so go for multiple comparison test"
    }
    if (anova.model[6,5]>0.05){
      multiple.comparison.BC<-"The means of levels of interaction between B and C factors are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.BC<-"The means of levels of interaction between B and C factors are not same, so go for multiple comparison test"
    }
    if (anova.model[7,5]>0.05){
      multiple.comparison.ABC<-"The means of levels of interaction between all the three factors ABC are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.ABC<-"The means of levels of interaction between all the three factors ABC are not same, so go for multiple comparison test"
    }
    if (mean.comparison.test == 2){
      m.test1A<-duncan.test(dependent.var,fact.A,anova.model[8,1],anova.model[8,3])
      m.testA<-list(m.test1A$statistics,m.test1A$duncan,m.test1A$groups)
      m.test1B<-duncan.test(dependent.var,fact.B,anova.model[8,1],anova.model[8,3])
      m.testB<-list(m.test1B$statistics,m.test1B$duncan,m.test1B$groups)
      m.test1C<-duncan.test(dependent.var,fact.C,anova.model[8,1],anova.model[8,3])
      m.testC<-list(m.test1C$statistics,m.test1C$duncan,m.test1C$groups)
      m.test1AB<-duncan.test(dependent.var,fact.A:fact.B,anova.model[8,1],anova.model[8,3])
      m.testAB<-list(m.test1AB$statistics,m.test1AB$duncan,m.test1AB$groups)
      m.test1AC<-duncan.test(dependent.var,fact.A:fact.C,anova.model[8,1],anova.model[8,3])
      m.testAC<-list(m.test1AC$statistics,m.test1AC$duncan,m.test1AC$groups)
      m.test1BC<-duncan.test(dependent.var,fact.B:fact.C,anova.model[8,1],anova.model[8,3])
      m.testBC<-list(m.test1BC$statistics,m.test1BC$duncan,m.test1BC$groups)
      m.test1ABC<-duncan.test(dependent.var,fact.A:fact.B:fact.C,anova.model[8,1],anova.model[8,3])
      m.testABC<-list(m.test1ABC$statistics,m.test1ABC$duncan,m.test1ABC$groups)
    }
    if (mean.comparison.test == 3){
      m.test1A<-HSD.test(dependent.var,fact.A,anova.model[8,1],anova.model[8,3])
      m.testA<-list(m.test1A$statistics,m.test1A$parameters,m.test1A$groups)
      m.test1B<-HSD.test(dependent.var,fact.B,anova.model[8,1],anova.model[8,3])
      m.testB<-list(m.test1B$statistics,m.test1B$parameters,m.test1B$groups)
      m.test1C<-HSD.test(dependent.var,fact.C,anova.model[8,1],anova.model[8,3])
      m.testC<-list(m.test1C$statistics,m.test1C$parameters,m.test1C$groups)
      m.test1AB<-HSD.test(dependent.var,fact.A:fact.B,anova.model[8,1],anova.model[8,3])
      m.testAB<-list(m.test1AB$statistics,m.test1AB$parameters,m.test1AB$groups)
      m.test1AC<-HSD.test(dependent.var,fact.A:fact.C,anova.model[8,1],anova.model[8,3])
      m.testAC<-list(m.test1AC$statistics,m.test1AC$parameters,m.test1AC$groups)
      m.test1BC<-HSD.test(dependent.var,fact.B:fact.C,anova.model[8,1],anova.model[8,3])
      m.testBC<-list(m.test1BC$statistics,m.test1BC$parameters,m.test1BC$groups)
      m.test1ABC<-HSD.test(dependent.var,fact.A:fact.B:fact.C,anova.model[8,1],anova.model[8,3])
      m.testABC<-list(m.test1ABC$statistics,m.test1ABC$parameters,m.test1ABC$groups)
    }
    if (mean.comparison.test == 1){
      m.test1A<-LSD.test(dependent.var,fact.A,anova.model[8,1],anova.model[8,3])
      m.testA<-list(m.test1A$statistics,m.test1A$groups)
      m.test1B<-LSD.test(dependent.var,fact.B,anova.model[8,1],anova.model[8,3])
      m.testB<-list(m.test1B$statistics,m.test1B$groups)
      m.test1C<-LSD.test(dependent.var,fact.C,anova.model[8,1],anova.model[8,3])
      m.testC<-list(m.test1C$statistics,m.test1C$groups)
      m.test1AB<-LSD.test(dependent.var,fact.A:fact.B,anova.model[8,1],anova.model[8,3])
      m.testAB<-list(m.test1AB$statistics,m.test1AB$groups)
      m.test1AC<-LSD.test(dependent.var,fact.A:fact.C,anova.model[8,1],anova.model[8,3])
      m.testAC<-list(m.test1AC$statistics,m.test1AC$groups)
      m.test1BC<-LSD.test(dependent.var,fact.B:fact.C,anova.model[8,1],anova.model[8,3])
      m.testBC<-list(m.test1BC$statistics,m.test1BC$groups)
      m.test1ABC<-LSD.test(dependent.var,fact.A:fact.B:fact.C,anova.model[8,1],anova.model[8,3])
      m.testABC<-list(m.test1ABC$statistics,m.test1ABC$groups)
    }
    if (mean.comparison.test == 0){
      m.testA<-"No multiple comparison test selected"
      m.testB<-m.testA
      m.testC<-m.testA
      m.testAB<-m.testA
      m.testAC<-m.testA
      m.testBC<-m.testA
      m.testABC<-m.testA
    }
    my.list<-list(anova.model,R.square,SEM,n.test,n.result,multiple.comparison.A,m.testA,multiple.comparison.B,m.testB,multiple.comparison.C,m.testC,multiple.comparison.AB,m.testAB,multiple.comparison.BC,m.testBC,multiple.comparison.AC,m.testAC,multiple.comparison.ABC,m.testABC)
    return(my.list)
  }
  fiftn<-convert(data)
  colnumber<-ncol(data)
  output<-list()
  for (j in 1:colnumber){
    output[[j]]<-fcrd.3fact1(fiftn[[j]],fact.A,fact.B,fact.C,Multiple.comparison.test)
  }
  names(output)<-names(data)
  return(output)
}
