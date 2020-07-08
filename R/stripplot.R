#' @title Analysis of Strip plot design
#' @description The function gives ANOVA, R-square of the model, normality testing of residuals, SEm (standard error of mean), SEd (standard error of difference), interpretation of ANOVA results and multiple comparison test for means
#' @param data dependent variables
#' @param block vector containing replications
#' @param column vector containing column strip levels
#' @param row vector containing row strip levels
#' @param mean.comparison.test 0 for no test, 1 for LSD test, 2 for Duncan test and 3 for HSD test
#' @importFrom agricolae LSD.test HSD.test duncan.test
#' @importFrom stats anova lm shapiro.test pf
#' @return ANOVA, interpretation of ANOVA, R-square, normality test result, SEm, SEd and multiple comparison test result
#' @export
#' @examples
#' data(splitdata)
#' #Split data is used for sake of demonstration
#' #Using Date of sowing as Column factor and varieties as Row factor and using LSD test for Yield only
#' stripplot(splitdata[4],splitdata$Replication,splitdata$Date_of_Sowing,splitdata$Varities,1)
#' #Using Date of sowing as Column factor and varieties as Row factor and using LSD test for both var.
#' stripplot(splitdata[4:5],splitdata$Replication,splitdata$Date_of_Sowing,splitdata$Varities,1)
stripplot<-function(data,block,column,row,mean.comparison.test){
  strip.design<-function(dependent.var,block,column,row,mean.comparison.test){
    dependent.var<-as.numeric(dependent.var)
    block<-as.factor(block)
    column<-as.factor(column)
    row<-as.factor(row)
    model<-lm(dependent.var~block+column+block:column+row+block:row+column:row)
    anova1<-as.matrix(anova(model))
    anova1[4,4]<-NA
    anova1[4,5]<-NA
    anova1[5,4]<-NA
    anova1[5,5]<-NA
    A<-anova1[4,]
    anova1[4,]<-anova1[3,]
    anova1[3,]<-A
    row.names(anova1)[3]<-"Ea"
    row.names(anova1)[2]<-"Column"
    row.names(anova1)[4]<-"Row"
    row.names(anova1)[5]<-"Eb"
    row.names(anova1)[6]<-"Interaction"
    row.names(anova1)[7]<-"Ec"
    anova1[2,4]<-round((anova1[2,3]/anova1[3,3]),3)
    anova1[2,5]<-round(pf(as.numeric(anova1[2,4]),as.numeric(anova1[2,1]),as.numeric(anova1[3,1]), lower.tail = FALSE),4)
    anova1[4,4]<-round((anova1[4,3]/anova1[5,3]),3)
    anova1[4,5]<-round(pf(as.numeric(anova1[4,4]),as.numeric(anova1[4,1]),as.numeric(anova1[5,1]), lower.tail = FALSE),4)
    CV<-paste("CV(a):",round(((sqrt(as.numeric(anova1[3,3]))/mean(dependent.var))*100),3),",","CV(b) :",round(((sqrt(as.numeric(anova1[5,3]))/mean(dependent.var))*100),3),",","CV(c) :",round(((sqrt(as.numeric(anova1[7,3]))/mean(dependent.var))*100),3))
    if (anova1[2,5]>0.05){
      multiple.comparison.A<-"All the column factor level means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.A<-"The means of one or more levels of column factor are not same, so go for multiple comparison test"
    }
    if (anova1[4,5]>0.05){
      multiple.comparison.B<-"All the row factor factor level means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.B<-"The means of one or more levels of row factor are not same, so go for multiple comparison test"
    }
    if (anova1[6,5]>0.05){
      multiple.comparison.AB<-"All the interaction level means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.AB<-"The means of one or more levels of interaction are not same, so go for multiple comparison test"
    }
    n.test<-shapiro.test(model$residuals)
    n.result<-n.test$p.value
    if (n.result > 0.05){
      n.result<-"Normality assumption is not violated"
    } else {
      n.result<-"Normality assumption is violated"
    }
    summary.model<-summary(model)
    R.square<-paste("R Square",round(summary.model$r.squared,digits = 3))
    if (mean.comparison.test == 0){
      m.testA<-"No multiple comparison test selected"
      m.testB<-m.testA
      m.testAB<-m.testA
    }
    if (mean.comparison.test == 1){
      m.test1A<-LSD.test(dependent.var,column,anova1[3,1],anova1[3,3])
      m.testA<-list(m.test1A$statistics,m.test1A$groups)
      m.test1B<-LSD.test(dependent.var,row,anova1[5,1],anova1[5,3])
      m.testB<-list(m.test1B$statistics,m.test1B$groups)
      m.test1AB<-LSD.test(dependent.var,column:row,anova1[7,1],anova1[7,3])
      m.testAB<-list(m.test1AB$statistics,m.test1AB$groups)
    }
    if (mean.comparison.test == 2){
      m.test1A<-duncan.test(dependent.var,column,anova1[3,1],anova1[3,3])
      m.testA<-list(m.test1A$statistics,m.test1A$duncan,m.test1A$groups)
      m.test1B<-duncan.test(dependent.var,row,anova1[5,1],anova1[5,3])
      m.testB<-list(m.test1B$statistics,m.test1B$duncan,m.test1B$groups)
      m.test1AB<-duncan.test(dependent.var,column:row,anova1[7,1],anova1[7,3])
      m.testAB<-list(m.test1AB$statistics,m.test1AB$duncan,m.test1AB$groups)
    }
    if (mean.comparison.test == 3){
      m.test1A<-HSD.test(dependent.var,column,anova1[3,1],anova1[3,3])
      m.testA<-list(m.test1A$statistics,m.test1A$parameters,m.test1A$groups)
      m.test1B<-HSD.test(dependent.var,row,anova1[5,1],anova1[5,3])
      m.testB<-list(m.test1B$statistics,m.test1B$parameters,m.test1B$groups)
      m.test1AB<-HSD.test(dependent.var,column:row,anova1[7,1],anova1[7,3])
      m.testAB<-list(m.test1AB$statistics,m.test1AB$parameters,m.test1AB$groups)
    }
    mylist<-list(anova1,CV,R.square,n.test,n.result,multiple.comparison.A,m.testA,multiple.comparison.B,m.testB,multiple.comparison.AB,m.testAB)
    done<-list(mylist)
    return(done)
  }
  fiftn<-convert(data)
  colnumber<-ncol(data)
  output<-list()
  for (j in 1:colnumber){
    output[[j]]<-strip.design(fiftn[[j]],block,column,row,mean.comparison.test)
  }
  names(output)<-names(data)
  return(output)
}
