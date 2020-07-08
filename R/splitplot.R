#' @title Analysis of Split plot design
#' @description The function gives ANOVA, R-square of the model, normality testing of residuals, SEm (standard error of mean), SEd (standard error of difference), interpretation of ANOVA results and multiple comparison test for means.
#' @param data dependent variables
#' @param block vector containing replications
#' @param main.plot vector containing main-plot levels
#' @param sub.plot vector containing sub-plot levels
#' @param mean.comparison.test 0 for no test, 1 for LSD test, 2 for Dunccan test and 3 for HSD test
#' @importFrom agricolae LSD.test HSD.test duncan.test
#' @importFrom stats anova lm shapiro.test pf
#' @return ANOVA, interpretation of ANOVA, R-square, normality test result, SEm, SEd and multiple comparison test result
#' @export
#' @examples
#' data(splitdata)
#' #Using Date of sowing as Main-plot factor and varieties as sub-plot factor and using LSD test
#' #Split plot analysis with LSD test for Yield
#' splitplot(splitdata[4],splitdata$Replication,splitdata$Date_of_Sowing,splitdata$Varities,1)
#' #Split plot analysis with LSD test for both Yield and Plant Height
#' splitplot(splitdata[4:5],splitdata$Replication,splitdata$Date_of_Sowing,splitdata$Varities,1)
splitplot<-function(data,block,main.plot,sub.plot,mean.comparison.test){
  split1<-function(dependent.var,block,main.plot,sub.plot,mean.comparison.test){
    dependent.var<-as.numeric(dependent.var)
    block<-as.factor(block)
    main.plot<-as.factor(main.plot)
    sub.plot<-as.factor(sub.plot)
    model<-lm(dependent.var~block+block:main.plot+main.plot*sub.plot)
    n.test<-shapiro.test(model$residuals)
    n.result<-n.test$p.value
    if (n.result > 0.05){
      n.result<-"Normality assumption is not violated"
    } else {
      n.result<-"Normality assumption is violated"
    }
    summary.model<-summary(model)
    R.square<-paste("R Square",round(summary.model$r.squared,digits = 3))
    anova1<-anova(model)
    a<-anova1[3,]
    anova1[3,]<-anova1[4,]
    anova1[4,]<-a
    b<-rownames(anova1)[3]
    rownames(anova1)[3]<-"Ea"
    rownames(anova1)[4]<-b
    row.names(anova1)[6]<-"Eb"
    anova1[1,4]<-anova1[1,3]/anova1[3,3]
    anova1[2,4]<-anova1[2,3]/anova1[3,3]
    anova1[3,4]<-NA
    anova1[3,5]<-NA
    anova1[1,5]<-pf(as.numeric(anova1[1,4]),as.numeric(anova1[1,1]),as.numeric(anova1[3,1]), lower.tail = FALSE)
    anova1[2,5]<-pf(as.numeric(anova1[2,4]),as.numeric(anova1[2,1]),as.numeric(anova1[3,1]), lower.tail = FALSE)
    anova1[4,5]<-pf(as.numeric(anova1[4,4]),as.numeric(anova1[4,1]),as.numeric(anova1[6,1]), lower.tail = FALSE)
    anova1[5,5]<-pf(as.numeric(anova1[5,4]),as.numeric(anova1[5,1]),as.numeric(anova1[6,1]), lower.tail = FALSE)
    CV<-paste("CV(a):",round(((sqrt(as.numeric(anova1[3,3]))/mean(dependent.var))*100),3),",","CV(b) :",round(((sqrt(as.numeric(anova1[6,3]))/mean(dependent.var))*100),3))
    if (anova1[2,5]>0.05){
      multiple.comparison.A<-"All the main plot factor level means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.A<-"The means of one or more levels of main plot factor are not same, so go for multiple comparison test"
    }
    if (anova1[4,5]>0.05){
      multiple.comparison.B<-"All the sub plot factor factor level means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.B<-"The means of one or more levels of sub plot factor are not same, so go for multiple comparison test"
    }
    if (anova1[5,5]>0.05){
      multiple.comparison.AB<-"All the interaction level means are same so dont go for any multiple comparison test"
    } else {
      multiple.comparison.AB<-"The means of one or more levels of interaction are not same, so go for multiple comparison test"
    }
    if (mean.comparison.test == 0){
      m.testA<-"No multiple comparison test selected"
      m.testB<-m.testA
      m.testAB<-m.testA
    }
    if (mean.comparison.test == 1){
      m.test1A<-LSD.test(dependent.var,main.plot,anova1[3,1],anova1[3,3])
      m.testA<-list(m.test1A$statistics,m.test1A$groups)
      m.test1B<-LSD.test(dependent.var,sub.plot,anova1[6,1],anova1[6,3])
      m.testB<-list(m.test1B$statistics,m.test1B$groups)
      m.test1AB<-LSD.test(dependent.var,main.plot:sub.plot,anova1[6,1],anova1[6,3])
      m.testAB<-list(m.test1AB$statistics,m.test1AB$groups)
    }
    if (mean.comparison.test == 2){
      m.test1A<-duncan.test(dependent.var,main.plot,anova1[3,1],anova1[3,3])
      m.testA<-list(m.test1A$statistics,m.test1A$duncan,m.test1A$groups)
      m.test1B<-duncan.test(dependent.var,sub.plot,anova1[6,1],anova1[6,3])
      m.testB<-list(m.test1B$statistics,m.test1B$duncan,m.test1B$groups)
      m.test1AB<-duncan.test(dependent.var,main.plot:sub.plot,anova1[6,1],anova1[6,3])
      m.testAB<-list(m.test1AB$statistics,m.test1AB$duncan,m.test1AB$groups)
    }
    if (mean.comparison.test == 3){
      m.test1A<-HSD.test(dependent.var,main.plot,anova1[3,1],anova1[3,3])
      m.testA<-list(m.test1A$statistics,m.test1A$parameters,m.test1A$groups)
      m.test1B<-HSD.test(dependent.var,sub.plot,anova1[6,1],anova1[6,3])
      m.testB<-list(m.test1B$statistics,m.test1B$parameters,m.test1B$groups)
      m.test1AB<-HSD.test(dependent.var,main.plot:sub.plot,anova1[6,1],anova1[6,3])
      m.testAB<-list(m.test1AB$statistics,m.test1AB$parameters,m.test1AB$groups)
    }
    my.list<-list(anova1,CV,R.square,n.test,n.result,multiple.comparison.A,m.testA,multiple.comparison.B,m.testB,multiple.comparison.AB,m.testAB)
    done<-list(my.list)
    return(done)
  }
  fiftn<-convert(data)
  colnumber<-ncol(data)
  output<-list()
  for (j in 1:colnumber){
    output[[j]]<-split1(fiftn[[j]],block,main.plot,sub.plot,mean.comparison.test)
  }
  names(output)<-names(data)
  return(output)
}
