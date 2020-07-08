#' @title Re-transform the log transformed data
#' @description Re-transform the log transformed data. When log transformation is done, the mean of the treatments needs to be re-transformed for comparison.
#' @param transformed.mean vector of mean which needs to be re-transformed
#' @param if.zero.present 0 if zero was present in the data prior to transformation of data.
#'     1 if zero was absent in the data prior to transformation
#' @return Log re-transformed values
#' @export
#' @examples
#' vector<-c(0,2.004,1.114,1.491,1.431,1.415,1.845)
#' #Re-transformation of data with zero present in data prior to transformation
#' logretransform(vector,0)
logretransform<-function(transformed.mean,if.zero.present){
  transformed.mean<-as.numeric(transformed.mean)
  output12<-round(10^(transformed.mean),3)
  if(if.zero.present==1){
    output<-output12
  }
  if(if.zero.present==0){
    output<-output12-1
  }
  else{
    output<-output12
  }
  return(output)
}

