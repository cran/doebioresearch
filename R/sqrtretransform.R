#' @title Re-transform the square root transformed data
#' @description Retransform the square root transformed data. When square root transformation is done, the mean of the treatments needs to be re-transformed for comparison.
#' @param transformed.mean vector of mean which needs to be re-transformed
#' @param if.zero.present 0 if zero was present in the data prior to transformation of data.
#'     1 if zero was absent in the data prior to transformation
#' @return Square root re-transformed vector
#' @export
#' @examples
#' vector<-c(19,10,30,60,50,10,5)
#' #Square root re-transform and zero was absent in the data prior to transformation
#' sqrtretransform(vector,1)
sqrtretransform<-function(transformed.mean,if.zero.present){
  transformed.mean<-as.numeric(transformed.mean)
  if (if.zero.present==0){
    output<-round(((transformed.mean)^2)-0.5,3)
  }
  if (if.zero.present==1){
    output<-round((transformed.mean^2),3)
  }
  return(output)
}
