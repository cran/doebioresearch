#' @title Arc sine transformation of the numeric vector
#' @description The function divide values by 100, does square root and than sin inverse of each values of vector.
#'     If any of the values of a vector is 0 or 100, it is replaced by 1/4n or 100-(1/4n), respectively.
#' @param numeric.vector data vector to be transformed
#' @param type 0 if data is in percentage and 1 if data is in proportion
#' @param n is the number of units upon which the percentage/proportion data is based
#' @return Arc sine transformed data
#' @export
#' @examples
#' vector<-c(23,0,29.6,35.6,33,35.6,10.5,100)
#' # Arc sine trnasformation for percentage data and n=10
#' arcsinetransform(vector,0,10)
arcsinetransform<-function(numeric.vector,type,n){
  if(type==1){
    numeric.vector<-numeric.vector*100
  }
  if(type==0){
    numeric.vector<-numeric.vector
  }else{
    numeric.vector<-numeric.vector
  }
  numeric.vector<-as.vector(numeric.vector)
  target0<-numeric.vector==0
  target100<-numeric.vector==100
  numeric.vector[target0]<-1/(4*n)
  numeric.vector[target100]<-100-(1/(4*n))
  transformed<-asin(sqrt(numeric.vector/100))*180*(1/pi)
  return(transformed)
}
