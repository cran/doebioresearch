#' @title Square root transformation of the numeric vector
#' @description The function carries out square root transformation of each values of vector.
#'     If one of values of a vector is 0, 0.5 is added to each observation.
#' @param numeric.vector data vector to be transformed
#' @return Square root transformed data
#' @export
#' @examples
#' vector<-c(0,25,36,6,9,25,70)
#' sqrttransform(vector)
sqrttransform<-function(numeric.vector){
  numeric.vector<-as.numeric(numeric.vector)
  zero.present<-0 %in% numeric.vector
  if (zero.present==TRUE){
    numeric.vector<-numeric.vector+0.5
  }else{
    numeric.vector<-numeric.vector
  }
  sqrttransformed<-round(sqrt(numeric.vector),3)
  if (zero.present==TRUE){
    comment1<-"Zero was observed in data so 0.5 is added to each observations"
  }else{
    comment1<-"Zero was not found in the data so no changes were made during tansformation"
  }
  my.list<-list(Transformed.Data=sqrttransformed,Comment=comment1)
  return(my.list)
}

