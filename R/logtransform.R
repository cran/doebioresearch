#' @title Log transformation of the numeric vector
#' @description The function carries out log with base 10 transformation of each values of vector.
#'     If one of values of a vector is 0, 1 is added to each observation.
#'     Log transformation is carried out for the data when variance is proportional to square of the mean and treatment effects are multiplicative in nature.
#' @param numeric.vector data vector to be transformed
#' @return A list of
#' \itemize{
#'   \item \code{Ratio}- A ratio of maximum and minimum values of the data
#'   \item \code{LogTransformedVector} - A vector of the transformed data
#'   \item \code{Comment} - A comment about zero being present in data or not
#' }
#' @export
#' @examples
#' vector<-c(100,0,120,1000,52,30,60)
#' logtransform(vector)
logtransform<-function(numeric.vector){
  numeric.vector<-as.numeric(numeric.vector)
  zero.present<-0 %in% numeric.vector
  if (zero.present==TRUE){
    numeric.vector<-numeric.vector+1
  }else{
    numeric.vector<-numeric.vector
  }
  if (zero.present==TRUE){
    zero<-"Zero was present so 1 was added to each observation"
  }else{
    zero<-"No observation was found to be zero"
  }
  max.to.min.ratio<-max(numeric.vector)/min(numeric.vector)
  logtransformed<-round(log10(numeric.vector),4)
  my.output<-list(Ratio=max.to.min.ratio,LogTransformedVector=logtransformed,Comment=zero)
  return(my.output)
}

