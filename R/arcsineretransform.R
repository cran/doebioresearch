#' @title Re-transform the Arc sine transformed data
#' @description Re-transform the arc sine transformed data. When arc sine transformation is done, the mean of the treatments needs to be re-transformed for comparison.
#' @param mean.vector vector of mean which needs to be re-transformed
#' @param type 0 if data was in proportion prior to re-transformation, 1 if data was in percentage prior to re-transformation
#' @return Arc sine re-transformed vector
#' @export
#' @examples
#' data<-c(60,63.43495,71.56505,78.46304)
#' #If data was in percentage prior to re-transformation
#' arcsineretransform(data,1)
#' #If data was in proportion prior to re-transformation
#' arcsineretransform(data,0)
arcsineretransform<-function(mean.vector,type){
  if(type==0){
    retransform<-((sin(mean.vector*pi/180))^2)
  }
  if(type==1){
    retransform<-((sin(mean.vector*pi/180))^2)*100
  }
  return(retransform)
}
