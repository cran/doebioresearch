#' @title Convert the data frame into list of numeric nature
#' @param data1 data-frame to be converted into list
#' @return list of numeric vectors
convert<-function(data1){
  data1<- as.data.frame(sapply(data1, as.numeric))
  data1<-as.list(data1)
  return(data1)
}
