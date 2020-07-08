#' @title Convert the yield data of plot into different units
#' @description The function converts the yield data of plot into qtl/ha, tonnes/ha, qtl/acre or tonnes/acre depending on the option chosen.
#' @param yield.in.kg yield data in kilograms
#' @param length.of.plot length of plot in m
#' @param width.of.plot width of the plot in m
#' @param choose.convert.to 0 for qtl/ha, 1 for tonnes/ha, 2 for qtl/acre and 3 for tonnes/acre
#' @return converted yield
#' @export
#' @examples
#' #Convert yield vector obtained from 10m x 5m plot into different forms
#' yield<-c(10,15,12,16,19,25,30,25,11)
#' #For converting into qtl/ha
#' yieldconvert(yield,10,5,0)
#' #For converting into tonnes/ha
#' yieldconvert(yield,10,5,1)
#' #For converting into qtl/acre
#' yieldconvert(yield,10,5,2)
#' #For converting into tonnes/acre
#' yieldconvert(yield,10,5,3)
yieldconvert<-function(yield.in.kg,length.of.plot,width.of.plot,choose.convert.to){
  if(choose.convert.to==0){
    converted.weight<-yield.in.kg/100
    area.of.plot<-length.of.plot*width.of.plot
    converted.yield<-10000*converted.weight/area.of.plot
    converted.to<-"Converted to Qtl/Ha"
  }
  if(choose.convert.to==1){
    converted.weight<-yield.in.kg/1000
    area.of.plot<-length.of.plot*width.of.plot
    converted.yield<-10000*converted.weight/area.of.plot
    converted.to<-"Converted to Tonnes/Ha"
  }
  if(choose.convert.to==2){
    converted.weight<-yield.in.kg/100
    area.of.plot<-length.of.plot*width.of.plot
    converted.yield<-4046.86*converted.weight/area.of.plot
    converted.to<-"Converted to Qtl/Acre"
  }
  if(choose.convert.to==3){
    converted.weight<-yield.in.kg/1000
    area.of.plot<-length.of.plot*width.of.plot
    converted.yield<-4046.86*converted.weight/area.of.plot
    converted.to<-"Converted to Tonnes/Acre"
  }
  my.output<-list(converted.to,converted.yield)
  return(my.output)
}

