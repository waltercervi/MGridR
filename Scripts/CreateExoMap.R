doCreateExoMap<-function(){
  # relaxExoNoLU : if demand more than available cropland then exo based on LU relaxed
  sumDemand=0
  for (x in 1:length(SectorsNm)){
    eval(parse(text=paste("sumDemand=sumDemand+D_",SectorsNm[x],sep="")))
  }
  sumLU=cellStats(LUtotal, 'sum')*Km2PerGrid
  if (sumDemand>sumLU){
    relaxExoNoLU=1.05*sumDemand/sumLU
  }else{
    relaxExoNoLU=1.05
  }

  # CREATE EXOGENOUS MAP -------------------------------------------------------------------------------------------------------------
  #
  ExogenousNm=GetINI("ExogenousLandUsesLis",1)
  ExogenousNm = ExogenousNm[1:2]
  out=stack()
  for (e in 1:length(ExogenousNm)){
    #e=3
    fileNm=paste("MagnetGridR/SpatialData/InputData_LandUseModel/",Scenarios,"/",Years[i],"/Exogenous/",ExogenousNm[e],".tif",sep="")
    ExoMap=raster(fileNm)
    #ExoMap=crop(raster(fileNm),MapREG)
    out <- stack(out, ExoMap)
  }
  out[is.na(out)]<-0
  ExoMap <- sum(out)
  ExoMap[ExoMap>1]<-1
  plot(ExoMap)
  
  # Water=raster("MagnetGridR/SpatialData/FRA/Water.tif")
  # Urban=raster("MagnetGridR/SpatialData/FRA/Urban.tif")
  # ExoWU=Water+Urban
  # ExoWU[ExoWU>1]<-1
  # ExoWU=crop(ExoWU,MapREG)
  ExoWU=ExoMap
  #ExoWU=doReprojShift(ExoMap,TRUE)
  ExoWU=raster::intersect(ExoWU,intersectMap)
  
  ExoCrop=1-relaxExoNoLU*LUtotal
  
  cellStats(1-ExoCrop, 'sum')*Km2PerGrid;sumDemand
  #ExoCrop=ExoCrop-minValue(ExoCrop)
  #ExoCrop=ExoCrop/maxValue(ExoCrop)
  ExoCrop[ExoCrop<0]<-0
  plot(ExoCrop)
  plot(ExoWU)
  ExoWU[is.na(ExoWU)]<-0
  Exo=ExoWU+ExoCrop
  Exo[Exo>1]<-1
  Exo[is.na(Exo)]<-0
  (sumExo=cellStats(1-Exo, 'sum')*Km2PerGrid);sumDemand
  Exo=Exo*Km2PerGrid
  plot(Exo)
  return(Exo)
}