############################################################################################################
############################################################################################################
##################################         MAGNETgrid model          #######################################
############################################################################################################
##################################        Spatial CBA module         #######################################
############################################################################################################
############################################################################################################
#__________________________________________________________________________________________________________#
#                                                                                                          #  
# This script combines MAGNET projections data with the sector-specific spatial data previously obtained   #
# with the SpatialData.R module script. It does so by performing spatial cost-benefit analysis of MAGNET   #
# agricultural sectors and respectively producing sector-specific NPV (EUR/ha) maps. In addition, it also  # 
# exports all the maps (eg current land use, NPV projections) and tables (investment costs, and land demand#
# projections) that will be required in the land-use modelling module.                                     #
#                                                                                                          #
#                                                                                                          #   
# For more information, please contact:                                                                    #
#   - Vasco Diogo (vascodiogo3@gmail.com)                                                                  #
#   - Wil Hennen (wil.hennen@wur.nl)                                                                       #
#__________________________________________________________________________________________________________#
############################################################################################################
# install.packages("e1071")
rm(list=ls()) 
MagnetGridPath= "C:/MagnetGridModel/" ## @USER INPUT: indicate the main directory where the model and files are stored

ProjectINI <<- read.csv(paste(MagnetGridPath,"ProjectINI.txt",sep=""))

source(paste(MagnetGridPath,"ModelConfiguration/SpatialDataAndSpatialCBAmodules/SpatialDataModuleScripts/INI.R", sep = ""))

Scenarios <- GetINI("Scenarios",1)     ## @USER INPUT: indicate here the scenarios to be run

setwd(MagnetGridPath)
dir.create(paste(MagnetGridPath,'/ModelConfiguration/SpatialDataAndSpatialCBAmodules/Rlibraries',sep=''))
.libPaths('ModelConfiguration/SpatialDataAndSpatialCBAmodules/Rlibraries')
packLis <- c('readxl','dplyr','maptools','RColorBrewer','classInt','rgdal',
             'raster','sp','latticeExtra','rgeos','foreign','openssl','terra') ## list with all the required  packages 
newPackages <- packLis[!(packLis %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages, repo="https://cloud.r-project.org/")

require(readxl)
library(dplyr)
library(maptools)
library(RColorBrewer)
library(classInt)
library(rgdal)
library(raster)#nodig voor shapefile()
library(sp)
library(latticeExtra)
library(rgeos)
library(foreign)
library(openssl)
library(terra)


OriginalProj <- GetINI("OriginalProj",0)                              ##@USER INPUT: specify the projection of the original data
LUMproj <- GetINI("LUMproj",0)                                        ##@USER INPUT: specify the projection of to be exported and used in the land-use module
GridcellSize <- GetINI("GridcellSize",1)                                               ##@USER INPUT: specify the gridcell resolution of tje data to be exported and used in the land-use module

OriginalAdmnBoundaries<-shapefile("SpatialData/WorldMaps/world_map_mollweide.shp")                         ##@USER INPUT: specify the original map with the administrative boundaries to be aggregated
OriginalAdmnBoundaries@data=OriginalAdmnBoundaries@data[,c("OBJECTID","ISO_3DIGIT","CNTRY_NAME","SQKM")]   ##@USER INPUT: specify the data attributes of original map with admnistrative boundaries

options(scipen=999)
StringAsFactor=FALSE

#_________________________________________________________________________________________________________
#
#  FUNCTIONS
#_________________________________________________________________________________________________________
GetMAGNETdata<-function(varnm){
  Mdata=subset(MAGNET_data_YrSect,MAGNET_data_YrSect$GRID_VAR==varnm)[,c("REG","Value")]
  Mdata=merge(RegionTab[,c("Mregion","ID")],Mdata,by.x="Mregion",by.y="REG",all.x=TRUE)
  Mdata=Mdata[order(Mdata$ID), ]
  return(Mdata$Value)
}

ReadLandUse<-function(Comm){
  CommTab = local({load(paste(MagnetGridPath,"/SpatialData/ThematicMaps/SectorLandUseMaps/",Comm,".rda",sep="")); environment()})
  return(CommTab$"Tab")
}  

ReadSuitability<-function(Comm){
  CommTab = local({load(paste(MagnetGridPath,"/SpatialData/ThematicMaps/SectorSuitabilityMaps/",Comm,".rda",sep="")); environment()})
  return(CommTab$"Tab")
}

# ReadRoadInf<-function(Exogenous){
#   ExogenousTab = local({load(paste(MagnetGridPath,"/SpatialData/ThematicMaps/RoadInfMaps/",Exogenous,".rda",sep="")); environment()})
#   return(ExogenousTab$"Tab")
# }

ReadLandUseExogenous<-function(Exogenous){
  ExogenousTab = local({load(paste(MagnetGridPath,"/SpatialData/ThematicMaps/ExogenousLandUseMaps/",Exogenous,".rda",sep="")); environment()})
  return(ExogenousTab$"AreaTab")
}

writeTiff<-function(tabel,tiffFileNm){
  r=rasterFromXYZ(tabel)
  projection(r) <- OriginalProj
  #tifOriginalProj <- projectRaster(r,crs=OriginalProj)
  tif_LUMproj <- projectRaster(r, res=GridcellSize, crs=LUMproj, method="bilinear")
  tif_LUMprojM <- mask(tif_LUMproj, SHPreg)
  writeRaster(tif_LUMprojM, tiffFileNm, format="GTiff", overwrite=TRUE, datatype='FLT4S', options=c('TFW=YES'))
}

writeTiffRegioGrid<-function(tabel,tiffFileNm){
  r=rasterFromXYZ(tabel)
  projection(r) <- OriginalProj
  tifOriginalProj <- projectRaster(r,crs=OriginalProj)
  tif_LUMproj <- projectRaster(tifOriginalProj, res=GridcellSize, crs=LUMproj, method="bilinear")
  writeRaster(tif_LUMproj, tiffFileNm, format="GTiff", overwrite=TRUE, datatype='INT1U', options=c('TFW=YES'))
}

writeRasterExtentTable <- function(tabel,tiffFileNm, csvFileNm){
  r=rasterFromXYZ(tabel)
  projection(r) <- OriginalProj
  tifOriginalProj <- projectRaster(r,crs=OriginalProj)
  tif_LUMproj <- projectRaster(tifOriginalProj, res=GridcellSize, crs=LUMproj, method="bilinear")  
  RegionExtent <- extent(tif_LUMproj)
  RegionExtent <- t(matrix(RegionExtent))
  RegionExtent <- cbind(RegionExtent, matrix(c(0,0), nrow=1))
  RegionExtent[1,5] <- (as.numeric(RegionExtent[1,2]) - as.numeric(RegionExtent[1,1]) )/ GridcellSize
  RegionExtent[1,6] <- (as.numeric(RegionExtent[1,4]) - as.numeric(RegionExtent[1,3]) )/ GridcellSize
  colnames(RegionExtent) <- c('xmin','xmax', 'ymin', 'ymax', 'colnr', 'rownr')
  write.csv(RegionExtent, csvFileNm, row.names = FALSE)
}

source(paste(MagnetGridPath,"ModelConfiguration/SpatialDataAndSpatialCBAmodules/SpatialDataModuleScripts/MakeSHP.R", sep = ""))

#_________________________________________________________________________________________________________
#
#  CREATE SCENARIO STRUCTURE
#_________________________________________________________________________________________________________
if (!file.exists(MagnetGridPath)){
  dir.create(MagnetGridPath)
  dir.create(paste(MagnetGridPath, '/SpatialData', sep=''))
  dir.create(paste(MagnetGridPath,'/SpatialData/InputData_LandUseModel',sep=''))
  dir.create(paste(MagnetGridPath,'/SpatialData/ThematicMaps',sep=''))
}

dir.create(paste(MagnetGridPath,'SpatialData',sep='/'))
dir.create(paste(MagnetGridPath,'SpatialData','InputData_LandUseModel',sep='/'))

for (i in 1:length(Scenarios)){
  Scenario <- Scenarios[i]
  dir.create(file.path(paste(MagnetGridPath, 'SpatialData/InputData_LandUseModel',Scenario, sep="/")))
}

#_________________________________________________________________________________________________________
#
#  READ INI FILES
#_________________________________________________________________________________________________________
for (i in 1:length(Scenarios)){
  # i=1
Scenario <- Scenarios[i]
print(Scenario)
ScenaPath=paste("SpatialData/InputData_LandUseModel/",Scenario,sep="")
ScenarioSpecsPath=paste("ScenarioSpecs/",Scenario, sep="")
Years = ( read.csv(paste(ScenarioSpecsPath,"INI_Years.csv",sep="/")) %>% subset(Select==1) ) [,1] %>% as.numeric()
Variables = ( read.csv(paste(ScenarioSpecsPath,"INI_Variables.csv",sep="/")) %>% subset(Select==1) ) [,1] %>% as.character()
Regions = ( read.csv2(paste(ScenarioSpecsPath,"INI_MappingMAGNETtoScenarioRegionsAggr.csv",sep="/"),sep=",") %>% subset(Select==1) ) [,4] %>% as.character() %>% unique()
Sectors = ( read.csv(paste(ScenarioSpecsPath,"INI_Sectors.csv",sep="/")) %>% subset(Select==1) ) [,1] %>% as.character()
Parameters = read.csv(paste(ScenarioSpecsPath,"INI_NpvParameters.csv",sep="/"))
ExogenousLandUses = ( read.csv(paste(ScenarioSpecsPath,"INI_ExogenousLandUses.csv",sep="/")) %>% subset(Select==1) ) [,1] %>% as.character()
ExogenousLandUses = ExogenousLandUses[1:3]
#Sectors = Sectors[c(-2)]

#_________________________________________________________________________________________________________
#
#  CREATE OUTPUT STRUCTURE
#_________________________________________________________________________________________________________
  for (r in Regions){
    dir.create(file.path(ScenaPath,r))
    for (y in Years){
      dir.create(file.path(paste(ScenaPath,"/",r,sep=""), y))
      dir.create(file.path(paste(ScenaPath,"/",r,"/",y,sep=""), "NPV"))
      dir.create(file.path(paste(ScenaPath,"/",r,"/",y,sep=""), "Exogenous"))
      dir.create(file.path(paste(ScenaPath,"/",r,"/",y,sep=""), "LandUse"))
    }
  }

Regionnames=Regions ## check if this is need -> LandDemand and InvestmentCosts tables

#_________________________________________________________________________________________________________
#
#  CREATE TABLE ALL REGIONS
#_________________________________________________________________________________________________________
  RegionAllTab=read.csv(paste(ScenarioSpecsPath,"/INI_LinkTableOriginalAdmnRegToMAGNET.csv",sep=""))
  MappingMAGNETtoScenarioRegions = read.csv(paste(ScenarioSpecsPath,"/INI_MappingMAGNETtoScenarioRegionsAggr.csv",sep=""),sep=",")
  ScenarioAggregation = read.csv(paste(ScenarioSpecsPath,"/INI_ScenarioAggregationRegions.csv",sep=""))
  ScenarioRegions=merge(MappingMAGNETtoScenarioRegions[,c("MAGNETregion","MAGNETregionNm","ScenaRegionCode")],
                        ScenarioAggregation[,c("ScenaRegionCode","ScenaRegionNm")],by="ScenaRegionCode",all.x=TRUE)
  ScenarioRegions=ScenarioRegions[,c(2,1,4)]
  RegionAllTab=merge(RegionAllTab,ScenarioRegions,by="MAGNETregion",all.x=TRUE)[,c(2:4,1,5:7)]                                   
  RegionAllTab=RegionAllTab[order(RegionAllTab$ISO3ID), ]
#_________________________________________________________________________________________________________
#
#  Create shapefile for the selected regions
#_________________________________________________________________________________________________________
  head(RegionAllTab) # new change 1
        ISO3lis=as.character(unique(RegionAllTab[RegionAllTab$ScenaRegionCode%in%Regions,]$ISO3))
        #ISO3lis <- ISO3lis[1]
        SHPscena=MakeSHP(ISO3lis,Regions,ScenaPath,'ScenarioRegions')
        plot(SHPscena)
        eval(parse(text=paste("dbfData=read.dbf('", ScenaPath, "/ScenarioRegions.dbf', as.is = TRUE)",sep="")))
        dbfData$r <- as.numeric(rand_bytes(nrow(dbfData)))
        dbfData$g <- as.numeric(rand_bytes(nrow(dbfData)))
        dbfData$b <- as.numeric(rand_bytes(nrow(dbfData)))
        eval(parse(text=paste("write.dbf(dbfData,'", ScenaPath, "/ScenarioRegions.dbf', factor2char=FALSE)",sep="")))

  for (r in Regions){
    SHPreg=subset(SHPscena,SHPscena$Regions==r)
    writeOGR(SHPreg, paste(ScenaPath,"/",r,sep=""), "RegionArea", driver = 'ESRI Shapefile', overwrite_layer = TRUE)
  }
#_________________________________________________________________________________________________________
#
#  Functions NPV etc.
#_________________________________________________________________________________________________________
  ISO3s=as.character(unique(RegionAllTab[RegionAllTab$ScenaRegionCode%in%Regions,]$ISO3)) # new change 2
  DiscountRate=subset(Parameters,Parameters$Parameter=="NPV_DiscountRate")$Value[1]
  Lifetime=subset(Parameters,Parameters$Parameter=="NPV_Lifetime")$Value[1]
  CapitalRecoveryFactor=(DiscountRate*(1+DiscountRate)^Lifetime)/
    (((1+DiscountRate)^Lifetime)-1)
  
  MAGNET_data=read.csv(paste(MagnetGridPath,"ScenarioSpecs/",Scenario,"/MAGNET_data.csv",sep=""), sep=",")
  MAGNET_data$YEAR=substr(MAGNET_data$YEAR,3,6)
  
  for (y in 1:length(Years)){
    # y=1
    Yr=Years[y]
    print(Yr)

    LandDemandTab=data.frame(matrix(, nrow=length(Regionnames),ncol=length(Sectors)+2))
    names(LandDemandTab)=c("Year","Region",Sectors)
    LandDemandTab[,1]=c(rep(Yr,length(Regionnames)))
    LandDemandTab[,2]=rep(Regionnames,1)
    InvestmentCostsTab=data.frame(matrix(, nrow=length(Regionnames),ncol=length(Sectors)+2))
    names(InvestmentCostsTab)=c("Year","Region",Sectors)
    InvestmentCostsTab[,1]=c(rep(Yr,length(Regionnames)))
    InvestmentCostsTab[,2]=rep(Regionnames,1)
    
    for (s in 1:length(Sectors)){
      # s=3
      print(Sectors[s])
      
      MAGNET_data_YrSect=subset(MAGNET_data,MAGNET_data$YEAR==Yr&MAGNET_data$GRID_SECT==Sectors[s])

      RegionTab=data.frame(Mregion=Regions,ID=1:length(Regions))
      #LINKING MAGNET DATA
      #1a
      ProductionValue=GetMAGNETdata("ProdVal") #MEuro
      ProductionVolume=GetMAGNETdata("ProdQuant") #ton
      RegionTab$MarketPrice=(ProductionValue*10^6)/ProductionVolume #E/ton
      
      #1b
      CapitalCosts=GetMAGNETdata("CapitalVal")  #MEuro
      LandCosts=GetMAGNETdata("LandVal")  #MEuro
      
      #1c
      LandDemand=GetMAGNETdata("LandQant") #km^2
      LandDemandTab[1:length(Regions),s+2]=LandDemand
      
      #1d
      InvestmentCosts=(((CapitalCosts+LandCosts)*10^6)/(LandDemand*10^2)) / CapitalRecoveryFactor#E/ha
      InvestmentCostsTab[1:length(Regions),s+2]=InvestmentCosts
      
      #1e
      LabourCosts=GetMAGNETdata("LabourVal")  #MEuro
      RegionTab$SpecificLabourCosts=(LabourCosts*10^6)/(LandDemand*10^2) #E/ha
      
      #1f
      IntermediateInputCosts=GetMAGNETdata("IntermVal")  #MEuro
      RegionTab$SpecificIntermediateInputCosts=(IntermediateInputCosts*10^6)/(LandDemand*10^2) #E/ha
      
      #1g
      RegionTab$YieldHa=ProductionVolume/(LandDemand*10^2) ##ton/ha
      
      #1h
      NetSubsidiesTaxes= GetMAGNETdata("ProdTax")  #MEuro
      RegionTab$SpecificNetSubsidiesTaxes=(NetSubsidiesTaxes*10^6)/ProductionVolume #E/ton
  
      #2 YIELD MAP
      Area=ReadLandUse(paste(Scenario,"/",Sectors[s],"Area",sep=""))
          
      Suitability=ReadSuitability(paste(Scenario,"/", Yr, "/", Sectors[s],"Suitability",sep=""))
      Suitability[,4][is.na(Suitability[,4])]<-0
      Suitability[,4][Suitability[,4]<0]<-0
      Suitability=Suitability[,c(1,2,4)]
      
      commMap=merge(x=Area, y=Suitability, by = c("X","Y"), all.x = TRUE)
      commMap$ID=rep(1:nrow(commMap), each=1)
      commMap[,5][is.na(commMap[,5])]<-0
      
      # new part where we add a factor for weighing the current land use (opportunitty cost) 
      commMap$normalizing= 0 + (commMap[,4] - min(commMap[,4])) * (1 - 0) / (max(commMap[,4]) - min(commMap[,4]))
      commMap[,5]=(commMap[,5]+commMap$normalizing)/2
      commMap[,7]=NULL

      # Regionalized yield map
      for (i in 1:length(Regions)){
        # i=1
        print(Regions[i])
        ISO3sRegion=subset(RegionAllTab,RegionAllTab$ScenaRegionCode==Regions[i])$ISO3 # new change 5
        ISOmap=subset(commMap,commMap$ISO3%in%Scenario)
        ISOmap=subset(commMap,commMap[,4]>0.01)                # Truncate min. area
        ISOmap[,4]=ifelse(ISOmap[,4]>10000, 10000, ISOmap[,4]) # Truncate max. area
        ISOmap=subset(ISOmap,ISOmap[,5]>0.01)                  # Truncate min. suitability
        ISOmap[,5]=ifelse(ISOmap[,5]>1.0, 0.99, ISOmap[,5])
        
        ISOmapCalc=subset(ISOmap,ISOmap[,5]>0.1)
        
        if (nrow(ISOmap)==0){
          MaxYield=0
        }else{
          AvgSuitability=sum(ISOmapCalc[,4]*ISOmapCalc[,5])/sum(ISOmapCalc[,4])
          AvgYield=RegionTab$YieldHa[RegionTab$Mregion==Regions[i]]
          MaxYield=AvgYield/AvgSuitability              
        }
        
        ISOmap[,c(1:4)]=NULL
        
        commMap$MaxYield[commMap$ISO3%in%ISO3sRegion]<-MaxYield
        commMap[,5]=ifelse(commMap[,3]=="AFG", 0, commMap[,5])
        commMap$YieldMap=commMap$MaxYield*commMap[,5]
      } #end i
      
      #3 CALC NPV map in the given region
      commMap$ID=NULL
      commMap$CostMap=0
      commMap$RevenuesMap=0
      
      for (c in 1:length(ISO3s)){
        #c=1
        cntr=ISO3s[c]
        cntrGroup=as.character(subset(RegionAllTab,RegionAllTab$ISO3==cntr)$ScenaRegionCode[1])
        SpecificLabourCostsCntr=subset(RegionTab,RegionTab$Mregion==cntrGroup)$SpecificLabourCosts
        SpecificIntermediateInputCostsCntr=subset(RegionTab,RegionTab$Mregion==cntrGroup)$SpecificIntermediateInputCosts
        SpecificNetSubsidiesTaxesCntr=subset(RegionTab,RegionTab$Mregion==cntrGroup)$SpecificNetSubsidiesTaxes
        #SpecificNetSubsidiesTaxesCntr=3
        commMap$CostMap[commMap$ISO3==cntr]<-(SpecificLabourCostsCntr+SpecificIntermediateInputCostsCntr)
        #commMap$CostMap=commMap$CostMap/commMap$RoadInf_ResArea # WRC_the transportation cost can be added here
        MarketPriceCntr=subset(RegionTab,RegionTab$Mregion==cntrGroup)$MarketPrice
        commMap$SubsidiesMap[commMap$ISO3==cntr]<-commMap$YieldMap[commMap$ISO3==cntr]*(SpecificNetSubsidiesTaxesCntr)
        commMap$MarketPriceMap[commMap$ISO3==cntr]<-commMap$YieldMap[commMap$ISO3==cntr]*(MarketPriceCntr)
        commMap$RevenuesMap <- commMap$SubsidiesMap + commMap$MarketPriceMap
        #commMap$RevenuesMap[commMap$ISO3==cntr]<-commMap$YieldMap[commMap$ISO3==cntr]*(MarketPriceCntr+SpecificNetSubsidiesTaxesCntr)
        #commMap$RevenuesMap=commMap$RevenuesMap/commMap$RoadInf_ResArea
      }
      
      commMap$NPVmap=-1*InvestmentCosts+((commMap$RevenuesMap-commMap$CostMap)/CapitalRecoveryFactor)
      
      #opportunity cost input
      
      #commMap$NPVmap=(commMap$RevenuesMap-(commMap$CostMap/commMap$RoadInf_ResArea))/CapitalRecoveryFactor
      #commMap$NPVmap=ifelse(commMap$NPVmap<0,NA,commMap$NPVmap)
      
      ###### --- write to tiff for each MAGNET commodity/sector, for every simulated year and region
      for (r in 1:length(Regions)){
        #r=1
        ISO3region=as.character(subset(RegionAllTab,RegionAllTab$ScenaRegionCode==Regions[r])$ISO3)
        currentLUmap=subset(commMap,commMap$ISO3%in%ISO3region)[,c("X","Y",Sectors[s])]
        NPVmap=subset(commMap,commMap$ISO3%in%ISO3region)[,c("X","Y","NPVmap")] # changed from 10 to 11
        if(y==1 && s==1){dir.create(paste(MagnetGridPath,'SpatialData/InputData_LandUseModel',Scenario, Regions[r],"SectorLandUse" ,sep='/'))}
        if(y==1){writeTiff(currentLUmap,paste("SpatialData/InputData_LandUseModel/",Scenario,"/",Regions[r],"/SectorLandUse","/",Sectors[s],sep=""))}
        writeTiff(NPVmap,paste("SpatialData/InputData_LandUseModel/",Scenario,"/",Regions[r],"/",Yr,"/NPV/NPV_",Sectors[s], sep=""))
        if(y==1 && s==1){writeRasterExtentTable(NPVmap,paste("SpatialData/InputData_LandUseModel/",Scenario,"/",Regions[r],"/",Yr,"/NPV/NPV_",Sectors[s], sep=""), paste("SpatialData/InputData_LandUseModel/",Scenario,"/",Regions[r],"/RegionExtent.csv",sep=""))}
        RegionGrid <- NPVmap
        RegionGrid$NPVmap <- 1
        colnames(RegionGrid)<- c("X","Y","Land")
        if(y==1 && s==1){writeTiffRegioGrid(RegionGrid,paste("SpatialData/InputData_LandUseModel/",Scenario,"/",Regions[r],"/RegionGrid", sep=""))}
  
      }
    } # end s
        # combine and sum all agri land uses
        out <- NULL
        for (s in 1:length(Sectors)){
          Area=ReadLandUse(paste(Scenario,"/",Sectors[s],"Area",sep=""))
          Coord <- Area[,1:2]
          Area=Area[,-1:-3]
          out=cbind(out,Area)
        }
        AreaFull=data.frame(out)
        AreaFull=cbind(Coord, AreaFull)
        AreaFull$Total=rowSums(AreaFull[,3:ncol(AreaFull)])/10000
        AreaFull=AreaFull[,c("X","Y","Total")]
        
        # adjust the exogenous land use in order to not exceed the grid size
        # write to tiff for each exogenous land-use, for every simulated year and region
    for (e in 1:length(ExogenousLandUses)){
      # e=3
      print(ExogenousLandUses[e])
      AreaExogenous=ReadLandUseExogenous(paste(Scenario,"/", Yr, "/", ExogenousLandUses[e],sep=""))
      AreaExogenousSS=subset(AreaExogenous,AreaExogenous$ISO3%in%ISO3s)
      AreaExogenousSS=merge(x=AreaExogenousSS, y=AreaFull, by = c("X","Y"), all.x = TRUE)
      AreaExogenousSS$Exceed=rowSums(AreaExogenousSS[,4:5])
      AreaExogenousSS$Exceed=ifelse(AreaExogenousSS$Exceed>=1,AreaExogenousSS$Exceed-1,0)
      AreaExogenousSS[,4]=AreaExogenousSS[,4]-AreaExogenousSS$Exceed
      AreaExogenousSS[,4]=ifelse(AreaExogenousSS[,4]<0,0,AreaExogenousSS[,4])
      AreaExogenousSS <- AreaExogenousSS[,c(1:4)]
      for (r in 1:length(Regions)){
      # r=1
        print(Regions[r])
        ISO3region=as.character(subset(RegionAllTab,RegionAllTab$ScenaRegionCode==Regions[r])$ISO3)
        ExogenousLUmap=subset(AreaExogenousSS,AreaExogenousSS$ISO3%in%ISO3region)[,c(1,2,4)]
        writeTiff(ExogenousLUmap,paste("SpatialData/InputData_LandUseModel/",Scenario,"/",Regions[r],"/",Yr,"/Exogenous/",ExogenousLandUses[e], sep=""))
        }
      }
     
     # write tables with land prices, land demand and investment costs for each MAGNET sector/commodity, for every simulated year and region
     for (r in 1:length(Regionnames)){
     # r=1
      
      LandDemandTabReg=LandDemandTab[r,][,-2]
      write.csv(LandDemandTabReg,paste("SpatialData/InputData_LandUseModel/",Scenario,"/",Regionnames[r],"/",Yr,"/LandDemandTab.csv",sep=""),row.names = FALSE)

      InvestmentCostsTabReg=InvestmentCostsTab[r,][,-2]
      write.csv(InvestmentCostsTabReg,paste("SpatialData/InputData_LandUseModel/",Scenario,"/",Regionnames[r],"/",Yr,"/InvestmentCostsTab.csv",sep=""),row.names = FALSE)
    }
  }  # end y
}
 