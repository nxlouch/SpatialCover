#Percent.cov function
Percent.cov <- function (vectorlayer = NULL, rasterlayer = NULL, zoneID = NULL, AdditCol = NULL, rasterwd = NULL, conditdf = NULL, conditvalue = NULL, savingwd = NULL, onlyCSV = F, dfnamebase = NULL){
  #Version 1.0 (July 7, 2018)
  #Variables -- 
  #vectorlayer: this is the ArcGIS shapefile or R compatible sp file with the zones that will be used to crop the rasters
  #rasterlayer: this is the raster layer that will be cropped and whose values will be used to determine percentage cover in each zone. If only one raster layer is to be used, this variable is required. If more than one rasterlayer is to be used according to specified conditions, leave rasterlayer blank and provide rasterwd, conditdf and conditvalue
  #zoneID: necessary variable to denote the name of the column in the vectorlayer that will be used to identify zones. This should be class character, only the name of the column eg. "colname"
  #AdditCol: the column number(s) of any additional columns with data you wish to append to the final dataframe. This is an optional variable
  #rasterwd: Where your raster layers that will act as clipping rasters will come from, this is ony necessary if no rasterlayer has been specified and more than one rasterlayer is to be used for clipping
  #conditdf: this is a dataframe that will contain the conditions for choosing the rasterlayer to use as a clipping raster. Must have three rows. First row must be the names of the rasterlayers to be chosen from, second row must be the from column (ie. lower bound of condition), and third row must be the to column (ie. upper cound of condition). Values in from and to columns must be integers
  #conditvalue: a list of values that will be compared to to and from columns of conditdf to determine which rasterlayer to choose as the clipping raster. This list will come from attributes in the vectorlayer which identify zones that require particular rasterlayers
  #savingwd: file pathname where reclassified raster layers and csv files of percentage cover per zone will be saved. This is optional, if not present neither reclassified rasters, nor csv files will be saved
  #onlyCSV: indicator of whether to only create and save csv files of percentage cover per zone. Default is F. If marked as T, savingwd must be specified
  #dfnamebase: character string of base naming convention for saving files. Is required when savingwd is provided
  #The final output will be a dataframe with all zoneIDs, Cover type (ie. the original raster cell values), Percentage cover (of each original raster cell value)
  
  dflist<-list()
  if (is.null(vectorlayer)){
    stop("vector layer input missing")
  }
  #turns vectorlayer into R compatible sp layer and identifies zoneID
  if (class(vectorlayer) != "SpatialPolygonsDataFrame"){
    stop("vector layer must be class SpatialPolygonsDataFrame ")
  }else{
    if(class(vectorlayer)== "SpatialPolygonsDataFrame"){
      zoneID_cov <- vectorlayer[[zoneID]]
    } 
  }
  
  for (a in 1:length(vectorlayer)){
    #Step 1: subset the appropriate zone according to ID 
    buff <- subset(vectorlayer, zoneID_cov == a)
    #identify the appropriate columns to include in the final dataframe
    AdditCol_cov = buff[,AdditCol]
    
    #Step 2: Identify the raster layer that will be clipped and whose values will be used to calculate percent cover
    if(!is.null(rasterlayer)){
      clipRaster <- rasterlayer
    } else{ 
      if(!is.null(rasterwd)){
        if(!is.null(conditdf)){
          if(!is.null(conditvalue)){
            setwd(rasterwd)
            rastsubset <- conditdf[conditvalue[a]>= conditdf[, 2] & conditvalue[a]<=conditdf[,3],][1,1]
            clipRaster <- raster(as.character(rastsubset))
          }else{
            stop("if no rasterlayer is provided, must provide conditvalue")
          }
        }else{
          stop("if no rasterlayer is provided, must provide conditdf")
        }
        
      }else{
        stop("if no rasterlayer is provided, must provide rasterwd")
      }
    }
    
    #Step 3: crop the raster layer according to the zone polygon
    buffcrop <- crop(clipRaster, extent(buff))
    buffraster <- mask(buffcrop, buff)
    
    #Step 4: run through the reclass.cov function, which splits the cropped rasted into individual reclassified rasters according to cover types present (will split into   the same number of rasters as there are cover types) then calculates the percent cover of each cover type and combines values into one dataframe per zone polygon
    if (!is.null(savingwd)){
      if(is.null(dfnamebase)){
        stop("dfnamebase must be provided when savingwd is provided")
      }else {
        if(onlyCSV == F){
          wd <- savingwd
          reclass <- reclass.cov(rasterlayer = buffraster, wd = wd, RasterfolderName = paste(dfnamebase, sep = "_", zoneID_cov[a]), zoneID_reclass = zoneID_cov[a], AdditCol_rast = AdditCol_cov, csvfoldername = paste(dfnamebase, sep = "-", zoneID_cov[a]))
        }else{
          if(onlyCSV == T){
            wd <- savingwd
            reclass <- reclass.cov(rasterlayer = buffraster, wd = wd, zoneID_reclass = zoneID_cov[a], AdditCol_rast = AdditCol_cov, csvfoldername = paste(dfnamebase, sep = "-", zoneID_cov[a]), onlyCSV = T)
          }
        }
      }
      
    }
    
    else{ if(is.null(savingwd)){
      if(onlyCSV == T){
        stop("savingwd must be specified in order to save csv files")
      }else{
        reclass <- reclass.cov(rasterlayer = buffraster, zoneID_reclass = zoneID_cov[a], AdditCol_rast = AdditCol_cov)
      }
      
    }
    }
    
    #Step 5: Combines all zone polygon specific dataframes into a list of dataframes
    assign(paste0(dfnamebase, zoneID_cov[a]), reclass)
    dfname <- paste0(dfnamebase, zoneID_cov[a])
    dfvalues <- get(paste0(dfnamebase, zoneID_cov[a]))
    dflist[[dfname]] <-dfvalues
    
  }
  #step 6: Combines the list of dataframes into a dataframe
  finaldf_allzone <- do.call(rbind, dflist)
  return(finaldf_allzone)
}#end of Percent.cov function 