#Reclass.cov function
reclass.cov <- function(rasterlayer = NULL, wd = NULL, RasterfolderName = NULL, onlyRast = F, zoneID_reclass = NULL, AdditCol_rast = NULL, csvfoldername = NULL, onlyCSV = F){
  #Version 1.0 (July 6, 2018)
  #Variables --
  #rasterlayer: The original raster layer that will be reclassified according to cover types and zone polygon extent
  #wd: the filepath name of the working directory where folders with reclassified LC rasters will be stored, this is an optional argument, and is only necessary when you choose to export your raster or csv files
  #RasterfolderName: Optional, if used this is the name of the folder that will be placed within the wd in which raster layers will be exported and stored. 
  #onlyRast: Optional, if used this will ensure that no csv files are saved. Default is F
  #zoneID_reclass: The ID number of the extent used to clip the raster and of which we want information about percent cover. The zoneID is useful for further analysis if it is important to know which clip the LC percent cover belongs to 
  #AdditCol: the additional attribute columns you want to include in the final df, simply specify the columns of interest by using df[, col#]
  #csvfoldername: optional argument, if provided, will be used to export and save csv files
  #onlyCSV: Optional, if used this will ensure that no raster files are saved. Default is F
  #Processing Steps--
  #Step 1: creates a list of matrices which will be be populated with the reclassify values 
  #Step 2: For loop which populates the list of matrices according to the landcover types present. The number   of matrices is based on the number of LCs present, and the values are populated so that each matrix is the   guiding principles to reclassify one raster layer per LC type
  #Step 3: Exports each reclassified LC specific raster to a new folder which is named according to the        buffer ID, so that each ID will have its own folder with a set of rasters for all LCs present
  #Step 4: Reads in each of the buff specific LC specific rasters one at a time and calculates the percent     cover of each LC, then appends a dataframe with the information
  #Final output will be a dataframe for the focal buffer which lists all LCs present and the percent cover of   each LC
  #Step 1
  list_value <- sort(getValues(rasterlayer), decreasing = F)
  m <- matrix(as.numeric(c("", "", "", "", "", "", "", "", "")), ncol = 3, byrow = T)
  l <- rep(list(m), times = length(as.numeric(levels(as.factor(list_value)))))
  #Turns AdditCol into the correct dataframe format
  AdditCol_rast_df <- as.data.frame(AdditCol_rast)
  AdditColName <- colnames(as.data.frame(AdditCol_rast_df))#takes the names of the added columns for use in the df later
  #Step 2
  for(i in 1:length(l)){
    l[[i]][1,] <- c( as.numeric(levels(as.factor(list_value)))[i], as.numeric(levels(as.factor(list_value)))[i]+1, 1)
    
    if (i<length(l)){
      l[[i]][2,] <- c(as.numeric(levels(as.factor(list_value)))[i+1], rev(as.numeric(levels(as.factor(list_value))))[1], 0)
    }
    
    else{
      l[[i]][2,] <- c(as.numeric(levels(as.factor(list_value)))[i], as.numeric(levels(as.factor(list_value)))[i]+1, 0)
      
    }
    l[[i]][3,] <-  c(0, as.numeric(levels(as.factor(list_value)))[i], 0)
    
  }#the for loop that goes through and populates the list of matrices  
  l
  #Step 3
  list <- list()  
  for (j in 1:length(l)){ 
    lc_reclass <- reclassify(rasterlayer, l[[j]], include.lowest = T, right = F)
    lc_reclass_filename <- paste("lcreclass", sep="_", as.numeric(levels(as.factor(list_value)))[j])#add yy in here when have it 
    if (!is.null(wd)){
      if(onlyCSV!=T){
        if (!is.null(RasterfolderName)){
          new_wd<- paste0(wd, sep = "/", RasterfolderName)
          dir.create(new_wd)#creates new folder based on the buffer being run so that everything is split up by the buffer
          setwd(new_wd)
          writeRaster(lc_reclass, filename = lc_reclass_filename, format = "GTiff", overwrite = T)
        }
      }
      
    }
    #for loop pulls in each newly created reclassed LC specific raster, and calculates the percentage cover of each data type, creating a df with the lc type, ID of the buffer being analyzed, and percentage cover
    list[[j]] <- lc_reclass
  }
  df <- as.data.frame(rbind(rep(data.frame(NA), 3+length(AdditCol_rast_df))))
  colnames(df) <- c("ID", AdditColName, "CoverType", "PercentageCover")
  for (k in 1:length(list)){
    raster <- list[[k]]
    df[k,] <- c(zoneID_reclass, AdditCol_rast_df, as.numeric(levels(as.factor(list_value)))[k], ((ncell(raster[raster>0])/ncell(raster[!is.na(raster)]))*100))
    
  }
  if (!is.null(wd)){
    if(onlyRast != T){
      if(!is.null(csvfoldername)){
        df <- apply(df,2,as.character)
        wdforcsv <- paste0(wd, sep = "/", csvfoldername)
        dir.create(wdforcsv)
        setwd(wdforcsv)
        csvfilename <-  paste(csvfoldername, ".csv", sep = "")
        write.csv(df, file = csvfilename)
      }
    }
    
  }
  
  
  return(df)
  
}#end of reclass.cov function