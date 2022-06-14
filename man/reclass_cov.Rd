\name{reclass.cov}
\alias{reclass.cov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reclassify Coverage
%%  ~~function to do ... ~~
}
\description{The function reclass.cov() reclassifies a raster layer according to coverage type, and creates new unique raster layers with each coverage type having a value of one, and all other coverage types having a value of 0. The final step of this function calculates the percentage cover of each coverage type and creates a data frame which lists all coverage types and the percentage cover of that type within the raster layer
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reclass.cov(rasterlayer = NULL, wd = NULL, RasterfolderName = NULL,
onlyRast = FALSE, zoneID_reclass = NULL, AdditCol_rast = NULL,
csvfoldername = NULL, onlyCSV = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rasterlayer}{The original raster layer that will be reclassified according to cover types and zone polygon extent
%%     ~~Describe \code{x} here~~
  }
 \item{wd}{the filepath name of the working directory where folders with reclassified LC rasters will be stored, this is an optional argument, and is only necessary when you choose to export your raster or csv files
%%     ~~Describe \code{x} here~~
  }
  \item{RasterfolderName}{Optional, if used this is the name of the folder that will be placed within the wd in which raster layers will be exported and stored.
  }
  \item{onlyRast}{Optional, if used this will ensure that no csv files are saved. Default is FALSE
  }
  \item{zoneID_reclass}{List of integers, single integer or column call for shapefile used to clip extent of raster being reclassified. This is the ID number of the extent used to clip the raster(if raster was clipped before being reclassified) and of which we want information about percent cover. The zoneID is useful for further analysis if it is important to know which clip the percent cover belongs to. This is a required argument but can be equal to 1 or 0
  }
  \item{AdditCol_rast}{the additional attribute columns from the shapefile used as the extent of the raster you want to include in the final dataframe(df), simply specify the columns of interest by using df[, col#]. This is optional.
  }
  \item{csvfoldername}{optional argument, if provided, will be used to export and save csv files
  }
  \item{onlyCSV}{Optional, if used this will ensure that no raster files are saved. Default is FALSE
  }
}
\details{

Step 1: creates a list of matrices which will be populated with the reclassify values

Step 2: For loop which populates the list of matrices according to the landcover types present. The number   of matrices is based on the number of cover types present, and the values are populated so that each matrix is the guiding principles to reclassify one raster layer per cover type

Step 3: Exports each reclassified cover specific raster to a new folder which is named according to the clipping extent ID, so that each ID will have its own folder with a set of rasters for all covers present

Step 4: Reads in each of the extent ID specific cover specific rasters one at a time and calculates the percent     cover of each cover type, then appends a dataframe with the information
%%  ~~ If necessary, more details than the description above ~~
}
\value{

A data frame is returned with columns ID, Covertype and Percentage Cover with any additional columns specified
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Naomi Louchouarn, MESM
%%  ~~who you are~~
}
\note{
requires packages raster and sp
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Begin by subsetting the polygon file so that only one polygon is selected
buffer_extent <- subset(buffers275m, ID == 1)

#use the polygon to crop the raster layer whose percent cover will be calculated
rast_to_reclass <- crop(WiscLandcover92, extent(buffer_extent))
rast_to_reclass  <- mask(rast_to_reclass, buffer_extent)

reclass.cov(rasterlayer = rast_to_reclass , zoneID_reclass = buffers275m$ID[1],
AdditCol_rast = buffers275m[1,3])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

