\name{Percent.cov}
\alias{Percent.cov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Percentage Cover
%%  ~~function to do ... ~~
}
\description{Percent.cov() applies the reclass.cov() function using one shapefile with multiple zones. This tool also provides the ability to choose multiple rasters for use in raster.cov() according to a decision criteria. The final output is a large dataframe with zoneID's and percentage cover of each coverage type within each zone precent in the shapefile.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Percent.cov(vectorlayer = NULL, rasterlayer = NULL, zoneID = NULL,
AdditCol = NULL, rasterwd = NULL, conditdf = NULL, conditvalue = NULL,
savingwd = NULL, onlyCSV = FALSE, dfnamebase = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vectorlayer}{this is the R compatible sp file with the zones that will be used to crop the rasters
  }
  \item{rasterlayer}{this is the raster layer that will be cropped and whose values will be used to determine percentage cover in each zone. If only one raster layer is to be used, this variable is required. If more than one rasterlayer is to be used according to specified conditions, leave rasterlayer blank and provide rasterwd, conditdf and conditvalue
%%     ~~Describe \code{x} here~~
  }
 \item{zoneID}{ This should be a character string; only the name of the column eg. "colname". necessary variable to denote the name of the column in the vectorlayer that will be used to identify zones.
%%     ~~Describe \code{x} here~~
  }
  \item{AdditCol}{Interger or list of integers. the column number(s) of any additional columns with data you wish to append to the final dataframe. This is an optional variable.
  }
  \item{rasterwd}{Character string, file pathname. Where your raster layers that will act as cropped rasters will come from, this is ony necessary if no rasterlayer has been specified and more than one rasterlayer is to be cropped
  }
  \item{conditdf}{this is a dataframe that will contain the conditions for choosing the rasterlayer to use as a raster to crop. Must have three rows. First row must be the names of the rasterlayers to be chosen from, second row must be the 'from' column (ie. lower bound of condition), and third row must be the 'to' column (ie. upper cound of condition). Values in 'from' and 'to' columns must be integers
  }
  \item{conditvalue}{a list of integers that will be compared to the 'to' and 'from' columns of conditdf to determine which rasterlayer to choose as the raster to crop. This list will come from attributes in the vectorlayer which identify zones that require particular rasterlayers. Can be the call for the attribute column in vectorlayer (eg. vectorlayer$colname)
  }
  \item{savingwd}{character string, file pathname where reclassified raster layers and csv files of percentage cover per zone will be saved. This is optional, if not present neither reclassified rasters, nor csv files will be saved
  }
  \item{onlyCSV}{indicator of whether to only create and save csv files of percentage cover per zone. Default is FALSE. If marked as TRUE, savingwd must be specified
  }
  \item{dfnamebase}{character string of base naming convention for saving files. Is required when savingwd is provided
  }
}

\details{

Step 1: subset the appropriate zone according to ID

Step 2: Identify the raster layer that will be clipped and whose values will be used to calculate percent cover

Step 3: crop the raster layer according to the zone polygon

Step 4: run through the reclass.cov function, which splits the cropped rasted into individual reclassified rasters according to cover types present (will split into   the same number of rasters as there are cover types) then calculates the percent cover of each cover type and combines values into one dataframe per zone polygon

Step 5: Combines all zone polygon specific dataframes into a list of dataframes

Step 6: Combines the list of dataframes into a dataframe
%%  ~~ If necessary, more details than the description above ~~
}
\value{

The final output will be a dataframe with all zoneIDs, Cover type (ie. the original raster cell values), Percentage cover (of each original raster cell value)
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
###If rasterlayer is provided
Percent.cov(vectorlayer = buffers275m, rasterlayer = WiscLandcover92, zoneID = "ID", AdditCol = 3)

###If rasterlayer is not provided
#begin by saving the raster datasets in a local directory as this function takes layers from the local directory
setwd("~/R/")
writeRaster(WiscLandcover92, filename = "WiscLandcover92", format = "GTiff")
writeRaster(WiscLandcover01, filename = "WiscLandcover01", format = "GTiff")
writeRaster(WiscLandcover06, filename = "WiscLandcover06", format = "GTiff")

#create the condition data frame from which rasters to crop will be chosen
filename <- c("WiscLandcover92.tif", "WiscLandcover01.tif", "WiscLandcover06.tif")
from <- c(1979, 1999, 2006)
to <- c(1998, 2004, 2010)
ex_conditdf <- data.frame("filename" = filename, "from" = from, "to" = to)


Percent.cov(vectorlayer = buffers275m, zoneID = "ID", AdditCol = 3,
rasterwd = "~/R/", conditdf = ex_conditdf, conditvalue = buffers275m$Year)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

