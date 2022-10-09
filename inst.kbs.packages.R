inst.kbs.packages <- function(...) {
# Giulio Curioni
# June 2015

#install R packages for KBS
#relaunch this script when a new version of R is available

## system requirements:

      # download and install Java development kit.
      # download it for example from http://download.cnet.com/Java-Development-Kit-64-Bit/3000-2218_4-75317068.html
      # choose the right version depending on the system (e.g. 32-bit or 64-bit)
      
      
      # download and install ActivePerl.
      # download it for example from: http://download.cnet.com/ActivePerl-Windows/3000-2069_4-10006395.html
      # choose the right version depending on the system (e.g. 32-bit or 64-bit)


## to check which packages are installed and loaded by default:
# i <- installed.packages()
# i[ i[,"Priority"] %in% c("base","recommended"), c("Package", "Priority")] 

 
## ## install additional packages into the library (e.g. "C:\\R\\R-3.2.0\\library")


# list of packages to be installed


#install.packages(c("car", "maptools", "R.matlab", "R.utils", "raster", "rgdal", "rgl", "sp", "xlsx", "zoom", "zoo"), lib="C:\\R\\R-3.2.0\\library") # "svSocket",  #"sfsmisc", "TinnR",  "gplots", "TTR", "oce", "RSAGA", "playwith",

### replace R-n.n.n according to the R version in use.
install.packages("gdata", lib="C:\\R\\R-3.2.0\\library")
install.packages("rJava", lib="C:\\R\\R-3.2.0\\library")
install.packages("svSocket", lib="C:\\R\\R-3.2.0\\library")
install.packages("xlsx", lib="C:\\R\\R-3.2.0\\library")
install.packages("gWidgetsRGtk2", lib="C:\\R\\R-3.2.0\\library")



}
