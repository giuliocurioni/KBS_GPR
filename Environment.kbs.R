Environment.kbs <- function(...) {
# Giulio Curioni
# June 2015
# this script may be freely copied and distributed 
# please reference the following projects if using the KBS:
# Mapping The Underworld (http://www.mappingtheunderworld.ac.uk/) 
# Assessing The Underworld (http://assessingtheunderworld.org/)


## ## ## ##
# this script must be run from KBS.R
# imports user information on the environmental conditions
# outputs environmental suitability classes for GPR



# search for ### if there are problems with the script


#the local function environment is -1, but use fun.envir (i.e. 1) for the script to work correctly:
#note that sys.nframe() will produce pos=2, which does not work when using mapply

     fun.envir2 <<- fun.envir



#import data                              
#      enviro <<- read.xls("KBS.input.xlsx", sheet="environment", verbose=F, method="tab", na.strings=c("NA","#DIV/0!"),
#                                perl="C:\\Perl64\\bin\\perl.exe", header=T)



      # convert everything to as.character
      enviro[,1:5] <<- as.matrix(sapply(enviro[,1:5], as.character), stringsAsFactors=FALSE)
      enviro <<- data.frame(enviro)

      # for interactive modification of the parameters within R:
      ###fix(enviro)



### define number of classes (length of column "symbol" in KBS.input file)
    length.symbol <- 2

## assign environment input classes to objects (note: objects are also somehow saved in the global environment)
      enviro.objects <- as.character(enviro$symbol[1:length.symbol])
      mapply(assign, enviro.objects, as.character(enviro$input.class[1:length.symbol]), pos=fun.envir2 )






## ## ## ##

## ENVIRONMENTAL CONDITIONS OUTPUT


## ## ## ## define suitability classes




## ## 1

## use recent.weather to estimate the suitability index:
# note: if it rained significantly in the past week it is likely that the ground is in wet conditions, especially under unpaved surfaces and in clayey soils

                ### note: change limits if these are thought to be incorrect

                # recent weather classes (see input file for more information):
                          # recent.weather.class1: recent.weather = a     # very high suitability
                          # recent.weather.class2: recent.weather = b     # moderate suitability
                          # recent.weather.class3: recent.weather = na    # unknown

      recent.weather.SI <<- 0
      if(is.na(recent.weather) == F) {
            if(recent.weather == "a")                                 {recent.weather.SI <<- 1}        # very high suitability
            if(recent.weather == "b")                                 {recent.weather.SI <<- 3}        # moderate suitability
            if(recent.weather == "na")                                {recent.weather.SI <<- 0}        # unknown suitability
            }










## ## 2

## use saturation to estimate the suitability index:
# note: if the soil is very wet or saturated it is considered less suited to GPR use

                ### note: change limits if these are thought to be incorrect

                # saturation classes (see input file for more information):
                          # saturation.class1: saturation = a     # very high suitability
                          # saturation.class2: saturation = b     # low suitability 
                          # saturation.class3: saturation = na    # unknown
                          # saturation.class4: saturation = b     # input is unknown but it is possible to estimate it from other inputs

      saturation.SI <<- 0
      if(is.na(saturation) == F) {
            if(saturation == "a")                                 {saturation.SI <<- 1}        # very high suitability
            if(saturation == "b")                                 {saturation.SI <<- 4}        # low suitability
            if(saturation == "na")                                {saturation.SI <<- 0}        # unknown suitability
            if(saturation == "na"  &&  recent.weather == "b"  &&  soil.type == "b")          {saturation.SI <<- 3}        # moderate suitability
            if(saturation == "na"  &&  recent.weather == "b"  &&  soil.type == "c")          {saturation.SI <<- 4}        # low suitability
            if(saturation == "na"  &&  recent.weather == "b"  &&  soil.type == "d")          {saturation.SI <<- 3}        # moderate suitability
            }
            



                            


                       




















## create environmental conditions object to be added in the output

          recent.weather.out <- enviro[which(enviro$input.type == "recent weather"), 4:5]
                    recent.weather.out[,1] <- as.character(recent.weather.out[,1])
                    recent.weather.out[,2] <- as.character(recent.weather.out[,2])
                    aa <- which(enviro$symbol == "recent.weather")
                    recent.weather.output <- recent.weather.out[which(recent.weather.out$class == enviro$input.class[aa]), ]

          saturation.out <- enviro[which(enviro$input.type == "saturation"), 4:5]
                    saturation.out[,1] <- as.character(saturation.out[,1])
                    saturation.out[,2] <- as.character(saturation.out[,2])
                    aa <- which(enviro$symbol == "saturation")
                    saturation.output <- saturation.out[which(saturation.out$class == enviro$input.class[aa]), ]


          enviro.out <- rbind(recent.weather.output, saturation.output)




          # try to catch error. Environment section must be completed with all "na" if no information is available.

          tryCatch(enviro.output <<- cbind(enviro$symbol[1:length.symbol], enviro.out), error=function(err) {
                stop("error: environment section in KBS.input file has not been completed or a non-existing class was chosen.\n  Please choose among the existing options and add 'na' to the input.class if no information is available.")
                }
          )
                            

          enviro.output <<- data.frame(enviro.output)
          colnames(enviro.output) <<- c("symbol", "input.class", "description")














# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## ENVIRONMENTAL CONDITIONS SUITABILITY CLASS

# define environmental conditions suitability class based on environment classes
# (i.e. recent.weather.SI, saturation.SI)

          enviro.vec <<- c(recent.weather.SI, saturation.SI)




}