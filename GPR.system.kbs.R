GPR.system.kbs <- function(...) {
# Giulio Curioni
# June 2015
# this script may be freely copied and distributed 
# please reference the following projects if using the KBS:
# Mapping The Underworld (http://www.mappingtheunderworld.ac.uk/) 
# Assessing The Underworld (http://assessingtheunderworld.org/)


## ## ## ##
# this script must be run from KBS.R
# imports user information on the GPR system
# outputs GPR.system suitability classes for GPR



# search for ### if there are problems with the script


#the local function environment is -1, but use fun.envir (i.e. 1) for the script to work correctly:
#note that sys.nframe() will produce pos=2, which does not work when using mapply

     fun.envir2 <<- fun.envir



#import data
#      gpr.system <<- read.xls("KBS.input.xlsx", sheet="gpr.system", verbose=F, method="tab", na.strings=c("NA","#DIV/0!"),
#                                perl="C:\\Perl64\\bin\\perl.exe", header=T)



      # convert everything to as.character
      gpr.system[,1:5] <<- as.matrix(sapply(gpr.system[,1:5], as.character), stringsAsFactors=FALSE)
      gpr.system <<- data.frame(gpr.system)

      # for interactive modification of the parameters within R:
      ###fix(gpr.system)



### define number of classes (length of column "symbol" in KBS.input file)
    length.symbol <- 3

## assign gpr system input classes to objects (note: objects are also somehow saved in the global environment)
      gpr.system.objects <- as.character(gpr.system$symbol[1:length.symbol])
      mapply(assign, gpr.system.objects, as.character(gpr.system$input.class[1:length.symbol]), pos=fun.envir2 )






## ## ## ##

## GPR SYSTEM OUTPUT


## ## ## ## define suitability classes




## ## 1

## use GPR system characteristics to estimate the suitability index:
# Utsi Electronics persnoal communication and:
# see Matthews S.L. (1998) Subsurface radar as an investigative technique, Building Research Establishment Report BR 340, CRC, Garston, Watford

                ### note: change limits if these are thought to be incorrect

                # GPR system characteristics classes:
                          # GPR.system.class1: GPR.system = a             # very high suitability
                          # GPR.system.class2: GPR.system = b             # very high or moderate suitability depending on soil surface
                          # GPR.system.class3: GPR.system = c             # moderate suitability
                          # GPR.system.class4: GPR.system = na            # unknown

      GPR.system.SI <<- 0
      if(is.na(GPR.system) == F) {
            if(GPR.system == "a")                            {GPR.system.SI <<- 1}        # very high suitability
            if(GPR.system == "b"  && surface.type ==  "a")   {GPR.system.SI <<- 1}        # very high suitability
            if(GPR.system == "b"  && surface.type ==  "b")   {GPR.system.SI <<- 3}        # moderate suitability
            if(GPR.system == "c")                            {GPR.system.SI <<- 3}        # moderate suitability
            if(GPR.system == "na")                           {GPR.system.SI <<- 0}        # unknown suitability
            }




## ## 2

## use known GPR suitability from previous surveys to estimate the suitability index:

                ### note: change limits if these are thought to be incorrect

                # known GPR suitability classes:
                          # known.GPR.suitability.class1: known.GPR.suitability = a     # very high suitability
                          # known.GPR.suitability.class2: known.GPR.suitability = b     # high suitability
                          # known.GPR.suitability.class3: known.GPR.suitability = c     # moderate suitability
                          # known.GPR.suitability.class4: known.GPR.suitability = d     # low suitability
                          # known.GPR.suitability.class5: known.GPR.suitability = e     # very low suitability
                          # known.GPR.suitability.class6: known.GPR.suitability = na    # unknown

      known.GPR.suitability.SI <<- 0
      if(is.na(known.GPR.suitability) == F) {
            if(known.GPR.suitability == "a")                  {known.GPR.suitability.SI <<- 1}        # very high suitability
            if(known.GPR.suitability == "b")                  {known.GPR.suitability.SI <<- 2}        # high suitability
            if(known.GPR.suitability == "c")                  {known.GPR.suitability.SI <<- 3}        # moderate suitability
            if(known.GPR.suitability == "d")                  {known.GPR.suitability.SI <<- 4}        # low suitability
            if(known.GPR.suitability == "e")                  {known.GPR.suitability.SI <<- 5}        # very low suitability
            if(known.GPR.suitability == "na")                 {known.GPR.suitability.SI <<- 0}        # unknown suitability
            }




## ## 3

## use KBS measured GPR suitability to estimate the suitability index:

                ### note: change limits if these are thought to be incorrect

                # GPR system characteristics classes:
                          # KBS.GPR.suitability.class1: KBS.GPR.suitability = a             # very high suitability
                          # KBS.GPR.suitability.class2: KBS.GPR.suitability = b             # high suitability
                          # KBS.GPR.suitability.class3: KBS.GPR.suitability = c             # moderate suitability
                          # KBS.GPR.suitability.class3: KBS.GPR.suitability = d             # low suitability
                          # KBS.GPR.suitability.class3: KBS.GPR.suitability = e             # very low suitability
                          # KBS.GPR.suitability.class3: KBS.GPR.suitability = na            # unknown
                          
      KBS.GPR.suitability.SI <<- 0
      if(is.na(KBS.GPR.suitability) == F) {
            if(KBS.GPR.suitability == "a")                  {KBS.GPR.suitability.SI <<- 1}        # very high suitability
            if(KBS.GPR.suitability == "b")                  {KBS.GPR.suitability.SI <<- 2}        # high suitability
            if(KBS.GPR.suitability == "c")                  {KBS.GPR.suitability.SI <<- 3}        # moderate suitability
            if(KBS.GPR.suitability == "d")                  {KBS.GPR.suitability.SI <<- 4}        # low suitability
            if(KBS.GPR.suitability == "e")                  {KBS.GPR.suitability.SI <<- 5}        # very low suitability
            if(KBS.GPR.suitability == "na")                 {KBS.GPR.suitability.SI <<- 0}        # unknown suitability
            }






















## create gpr system object to be added in the output

          GPR.system.out <- gpr.system[which(gpr.system$input.type == "GPR system"), 4:5]
                    GPR.system.out[,1] <- as.character(GPR.system.out[,1])
                    GPR.system.out[,2] <- as.character(GPR.system.out[,2])
                    aa <- which(gpr.system$symbol == "GPR.system")
                    GPR.system.output <- GPR.system.out[which(GPR.system.out$class == gpr.system$input.class[aa]), ]

          known.GPR.suitability.out <- gpr.system[which(gpr.system$input.type == "known GPR suitability"), 4:5]
                    known.GPR.suitability.out[,1] <- as.character(known.GPR.suitability.out[,1])
                    known.GPR.suitability.out[,2] <- as.character(known.GPR.suitability.out[,2])
                    aa <- which(gpr.system$symbol == "known.GPR.suitability")
                    known.GPR.suitability.output <- known.GPR.suitability.out[which(known.GPR.suitability.out$class == gpr.system$input.class[aa]), ]

          KBS.GPR.suitability.out <- gpr.system[which(gpr.system$input.type == "KBS measured GPR suitability"), 4:5]
                    KBS.GPR.suitability.out[,1] <- as.character(KBS.GPR.suitability.out[,1])
                    KBS.GPR.suitability.out[,2] <- as.character(KBS.GPR.suitability.out[,2])
                    aa <- which(gpr.system$symbol == "KBS.GPR.suitability")
                    KBS.GPR.suitability.output <- KBS.GPR.suitability.out[which(KBS.GPR.suitability.out$class == gpr.system$input.class[aa]), ]



          gpr.system.out <- rbind(GPR.system.output, known.GPR.suitability.output, KBS.GPR.suitability.output)




          # try to catch error. gpr system section must be completed with all "na" if no information is available.

          tryCatch(gpr.system.output <<- cbind(gpr.system$symbol[1:length.symbol], gpr.system.out), error=function(err) {
                stop("error: gpr.system section in KBS.input file has not been completed or a non-existing class was chosen.\n  Please choose among the existing options and add 'na' to the input.class if no information is available.")
                }
          )

          gpr.system.output <<- cbind(gpr.system$symbol[1:length.symbol], gpr.system.out)
          gpr.system.output <<- data.frame(gpr.system.output)
          colnames(gpr.system.output) <<- c("symbol", "input.class", "description")














# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## GPR.SYSTEM SUITABILITY CLASS

# define GPR suitability class based on gpr.system classes
# (i.e. GPR.system.SI, known.GPR.suitability.SI, KBS.GPR.suitability.SI)

          GPR.vec <<- c(GPR.system.SI, known.GPR.suitability.SI, KBS.GPR.suitability.SI)


}