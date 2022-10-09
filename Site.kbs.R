Site.kbs <- function(...) {
# Giulio Curioni
# June 2015
# this script may be freely copied and distributed 
# please reference the following projects if using the KBS:
# Mapping The Underworld (http://www.mappingtheunderworld.ac.uk/) 
# Assessing The Underworld (http://assessingtheunderworld.org/)


## ## ## ##
# this script must be run from KBS.R
# imports user information on the site
# outputs site suitability classes for GPR



# search for ### if there are problems with the script


#the local function environment is -1, but use fun.envir (i.e. 1) for the script to work correctly:
#note that sys.nframe() will produce pos=2, which does not work when using mapply
     
     fun.envir2 <<- fun.envir
     
     
     
#import data
#      site <<- read.xls("KBS.input.xlsx", sheet="site", verbose=F, method="tab", na.strings=c("NA","#DIV/0!"),
#                                perl="C:\\Perl64\\bin\\perl.exe", header=T)



      # convert everything to as.character
      site[,1:5] <<- as.matrix(sapply(site[,1:5], as.character), stringsAsFactors=FALSE)
      site <<- data.frame(site)

      # for interactive modification of the parameters within R:
      ###fix(site)



### define number of classes (length of column "symbol" in KBS.input file)
    length.symbol <- 6

## assign site input classes to objects (note: objects are also somehow saved in the global environment)
      site.objects <- as.character(site$symbol[1:length.symbol])
      mapply(assign, site.objects, as.character(site$input.class[1:length.symbol]), pos=fun.envir2 )
                                                      





## ## ## ##

## SITE OUTPUT
                      

## ## ## ## define suitability classes




## ## 1

## use surface type to estimate the suitability index:
# grass and unpaved surfaces are considered very slightly less suited to GPR because they can be a lot wetter and perhaps more rough than paved surfaces
# this is still an important input for other parameters
                ### note: change limits if these are thought to be incorrect

                # surface type classes (see input file for more information):
                          # surface.type.class1: surface.type = a     # very high suitability
                          # surface.type.class2: surface.type = b     # high suitability
                          # surface.type.class3: surface.type = na    # unknown
      
      surface.type.SI <<- 0
      if(is.na(surface.type) == F) {
            if(surface.type == "a")                  {surface.type.SI <<- 1}        # very high suitability
            if(surface.type == "b")                  {surface.type.SI <<- 2}        # high suitability
            if(surface.type == "na")                 {surface.type.SI <<- 0}        # unknown suitability
            }





                          




## ## 2

## use surface conditions (for both paved and unpaved surfaces) to estimate the suitability index:

                ### note: change limits if these are thought to be incorrect

                # surface conditions classes (see input file for more information):
                          # surface.conditions.class1: surface.conditions = a     # very high suitability
                          # surface.conditions.class2: surface.conditions = b     # moderate suitability 
                          # surface.conditions.class3: surface.conditions = c     # low suitability
                          # surface.conditions.class4: surface.conditions = d     # very low suitability
                          # surface.conditions.class5: surface.conditions = na    # unknown
      
      surface.conditions.SI <<- 0
      if(is.na(surface.conditions) == F) {
            if(surface.conditions == "a")                  {surface.conditions.SI <<- 1}        # very high suitability
            if(surface.conditions == "b")                  {surface.conditions.SI <<- 3}        # moderate suitability
            if(surface.conditions == "c")                  {surface.conditions.SI <<- 4}        # low suitability
            if(surface.conditions == "d")                  {surface.conditions.SI <<- 5}        # very low suitability
            if(surface.conditions == "na")                 {surface.conditions.SI <<- 0}        # unknown suitability            
            }


                                 










## ## 3

## use metallic structures to estimate the suitability index:
# buried metal rebar kills the GPR signals. Metal structures above the ground (e.g., in factories) can produce unwanted reflections near the surface.

                ### note: change limits if these are thought to be incorrect

                # metallic structures classes:
                          # metallic.structures.class1: metallic.structures = a     # very high suitability
                          # metallic.structures.class2: metallic.structures = b     # moderate suitability
                          # metallic.structures.class3: metallic.structures = c     # very low suitability
                          # metallic.structures.class4: metallic.structures = na    # unknown

      metallic.structures.SI <<- 0
      if(is.na(metallic.structures) == F) {
            if(metallic.structures == "a")                  {metallic.structures.SI <<- 1}        # very high suitability
            if(metallic.structures == "b")                  {metallic.structures.SI <<- 3}        # moderate suitability
            if(metallic.structures == "c")                  {metallic.structures.SI <<- 5}        # very low suitability
            if(metallic.structures == "na")                 {metallic.structures.SI <<- 0}        # unknown suitability
            }





                   




## ## 4

## use target type to estimate the suitability index:
# the distinction is made for metallic and non metallic pipes and for their approximate size.

                ### note: change limits if these are thought to be incorrect

                # target type classes:
                          # target.type.class1: target.type = a             # metallic (< 100 mm): high suitability
                          # target.type.class2: target.type = b             # metallic (>= 100 mm): very high suitability
                          # target.type.class3: target.type = c             # non metallic (< 100 mm): moderate suitability
                          # target.type.class4: target.type = d             # non metallic (>= 100 mm): high suitability
                          # target.type.class5: target.type = na            # unknown

      target.type.SI <<- 0
      if(is.na(target.type) == F) {
            if(target.type == "a")                            {target.type.SI <<- 2}        # high suitability
            if(target.type == "b")                            {target.type.SI <<- 1}        # very high suitability
            if(target.type == "c")                            {target.type.SI <<- 3}        # moderate suitability
            if(target.type == "d")                            {target.type.SI <<- 2}        # high suitability
            if(target.type == "na")                           {target.type.SI <<- 0}        # unknown suitability
            }





                         




## ## 5

## use ground clutter to estimate the suitability index:
# the presence of cobbles, boulders and other objects can create unwanted multiple reflections that deteriorates the GPR signals

                ### note: change limits if these are thought to be incorrect

                # ground clutter classes:
                          # ground.clutter.class1: ground.clutter = a     # very high suitability
                          # ground.clutter.class2: ground.clutter = b     # moderate suitability
                          # ground.clutter.class3: ground.clutter = na    # unknown

      ground.clutter.SI <<- 0
      if(is.na(ground.clutter) == F) {
            if(ground.clutter == "a")                  {ground.clutter.SI <<- 1}        # very high suitability
            if(ground.clutter == "b")                  {ground.clutter.SI <<- 3}        # moderate suitability
            if(ground.clutter == "na")                 {ground.clutter.SI <<- 0}        # unknown suitability
            }







                      
                


## ## 6

## use soil.type to estimate the suitability index:
# shrink-swell clays such as smectite-based clays are dispersive and are less suited to GPR

                ### note: change limits if these are thought to be incorrect

                # soil type classes:
                          # soil.type.class1: soil.type = a             # very high suitability
                          # soil.type.class2: soil.type = b             # high suitability
                          # soil.type.class2: soil.type = c             # moderate suitability
                          # soil.type.class2: soil.type = d             # moderate suitability
                          # soil.type.class3: soil.type = na            # unknown

      soil.type.SI <<- 0
      if(is.na(soil.type) == F) {
            if(soil.type == "a")                            {soil.type.SI <<- 1}        # very high suitability
            if(soil.type == "b")                            {soil.type.SI <<- 2}        # high suitability
            if(soil.type == "c")                            {soil.type.SI <<- 3}        # moderate suitability
            if(soil.type == "d")                            {soil.type.SI <<- 4}        # low suitability
            if(soil.type == "e")                            {soil.type.SI <<- 3}        # moderate suitability
            if(soil.type == "na")                           {soil.type.SI <<- 0}        # unknown suitability
            }






                             



## ##
#
## use known acoustics suitability from previous surveys to estimate the suitability index of the soil to acoustic techniques:
#
#                ## note: change limits if these are thought to be incorrect
#
#                # known acoustics suitability classes:
#                          # known.acoustics.suitability.class1: known.acoustics.suitability = a     # high suitability
#                          # known.acoustics.suitability.class2: known.acoustics.suitability = b     # moderate suitability
#                          # known.acoustics.suitability.class3: known.acoustics.suitability = c     # low suitability
#                          # known.acoustics.suitability.class4: known.acoustics.suitability = na    # unknown
#
#      known.acoustics.suitability.SI <<- 0
#      if(is.na(known.acoustics.suitability) == F) {
#            if(known.acoustics.suitability == "a")                  {known.acoustics.suitability.SI <<- 1}        # high suitability
#            if(known.acoustics.suitability == "b")                  {known.acoustics.suitability.SI <<- 3}        # moderate suitability
#            if(known.acoustics.suitability == "c")                  {known.acoustics.suitability.SI <<- 4}        # low suitability
#            if(known.acoustics.suitability == "na")                 {known.acoustics.suitability.SI <<- 0}        # unknown suitability
#            }














## create site object to be added in the output

          surface.type.out <- site[which(site$input.type == "surface type"), 4:5]
                    surface.type.out[,1] <- as.character(surface.type.out[,1])
                    surface.type.out[,2] <- as.character(surface.type.out[,2])
                    aa <- which(site$symbol == "surface.type")
                    surface.type.output <- surface.type.out[which(surface.type.out$class == site$input.class[aa]), ]
          
          surface.conditions.out <- site[which(site$input.type == "surface conditions"), 4:5]
                    surface.conditions.out[,1] <- as.character(surface.conditions.out[,1])
                    surface.conditions.out[,2] <- as.character(surface.conditions.out[,2])
                    aa <- which(site$symbol == "surface.conditions")
                    surface.conditions.output <- surface.conditions.out[which(surface.conditions.out$class == site$input.class[aa]), ]
                    
          metallic.structures.out <- site[which(site$input.type == "metallic structures"), 4:5]
                    metallic.structures.out[,1] <- as.character(metallic.structures.out[,1])
                    metallic.structures.out[,2] <- as.character(metallic.structures.out[,2])
                    aa <- which(site$symbol == "metallic.structures")
                    metallic.structures.output <- metallic.structures.out[which(metallic.structures.out$class == site$input.class[aa]), ]

          target.type.out <- site[which(site$input.type == "target type"), 4:5]
                    target.type.out[,1] <- as.character(target.type.out[,1])
                    target.type.out[,2] <- as.character(target.type.out[,2])
                    aa <- which(site$symbol == "target.type")
                    target.type.output <- target.type.out[which(target.type.out$class == site$input.class[aa]), ]
                    
          ground.clutter.out <- site[which(site$input.type == "ground clutter"), 4:5]
                    ground.clutter.out[,1] <- as.character(ground.clutter.out[,1])
                    ground.clutter.out[,2] <- as.character(ground.clutter.out[,2])
                    aa <- which(site$symbol == "ground.clutter")
                    ground.clutter.output <- ground.clutter.out[which(ground.clutter.out$class == site$input.class[aa]), ]

          soil.type.out <- site[which(site$input.type == "soil type"), 4:5]
                    soil.type.out[,1] <- as.character(soil.type.out[,1])
                    soil.type.out[,2] <- as.character(soil.type.out[,2])
                    aa <- which(site$symbol == "soil.type")
                    soil.type.output <- soil.type.out[which(soil.type.out$class == site$input.class[aa]), ]

                    
          site.out <<- rbind(surface.type.output, surface.conditions.output, metallic.structures.output,
                            target.type.output, ground.clutter.output, soil.type.output)


                       

          # try to catch error. Site section must be completed with all "na" if no information is available.

          tryCatch(site.output <<- cbind(site$symbol[1:length.symbol], site.out), error=function(err) {
                stop("error: site section in KBS.input file has not been completed or a non-existing class was chosen.\n  Please choose among the existing options and add 'na' to the input.class if no information is available.")
                }
          )


          site.output <<- data.frame(site.output)
          colnames(site.output) <<- c("symbol", "input.class", "description")

  
  
  
          

        

        





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## SITE SUITABILITY CLASS

# define GPR suitability class based on site classes
# (i.e. surface.type.SI, surface.conditions.SI, metallic.objects.SI, target.type.SI)

          site.vec <<- c(surface.type.SI, surface.conditions.SI, metallic.structures.SI, target.type.SI,
                         ground.clutter.SI, soil.type.SI)

                                
}