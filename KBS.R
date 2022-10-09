KBS <- function(...) {
# Giulio Curioni
# June 2015
# this script may be freely copied and distributed 
# please reference the following projects if using the KBS:
# Mapping The Underworld (http://www.mappingtheunderworld.ac.uk/) 
# Assessing The Underworld (http://assessingtheunderworld.org/)
 


## ## ## ##
# imports available data from KBS.input.xlsx
# and outputs:
# 1) soil properties relevant to sensors
# 2) soil suitability class for GPR


# search for ### if there are problems with the script

### check inst.kbs.packages.R to make sure that all the necessary software and R packages are correctly installed







# import existing data and try to calculate geophysical parameters based on existing information
message(" ")
message(" ")
message("open 'KBS.input.xlsx' and manually input the available information (geophysical, soil, site, environment, gpr.system) in the corresponding sheets.")
message("use GLOSSARY for more information. [note: SI stands for Suitability Index]")
message(" ")
project.name <<- as.character(readline("Input project name (e.g., location): "))
message(" ")
              
#creates directory for output (don't run this if the directory already exists)
### dir.create("C:/R_KBS/KBS.output")


#the local function environment is -1, but use sys.nframe (i.e. 1) for the script to work correctly:
     fun.envir <<- sys.nframe()     



# remove warnings (these normally do not affect the results. Use options(warn=0) to see potential warnings)   
options(warn=-1)















## import data
### change Perl directory if necessary

      geophysical <<- read.xls("KBS.input.xlsx", sheet="geophysical", verbose=F, method="tab", na.strings=c("NA","#DIV/0!"),
                                perl="C:\\Perl64\\bin\\perl.exe", header=T)
      
      soil <<- read.xls("KBS.input.xlsx", sheet="soil", verbose=F, method="tab", na.strings=c("NA","#DIV/0!"),
                                perl="C:\\Perl64\\bin\\perl.exe", header=T)
      
      site <<- read.xls("KBS.input.xlsx", sheet="site", verbose=F, method="tab", na.strings=c("NA","#DIV/0!"),
                                perl="C:\\Perl64\\bin\\perl.exe", header=T)
                                                                                         
      enviro <<- read.xls("KBS.input.xlsx", sheet="environment", verbose=F, method="tab", na.strings=c("NA","#DIV/0!"),
                                perl="C:\\Perl64\\bin\\perl.exe", header=T)
                                
      gpr.system <<- read.xls("KBS.input.xlsx", sheet="gpr.system", verbose=F, method="tab", na.strings=c("NA","#DIV/0!"),
                                perl="C:\\Perl64\\bin\\perl.exe", header=T)




                    

## run scripts with individual sections. Do not change the order.


## ## run Site.kbs ## ##
      source("C:\\R_KBS\\Site.kbs.R"); Site.kbs()                 # must be run before 


                                       
                          

    ## ## run Environment.kbs ## ##
      source("C:\\R_KBS\\Environment.kbs.R"); Environment.kbs()   # must be run before to provide wilting point, field capacity, saturation levels





## ## run Geophysical.kbs ## ##
      source("C:\\R_KBS\\Geophysical.kbs.R"); Geophysical.kbs()
      
#note: this is run from Geophysical.kbs
## ## run Soil.kbs ## ##
#      source("C:\\R_KBS\\Soil.kbs.R"); Soil.kbs()      





## ## run GPR.system.kbs ## ##
      source("C:\\R_KBS\\GPR.system.kbs.R"); GPR.system.kbs()

















## ## save output in MS Excel ## ##

error.message <<- NA

## save output also in xlsx format
tryCatch(write.xlsx(geophysical, file = paste("C:/R_KBS/KBS.output/KBS.output.", project.name, ".xlsx", sep=""), sheetName="geophysical",
           col.names=T, row.names=F, append=F), error=function(err) {
           error.message <<- paste("error: KBS output could not be saved. Please close KBS.output.", project.name, ".xlsx ", "and rerun the script.", sep="")
           }
        )

tryCatch(write.xlsx(soil, file = paste("C:/R_KBS/KBS.output/KBS.output.", project.name, ".xlsx", sep=""), sheetName="soil",
           col.names=T, row.names=F, append=T), error=function(err) {
           # error.message <<- paste("error: KBS output could not be saved. Please close KBS.output.", project.name, ".xlsx ", "and rerun the script.", sep="")
           }
        )

## save output also in xlsx format
tryCatch(write.xlsx(site.output, file = paste("C:/R_KBS/KBS.output/KBS.output.", project.name, ".xlsx", sep=""), sheetName="site",
           col.names=T, row.names=F, append=T), error=function(err) {
           # error.message <<- paste("error: KBS output could not be saved. Please close KBS.output.", project.name, ".xlsx ", "and rerun the script.", sep="")
           }
        )

## save output also in xlsx format
tryCatch(write.xlsx(enviro.output, file = paste("C:/R_KBS/KBS.output/KBS.output.", project.name, ".xlsx", sep=""), sheetName="environment",
           col.names=T, row.names=F, append=T), error=function(err) {
           # error.message <<- paste("error: KBS output could not be saved. Please close KBS.output.", project.name, ".xlsx ", "and rerun the script.", sep="")
           }
        )

## save output also in xlsx format
tryCatch(write.xlsx(gpr.system.output, file = paste("C:/R_KBS/KBS.output/KBS.output.", project.name, ".xlsx", sep=""), sheetName="gpr.system",
           col.names=T, row.names=F, append=T), error=function(err) {
           # error.message <<- paste("error: KBS output could not be saved. Please close KBS.output.", project.name, ".xlsx ", "and rerun the script.", sep="")
           }
        )












# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## GEOPHYSICAL + SOIL + SITE + ENVIRONMENTAL CONDITIONS CLASS


# define GPR suitability index (SI).
# the most limiting SI is chosen to define the GPR suitability

          ## suitability indices:

                    ## SI 0: no suitability
                    ## SI 1: very high suitability
                    ## SI 2: high suitability
                    ## SI 3: moderate suitability
                    ## SI 4: low suitability
                    ## SI 5: very low suitability



       SI.definition <- c(0,1,2,3,4,5)
       SI.definition <- data.frame(SI.definition)
       rownames(SI.definition) <- c("insufficient data", "very high suitability", "high suitability", "moderate suitability", 
                                    "low suitability", "very low suitability")




          
          # create a dataframe with single classes [note: USE THIS TO DOUBLE CHECK SINGLE classes]
          
          # it is useful to keep kbs.class for a nicer output:
          
          kbs.class <<- c(geophys.vec, soil.vec, site.vec, enviro.vec, GPR.vec)
          kbs.class <<- data.frame(kbs.class)

          colnames(kbs.class) <<- c("GPR.SI")
          rownames(kbs.class) <<- c("Mdisp.geophysical",
                                    "BEC",
                                    "La",
                                    "Mdisp.geotechnical",
                                    "clay",
                                    "water.table",
                                    "SAR",
                                    "CaCO3",
                                    "surface.type",
                                    "surface.conditions",
                                    "metallic.structures",
                                    "target.type",
                                    "ground.clutter",
                                    "soil.type",
                                    "recent.weather",
                                    "saturation",
                                    "GPR.system",
                                    "known.GPR.suitability",
                                    "KBS.GPR.suitability")



          gpr.soil.section <<- c(geophys.vec, soil.vec)
          gpr.soil.section <<- data.frame(gpr.soil.section)

          colnames(gpr.soil.section) <<- c("GPR.soil.SI")
          rownames(gpr.soil.section) <<- c("Mdisp.geophysical",
                                           "BEC",
                                           "La",
                                           "Mdisp.geotechnical",
                                           "clay",
                                           "water.table",
                                           "SAR",
                                           "CaCO3")
                                          
          gpr.site.section <<- site.vec
          gpr.site.section <<- data.frame(gpr.site.section)

          colnames(gpr.site.section) <<- c("GPR.site.SI")
          rownames(gpr.site.section) <<- c("surface.type",
                                          "surface.conditions",
                                          "metallic.structures",
                                          "target.type",
                                          "ground.clutter",
                                          "soil.type")

          gpr.environment.section <<- enviro.vec
          gpr.environment.section <<- data.frame(gpr.environment.section)

          colnames(gpr.environment.section) <<- c("GPR.site.SI")
          rownames(gpr.environment.section) <<- c("recent.weather",
                                                  "saturation")
                                          
          gpr.system.section  <<- GPR.vec
          gpr.system.section <<- data.frame(gpr.system.section)

          colnames(gpr.system.section) <<- c("GPR.system.SI")
          rownames(gpr.system.section) <<- c("GPR.system",
                                             "known.GPR.suitability",
                                             "KBS.GPR.suitability")


           
          


          ## ## find final suitability class ## ##
          


                ## ##  find SOIL SI, including only soil parameters  ## ##
                
                
                
                # define conditions when data are not enough to provide a GPR soil suitability class:

                sufficient.soil.data <<- TRUE
                if(Mdisp.geotechnical.SI == 0  &&  BEC.SI == 0  &&  La.SI == 0)  {sufficient.soil.data <<- FALSE}

                # calculate gpr.soil.SI (it includes only soil parameters)
                gpr.soil.SI <<- max(gpr.soil.section)

                      if(gpr.soil.SI == 0)                     { GPR.soil.suitability.class <<- "NOT AVAILABLE" }
                      if(gpr.soil.SI == 1)                     { GPR.soil.suitability.class <<- "VERY HIGH" }
                      if(gpr.soil.SI == 2)                     { GPR.soil.suitability.class <<- "HIGH" }
                      if(gpr.soil.SI == 3)                     { GPR.soil.suitability.class <<- "MODERATE" }
                      if(gpr.soil.SI == 4)                     { GPR.soil.suitability.class <<- "LOW" }
                      if(gpr.soil.SI == 5)                     { GPR.soil.suitability.class <<- "VERY LOW" }



                ## ##  find SITE SI, including only site parameters  ## ##



                # define conditions when data are not enough to provide a GPR site suitability class:

                sufficient.site.data <<- TRUE
                if(surface.conditions.SI == 0  &&  metallic.structures.SI == 0)  {sufficient.site.data <<- FALSE}

                gpr.site.SI <<- max(gpr.site.section)

                      if(gpr.site.SI == 0)                     { GPR.site.suitability.class <<- "NOT AVAILABLE" }
                      if(gpr.site.SI == 1)                     { GPR.site.suitability.class <<- "VERY HIGH" }
                      if(gpr.site.SI == 2)                     { GPR.site.suitability.class <<- "HIGH" }
                      if(gpr.site.SI == 3)                     { GPR.site.suitability.class <<- "MODERATE" }
                      if(gpr.site.SI == 4)                     { GPR.site.suitability.class <<- "LOW" }
                      if(gpr.site.SI == 5)                     { GPR.site.suitability.class <<- "VERY LOW" }



                ## ##  find ENVIRONMENT SI, including only environmental parameters  ## ##



                gpr.environment.SI <<- max(gpr.environment.section)

                      if(gpr.environment.SI == 0)                     { GPR.environment.suitability.class <<- "NOT AVAILABLE" }
                      if(gpr.environment.SI == 1)                     { GPR.environment.suitability.class <<- "VERY HIGH" }
                      if(gpr.environment.SI == 2)                     { GPR.environment.suitability.class <<- "HIGH" }
                      if(gpr.environment.SI == 3)                     { GPR.environment.suitability.class <<- "MODERATE" }
                      if(gpr.environment.SI == 4)                     { GPR.environment.suitability.class <<- "LOW" }



                ## ##  find GPR system SI, including only GPR system parameters  ## ##



                gpr.system.SI <<- max(gpr.system.section)

                      if(gpr.system.SI == 0)                   { GPR.system.suitability.class <<- "NOT AVAILABLE" }
                      if(gpr.system.SI == 1)                   { GPR.system.suitability.class <<- "VERY HIGH" }
                      if(gpr.system.SI == 2)                   { GPR.system.suitability.class <<- "HIGH" }
                      if(gpr.system.SI == 3)                   { GPR.system.suitability.class <<- "MODERATE" }
                      if(gpr.system.SI == 4)                   { GPR.system.suitability.class <<- "LOW" }
                      if(gpr.system.SI == 5)                   { GPR.system.suitability.class <<- "VERY LOW" }




               # calculate final GPR soil suitability class, based on limiting factor for each section:
               
               
               
               
                      GPR.suitability.class <<- max(gpr.soil.SI, gpr.site.SI, gpr.environment.SI, gpr.system.SI)
                      
                            if(GPR.suitability.class == 0)                   { GPR.suitability.class.def <<- "NOT AVAILABLE" }
                            if(GPR.suitability.class == 1)                   { GPR.suitability.class.def <<- "VERY HIGH" }
                            if(GPR.suitability.class == 2)                   { GPR.suitability.class.def<<- "HIGH" }
                            if(GPR.suitability.class == 3)                   { GPR.suitability.class.def <<- "MODERATE" }
                            if(GPR.suitability.class == 4)                   { GPR.suitability.class.def <<- "LOW" }
                            if(GPR.suitability.class == 5)                   { GPR.suitability.class.def <<- "VERY LOW" }

                
                



# find current date:
date.of.modelling <<- as.character(Sys.Date())               
                            

                    
                    
## print results on the R console
          message(paste(date.of.modelling, "  KBS.", project.name, sep=""))
          message(" ")
          message("--------------------------------------------------------------------------------------------------------------------------------------")
          message("GEOPHYSICAL DATA")
          print(geophysical, row.names=F)
          message(" ")
          message("--------------------------------------------------------------------------------------------------------------------------------------")
          message("SOIL DATA")
          print(soil, row.names=F)
          message(" ")
          message("--------------------------------------------------------------------------------------------------------------------------------------")
          message("EXPERT KNOWLEDGE")
          message(" ")
          message("SITE")
          print(site.output, row.names=F)
          message(" ")
          message("ENVIRONMENTAL CONDITIONS")
          print(enviro.output, row.names=F)
          message(" ")
          message("GPR SYSTEM")
          print(gpr.system.output, row.names=F)
          message(" ")
          message("--------------------------------------------------------------------------------------------------------------------------------------")
          message("KBS SUITABILITY")
          print(SI.definition)
          message(" ")
          print(kbs.class)
#          print(gpr.soil.section)
#          message(" ")
#          print(gpr.site.section)
#          message(" ")
#          print(gpr.system.section)
          message(" ")
#          message("The GPR suitability score is calculated as the percentage of the sum of the available GPR.SI with respect to the max sum of the available GPR.SI\n(i.e., if all the SI of the available classes were their maximum possible value).")
#          message(" ")
#          message("Criteria:\n     score <= 15 : VERY HIGH\n15 < score <= 30 : HIGH\n30 < score <= 45 : MODERATE\n45 < score <= 60 : LOW\n      score > 60 : VERY LOW")
          warningflag <<- FALSE
          message(" ")
          message(paste("GPR soil suitability:", round(gpr.soil.SI, digits=0), GPR.soil.suitability.class, sep = "  "))
          message(paste("GPR site suitability:", round(gpr.site.SI, digits=0), GPR.site.suitability.class, sep = "  "))
          message(paste("GPR environment suitability:", round(gpr.environment.SI, digits=0), GPR.environment.suitability.class, sep = "  "))
          message(paste("GPR system suitability:", round(gpr.system.SI, digits=0), GPR.system.suitability.class, sep = "  "))
          message(" ")
          message(paste("Final GPR suitability:", GPR.suitability.class, GPR.suitability.class.def, sep = "  "))
          message(" ")
          message(" ")
          if(gpr.soil.SI  ==  0)  {message("Warning: The KBS could not identify a GPR soil suitability."); warningflag <<- TRUE}
          if(sufficient.soil.data == F  &&  gpr.soil.SI  !=  0) {message("Warning: Important GPR.SI are missing in the soil section. The GPR soil suitability might not be reliable."); warningflag <<- TRUE}

          if(gpr.site.SI  ==  0)  {message("Warning: The KBS could not identify a GPR site suitability."); warningflag <<- TRUE}
          if(sufficient.site.data == F  &&  gpr.site.SI  !=  0) {message("Warning: Important GPR.SI are missing in the site section. The GPR site suitability might not be reliable."); warningflag <<- TRUE}

          if(gpr.environment.SI  ==  0)  {message("Warning: The KBS could not identify a GPR environment suitability.")} # warningflag <<- TRUE}  #warningflag is not necessary in this case

          if(gpr.system.SI  ==  0)  {message("Warning: The KBS could not identify a GPR system suitability.")} # warningflag <<- TRUE}  #warningflag is not necessary in this case

          message(" ")
          if(La.SI >= 4  &&  is.na(La) == F  &&  is.na(f.in) == F) {message("Attenuation loss seems high. If possible, use a lower frequency antenna.")}
          if(La.SI >= 4  &&  is.na(La) == F  &&  is.na(f.in) == T) {message("Attenuation loss for a 500 MHz antenna seems high. If possible, use a lower frequency antenna.")}
          if(La.SI < 4  &&  is.na(La.1m) == F  &&  is.na(La.1m.500) == F   &&  is.na(f.in) == F) { 
                   if(La.1m > 80  ||  La.1m.500 > 80) {
                   message("Attenuation loss at 1 m depth seems high.")
                   message("If the inputs for La, La.1m and La.1m.500 are correct the GPR soil suitability might not be reliable.") 
                   message("If possible, use a lower frequency antenna.")
                   }
          }
          if(La.SI < 4  &&  is.na(La.1m.500) == F  &&  is.na(f.in) == T) {
                   if(La.1m.500 > 80) {
                   message("Attenuation loss at 1 m depth for a 500 MHz antenna seems high.")
                   message("If the inputs for La, La.1m and La.1m.500 are correct the GPR soil suitability might not be reliable.")
                   message("If possible, use a lower frequency antenna.")
                   }
          }
          if(is.na(La.1m.in) == F  &&  is.na(La.1m) == F) {
                   if(La.1m.in != La.1m) {message("Output attenuation loss at 1 m depth is different from input attenuation loss. Make sure that the inputs for depth, f, La, La.1m and La.1m.500 are correct.")} 
          }
          if(is.na(La.in) == F  &&  is.na(La.1m.in) == F  &&  is.na(La.1m.500.in) == F  &&  f == 500e6  &&  depth == 1) {
                   if(La.in != La.1m.in  ||  La.in != La.1m.500.in  ||  La.1m.in != La.1m.500.in ) {message("Please make sure that the inputs for depth, f, La, La.1m and La.1m.500 are correct.")} 
          }
          
          
          if(target.type == 'a'  ||  target.type == 'c'  &&  wavelength.div10 > 0.1) {message("The target is smaller than 1/10 of the wavelength corresponding to the centre frequency. Please consider using a higher frequency antenna. However, the target must be within the penetration depth range at the new frequency.")}

          if(gpr.environment.SI == 3) {message("Please consider carrying out the survey at a different time, during drier conditions.")}
          if(gpr.environment.SI == 4) {message("Please consider carrying out the survey at a different time, during drier conditions.")}

          if(GPR.system.SI == 4) {message("If possible, use a GPR system mechanically suited to grass and unpaved surfaces.")}
          if(GPR.system.SI == 3) {message("Please consider using a GPR system coupled to the ground surface.")}

          message(" ")
          if(warningflag == TRUE) {message("The available information is limited. The final GPR suitability might not be reliable.")}
          message("--------------------------------------------------------------------------------------------------------------------------------------")














# saves KBS output (i.e. display on the console) in a .txt file
      capture.output(
            save.txt.output <- function(...){

                  cat("\n", paste(date.of.modelling, "  KBS.", project.name, sep=""))
                  cat("\n")
                  cat("\n", "---------------------------------------------------------------------------------------------------------------------------------", "\n", "GEOPHYSICAL DATA")
                  cat("\n")
                  print(quote=F, geophysical, row.names=F)
                  cat("\n")
                  cat("\n", "---------------------------------------------------------------------------------------------------------------------------------", "\n", "SOIL DATA")
                  cat("\n")
                  print(quote=F, soil, row.names=F)
                  cat("\n")
                  cat("\n")
                  cat("\n")
                  cat("\n")
                  cat("\n")
                  cat("\n")
                  cat("\n", "---------------------------------------------------------------------------------------------------------------------------------", "\n", "EXPERT KNOWLEDGE")
                  cat("\n")
                  cat("\n", "SITE")
                  cat("\n")
                  print(quote=F, site.output, row.names=F)
                  cat("\n")
                  cat("\n", "ENVIRONMENTAL CONDITIONS")
                  cat("\n")
                  print(quote=F, enviro.output, row.names=F)
                  cat("\n")
                  cat("\n", "GPR SYSTEM")
                  cat("\n")
                  print(quote=F, gpr.system.output, row.names=F)
                  cat("\n")
                  cat("\n", "---------------------------------------------------------------------------------------------------------------------------------", "\n", "KBS SUITABILITY")
                  cat("\n")
                  print(quote=F, SI.definition)
                  cat("\n")
                  print(quote=F, kbs.class)
                  cat("\n")
                  cat("\n")
                  cat("\n", paste("GPR soil suitability:", round(gpr.soil.SI, digits=0), GPR.soil.suitability.class, sep = "  "))
                  cat("\n", paste("GPR site suitability:", round(gpr.site.SI, digits=0), GPR.site.suitability.class, sep = "  "))
                  cat("\n", paste("GPR environment suitability:", round(gpr.environment.SI, digits=0), GPR.environment.suitability.class, sep = "  "))
                  cat("\n", paste("GPR system suitability:", round(gpr.system.SI, digits=0), GPR.system.suitability.class, sep = "  "))
                  cat("\n")
                  cat("\n", paste("Final GPR suitability:", GPR.suitability.class, GPR.suitability.class.def, sep = "  "))
                  cat("\n")
                  cat("\n")

                  if(gpr.soil.SI  ==  0)  {cat("\n", "Warning: The KBS could not identify a GPR soil suitability."); warningflag <<- TRUE}
                  if(sufficient.soil.data == F  &&  gpr.soil.SI  !=  0) {cat("\n", "Warning: Important GPR.SI are missing in the soil section. The GPR soil suitability might not be reliable."); warningflag <<- TRUE}

                  if(gpr.site.SI  ==  0)  {cat("\n", "Warning: The KBS could not identify a GPR site suitability."); cat("\n"); warningflag <<- TRUE}
                  if(sufficient.site.data == F  &&  gpr.site.SI  !=  0) {cat("\n", "Warning: Important GPR.SI are missing in the site section. The GPR site suitability might not be reliable."); warningflag <<- TRUE}

                  if(gpr.environment.SI  ==  0)  {cat("\n", "Warning: The KBS could not identify a GPR environment suitability.")} #; warningflag <<- TRUE}
                                    
                  if(gpr.system.SI  ==  0)  {cat("\n", "Warning: The KBS could not identify a GPR system suitability.")} #; warningflag <<- TRUE}

                  cat("\n")
                  if(La.SI == 4  &&  is.na(La) == F  &&  is.na(f.in) == F) {cat("\n", "Attenuation loss seems high. If possible, try to reduce input frequency.")}
                  if(La.SI == 5  &&  is.na(La) == F  &&  is.na(f.in) == F) {cat("\n", "Attenuation loss seems high. If possible, try to reduce input frequency.")}

                  if(gpr.environment.SI == 3) {cat("\n", "Please consider carrying out the survey at a different time, during drier conditions.")}
                  if(gpr.environment.SI == 4) {cat("\n", "Please consider carrying out the survey at a different time, during drier conditions.")}

                  if(GPR.system.SI == 4) {cat("\n", "If possible, use a GPR system mechanically suited to grass and unpaved surfaces.")}
                  if(GPR.system.SI == 3) {cat("\n", "Please consider using a GPR system coupled to the ground surface.")}

                  cat("\n")
                  if(warningflag == TRUE) {cat("\n", "The available information is limited. The final GPR suitability might not be reliable.")}

                  cat("\n", "---------------------------------------------------------------------------------------------------------------------------------", "\n")
                  cat("\n")
                  cat("\n")
                  },
           save.txt.output(),
           file = paste("C:/R_KBS/KBS.output/KBS.output.", project.name, ".txt", sep="")
           )
          
           
           
                      
           
           if(is.na(error.message) == F) { 
                    message(error.message)
                    message(" ")
                    }
          
          
                              
          
           ###stop("objects are not removed")
             

## remove objects

      rm(list=as.character(geoph.objects.in), pos=fun.envir)
      rm(list=as.character(geophysical$symbol), pos=fun.envir)

      all.other.objects <<- objects()
      rm(list=all.other.objects)

#default settings for warnings (i.e., able warnings to be printed):
options(warn=0)

## make the results available to user in R global environment
      source("C:\\R_KBS\\loadkbs.R"); loadkbs()
      #soil <<- read.delim("C:/R_KBS/KBS.output/soil.soil.txt", header=T)
      #geophysical <<- read.delim("C:/R_KBS/KBS.output/geophysical.soil.txt", header=T)
      
      

           
           
           
}