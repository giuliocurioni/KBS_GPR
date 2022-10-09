Soil.kbs <- function(...) {
# Giulio Curioni
# June 2015
# this script may be freely copied and distributed 
# please reference the following projects if using the KBS:
# Mapping The Underworld (http://www.mappingtheunderworld.ac.uk/) 
# Assessing The Underworld (http://assessingtheunderworld.org/)


## ## ## ##
# this script must be run from KBS.R
# imports available soil data
# runs empirical equations by Saxton and Rawls, 2006. If PSD is available, this allows the calculation of:
          # bulk density
          # volumetric water content at field capacity (if no input is available this value is used)
# runs both Peplinski and Mironov models (or just Mironov. Peplinski seems to produce unreliable results)
# outputs soil suitability classes for GPR




#the local function environment is -1, but use fun.envir (i.e. 1) for the script to work correctly:
#note that sys.nframe() will produce pos=2, which does not work when using mapply

     fun.envir2 <<- fun.envir


#import data
#      soil <<- read.xls("KBS.input.xlsx", sheet="soil", verbose=F, method="tab", na.strings=c("NA","#DIV/0!"),
#                                perl="C:\\Perl64\\bin\\perl.exe", header=T)
                          #note: for read.xls to work the computer needs perl (download it for example from: http://download.cnet.com/ActivePerl-Windows/3000-2069_4-10006395.html )
                          #select perl for the right file depending on the system (i.e. 32-bit or 64-bit)
                          #select the path in perl = "../perl.exe"


      # convert everything to as.character
      soil[,1:4] <<- as.matrix(sapply(soil[,1:4], as.character), stringsAsFactors=FALSE)
      soil <<- data.frame(soil)

      # convert input to as.numeric
      soil$input <<- as.numeric(soil$input)

      
      #degree symbol is not imported correctly, replace it:
      l.temp <- which(soil$parameter == "soil temperature")
      soil$unit[l.temp] <<- c("°C")

      #'greater than' symbol is not imported correctly, replace it:
      l.greater.than <- which(soil$parameter == "percentage of gravel (&gt; 2mm)")
      soil$parameter[l.greater.than] <<- c("percentage of gravel (> 2mm)")
      
      
      # for interactive modification of the parameters within R:
      ###fix(soil)
                                                  
                                                           
                               
## assign soil properties to objects (note: objects are also saved in the global environment)
      geote.objects <- as.character(soil$symbol)
      soil.input <- mapply(assign, geote.objects, soil$input, pos=fun.envir2 )
                     
#assign initial values to final objects (for the moment) (note: objects are also saved in the global environment)
      geote.objects <- as.character(soil$symbol)
      soil.output <<- mapply(assign, geote.objects, soil$input, pos=fun.envir2 )

                                  
                                       

## define parameters:

# frequency. E.g. Freq <- seq(100e6, 1.0e9, by=20e6) is from 100MHz to 1GHz by 20MHz
      Freq <<- seq(100e6, 1.0e9, by=10e6)

      j <<- 0+1i                 #imaginary number sqrt(-1)

      
#Peplinski requires PSD/100, while Mironov requires PSD in %  (note: PSD is particle size distribution)
#important note: PSD is defined as weight % for the particles < 2 mm
#gravel %w is in addition, it is accounted for separately in Saxton and Rawls equations (sum sand+clay+silt+gravel is > 100 %)
      sand.dec <<- sand/100
      clay.dec <<- clay/100
      silt <<- abs(100-(sand+clay))
      silt.dec <<- silt/100
      gravel.dec <<- gravel/100   #note: gravel is the %w > 2 mm on the TOTAL weight of the soil
      theta <<- vwc/100           #note: theta is vwc in decimal units (m3/m3)
      gwc <<- gwc/100









## try to find missing parameters based on existing inputs:








## ## clay content, clay ## ##




      # clay from sand percentage (note: clay is required for the models to work, therefore an estimation should be done whenever possible)
      if(is.na(clay) == T) {
            clay <<- (100 - sand) / 2   # note: it is assumed that the fines (silt+clay) are equally distributed in clay and silt percentages
            clay.dec <<- clay/100
            }








## ## sand content, sand ## ##




      # sand from clay percentage (note: sand is required to estimate the field capacity, therefore an estimation should be done whenever possible)
      if(is.na(sand) == T) {
            sand <<- (100 - clay) / 2   # note: it is assumed that the fines (silt+sand) are equally distributed in sand and silt percentages
            sand.dec <<- sand/100
            }




             

## ## volumetric water content, theta ## ##




      # volumetric water content from gravimetric (gwc) and dry density (dryD)
      if(is.na(theta) == T  &&  is.na(gwc) == F  &&  is.na(dryD) == F) {
            theta <<- dryD * gwc
            }




      # volumetric water content from apparent permittivity (Topp et al., 1980 model)
      if(is.na(theta) == T  &&  is.na(Ea) == F) {
            theta <<- -5.30e-2 + 2.92e-2 * Ea - 5.5e-4 * Ea^2 + 4.3e-6 * Ea^3
            }



                     














## ## run equations by Saxton et al., 2006 ## ##

      # to calculate bulkD for Peplinski
      # to calculate the field capacity volumetric water content (which should be a typical value for the soil in the field). This varies depending on compaction (density factor DF is necessaru)

      source("C:\\R_KBS\\Saxton.Rawls2006.kbs.R"); Saxton.Rawls2006.kbs()





















## ## bulk density, bulkD ## ##

      # bulk density from soil parameters (see Saxton and Rawls, 2006)
      # [note: it does not account for gravel percentage]
      if(is.na(bulkD) == T) {
            bulkD <<- roDF
            }









          totper <- sand + silt + clay




          peplinski <- function(...) {

          #calculate peplinski model (ref: Peplinski et al., 1995 IEEE transactions on geoscience and remote sensing, vol. 33, n. 3, May 1995)
          #note: all water contents are defined in %v (volume), soil particles as %w (weight) ALL IN % / 100
          #if particle density is missing, it is assumed to be 2.65 (see above)


                #check that the total percentage is not different from 100
                      totper <- sand + silt + clay

                          if(totper != 100) {stop("\n the total percentage of sand+clay+silt must be 100%\n percentage of silt is calculated from the difference between percentage of sand and percentage of clay")}

                #check that percentage is not greater than 100 or smaller than 0
                          if (theta > 1 || theta < 0) {stop("\n the volumetric water percentage must not exceed 100% or be smaller than 0%")}



                alpha.pep <<- 0.65      ## see Peplinski et al., 1995


                #eq. 4 and 5 in Pepliski et al. 1995
                beta1 <- 1.2748 - 0.519*sand.dec - 0.152*clay.dec
                beta2 <- 1.33797 - 0.603*sand.dec - 0.166*clay.dec


                ##debye for free water:

                Einfw <- 4.9          # high-freq limit for free water (Peplinski et al., 1995)

                # if temperature is available correct free water static permittivity for temperature (Weast, 1986):
                if(is.na(temp) == F) { Ezerow <<- 78.54*(1-4.579e-3 *(temp-25) + 1.19e-5 * (temp-25)^2 - 2.8e-8 * (temp-25)^3) } else {
                      Ezerow <<- 83.38      # static permittivity for free water at 12 °C  [note: 12 °C is an average temperature value found in the soil]
                      }


                twopi <- 2 * pi     # two times greek pi
                frelw <- 17e9       # relaxation frequency for free water
                tauw <- 1/frelw     # relaxation time for free water

                # real part of permittivity for free water (Debye), see Peplinski et al 1995, equation 6
                Ewr <<- Einfw + ( (Ezerow - Einfw) / (1 + (twopi*Freq*tauw)^2) )

                # effective conductivity (eq. 8)
                effcond <- -1.645 + 1.939*bulkD - 2.25622*sand.dec + 1.594*clay.dec

                # imaginary part of permittivity for free water (Debye), equation 7
                Ewi <<- ( (twopi*Freq*tauw * (Ezerow - Einfw)) / (1 + (twopi*Freq*tauw)^2) )  +  ( (effcond * (partD - bulkD)) / (twopi*E0*Freq * partD*theta))



                # soil static permittivity (derived by Dobson et al., 1985, see also corrections to Peplinski et al. 1995)
                Estat <- (1.01 + 0.44 * partD)^2 - 0.062

                ## SOIL DIELECTRIC MIXING MODEL

                # real permittivity of the soil mixture (bulk) eq. 2 Pepliski et al 1995 (corrected. See corrections to paper)
                Er.pep <<- (1 + (bulkD/partD) * (Estat^alpha.pep - 1) + (theta^beta1) * (Ewr^alpha.pep) - theta)^(1/alpha.pep)

                # imaginary permittivity of the soil mixture (bulk) eq. 3 Pepliski et al 1995
                Ei.pep <<-  ( (theta^beta2) * (Ewi^alpha.pep) )^(1/alpha.pep)

                # define complex permittivity
                Em.pep <<- Er.pep + j*Ei.pep

                # calculate apparent permittivity from Er and Ei
                Ea.pep <<- (Er.pep/2) * ( 1 + sqrt(1 + (Ei.pep/Er.pep)^2 ) )

          }











          mironov <- function(...) {

          #calculate mironov model (ref: Mironov et al., 2009 IEEE transactions on geoscience and remote sensing, vol. 47, n. 7, July 2009)
          #note: water contents is defined in %v (volume), soil particles as %w (weight) (soil must be kept to %, don't divide by 100 because this is already done in each equation)
          # however, water seems that must be divided by 100 at the beginning (m3 m-3)


                # if only clay percentage is available check that it has a plausible value
                          if(clay > 100  ||  clay < 0) {stop("\n clay percentage must be between 0% and 100%")}

                #check that percentage is not greater than 100 or smaller than 0
                          if (theta > 1 || theta < 0) {stop("\n volumetric water percentage must not exceed 100% or be smaller than 0%")}



                Einf <- 4.9    #both for bound and free water

                #eq 17 to 25 in Mironov et al., 2009
                nd <- 1.634 - 0.539e-2 * clay + 0.2748e-4 * clay^2
                kd <- 0.03952 - 0.04038e-2 * clay
                Mvt <- 0.02863 + 0.30673e-2 * clay
                E0b <- 79.8 - 85.4e-2 * clay + 32.7e-4 * clay^2
                taub <- 1.062e-11 + 3.450e-12 * (10^-2) * clay
                sigmab <- 0.3112 + 0.467e-2 * clay
                sigmau <- 0.3631 + 1.217e-2 * clay
                E0u <- 100
                tauu <- 8.5e-12


                #eq. 16
                Eb1 <- Einf + ( (E0b - Einf) / (1 + (2*pi*Freq*taub)^2) )
                Eb2 <- ( (E0b - Einf) / (1 + (2*pi*Freq*taub)^2 ) ) * 2*pi*Freq*taub + (sigmab / (2*pi*E0*Freq) )
                Eu1 <- Einf + ( (E0u - Einf) / (1 + (2*pi*Freq*tauu)^2) )
                Eu2 <- ( (E0u - Einf) / (1 + (2*pi*Freq*tauu)^2 ) ) * 2*pi*Freq*tauu + (sigmau / (2*pi*E0*Freq) )


                #eq. 14
                nb <- sqrt( sqrt(Eb1^2 + Eb2^2) + Eb1 ) / sqrt(2)
                nu <- sqrt( sqrt(Eu1^2 + Eu2^2) + Eu1 ) / sqrt(2)


                #eq. 15
                kb <- sqrt( sqrt(Eb1^2 + Eb2^2) - Eb1 ) / sqrt(2)
                ku <- sqrt( sqrt(Eu1^2 + Eu2^2) - Eu1 ) / sqrt(2)


                #eq. 12 and 13
                if(theta < Mvt) {
                      nm <- nd + (nb - 1) * theta
                      km <- kd + kb*theta} else {
                      nm <- nd + (nb - 1) * Mvt + (nu - 1) * (theta - Mvt)
                      km <- kd + kb*Mvt + ku * (theta - Mvt)}


                #eq. 11
                Er.mir <<- nm^2 - km^2
                Ei.mir <<- 2*nm*km


                # define complex permittivity
                Em.mir <<- Er.mir + j*Ei.mir

                # calculate apparent permittivity from Er and Ei
                Ea.mir <<- (Er.mir/2) * ( 1 + sqrt(1 + (Ei.mir/Er.mir)^2 ) )

          }








      ## PLOTS

      #par(mar=c(5,5,3,3) + 0.1)
      #plot(Freq, Er.mir, ylim=c(0, 40), pch=1, font=2, xlab="Frequency", ylab="Real permittivity", font.lab=2, cex.lab=1.5, cex.axis=1.5)
      #par(new=T)
      #plot(Freq, Er.pep, ylim=c(0, 40), pch=20, font=2, xlab="Frequency", ylab="Real permittivity", font.lab=2, cex.lab=1.5, cex.axis=1.5)
      #      par(font=2)
      #      legend(x="topright",
      #            c("Mironov", "Peplinski"),
      #            pch=c(1,20),
      #            col=c("black", "black"),
      #            text.col=c("black", "black"),
      #            cex=c(1.5,1.5),
      #            bty=c("n","n"))
      #
      #windows()
      #
      #par(mar=c(5,5,3,3) + 0.1)
      #plot(Freq, Ei.mir, ylim=c(0, 40), pch=1, font=2, xlab="Frequency", ylab="Imaginary permittivity", font.lab=2, cex.lab=1.5, cex.axis=1.5)
      #par(new=T)
      #plot(Freq, Ei.pep, ylim=c(0, 40), pch=20, font=2, xlab="Frequency", ylab="Imaginary permittivity", font.lab=2, cex.lab=1.5, cex.axis=1.5)
      #      par(font=2)
      #      legend(x="topright",
      #            c("Mironov", "Peplinski"),
      #            pch=c(1,20),
      #            col=c("black", "black"),
      #            text.col=c("black", "black"),
      #            cex=c(1.5,1.5),
      #            bty=c("n","n"))

      
      
      
      
      
      
      
## run both models if all input parameters are available

# initially assign NA to output parameters
      Er.pep <<- NA
      Ei.pep <<- NA
      Ea.pep <<- NA
      Er.mir <<- NA
      Ei.mir <<- NA
      Ea.mir <<- NA



      # create object for model results

            SE.output <<- matrix(nrow=length(Freq), ncol=7, NA)
            SE.output <<- data.frame(SE.output)
            colnames(SE.output) <<- c("Freq", "Er.pep", "Ei.pep", "Ea.pep", "Er.mir", "Ei.mir", "Ea.mir")





      # run only Mironov model if only clay and theta are available
      if (is.na(clay) == F  &&  is.na(theta) == F) {
            try(mironov())

            if (is.na(totper) == F  &&  is.na(theta) == F  && is.na(bulkD) == F) {
                  try(peplinski())
                  }
            
            # save models output

            SE.output <<- cbind(Freq, Er.pep, Ei.pep, Ea.pep, Er.mir, Ei.mir, Ea.mir)         #SE.output: semi empirical output
            SE.output <<- data.frame(SE.output)
            colnames(SE.output) <<- c("Freq", "Er.pep", "Ei.pep", "Ea.pep", "Er.mir", "Ei.mir", "Ea.mir")
#            write.table(SE.output, "C:/R_KBS/KBS.output/semi.empirical.model.output.txt",  sep='\t', row.names=F, col.names=T)
            }


      ## creates final Peplinski output

            Er.pep <<- as.numeric(SE.output$Er.pep [which(SE.output$Freq == f)])
            Ei.pep <<- as.numeric(SE.output$Ei.pep [which(SE.output$Freq == f)])
            Ea.pep <<- as.numeric(SE.output$Ea.pep [which(SE.output$Freq == f)])

            Er.pep.100 <<- as.numeric(SE.output$Er.pep [which(SE.output$Freq == 1.0e+08)])
            Ei.pep.100 <<- as.numeric(SE.output$Ei.pep [which(SE.output$Freq == 1.0e+08)])
            Er.pep.500 <<- as.numeric(SE.output$Er.pep [which(SE.output$Freq == 5.0e+08)])
            Ei.pep.500 <<- as.numeric(SE.output$Ei.pep [which(SE.output$Freq == 5.0e+08)])
            Er.pep.1000 <<- as.numeric(SE.output$Er.pep [which(SE.output$Freq == 1.0e+09)])
            Ei.pep.1000 <<- as.numeric(SE.output$Ei.pep [which(SE.output$Freq == 1.0e+09)])

      ## creates final Mironov output

            Er.mir <<- as.numeric(SE.output$Er.mir [which(SE.output$Freq == f)])
            Ei.mir <<- as.numeric(SE.output$Ei.mir [which(SE.output$Freq == f)])
            Ea.mir <<- as.numeric(SE.output$Ea.mir [which(SE.output$Freq == f)])

            Er.mir.100 <<- as.numeric(SE.output$Er.mir [which(SE.output$Freq == 1.0e+08)])
            Ei.mir.100 <<- as.numeric(SE.output$Ei.mir [which(SE.output$Freq == 1.0e+08)])
            Er.mir.500 <<- as.numeric(SE.output$Er.mir [which(SE.output$Freq == 5.0e+08)])
            Ei.mir.500 <<- as.numeric(SE.output$Ei.mir [which(SE.output$Freq == 5.0e+08)])
            Er.mir.1000 <<- as.numeric(SE.output$Er.mir [which(SE.output$Freq == 1.0e+09)])
            Ei.mir.1000 <<- as.numeric(SE.output$Ei.mir [which(SE.output$Freq == 1.0e+09)])
      




## ## note: Peplinski gives exagerated values of attenuation loss. Use only Mironov!

## calculate average between two models (if both are available) as final output 
## otherwise assign results from Mironov only (if also Mironov produces NAs, NAs are kept)
# note: if peplinski elements are missing they will not be NAs but they will not exist (length = zero)

#if(length(Er.pep) != 0  &&  length(Ei.pep) != 0  &&  length(Ea.pep) != 0) {
#
#      Er.model <<- (Er.pep + Er.mir)/2
#            Er.model <<- round(Er.model, digits=2)
#      Ei.model <<- (Ei.pep + Ei.mir)/2
#            Ei.model <<- round(Ei.model, digits=2)
#      Ea.model <<- (Ea.pep + Ea.mir)/2
#            Ea.model <<- round(Ea.model, digits=2)
#      
#      ### note: Peplinski was built on the frequency range 300 MHz - 1.3 GHz, it does not give reliable results in the low frequency range (e.g. 100 MHz)     
##      Er.model.100 <<- (Er.pep.100 + Er.mir.100)/2
##            Er.model.100 <<- round(Er.model.100, digits=2)
##      Ei.model.100 <<- (Ei.pep.100 + Ei.mir.100)/2
##            Ei.model.100 <<- round(Ei.model.100, digits=2)
#       Er.model.100 <<- Er.mir.100
#             Er.model.100 <<- round(Er.model.100, digits=2)
#       Ei.model.100 <<- Ei.mir.100  
##             Ei.model.100 <<- round(Ei.model.100, digits=2) 
#
##      Er.model.500 <<- (Er.pep.500 + Er.mir.500)/2
##            Er.model.500 <<- round(Er.model.500, digits=2)
##      Ei.model.500 <<- (Ei.pep.500 + Ei.mir.500)/2
##            Ei.model.500 <<- round(Ei.model.500, digits=2)
#       Er.model.500 <<- Er.mir.500
#             Er.model.500 <<- round(Er.model.500, digits=2)
#       Ei.model.500 <<- Ei.mir.500
#             Ei.model.500 <<- round(Ei.model.500, digits=2)
#      
#      Er.model.1000 <<- (Er.pep.1000 + Er.mir.1000)/2
#            Er.model.1000 <<- round(Er.model.1000, digits=2)
#      Ei.model.1000 <<- (Ei.pep.1000 + Ei.mir.1000)/2
#            Ei.model.1000 <<- round(Ei.model.1000, digits=2)
#      }  else  {
            Er.model <<- Er.mir
                  Er.model <<- round(Er.model, digits=2)
            Ei.model <<- Ei.mir
                  Ei.model <<- round(Ei.model, digits=2)
            Ea.model <<- Ea.mir
                  Ea.model <<- round(Ea.model, digits=2)

            Er.model.100 <<- Er.mir.100
                  Er.model.100 <<- round(Er.model.100, digits=2)
            Ei.model.100 <<- Ei.mir.100
                  Ei.model.100 <<- round(Ei.model.100, digits=2)
            Er.model.500 <<- Er.mir.500
                  Er.model.500 <<- round(Er.model.500, digits=2)
            Ei.model.500 <<- Ei.mir.500
                  Ei.model.500 <<- round(Ei.model.500, digits=2)
            Er.model.1000 <<- Er.mir.1000
                  Er.model.1000 <<- round(Er.model.1000, digits=2)
            Ei.model.1000 <<- Ei.mir.1000
                  Ei.model.1000 <<- round(Ei.model.1000, digits=2)
#            }



if(length(Er.model) == 0) { Er.model <<- NA }
if(length(Ei.model) == 0) { Ei.model <<- NA }
if(length(Ea.model) == 0) { Ea.model <<- NA }
if(length(Er.model.100) == 0) { Er.model.100 <<- NA }
if(length(Ei.model.100) == 0) { Ei.model.100 <<- NA }
if(length(Er.model.500) == 0) { Er.model.500 <<- NA }
if(length(Ei.model.500) == 0) { Ei.model.500 <<- NA }
if(length(Er.model.1000) == 0) { Er.model.1000 <<- NA }
if(length(Ei.model.1000) == 0) { Ei.model.1000 <<- NA }






                              
                        











## ## ## ##

## soil OUTPUT



## create objects corresponding to the final soil properties (reassign final values) and round values

      soil.output <<- c(sand, clay, gravel, theta*100, gwc*100, bulkD, dryD, partD, DF, LL, PL, Ip, Ls, temp, OM, SAR, CaCO3, water.table)

      round.output <- sapply(soil.output, FUN=round, digits=2)

      soil.output <<- data.frame(round.output)




## create final object with soil data

      soil <<- cbind(soil, soil.output)
      colnames(soil) <<- c("parameter", "unit", "symbol", "input", "output")
#      write.table(soil, "C:/R_KBS/KBS.output/soil.soil.txt", sep='\t', row.names=F, col.names=T)









## ## ## ## define suitability classes



## ##  1

## use liquid limit, plasticity index, linear shrinkage to estimate EM dispersion
# based on findings by Thomas et al., 2010a, 2010b the magnitude of dispersion is proportional to:
                          # the liquid limit
                          # the linear shrinkage
# in addition, the plasticity index is an indication of shrink-swell clays, which are considered not well suited to GPR because they could retain large amounts of water

                  # note: only 3 classes have been identified
                ### note: change limits if these are thought to be incorrect


                # liquid limit classes (based on typical liquid limit values for different soils):
                          # LL.class1: LL <= 30 %
                          # LL.class2: 30 < LL <= 60 %
                          # LL.class3: LL > 60 %

                # linear shrinkage classes (Fig.9 in Thomas et al., 2010a):
                          # LL.class1: Ls <= 8 %
                          # LL.class2: 8 < Ls <= 15 %
                          # LL.class3: Ls > 15 %

                # plasticity index classes (Table 2 in Thomas et al., 2010a):
                          # Ip.class1: Ip <= 20 %
                          # Ip.class2: 20 < Ip <= 40 %
                          # Ip.class3: Ip > 40 %


# note: this output might be in contrast with the geophysical output. 
# The KBS will combine the sections to give a final output.


# calculate LL if only PL and Ip are available:
      if(is.na(LL == T)  &&  is.na(Ip) == F  &&  is.na(PL) == F){
                LL <<- Ip + PL
                }
                
# calculate Ip if only LL and PL are available:
      if(is.na(Ip == T)  &&  is.na(LL) == F  &&  is.na(PL) == F){
                Ip <<- LL - PL
                }
                

## find suitability indices (SI)                

      # from liquid limit
      if(is.na(LL) == F) {
            if(LL <= 30)                {LL.SI <<- 1}        # very little dispersive
            if(LL > 30  &&  LL <= 60)   {LL.SI <<- 3}        # slightly dispersive
            if(LL > 60)                 {LL.SI <<- 5}        # dispersive
            } else {LL.SI <<- 0}


      # from linear shrinkage
      if(is.na(Ls) == F) {
            if(Ls <= 8)                 {Ls.SI <<- 1}        # very little dispersive
            if(Ls > 8  &&  Ls <= 15)    {Ls.SI <<- 3}        # slightly dispersive
            if(Ls > 15)                 {Ls.SI <<- 5}        # dispersive
            } else {Ls.SI <<- 0}


      # from plasticity index
      if(is.na(Ip) == F) {
            if(Ip <= 20)                {Ip.SI <<- 1}        # very little dispersive
            if(Ip > 20  &&  Ls <= 40)   {Ip.SI <<- 3}        # slightly dispersive
            if(Ip > 40)                 {Ip.SI <<- 5}        # dispersive
            } else {Ip.SI <<- 0}



      ## create final suitability index for EM dispersion based on liquid limit and linear shrinkage
      
      # start with assigning 0
      Mdisp.geotechnical.SI <<- 0
      
      # then run these commands in sequence. If they are all missing Mdisp.geotechnical.SI == 0

      # if only one is SI 1, choose SI 1 (note: better not to use the 'OR' statement):
      if(LL.SI == 1)    {Mdisp.geotechnical.SI <<- 1}
      if(Ls.SI == 1)    {Mdisp.geotechnical.SI <<- 1}
      if(Ip.SI == 1)    {Mdisp.geotechnical.SI <<- 1}
      
      # if only one is SI 3, choose SI 3 (note: better not to use the 'OR' statement):
      if(LL.SI == 3)                    {Mdisp.geotechnical.SI <<- 3}
      if(Ls.SI == 3)                    {Mdisp.geotechnical.SI <<- 3}
      if(Ip.SI == 3)                    {Mdisp.geotechnical.SI <<- 3}

      # if only one is SI 5, choose SI 5 (note: better not to use the 'OR' statement):
      if(LL.SI == 5)                    {Mdisp.geotechnical.SI <<- 5}
      if(Ls.SI == 5)                    {Mdisp.geotechnical.SI <<- 5}
      if(Ip.SI == 5)                    {Mdisp.geotechnical.SI <<- 5}
      














## ## 2

## use clay percentage by weight (%w) to estimate the suitability index:
# see Doolittle et al., 2007

                ### note: change limits if these are thought to be incorrect

                # clay classes (%w), Doolittle et al., 2007:
                          # clay.class1: clay <= 10 %w
                          # clay.class2: 10 < clay <= 18 %w
                          # clay.class3: 18 < clay <= 35 %w
                          # clay.class4: 35 < clay <= 60 %w
                          # clay.class5: clay > 60 %w

      if(is.na(clay) == F) {
            if(clay <= 10)                   {clay.SI <<- 1}        # very high suitability
            if(clay > 10  &&  clay <= 18)    {clay.SI <<- 2}        # high suitability
            if(clay > 18  &&  clay <= 35)    {clay.SI <<- 3}        # moderate suitability
            if(clay > 35  &&  clay <= 60)    {clay.SI <<- 4}        # low suitability
            if(clay > 60)                    {clay.SI <<- 5}        # very low suitability
            } else {clay.SI <<- 0}















## ## 3

## use depth of water table (m) to estimate the suitability index:
# these classes do not consider measurements of fluctuations of the water table. If the water table is deeper than the target but within 1 m from it, the suitability is considered moderate


                ### note: change limits if these are thought to be incorrect

                # water.table classes:
                          # water.table.class1: water.table < depth (i.e. water table is shallower than target)            # low suitability
                          # water.table.class2: depth < water.table < depth+1m (i.e. water table is deeper than target but within 1 m from it)  # moderate suitability

      if(is.na(water.table) == F) {
            if(water.table <= depth)                                            {water.table.SI <<- 4}               # low suitability
            if(water.table > depth  &&  water.table <= (depth+1))               {water.table.SI <<- 3}               # moderate suitability
            if(water.table > (depth+1))                                         {water.table.SI <<- 1}               # very high suitability
            } else {water.table.SI <<- 0}















## ## 4

## use sodium absorption ratio (SAR) to estimate the suitability index:
# Doolittle et al., 2007  (check in the literature to identify further classes)

                ### note: change limits if these are thought to be incorrect

                # SAR classes (Doolittle et al., 2007):
                          # SAR.class1: SAR < 13     # very high suitability
                          # SAR.class2: SAR >= 13    # very low suitability

      if(is.na(SAR) == F) {
            if(SAR < 13)  {SAR.SI <<- 1}             # very high suitability  #NOTE: more tests should provide more info on lower values 
            if(SAR >= 13)   {SAR.SI <<- 5}             # very low suitability
            } else {SAR.SI <<- 0}















## ## 5

## use calcium carbonate to estimate the suitability index:
# http://soils.usda.gov/survey/geography/maps/GPR/methodology.html  (check in the literature to identify further classes)
# World Reference Base for soil resources (WRB) identifies a calcic horizon if it contains 15 % of calcium carbonate.
# but Doolittle et al. (2007, 2010) use CaCO3 >= 10.

                ### note: change limits if these are thought to be incorrect

                # CaCO3 classes (Doolittle et al., 2007 and 2010):
                          # CaCO3.class1: CaCO3 < 10      # very high suitability
                          # CaCO3.class1: CaCO3 >= 10     # low suitability

      
      if(is.na(CaCO3) == F) {
            if(CaCO3 < 10)    {CaCO3.SI <<- 1}             # very high suitability  #NOTE: more tests should provide more info on lower values 
            if(CaCO3 >= 10)   {CaCO3.SI <<- 4}             # low suitability
            } else {CaCO3.SI <<- 0}














# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## SOIL SUITABILITY CLASS

# define GPR suitability class based on soil classes (i.e. disp.class, clay.class, water.table, SAR, CaCO3)

          soil.vec <<- c(Mdisp.geotechnical.SI, clay.SI, water.table.SI, SAR.SI, CaCO3.SI)





}