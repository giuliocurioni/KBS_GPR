Geophysical.kbs <- function(...) {
# Giulio Curioni
# June 2015
# this script may be freely copied and distributed 
# please reference the following projects if using the KBS:
# Mapping The Underworld (http://www.mappingtheunderworld.ac.uk/) 
# Assessing The Underworld (http://assessingtheunderworld.org/)


## ## ## ##
# this script must be run from KBS.R
# geophysical section


# search for ### if there are problems with the script


#the local function environment is -1, but use fun.envir (i.e. 1) for the script to work correctly:
#note that sys.nframe() will produce pos=2, which does not work when using mapply

     fun.envir2 <<- fun.envir


#import geophysical data
#      geophysical <<- read.xls("KBS.input.xlsx", sheet="geophysical", verbose=F, method="tab", na.strings=c("NA","#DIV/0!"),
#                                perl="C:\\Perl64\\bin\\perl.exe", header=T)
                          #for read.xls to work the computer needs Perl (see inst.kbs.packages.R)
                          #select the path in perl = "../perl.exe"


      # convert everything to as.character
      geophysical[,1:4] <<- as.matrix(sapply(geophysical[,1:4], as.character), stringsAsFactors=FALSE)
      geophysical <<- data.frame(geophysical)

      # convert input to as.numeric
      geophysical$input <<- as.numeric(geophysical$input)

      # for interactive modification of the parameters within R:
      ###fix(geophysical)



## create objects corresponding to INITIAL geophysical properties (note: objects are also saved in the global environment)
      geoph.objects.in <<- paste(as.character(geophysical$symbol), ".in", sep="")
      geophysical.output.in <- mapply(assign, geoph.objects.in, geophysical$input, pos=fun.envir)
      geophysical.output.in <- data.frame(geophysical.output.in)

      #assign initial values to final objects (for the moment) (note: objects are also saved in the global environment)
      geoph.objects <- as.character(geophysical$symbol)
      geophysical.output <<- mapply(assign, geoph.objects, geophysical$input, pos=fun.envir )

                       

      #if input depth is missing, the calculations are done anyway on depth: 1 m
            if(is.na(depth) == T) {depth <<- 1}
      f <<- f * 10^6                #convert frequency from MHz to Hz
      #if input frequency is missing, the calculations are done anyway on f = 500 MHz (typical frequency used for utility detection)
            if(is.na(f) == T) {f <<- 500e6}
      BEC.in <<- BEC.in / 1000      #convert bulk electrical conductivity from mS/m to S/m
      BEC <<- BEC / 1000            #convert bulk electrical conductivity from mS/m to S/m

      conversion.nepers.to.dB <- 8.68588
      alpha <<- alpha / conversion.nepers.to.dB   #convert attenuation coeff from dB to Np



## define some useful parameters:

      c0 <<- 299792458              #speed of light in a vacuum (m/s)
      mu0 <<- 1.2564e-6             #absolute permeability of free space (H/m)
      #magnetic permeability, mu (if the magnetic permeability is not known, it is assumed to be 1, unless Ea, Er and Ei are available (see below))
      #note: mu is the real magnetic permeability, its imaginary part is generally neglected in soils
            if(is.na(mu.in) == T) {mu <<- 1.00}
      mu.abs <<- mu0*mu             #absolute permeability of the material (H/m)
      E0 <<- 8.854e-12              #absolute permittivity of free space (F/m)
      E.abs <<- E0*Er.in            #abolute permittivity of the material (F/m)
      omega <<- 2*pi*f              #angular frequency
      loss.tan <<- Ei.in/Er.in      #loss tangent








## try to find missing parameters based on existing inputs:








## ## apparent permittivity, Ea ## ##




      #apparent permittivity (Ea) from real (Er) and imaginary (Ei) permittivities (e.g. Robinson et al., 2003a)

      if(is.na(Ea.in) == T  &&  is.na(Er.in) == F  &&  is.na(Ei.in) == F){
          Ea <<- ((Er*mu) / 2) * (sqrt(1 + (loss.tan)^2) + 1)
          Ea <<- round(Ea, digits=2)
          }






## ## imaginary permittivity, Ea ## ##




      #imaginary permittivity from Ea and Er

      if(is.na(Ei.in) == T  &&  is.na(Er.in) == F  &&  is.na(Ea.in) == F  &&  is.na(BEC) == T){
          Ei <<- Er * sqrt( (((2*Ea.in)/(mu*Er)) - 1)^2  - 1 )
          Er <<- round(Er, digits=2)
          }








## ## apparent permittivity at 100 MHz and 1000 MHz, Ea.100 and Ea.1000 ## ##




      # apparent permittivity at 100 MHz and 1000 MHz from real and imaginary permittivity at 100 MHz and 1000 MHz, if available

      if(is.na(Ea.100) == T  &&  is.na(Er.100.in) == F  &&  is.na(Ei.100.in) == F){
          Ea.100 <<- ((Er.100.in * mu) / 2) * (sqrt(1 + (Ei.100.in/Er.100.in)^2) + 1)
          Ea.100 <<- round(Ea.100, digits=2)
          }

      #apparent permittivity at 1000 MHz (Ea.1000) from real (Er.1000) and imaginary (Ei.1000) permittivities at 1000 MHz, if available (e.g. Robinson et al., 2003a)

      if(is.na(Ea.1000) == T  &&  is.na(Er.1000.in) == F  &&  is.na(Ei.1000.in) == F){
          Ea.1000 <<- ((Er.1000.in * mu) / 2) * (sqrt(1 + (Ei.1000.in/Er.1000.in)^2) + 1)
          Ea.1000 <<- round(Ea.1000, digits=2)
          }








## ## magnitude of dispersion, Mdisp ## ##




      #magnitude of dispersion from permittivity at 100 MHz and 1000 MHz


      if(is.na(Mdisp.in) == T  &&  is.na(Er.100.in) == F  &&  is.na(Ei.100.in) == F  &&  is.na(Er.1000.in) == F  &&  is.na(Ei.1000.in) == F) {
          Mdisp <<- abs(Ea.1000.in - Ea.100.in)
          Mdisp <<- round(Mdisp, digits=2)
          }








## ## magnetic permeability, mu (note: mu is the real magnetic permeability, its imaginary part is generally neglected in soils) ## ##




      #magnetic permeability from Ea, Er and Ei
      #note: generally mu is approximated to 1, but if all these data are available then try to calculate mu (e.g. Robinson et al., 2003a)

      if(is.na(mu.in) == T  &&  is.na(Ea.in) == F  &&  is.na(Er.in) == F  &&  is.na(Ei.in) == F) {
          mu <<- (2*Ea.in) / (Er.in * (sqrt(1+loss.tan^2) + 1))
          mu <<- round(mu, digits=2)
          }






## reassign initial values to final variables in case these are still missing (if the initial variables weren't known a NA value is kept)

      if(is.na(Ea) == T) {Ea <<- Ea.in}
      if(is.na(Er) == T) {Er <<- Er.in}
      if(is.na(Ei) == T) {Ei <<- Ei.in}
      if(is.na(Ea.100) == T) {Ea.100 <<- Ea.100.in}
      if(is.na(Ea.1000) == T) {Ea.1000 <<- Ea.1000.in}
      if(is.na(Mdisp) == T) {Mdisp <<- Mdisp.in}
      if(is.na(BEC) == T) {BEC <<- BEC.in}


















## ## modelling ## ##

# run semi-empirical models (e.g. Mironov and Peplinski models): try to find geophysical parameters from soil parameters

      source("C:\\R_KBS\\Soil.kbs.R"); Soil.kbs()











# assign results from semi-empirical modelling (these values are the average of Peplinski and Mironov models)
# only if previous calculations were not successful (i.e. values are still NAs)

      if(is.na(Ea) == T) {Ea <<- Ea.model}
      if(is.na(Er) == T) {Er <<- Er.model}
      if(is.na(Ei) == T) {Ei <<- Ei.model}
      if(is.na(Er.100) == T) {Er.100 <<- Er.model.100}
      if(is.na(Ei.100) == T) {Ei.100 <<- Ei.model.100}
      Er.500 <<- Er.model.500    # note: this is not within the list of inputs 
      Ei.500 <<- Ei.model.500    # note: this is not within the list of inputs
      if(is.na(Er.1000) == T) {Er.1000 <<- Er.model.1000}
      if(is.na(Ei.1000) == T) {Ei.1000 <<- Ei.model.1000}

















## ## rerun a second script with the new calculated parameters ## ##
# in some cases additional parameters can be found from new calculated or modelled parameters

      source("C:\\R_KBS\\Soil.Input2.R"); Soil.Input2()

















## ## calculate other relevant parameters ## ##




      #re-calculate mu.abs, E.abs, loss.tan based on the results above
      mu.abs <<- mu0*mu
      E.abs <<- E0*Er
      loss.tan <<- Ei/Er



      #account for two-way travel of EM signals
      twoway.depth <- 2*depth
      #account for two-way travel of EM signals (1m depth)
      twoway.depth.1m <- 2*1



## ## attenuation coefficient, alpha (Np/m) ## ##




      # attenuation coefficient from Er and Ei

      if(is.na(alpha.in == T)  &&  is.na(Er) == F  &&  is.na(Ei) == F){
          alpha <<- omega * sqrt(((mu.abs*E.abs)/2) * (sqrt(1+loss.tan^2) - 1))
          alpha <<- round(alpha, digits=4)
          }








## ## attenuation coefficient, alpha (Np/m) for 500MHz antennas ## ##

      #re-calculate E.abs, loss.tan at 100MHz and 1000MHz based on the above results
      E.abs.1m.100 <<- E0*Er.100
      loss.tan.1m.100 <<- Ei.100/Er.100
      E.abs.1m.500 <<- E0*Er.500
      loss.tan.1m.500 <<- Ei.500/Er.500
      E.abs.1m.1000 <<- E0*Er.1000
      loss.tan.1m.1000 <<- Ei.1000/Er.1000




      # attenuation coefficient from Er.100 and Ei.100

#      if(is.na(Er.100) == F  &&  is.na(Ei.100) == F){
#          alpha.1m.100 <<- 2*pi*100e6 * sqrt(((mu.abs*E.abs.1m.100)/2) * (sqrt(1+loss.tan.1m.100^2) - 1))
#          alpha.1m.100 <<- round(alpha.1m.100, digits=4)
#          } else {alpha.1m.100 <<- NA}

      if(is.na(Er.500) == F  &&  is.na(Ei.500) == F){
          alpha.1m.500 <<- 2*pi*500e6 * sqrt(((mu.abs*E.abs.1m.500)/2) * (sqrt(1+loss.tan.1m.500^2) - 1))
          alpha.1m.500 <<- round(alpha.1m.500, digits=4)
          } else {alpha.1m.500 <<- NA}


      # attenuation coefficient from Er.1000 and Ei.1000

#      if(is.na(Er.1000) == F  &&  is.na(Ei.1000) == F){
#          alpha.1m.1000 <<- 2*pi*1000e6 * sqrt(((mu.abs*E.abs.1m.1000)/2) * (sqrt(1+loss.tan.1m.1000^2) - 1))
#          alpha.1m.1000 <<- round(alpha.1m.1000, digits=4)
#          } else {alpha.1m.1000 <<- NA}



## ## skin depth (m) (i.e. when the signal amplitude has decreased to 1/e, that is approx 37% of initial amplitude) ## ##




      # skin depth from attenuation coefficient

      if(is.na(skin.depth.in == T)  &&  is.na(alpha) == F){
          skin.depth <<- 1/alpha
          skin.depth <<- round(skin.depth, digits=2)
          }




## ## attenuation loss (dB) ## ##




      # attenuation loss from alpha

      if(is.na(La.in == T)  &&  is.na(alpha) == F){
          La <<- conversion.nepers.to.dB * twoway.depth * alpha
          La <<- round(La, digits=2)
          }



      # attenuation loss at 1m depth, for frequency = f

      if(is.na(La) == F) {
                La.1m <<- (La/depth)*1    # find La corresponding to 1 m depth 
                }



      # attenuation loss at 1m depth, for 500MHz antennas from alpha.1m ONLY in case it is not known from the input file

      if(is.na(La.1m.500 == T)  &&  is.na(alpha.1m.500) == F) {
                La.1m.500 <<- conversion.nepers.to.dB * twoway.depth.1m * alpha.1m.500
                La.1m.500 <<- round(La.1m.500, digits=2)
                }

          
          
#      if(is.na(alpha.1m.100) == F) {
#          La.1m.100 <<- conversion.nepers.to.dB * twoway.depth.1m * alpha.1m.100
#          La.1m.100 <<- round(La.1m.100, digits=2)
#          }


#      if(is.na(alpha.1m.1000) == F) {
#          La.1m.1000 <<- conversion.nepers.to.dB * twoway.depth.1m * alpha.1m.1000
#          La.1m.1000 <<- round(La.1m.1000, digits=2)
#          }




## ## velocity, v (m/s) ## ##




      #v from Er, Ei, mu

      if(is.na(v.in) == T  &&  is.na(Er) == F  &&  is.na(Ei) == F) {
          v <<- c0 / sqrt( ((Er*mu)/2) * (1 + sqrt(1 + loss.tan^2)) )
          }


      #v from Ea

      if(is.na(v.in) == T  &&  is.na(Er) == T  ||  is.na(Ei) == T) {
          v <<- c0 / sqrt(Ea)
          }


      #v from user input information (soil.type, recent.weather, saturation)

      # soil is coarse and dry
      if(is.na(v) == T  &&  soil.type == 'a'  ||  soil.type == 'b'  &&  recent.weather  == 'a'  &&  saturation == 'a') {
          Ea <<- 4  # typical value for dry sand (Daniels, 2004) [note: dry sand can be drier than dry sandy soil]
          v <<- c0 / sqrt(Ea)
          }
      if(is.na(v) == T  &&  soil.type == 'a'  ||  soil.type == 'b'  &&  recent.weather  == 'na'  &&  saturation == 'a') {
          Ea <<- 7  # typical value for dry sandy soil (Daniels, 2004) 
          v <<- c0 / sqrt(Ea)
          }
      # soil is coarse and wet
      if(is.na(v) == T  &&  soil.type == 'a'  ||  soil.type == 'b'  &&  saturation == 'b') {
          Ea <<- 20  # typical value for wet sand and wet sandy soil (Daniels, 2004)
          v <<- c0 / sqrt(Ea)
          }
      # soil is clayey and dry
      if(is.na(v) == T  &&  soil.type == 'c'  ||  soil.type == 'e'  &&  recent.weather  == 'a'  &&  saturation == 'a') {
          Ea <<- 7  # typical value for dry clayey soil (Daniels, 2004)
          v <<- c0 / sqrt(Ea)
          }
      # soil is clayey and wet
      if(is.na(v) == T  &&  soil.type == 'c'  ||  soil.type == 'e'  &&  saturation == 'b') {
          Ea <<- 25  # typical value for dry clayey soil (Daniels, 2004)
          v <<- c0 / sqrt(Ea)
          }
      # soil is clayey (high activity) and dry
      if(is.na(v) == T  &&  soil.type == 'd'  &&  saturation == 'a') {
          Ea <<- 10  # typical value for dry clayey soil (Daniels, 2004) [note: a slightly higher value is used for high activity clays since they typically do not dry completely]
          v <<- c0 / sqrt(Ea)
          }
      # soil is clayey (high activity) and wet
      if(is.na(v) == T  &&  soil.type == 'd'  &&  saturation == 'b') {
          Ea <<- 30  # typical value for wet clayey soil (Daniels, 2004) [note: a slightly higher value is used for high activity clays]
          v <<- c0 / sqrt(Ea)
          }






## ## wavelength corresponding to centre frequency, wavelength (m) ## ##


      if(is.na(v) == F) {wavelength <<- v/f} else {wavelength <<- (c0/sqrt(4))/f}         
      # note: if f does not exist, 500 MHz is used (see above); if v does not exist, assume a permittivity of 4 (dry soil) for estimating v and therefore wavelength
      # note: this is only used to produce a warning when target dimension is smaller than 1/10 of a wavelength; a value of 4 for permittivity is conservative, higher values will allow the detection of smaller objects.
                                                                                
      wavelength.div10 <<- wavelength/10





## ## ## ##

## GEOPHYSICAL OUTPUT:


## create objects corresponding to the final geophysical properties (reassign final values) and round values

      f <<- f / 10^6                #convert frequency from Hz to MHz for more clarity
      BEC <<- BEC * 1000            #convert BEC from S/m to mS/m for more clarity
      alpha <<- alpha * conversion.nepers.to.dB

      geophysical.output <<- c(depth, f, v, Ea, Er, Ei, Er.100, Ei.100, Er.1000, Ei.1000, Ea.100, Ea.1000,
                Mdisp, BEC, mu, alpha, skin.depth, La.1m.500, La.1m, La)

      round.output <- sapply(geophysical.output, FUN=round, digits=2)

      geophysical.output <<- data.frame(round.output)

                


## create final object with geophysical data

       geophysical <<- cbind(geophysical, geophysical.output)
       colnames(geophysical) <<- c("parameter", "unit", "symbol", "input", "output")
#      write.table(geophysical, "C:/R_KBS/KBS.output/geophysical.soil.txt", sep='\t', row.names=F, col.names=T)




       



## ## ## ## define suitability classes



## ##  1

## use magnitude of dispersion to estimate the suitability index (SI):
# see Thomas et al., 2010a

                ### note: change limits if these are thought to be incorrect

                # Mdisp classes (Thomas et al., 2010a):
                          # Mdisp.geophysical.class1: Mdisp < 6
                          # Mdisp.geophysical.class2: 6 < Mdisp <= 12
                          # Mdisp.geophysical.class3: 12 < Mdisp <= 18
                          # Mdisp.geophysical.class4: 18 < Mdisp <= 24
                          # Mdisp.geophysical.class5: Mdisp > 24

      if(is.na(Mdisp) == F) {
            if(Mdisp <= 6)                     {Mdisp.geophysical.SI <<- 1}        # very little dispersive
            if(Mdisp > 6  &&  Mdisp <= 12)     {Mdisp.geophysical.SI <<- 2}        # little dispersive
            if(Mdisp > 12  &&  Mdisp <= 18)    {Mdisp.geophysical.SI <<- 3}        # moderately dispersive
            if(Mdisp > 18  &&  Mdisp <= 24)    {Mdisp.geophysical.SI <<- 4}        # highly dispersive
            if(Mdisp > 24)                     {Mdisp.geophysical.SI <<- 5}        # very highly dispersive
            } else {Mdisp.geophysical.SI <<- 0}





## ## 2

## use bulk electrical conductivity to estimate the suitability index:
# see Davis and Annan, 1989 and Doolittle et al., 2010

                ### note: change limits if these are thought to be incorrect

                # BEC classes (Davis and Annan, 1989 and Doolittle et al., 2010):
                # note: BEC is at this point in mS/m (see above)
                          # BEC.class1: BEC <= 50 mS/m
                          # BEC.class2: 50 < BEC <= 100   mS/m
                          # BEC.class3: 100 < BEC <= 150  mS/m
                          # BEC.class4: 150 < BEC <= 200  mS/m
                          # BEC.class5: BEC > 200 mS/m

      if(is.na(BEC) == F) {
            if(BEC <= 50)                    {BEC.SI <<- 1}        # very high suitability
            if(BEC > 50  &&  BEC <= 150)     {BEC.SI <<- 2}        # high suitability
            if(BEC > 150  &&  BEC <= 150)    {BEC.SI <<- 3}        # moderate suitability
            if(BEC > 150  &&  BEC <= 200)    {BEC.SI <<- 4}        # low suitability
            if(BEC > 200)                    {BEC.SI <<- 5}        # very low suitability
            } else {BEC.SI <<- 0}





## ## 3

## use attenuation loss (dB) to estimate the suitability index:
# see Daniels, 2004

                ### note: change limits if these are thought to be incorrect

                # attenuation classes:
                # IMPORTANT NOTE: these classes are identified based on the attenuation loss at the input frequency and depth: 1 m (no matter the input depth)
                # if input frequency is not available, attenuation loss for a 500 MH antenna is used 

                          # La.class1: La <= 40
                          # La.class2: 40 < La <= 60
                          # La.class3: 60 < La <= 80
                          # La.class4: 80 < La <= 100
                          # La.class5: La > 100


      if(is.na(La) == F) {
      # attenuation loss at depth = input depth

                if(La <= 40)                             {La.SI <<- 1}        # very high suitability
                if(La > 40  &&  La <= 60)                {La.SI <<- 2}        # high suitability
                if(La > 60  &&  La <= 80)                {La.SI <<- 3}        # moderate suitability
                if(La > 80  &&  La <= 100)               {La.SI <<- 4}        # low suitability
                if(La > 100)                             {La.SI <<- 5}        # very low suitability
                }     


      if(is.na(La) == T  &&  is.na(La.1m) == F) {
                La.1m <<- (La/depth)*1    # find La corresponding to 1 m depth (note: this depth is used to evaluate the GPR soil suitability)

                if(La.1m <= 40)                             {La.SI <<- 1}        # very high suitability
                if(La.1m > 40  &&  La.1m <= 60)             {La.SI <<- 2}        # high suitability
                if(La.1m > 60  &&  La.1m <= 80)             {La.SI <<- 3}        # moderate suitability
                if(La.1m > 80  &&  La.1m <= 100)            {La.SI <<- 4}        # low suitability
                if(La.1m > 100)                             {La.SI <<- 5}        # very low suitability
                } 



                # important note: these classes are identified based on the attenuation loss at 1 m depth, for 500 MH antennas, if La is not available

      if(is.na(La) == T  &&  is.na(La.1m) == T  &&  is.na(La.1m.500) == F) {
                if(La.1m.500 <= 40)                                {La.SI <<- 1}        # very high suitability
                if(La.1m.500 > 40  &&  La.1m.500 <= 60)            {La.SI <<- 2}        # high suitability
                if(La.1m.500 > 60  &&  La.1m.500 <= 80)            {La.SI <<- 3}        # moderate suitability
                if(La.1m.500 > 80  &&  La.1m.500 <= 100)           {La.SI <<- 4}        # low suitability
                if(La.1m.500 > 100)                                {La.SI <<- 5}        # very low suitability
                }    


      if(is.na(La) == T  &&  is.na(La.1m) == T  &&  is.na(La.1m.500) == T)      {
                La.SI <<- 0   # unknown suitability
                }


                                



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## GEOPHYSICAL SUITABILITY CLASS

# define GPR suitability class based on geophysical classes (i.e. Mdisp.geophysical.SI, BEC.SI, La.SI)

          geophys.vec <<- c(Mdisp.geophysical.SI, BEC.SI, La.SI)



}