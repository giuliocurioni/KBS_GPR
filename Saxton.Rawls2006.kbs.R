Saxton.Rawls2006.kbs <- function(...){
# Giulio Curioni
# June 2015
# this script may be freely copied and distributed 
# please reference the following projects if using the KBS:
# Mapping The Underworld (http://www.mappingtheunderworld.ac.uk/) 
# Assessing The Underworld (http://assessingtheunderworld.org/)


## ## ## ##
# calculates soil water characteristics from texture and OM according to Saxton and Rawls 2006
# note: all water contents are defined in %v (volume), soil particles as %w (weight), ALL in decimal units
# this script must be run from geotechnical.kbs.R (which in turn is run from KBS.R)

## if water content is not available use FIELD CAPACITY
## if water content is not available AND there is some information on the environmental conditions use:
    ## WILTING POINT if saturation = a [the soil is thought to be very dry (after a prolonged periods of dry conditions)]
    ## SATURATION if saturation = b [the soil is thought to be very wet or saturated (after prolonged periods of wet conditions)]


###PROBLEMS:
### 1)
#some problems persist when theta > saturation and when theta < wilting point (theta1500):
# in such cases, unsat hydr. cond. and matric/osmotic potentials are probably inaccurate (this does not affect modelling by SPAW...(?))
### 2)
# conductivity increases wilting point (theta1500) and decreases plant available water (PAW) but these effects are not accounted for in here
# this is not a big problem because these changes become important only at very high conductivities (e.g. >> 3 dS), in saline soils
# if the correct values of wilting point and PAW are needed use "soil water characteristics" software developed by Saxton et al 2006 (USDA) and freely available from:
# http://hydrolab.arsusda.gov/SPAW/SPAWDownload.html (checked in December 2012)



# use decimal units for PSD and vwc: sand.dec, clay.dec, silt.dec, theta (theta is vwc in m3/m3)



# density factor to account for compaction, if NA use 1 as default ('normal' density). This is used to calculate the FIELD CAPACITY water content

      if(is.na(DF) == T) {DF <<- 1}
      if (DF < 0.9 | DF >1.3) {stop(
          "Input density factor between 0.9 and 1.3 to account for compaction.\n  loose: 0.9\n  normal: 1\n  dense: 1.1\n  hard: 1.2\n  severe: 1.3\n  input 1 if uncertain"
          )}



# convert electrical conductivity of the soil extract (this is an approximation, BEC is used instead) from mS/m to S/m
      EC <<- BEC
      #EC <<- EC / 1000

      #if EC is not available use BEC as an approximation (this also includes the conductivity of the soil particles)
      #important note: this script must be run from Soil.Input before reconverting BEC mS/m for final output
      #BEC should be in S/m at this point, but equations by Saxton and Rawls need EC in dS/m:
      if(is.na(EC) == T) { EC <<- BEC * 10 }      # EC is in dS/m now



# assign average soil particle density (2.65 g/cm3) if input is missing

      if(is.na(partD) == T) {partD <<- 2.65}



# assign 0.5 % of soil organic matter if input is missing (note: tin urban environments the OM is assumed to be quite low)

      if(is.na(OM) == T) {OM <<- 0.5}



##moisture regressions:
#1.moisture at 1500 kPa potential
theta1500t <- -0.024*sand.dec + 0.487*clay.dec + 0.006*OM + 0.005*(sand.dec*OM) - 0.013*(clay.dec*OM) + 0.068*(sand.dec*clay.dec) + 0.031      #vwc and W40 in kbs
theta1500 <- theta1500t + 0.14*theta1500t - 0.02   #vwc1500 in kbs
#2.moisture at 33 kPa potential, normal density
theta33t <- -0.251*sand.dec + 0.195*clay.dec + 0.011*OM + 0.006*(sand.dec*OM) - 0.027*(clay.dec*OM) + 0.452*(sand.dec*clay.dec) + 0.299  #vwc and Y40 in kbs
theta33 <<- theta33t + 1.283*(theta33t^2) - 0.374*theta33t - 0.015   #vwc33 in kbs
#3.SAT-33 kPa moisture, normal density (i.e. POROSITY at potential 33 kPa)
thetaS_33t <- 0.278*sand.dec + 0.034*clay.dec + 0.022*OM - 0.018*(sand.dec*OM) - 0.027*(clay.dec*OM) - 0.584*(sand.dec*clay.dec) + 0.078   #por in kbs
thetaS_33 <- thetaS_33t + 0.636*thetaS_33t - 0.107     #por33 in kbs
#4.tension (potential) at air entry (bubbling pressure), in kPa (see below, adj dens)
##psiet <- -21.67*sand.dec - 27.93*clay.dec - 81.97*thetaS_33 + 71.12*(sand.dec*thetaS_33) + 8.29*(clay.dec*thetaS_33) + 14.05*(sand.dec*clay.dec) + 27.16
##psie <- psiet + 0.02*(psiet^2) - 0.113*psiet - 0.70
#5.saturated moisture (0 kPa), normal density
thetaS <- theta33 + thetaS_33 - 0.097*sand.dec + 0.043       #sand.decsat in kbs [thetaS_33 + thetaS_33 = sat (por33 + moist33) in kbs]
#6.normal density (g/cm3)
roN <- (1 - thetaS)*partD




##density effects:
#7.adjusted density (g/cm3), to account for compaction
roDF <<- roN * DF         # a density adjustement factor DF=0.9-1.3 must be defined (DF=1 -> normal density)
#8.SATURATED MOISTURE (0 kPa), adjusted density (i.e. TOTAL POROSITY)
thetaS_DF <<- 1 - (roDF/partD)       
#9. 33 kPa moisture, adjusted density: FIELD CAPACITY [note: this value is the "typical water content" used in the KBS when theta is not available]
theta33_DF <<- theta33 - 0.2*(thetaS - thetaS_DF)
#10. SAT-33 kPa moisture, adjusted density 
thetaS_33_DF <- thetaS_DF - theta33_DF        


#4.tension (potential) at air entry (bubbling pressure), in kPa, ADJ DENSITY
psiet <- -21.67*sand.dec - 27.93*clay.dec - 81.97*thetaS_33_DF + 71.12*(sand.dec*thetaS_33_DF) + 8.29*(clay.dec*thetaS_33_DF) + 14.05*(sand.dec*clay.dec) + 27.16   
psie <- psiet + 0.02*(psiet^2) - 0.113*psiet - 0.70   
#convert kPa in cm of water: 1 Pa = 0.0101971621298 cm of water, 1 kPa = 10.1971621298 cm of water
psie <- psie * 10.1971621298 
 

##moisture-tension (potential)  (using adj dens)
#coefficients of moisture-tension
B <- (log(1500) - log(33)) / (log(theta33_DF) - log(theta1500))           
A <- exp(log(33) + B * log(theta33_DF))                                   






### if theta is not available and environmental conditions are not available (i.e., saturation = na) use the FIELD CAPACITY:

if(is.na(theta) == T  &&  saturation == "na") { theta <<- theta33_DF }
#    # if field capacity is not available, add zero to avoid errors in the script
#    if(is.na(theta) == T) { theta <<- 0 }



### if theta is not available and saturation is a (dry  conditions) use the WILTING POINT:

if(is.na(theta) == T  &&  saturation == "a") { theta <<- theta1500 }
#    # if field capacity is not available, add zero to avoid errors in the script
#    if(is.na(theta) == T) { theta <<- 0 }



### if theta is not available and saturation is b (wet conditions) use the SATURATION:
    
if(is.na(theta) == T  &&  saturation == "b") { theta <<- thetaS_DF }
    # if field capacity is not available, add zero to avoid errors in the script
    if(is.na(theta) == T) { theta <<- 0 }




    # assign other zeros instead of NAs to avoid errors
    if(is.na(theta1500) == T) { theta1500 <- 0 }
    if(is.na(theta33_DF) == T) { theta33_DF <- 0 }









#tension at potentials from 1500 and 33 kPa:
###if (theta < theta1500) {print("theta is < than wilting point (theta at -1500 kPa)! matric pot., osmotic pot., and unsat. h. cond. might be wrong")}

if (theta > theta1500 & theta < theta33_DF) {psi <<- A * (theta)^(-B)
} else {
psi <<- 33 - ( ((theta - theta33_DF) * (33 - thetaS_33_DF)) / (thetaS_DF - theta33_DF) )
}

    # assign other zeros instead of NAs to avoid errors
    if(is.na(psi) == T) { psi <- 0 }
    
###if (psi < 1  &  psi != 0) {print("potential is < 1 kPa, unsat h. cond. is wrong and matric pot might be not physically meaningful, theta > saturation")}


##moisture-hydraulic conductivity
#slope of logarithmic tension-moisture curve
lambda <- 1/B
#saturated hydraulic conductivity (matric soil), mm/h
Ks <- 1930 * (thetaS_DF - theta33_DF)^(3 - lambda)        #using adj dens
 
#unsaturated hydraulic conductivity at moisture theta, mm/h
Ktheta <- Ks * (theta / thetaS_DF)^(3 + 2/lambda)





##gravel effects:
#matric soil density/gravel density (partD)
alphagravel <- roDF/partD
#volume fraction of gravel (decimal), g/cm3
Rv <- (alphagravel * gravel.dec) / (1 - gravel.dec * (1 - alphagravel))
#bulk soil density (matric plus gravel), g/cm3 
roB <- roDF * (1 - Rv) + (Rv * partD)
#plant available moisture (33-1500 kPa, matric soil), %v
PAW <- (theta33_DF - theta1500)        #adj dens
#plant available moisture (33-1500 kPa, bulk soil), %v
PAWb <- PAW * (1 - Rv)                #adj dens
#ratio Kb/Ks (saturated hydraulic conductivity (bulk soil) / sat. h. conductivity (matric soil) )
Kb_Ks <- (1 - gravel.dec) / (1 - gravel.dec * (1 - 3*alphagravel/2))

#Kb (sat h cond (bulk soil)
Kb <- Kb_Ks * Ks
#unsaturated hydraulic conductivity at moisture theta, mm/h, with gravel effect
Kbtheta <- Kb * (theta / thetaS_DF)^(3 + 2/lambda)



##salinity effects:
#osmotic potential in function of theta, kPa
psiOtheta <- (thetaS/theta) * (36 * EC)


# bring back to NA theta, if it was missing
if( theta == 0 ) { theta <<- NA }


##all the data are saved in soilwaterall
soilwaterall <<- matrix(nrow=32, ncol=2); soilwaterall <<- data.frame(soilwaterall);
soilwaterall[1,] <<- c("matric normal density (g/cm3)", roN);
soilwaterall[2,] <<- c("matric adjusted density (g/cm3)", roDF)
soilwaterall[3,] <<- c("1500 kPa vwc, WILTING POINT, NORM DENS (kPa)", theta1500)
soilwaterall[4,] <<- c("33 kPa vwc, norm dens (kPa)", theta33)
soilwaterall[5,] <<- c("33 kPa vwc, FIELD CAP, ADJ DENS (kPa)", theta33_DF)
soilwaterall[6,] <<- c("SAT-33 kPa vwc, norm dens (m3/m3)", thetaS_33)
soilwaterall[7,] <<- c("SAT-33 kPa vwc, adj dens, porosity (m3/m3)", thetaS_33_DF)
soilwaterall[8,] <<- c("air entry potential, adj dens, in cm of water (cm)", psie)
soilwaterall[9,] <<- c("saturated vwc, norm dens (m3/m3)", thetaS)
soilwaterall[10,] <<- c("SATURATED VWC, ADJ DENS (m3/m3)", thetaS_DF)

soilwaterall[11,] <<- c("A coeff", A)
soilwaterall[12,] <<- c("B coeff", B)
soilwaterall[13,] <<- c("MATRIC POTENTIAL, ADJ DENS (kPa)", psi)
soilwaterall[14,] <<- c("slope tension-moisture curve (lambda)", lambda)

soilwaterall[15,] <<- c("sat hydraulic conductivity (mm/h)", Ks)
soilwaterall[16,] <<- c("unsat hydraulic conductivity (mm/h)", Ktheta)
soilwaterall[17,] <<- c("UNSAT HYDR. COND., WITH GRAVEL EFFECT (mm/h)", Kbtheta)

soilwaterall[18,] <<- c("volume fraction of gravel (g/cm3)", Rv)
soilwaterall[19,] <<- c("BULK ADJ DENSITY (MATRIC + GRAVEL) (g/cm3)", roB)
soilwaterall[20,] <<- c("plant available water (m3/m3)", PAW)
soilwaterall[21,] <<- c("PLANT AVAILABLE WATER, BULK SOIL (m3/m3)", PAWb)
soilwaterall[22,] <<- c("ratio Kb/Ks (sat h cond (bulk soil) / sat h cond (matric soil)", Kb_Ks)
soilwaterall[23,] <<- c("Kb (SAT H COND, BULK SOIL) (mm/h)", Kb)

soilwaterall[24,] <<- c("osmotic potential, adj dens (kPa)", psiOtheta)

soilwaterall[25,] <<- c("MATRIC + OSMOTIC POTENTIAL, ADJ DENS (kPa)", psi + psiOtheta)

soilwaterall[26,] <<- c("sand.dec (%w)", sand.dec*100)
soilwaterall[27,] <<- c("clay (%w)", clay.dec*100)
soilwaterall[28,] <<- c("OM (%)", OM)
soilwaterall[29,] <<- c("theta (%v)", theta*100)
soilwaterall[30,] <<- c("density factor, DF", DF)
soilwaterall[31,] <<- c("gravel (%w)", gravel.dec*100)
soilwaterall[32,] <<- c("EC (dS/m)", EC)

soilwaterall[,2] <<- as.numeric(soilwaterall[,2])
soilwaterall[1,2] <<- round(soilwaterall[1,2], digits=6)
soilwaterall[2,2] <<- round(soilwaterall[2,2], digits=6)
soilwaterall[3,2] <<- round(soilwaterall[3,2], digits=6)
soilwaterall[4,2] <<- round(soilwaterall[4,2], digits=6)
soilwaterall[5,2] <<- round(soilwaterall[5,2], digits=6)
soilwaterall[6,2] <<- round(soilwaterall[6,2], digits=6)
soilwaterall[7,2] <<- round(soilwaterall[7,2], digits=6)
soilwaterall[8,2] <<- round(soilwaterall[8,2], digits=6)
soilwaterall[9,2] <<- round(soilwaterall[9,2], digits=6)
soilwaterall[10,2] <<- round(soilwaterall[10,2], digits=6)
soilwaterall[11,2] <<- round(soilwaterall[11,2], digits=6)
soilwaterall[12,2] <<- round(soilwaterall[12,2], digits=6)
soilwaterall[13,2] <<- round(soilwaterall[13,2], digits=6)
soilwaterall[14,2] <<- round(soilwaterall[14,2], digits=6)
soilwaterall[15,2] <<- round(soilwaterall[15,2], digits=6)
soilwaterall[16,2] <<- round(soilwaterall[16,2], digits=6)
soilwaterall[17,2] <<- round(soilwaterall[17,2], digits=10)
soilwaterall[18,2] <<- round(soilwaterall[18,2], digits=6)
soilwaterall[19,2] <<- round(soilwaterall[19,2], digits=6)
soilwaterall[20,2] <<- round(soilwaterall[20,2], digits=6)
soilwaterall[21,2] <<- round(soilwaterall[21,2], digits=6)
soilwaterall[22,2] <<- round(soilwaterall[22,2], digits=6)
soilwaterall[23,2] <<- round(soilwaterall[23,2], digits=6)
soilwaterall[24,2] <<- round(soilwaterall[24,2], digits=6)
soilwaterall[25,2] <<- round(soilwaterall[25,2], digits=6)

soilwaterall[26,2] <<- round(soilwaterall[26,2], digits=6)
soilwaterall[27,2] <<- round(soilwaterall[27,2], digits=6)
soilwaterall[28,2] <<- round(soilwaterall[28,2], digits=6)
soilwaterall[29,2] <<- round(soilwaterall[29,2], digits=6)
soilwaterall[30,2] <<- round(soilwaterall[30,2], digits=6)
soilwaterall[31,2] <<- round(soilwaterall[31,2], digits=6)
soilwaterall[32,2] <<- round(soilwaterall[32,2], digits=6)


##only "important" data are saved in soilwater
soilwater <<- matrix(nrow=16, ncol=2); soilwater <<- data.frame(soilwater);
soilwater[1,] <<- c("sand (%w)", sand.dec*100)
soilwater[2,] <<- c("clay (%w)", clay.dec*100)
soilwater[3,] <<- c("OM (%)", OM)
soilwater[4,] <<- c("theta (%v)", theta*100)
soilwater[5,] <<- c("density factor, DF", DF)
soilwater[6,] <<- c("gravel (%w)", gravel.dec*100)
soilwater[7,] <<- c("EC (dS/m)", EC)

soilwater[8,] <<- c("matric adjusted density (g/cm3)", roDF)
soilwater[9,] <<- c("bulk soil adj density (matric plus gravel) (g/cm3)", roB)
soilwater[10,] <<- c("air entry potential, adj dens, (cm of water)", psie)
soilwater[11,] <<- c("sat hydr cond, adj dens (bulk soil) (cm/day)", Kb*2.4)
soilwater[12,] <<- c("33 kPa vwc, field capacity, adj dens (m3/m3)", theta33_DF)
soilwater[13,] <<- c("1500 kPa vwc, wilting point, norm dens (m3/m3)", theta1500)
soilwater[14,] <<- c("A coeff *", A)
soilwater[15,] <<- c("B coeff *", B)
soilwater[16,] <<- c("plant available water bulk soil (cm/cm)", PAWb)
#soilwater[17,] <<- c("unsat hydr cond, adj dens, gravel effect (mm/h)", Kbtheta)
#soilwater[18,] <<- c("matric potential, adj dens (kPa)", psi)
#soilwater[19,] <<- c("matric + osmotic potential, adj dens (kPa)", psi + psiOtheta)
#soilwater[20,] <<- c("sat vwc, adj dens (m3/m3)", thetaS_DF)

soilwater[,2] <<- as.numeric(soilwater[,2])
soilwater[1,2] <<- round(soilwater[1,2], digits=6)
soilwater[2,2] <<- round(soilwater[2,2], digits=6)
soilwater[3,2] <<- round(soilwater[3,2], digits=6)
soilwater[4,2] <<- round(soilwater[4,2], digits=6)
soilwater[5,2] <<- round(soilwater[5,2], digits=6)
soilwater[6,2] <<- round(soilwater[6,2], digits=6)
soilwater[7,2] <<- round(soilwater[7,2], digits=6)

soilwater[8,2] <<- round(soilwater[8,2], digits=2)
soilwater[9,2] <<- round(soilwater[9,2], digits=2)
soilwater[10,2] <<- round(soilwater[10,2], digits=2)
soilwater[11,2] <<- round(soilwater[11,2], digits=2)
soilwater[12,2] <<- round(soilwater[12,2], digits=2)
soilwater[13,2] <<- round(soilwater[13,2], digits=2)
soilwater[14,2] <<- round(soilwater[14,2], digits=6)
soilwater[15,2] <<- round(soilwater[15,2], digits=6)
soilwater[16,2] <<- round(soilwater[16,2], digits=6)
#soilwater[17,2] <<- round(soilwater[17,2], digits=10)
#soilwater[18,2] <<- round(soilwater[18,2], digits=6)
#soilwater[19,2] <<- round(soilwater[19,2], digits=6)
#soilwater[20,2] <<- round(soilwater[20,2], digits=6)

###fix(soilwater)
###fix(soilwaterall)


# reconvert EC from dS/m to mS/m for output

EC <<- EC * 100
}
