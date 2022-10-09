Soil.Input2 <- function(...) {
# Giulio Curioni
# June 2015
# this script may be freely copied and distributed 
# please reference the following projects if using the KBS:
# Mapping The Underworld (http://www.mappingtheunderworld.ac.uk/) 
# Assessing The Underworld (http://assessingtheunderworld.org/)


## ## ## ##
# script must be run from KBS.R
# imports final objects obtained from KBS and try to calculate geophysical parameters based on those
# note: this script might be useless in case many initial parameters were missing
# however, this script might be useful if geotechnical data are available (e.g. model with Mironov and Peplinski)


## redefine some useful parameters:

      mu.abs <<- mu0*mu        #absolute permeability of the material (H/m)
      E.abs <<- E0*Er          #abolute permittivity of the material (F/m)
      loss.tan <<- Ei/Er       #loss tangent








## try to find missing parameters based on (possibly new) existing inputs:








## ## apparent permittivity, Ea ## ##




      #apparent permittivity (Ea) from real (Er) and imaginary (Ei) permittivities (e.g. Robinson et al., 2003a)

      if(is.na(Ea) == T  &&  is.na(Er) == F  &&  is.na(Ei) == F){
                Ea <<- ((Er*mu) / 2) * (sqrt(1 + (loss.tan)^2) + 1)
                Ea <<- round(Ea, digits=2)
                }


      #apparent permittivity (Ea) from real (Er) permittivity only (if only Er is available) (e.g. Topp et al., 1980)
      #note: this is an approximation, Ea calculated in this way does not include the imaginary part of the permittivity, Ei

      if(is.na(Ea) == T  &&  is.na(Er) == F  &&  is.na(Ei) == T){
          Ea <<- Er
          Ea <<- round(Ea, digits=2)
          }


      #apparent permittivity (Ea) from velocity (e.g. Topp et al., 1980)
      #note: this is an approximation, Ea calculated in this way does not include the imaginary part of the permittivity, Ei

      if(is.na(Ea) == T  &&  is.na(Er) == T  &&  is.na(Ei) == T  && is.na(v) == F){
          Ea <<- c0/v
          Ea <<- round(Ea, digits=2)
          }








## ## real permittivity, Er ## ##




      #loss tangent and real permittivity from Ea and Ei
      #note: this is an approximation, it uses Ea in the calculation of the loss tangent.
      #it follows a similar principle as Topp et al., 2000

#      if(is.na(Er) == T  &&  is.na(Ei) == F  &&  is.na(Ea) == F){
#          loss.tan <<-  Ei / Ea
#          loss.tan <<- round(loss.tan, digits=5)
#
#          Er <<- ( (2*Ea)/mu ) * ( 1/(sqrt(1+loss.tan^2) + 1) )
#          Er <<- round(Er, digits=2)
#          }


      #loss tangent and real permittivity from Ea and BEC
      #note: this is an approximation, it does not include relaxation losses and uses Ea in the calculation of the loss tangent.
      #it follows a similar principle as Topp et al., 2000

### this seems to produce contraddictory results and should not be included:

#      if(is.na(Er) == T  &&  is.na(Ei) == T  &&  is.na(Ea) == F  &&  is.na(BEC) == F){
#          loss.tan <<- ( BEC/(omega*E0) ) / Ea
#          loss.tan <<- round(loss.tan, digits=5)
#
#          Er <<- ( (2*Ea)/mu ) * ( 1/(sqrt(1+loss.tan^2) + 1) )
#          Er <<- round(Er, digits=2)
#          }


      #real permittivity from Ea (e.g. Topp et al., 1980)
      #note: this is an approximation, Er calculated in this way also includes the imaginary part of the permittivity, Ei

#      if(is.na(Er) == T  &&  is.na(Ei) == T  &&  is.na(Ea) == F  &&  is.na(BEC) == T){
#          Er <<- Ea
#          Er <<- round(Er, digits=2)
#          }


      #real permittivity from Ea and v (e.g. Topp et al., 1980)
      #note: this is an approximation, Er calculated in this way also includes the imaginary part of the permittivity, Ei

#      if(is.na(Er) == T  &&  is.na(Ei) == T  &&  is.na(Ea) == T  &&  is.na(v) == F){
#          Ea <- c0/v
#          Er <<- Ea
#          Er <<- round(Er, digits=2)
#          }








## ## imaginary permittivity, Ei ## ##




      ## ## note: this way seems to be not accurate (I obtain La << than La calculated by semi-empirical models, which seem more accurate)
      #imaginary permittivity from BEC
      #note: this is an approximation, it does not include relaxation losses.

#      if(is.na(Ei) == T  &&  is.na(BEC) == F){
#          Ei <<- BEC / (omega*E0)
#          Ei <<- round(Ei, digits=2)
#          }


      #imaginary permittivity from Ea and Er

      if(is.na(Ei) == T  &&  is.na(Er) == F  &&  is.na(Ea) == F  &&  is.na(BEC) == T){
          Ei <<- Er * sqrt( (((2*Ea)/(mu*Er)) - 1)^2  - 1 )
          Er <<- round(Er, digits=2)
          }








## ## apparent permittivity at 100 MHz and 1000 MHz, Ea.100 and Ea.1000 ## ##




      # apparent permittivity at 100 MHz and 1000 MHz from real and imaginary permittivity at 100 MHz and 1000 MHz, if available (e.g. Robinson et al., 2003a)  

      if(is.na(Ea.100) == T  &&  is.na(Er.100) == F  &&  is.na(Ei.100) == F){
          Ea.100 <<- ((Er.100 * mu) / 2) * (sqrt(1 + (Ei.100/Er.100)^2) + 1)
          Ea.100 <<- round(Ea.100, digits=2)
          }
          

#      if(is.na(Ea.500) == T  &&  is.na(Er.500) == F  &&  is.na(Ei.500) == F){
#          Ea.500 <<- ((Er.500 * mu) / 2) * (sqrt(1 + (Ei.500/Er.500)^2) + 1)
#          Ea.500 <<- round(Ea.500, digits=2)
#          }


      if(is.na(Ea.1000) == T  &&  is.na(Er.1000) == F  &&  is.na(Ei.1000) == F){
          Ea.1000 <<- ((Er.1000 * mu) / 2) * (sqrt(1 + (Ei.1000/Er.1000)^2) + 1)
          Ea.1000 <<- round(Ea.1000, digits=2)
          }








## ## magnitude of dispersion, Mdisp ## ##




      #magnitude of dispersion from permittivity at 100 MHz and 1000 MHz

      if(is.na(Mdisp) == T  &&  is.na(Er.100) == F  &&  is.na(Ei.100) == F  &&  is.na(Er.1000) == F  &&  is.na(Ei.1000) == F) {
          Mdisp <<- abs(Ea.1000 - Ea.100)
          Mdisp <<- round(Mdisp, digits=2)
          }








## ## bulk electrical conductivity, BEC (S/m) ## ##




      #bulk electrical conductivity from Ei
      #note: this is an approximation, BEC calculated in this way also includes relaxation losses.

## try to leave this:
      if(is.na(BEC) == T  &&  is.na(Ei) == F) {
          BEC <<- omega*E0*Ei
          BEC <<- round(BEC, digits=5)
          }








## ## magnetic permeability, mu (note: mu is the real magnetic permeability, its imaginary part is generally neglected in soils) ## ##




      #magnetic permeability from Ea, Er and Ei
      #note: generally mu is approximated to 1, but if all these data are available then try to calculate mu (e.g. Robinson et al., 2003a)

      if(is.na(mu.in) == T  &&  mu == 1  &&  is.na(Ea) == F  &&  is.na(Er) == F  &&  is.na(Ei) == F) {
          mu <<- (2*Ea) / (Er * (sqrt(1+loss.tan^2) + 1))
          mu <<- round(mu, digits=2)
          }
      



}