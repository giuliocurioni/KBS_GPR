rm(list=ls())

# set working directory
setwd("C:/R_KBS")


loadkbs <- function(...) {

        # load packages
        library("gdata")
        library("rJava")
        #library("svSocket")       
        library("xlsx")
        #library("gWidgetsRGtk2")

        

        # load scripts
        #source("C:\\R_KBS\\ar.prediction.R")
        #source("C:\\R_KBS\\attenuation.loss.R")
        #source("C:\\R_KBS\\debye.R")
        source("C:\\R_KBS\\GPR.system.kbs.R")
        source("C:\\R_KBS\\inst.kbs.packages.R")
        source("C:\\R_KBS\\KBS.R")
        #source("C:\\R_KBS\\Mironov.R")
        #source("C:\\R_KBS\\Peplinski.R")
        source("C:\\R_KBS\\Saxton.Rawls2006.kbs.R")
        #source("C:\\R_KBS\\saxton2006.R")
        #source("C:\\R_KBS\\semi.empirical.models.R")
        #source("C:\\R_KBS\\settime.R")
        source("C:\\R_KBS\\Site.kbs.R")
        source("C:\\R_KBS\\Soil.kbs.R")
        source("C:\\R_KBS\\Soil.Input2.R")
        #source("C:\\R_KBS\\stat.weather.R")
        #source("C:\\R_KBS\\Topp.R")
        #source("C:\\R_KBS\\waterbalance.R")
        #source("C:\\R_KBS\\weatherlast.H.R")
 
        }


