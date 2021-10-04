
# Linear and exponential fluxes calculated from LGR measurements    (created by jbarba at 09262018)
## This script contents data compilation from different LGR files and fluxes processing using linear and exponential fits

# Load packages
library(data.table)
library(lubridate)
library(doBy)
library(dplyr)
library(padr)
library(zoo)

  
# Merging LGR files -----------------------------------------------------------
  
  ## With this chunk of script we can make one data table from a list of many. It reads all files from a folder and their Tree subfolders and merge them.
  # It does not read compressed files

  # LGR sometimes saves files in .zip. The first part of this chunk is for decompress all .zip files from a given folder
    # List all .zip files including sub-folders
    list_of_zip_files <- list.files(path = "//anr.udel.edu/files/shares/jbarba/My Documents/SJ_Data/SJ_stem_fluxes/SJ_stem_fluxes_LGR/SJ_LGR_RawData", recursive=TRUE, 
                      pattern="\\.zip$", full.names=TRUE)
    
    # Decompress all .zip files from a given folder and subfolders
    sapply(list_of_zip_files, function(i) unzip(i, exdir=gsub("\\.zip$", "", i))) #This makes a copy of the uncompressed folder in the same folder as .zip was
  
  # List all txt files, merge them in one single file and create a variable with the name of each original file 
    # List all txt files
    list_of_txt_files <- list.files(path = "//anr.udel.edu/files/shares/jbarba/My Documents/SJ_Data/SJ_stem_fluxes/SJ_stem_fluxes_LGR/SJ_LGR_RawData", recursive = TRUE,
                              pattern = "\\.txt$", full.names = T)  #with full.name=F the function save the name of each file instead of the name of each path. This is useful for the idcol in the next section 
 
    # Read all the files and create a Path column to store filenames
    LGR_data <- rbindlist(sapply(list_of_txt_files, fread, simplify = FALSE),
                    use.names = TRUE, idcol = "Path", fill=T)
           
# Nezha>> I don't know what happen with the next 7 lines of code, but iot takes more than 2 hours for runing it.
        aaa<-strsplit(LGR_data$Path, "/")

        x <- data.frame(name=character())
        x$name <-as.character(x$name)
        for(i in 1:length(aaa)){
            x[i, 1] <- aaa[[i]][[length(aaa[[i]])]]
                }
    
  # Nezha>> Sometimes, I get an error with a subfolder: sometimes a zip folder cannot be opened. When that happens, I have to copy-paste the txt file outside the compressed folder. Could this be solved?
    
    LGR_data$file_name <- x$name
    
    #Some names of the variables have " ", [, ], and '. We have to remove these characters for further analysis.
    colnames(LGR_data) <- c('Path', 'Time', 'CH4_ppm', 'CH4_ppm_sd', 'H2O_ppm', 'H2O_ppm_sd', 'CO2_ppm', 'CO2_ppm_sd', 'CH4d_ppm', 'CH4d_ppm_sd', 'CO2d_ppm', 'CO2d_ppm_sd', 'GasP_torr', 'GasP_torr_sd', 'GasT_C', 
                            'GasT_C_sd', 'AmbT_C', 'AmbT_C_sd', 'RD0_us', 'RD0_us_sd', 'RD1_us', 'D1_us_sd', 'Gnd', 'Gnd_sd', 'LTC0_b', 'LTC0_b_sd', 'LTC0_v', 'LTC0_v_sd', 'LTC1_b', 'LTC1_b_sd', 'LTC1_v', 'LTC1_v_sd', 
                            'BL0T', 'BL0T_sd', 'BL1T', 'BL1T_sd', 'BL2F', 'BL2F_sd', 'BL3F', 'BL3F_sd', '12CH4_4_CT', '12CH4_4_CT_sd', '12CH4_4_AT', '12CH4_4_AT_sd', '12CH4_4_DF', '12CH4_4_DF_sd', '12CH4_4_PF', 
                            '12CH4_4_PF_sd', 'H2O_12_CF', 'H2O_12_CF_sd', 'H2O_12_AT', 'H2O_12_AT_sd', 'H2O_12_DF', 'H2O_12_DF_sd', 'H2O_12_PF', 'H2O_12_PF_sd', 'BL0T_B', 'BL0T_B_sd', 'BL1T_B', 'BL1T_B_sd', 'BL2F_B', 
                            'BL2F_B_sd', 'BL3F_B', 'BL3F_B_sd', '12CO2_0_CT_B', '12CO2_0_CT_B_sd', '12CO2_0_AT_B', '12CO2_0_AT_B_sd', '12CO2_0_DF_B', '12CO2_0_DF_B_sd', '12CO2_0_PF_B', '12CO2_0_PF_B_sd', 'Fit_Flag', 
                            'N_Fits_Avg_d', 'MIU', 'file_name')

    LGR_data<- as.data.frame(LGR_data) 
        
# Data preparation --------------------------------------------------------
  ## LGR_data
    # Re-adjust time format
    # Create new time variables for matching time from LGR_data and LGR_fn (field notes)

#Nezha>> I worte the following section for formating time, but I don't know if this is efficient.            
        ## Format time&date into useful columns 
        ##  Pull out date and time data
        date_time=strptime(LGR_data$Time,format='%m/%d/%Y %H:%M:%S')
        
        ##  Add year, month, day, JD, hour, min, sec columns to dataframe
        Year<-as.numeric(format(date_time,'%Y'))
        Month<-as.numeric(format(date_time,'%m'))
        Day<-as.numeric(format(date_time,'%d'))
        fDOY<-as.numeric(julian(date_time,'2017-12-31'))  ## Fractional day of year. 
        DOY<-as.POSIXlt(date_time, format = "%d%b%y")
        Hour<-as.character(format(date_time,'%k'))
        Min<-as.character(format(date_time,'%M'))
  
 #Nezha>> the next section clearly has to be optimized. There is one moment in the script that I have to match LGR_data time (data coming from the analyser) with LGR_fn (field notes specifying the initial time for each measurement). Sometimes, both times don't metch by one or two seconds, so what I'm trying to do here is creating new variables with 1, 2, 3... seconds of delay, and then create a loop to match the closest time possible. There should be a way to find the closest time without having to create new variables. 
           
        # LGR does not measure every second, so there is a chance that one "initial time" in the field notes doesn't exactly match with a time listed in LGR_data
          # so I created Sec2, Sec3, Sec4 and Sec5 variables to be sure that there is one LGR_data time for each "field notes" time 
        Sec<-as.character(format(date_time,'%S'))
        Sec2<-as.character(sprintf("%02d", (as.numeric(Sec)+1))) 
        Sec3<-as.character(sprintf("%02d", (as.numeric(Sec)-1))) 
        Sec4<-as.character(sprintf("%02d", (as.numeric(Sec)+2)))
        Sec5<-as.character(sprintf("%02d", (as.numeric(Sec)-2)))
        Sec6<-as.character(sprintf("%02d", (as.numeric(Sec)+3)))
        Sec7<-as.character(sprintf("%02d", (as.numeric(Sec)-3)))
        Sec8<-as.character(sprintf("%02d", (as.numeric(Sec)+4)))
        Sec9<-as.character(sprintf("%02d", (as.numeric(Sec)-4)))
        Sec_cont<- as.numeric(Sec)+as.numeric(Min)*60+as.numeric(Hour)*60*60
        fDOY<-DOY$yday + as.numeric(Hour)/24 + as.numeric(Min)/(24*60) +as.numeric(Sec)/(24*60*60)
        LGR_data<-cbind(date_time, Year, Month, Day, fDOY, Hour, Min, Sec, Sec2, Sec3, Sec4, Sec5, Sec6, Sec7, Sec8, Sec9, Sec_cont, LGR_data[,-1])
        
        LGR_data$TIMEnew <- paste(as.factor(LGR_data$Hour), as.factor(LGR_data$Min), as.factor(LGR_data$Sec), sep=':')
        LGR_data$TIMEnew2 <- paste(as.factor(LGR_data$Hour), as.factor(LGR_data$Min), as.factor(LGR_data$Sec2), sep=':')
        LGR_data$TIMEnew3 <- paste(as.factor(LGR_data$Hour), as.factor(LGR_data$Min), as.factor(LGR_data$Sec3), sep=':')
        LGR_data$TIMEnew4 <- paste(as.factor(LGR_data$Hour), as.factor(LGR_data$Min), as.factor(LGR_data$Sec4), sep=':')
        LGR_data$TIMEnew5 <- paste(as.factor(LGR_data$Hour), as.factor(LGR_data$Min), as.factor(LGR_data$Sec5), sep=':')
        LGR_data$TIMEnew6 <- paste(as.factor(LGR_data$Hour), as.factor(LGR_data$Min), as.factor(LGR_data$Sec6), sep=':')
        LGR_data$TIMEnew7 <- paste(as.factor(LGR_data$Hour), as.factor(LGR_data$Min), as.factor(LGR_data$Sec7), sep=':')
        LGR_data$TIMEnew8 <- paste(as.factor(LGR_data$Hour), as.factor(LGR_data$Min), as.factor(LGR_data$Sec8), sep=':')
        LGR_data$TIMEnew9 <- paste(as.factor(LGR_data$Hour), as.factor(LGR_data$Min), as.factor(LGR_data$Sec9), sep=':')
        
        write.table(LGR_data,file="//anr.udel.edu/files/shares/jbarba/My Documents/SJ_Data/SJ_stem_fluxes/SJ_full_paper/txt/LGR_data.txt",sep="\t")   
        #This LGR_data is the data frame combining all LGR files, with renamed variables, and with several new variables required for flux estimation. 
    
            
  ## LGR_field_notes
    # Create new variables for meteo, diameter, collar height

      setwd("C:/Users/barbafej/Dropbox/SJ_stem_fluxes/SJ_studies/SJ_full_paper/txt")                        
      LGR_fn <- read.table("C:/Users/barbafej/Dropbox/SJ_stem_fluxes/SJ_studies/SJ_full_paper/txt/LGR_field_notes.txt", fill=TRUE,header=TRUE,sep="\t",na.strings=c("NA","#N/A!","#N/A","#NA!"))    
      
      #Meteo: meteo information measured every 15 min. If Atm pressure is used for calculating the fluxes, you might need this file. Otherwise, a constant AtmPress could be used. 
      Meteo <- read.table("C:/Users/barbafej/Dropbox/SJ_stem_fluxes/SJ_studies/SJ_full_paper/txt/SJ_meteo_data.txt", fill=TRUE,header=TRUE,sep="\t",na.strings=c("NA","#N/A!","#N/A","#NA!"))    
                                              
      LGR_fn$Hour <- as.integer(sapply(strsplit(as.character(LGR_fn$Real_time), ":"), "[", 1))
      #LGR_fn (field notes) is a txt file with the information recorded in the field (LGR file name, code for sampling point, initial measurement time...[see the LGR_field_notes_example Excel])
      LGR_fn<-LGR_fn[complete.cases(LGR_fn), ]
      colnames(LGR_fn)[3]<-"Location"
      LGR_fn$DOY <- strftime(as.POSIXlt(LGR_fn$Date, format = "%m/%d/%Y"), format="%j")
      
      Meteo$Date_2 <- sapply(strsplit(as.character(Meteo$Date), " "), "[", 1)
      Meteo$Date_2 <- as.factor(Meteo$Date_2)
      Meteo_mean <- summaryBy(VWC + Soil_temp + EC + RH + Air_temp + AtmPress + Wind_speed + Wind_Gusts + Air_Direction ~ Date_2 + Hour, data=Meteo,FUN=c(mean,sd), na.rm=TRUE)
      
      #This loop is for adding meteorological data to LGR_fn
      for (i in 1:length(LGR_fn$Date)){
        for (k in 1:length(Meteo_mean$Date_2)){
          if(LGR_fn$Date[i]== Meteo_mean$Date_2[k] & LGR_fn$Hour[i]== Meteo_mean$Hour[k]){
            LGR_fn$Air_temp[i]<-Meteo_mean$Air_temp.mean[k]  #This could be used for flux calculations
            LGR_fn$AtmPress[i]<-Meteo_mean$AtmPress.mean[k]  #This could be used for flux calculations

          }
        }    
      }      

  write.table(LGR_fn,file="C:/Users/barbafej/Dropbox/SJ_stem_fluxes/SJ_studies/SJ_full_paper/txt/LGR_fn.txt",sep="\t")   
                                                         

# Flux calculation --------------------------------------------------------
                            
    LGR_data <- read.table("C:/Users/barbafej/Dropbox/SJ_stem_fluxes/SJ_studies/SJ_full_paper/txt/LGR_data.txt", fill=TRUE,header=TRUE,sep="\t",na.strings=c("NA","#N/A!","#N/A","#NA!"))
    LGR_fn <- read.table("C:/Users/barbafej/Dropbox/SJ_stem_fluxes/SJ_studies/SJ_full_paper/txt/LGR_fn.txt", fill=TRUE,header=TRUE,sep="\t",na.strings=c("NA","#N/A!","#N/A","#NA!"))

    levfn <- levels(as.factor(LGR_fn$file_name)) #This is a list of the different file names in the field notes
    levdata <- levels(as.factor(LGR_data$file_name)) # This is a list of the different file names in the LGR_data. This list SHOULD HAVE AT LEAST the same files as in LGR_fn (but probably will have more files)
                                                      # Check that the files spelling is the same for LGR_data and LGR_fn 
    LGR_fluxes<-data.frame() 
    area<-pi*0.055^2  #area of the chamber in m2
    vol<-area*0.095 + pi*0.00175^2*(1.62+1.70) + 0.0002 #volume chamber +volume tubbing + volume LGR cell in m3
    #AtmPress<-101.325 # If Atmospheric Pressure is not available at the measuring time, we can put a constant value (kg m-2 s-1)
    glc<-0.00831447 # ideal gas law constant in kg m2 micromol-1 k-1
    
    for (i in 1:length(levfn)){
    
    datasub <- LGR_data[LGR_data$file_name==levfn[i],]
    fn <- LGR_fn[LGR_fn$file_name==levfn[i],]
    fn$slopeCO2_lin <- NA
    fn$intCO2_lin <- NA
    fn$adjR2CO2_lin <- NA
    fn$pvalueCO2_lin <- NA
    fn$initial <- NA
    
    fn$aCO2_exp <- NA
    fn$assymptCO2_exp <- NA
    fn$slopeCO2_exp <- NA
    fn$adjR2CO2_exp <- NA
    fn$pvalueCO2_exp <- NA

    fn$slopeCH4_lin <- NA
    fn$intCH4_lin <- NA
    fn$adjR2CH4_lin <- NA
    fn$pvalueCH4_lin <- NA
    
    fn$aCH4_exp <- NA
    fn$assymptCH4_exp <- NA
    fn$slopeCH4_exp <- NA
    fn$adjR2CH4_exp <- NA
    fn$pvalueCH4_exp <- NA
    
    fn$slopeH2O_lin <- NA
    fn$intH2O_lin <- NA
    fn$adjR2H2O_lin <- NA
    fn$pvalueH2O_lin <- NA
    
    fn$aH2O_exp <- NA
    fn$assymptH2O_exp <- NA
    fn$slopeH2O_exp <- NA
    fn$adjR2H2O_exp <- NA
    fn$pvalueH2O_exp <- NA
    
    fn$CO2_lin_flux <- NA
    fn$CH4_lin_flux <- NA
    fn$H2O_lin_flux <- NA
    
    fn$CO2_flux <- NA
    fn$CO2_adjR2 <- NA
    fn$CO2_fit <- NA
    
    fn$CH4_flux <- NA
    fn$CH4_adjR2 <- NA
    fn$CH4_fit <- NA
    
    fn$H2O_flux <- NA
    fn$H2O_adjR2 <- NA
    fn$H2O_fit <- NA
    
#Nezha -> Thgis part clearly need an improvement. This loop is just for selecting the closest LGR_data time to the LGR_fn time      
    levdat <- levels(as.factor(as.character(datasub$TIMEnew)))
    levdat2 <- levels(as.factor(as.character(datasub$TIMEnew2)))
    levdat3 <- levels(as.factor(as.character(datasub$TIMEnew3)))
    levdat4 <- levels(as.factor(as.character(datasub$TIMEnew4)))
    levdat5 <- levels(as.factor(as.character(datasub$TIMEnew5)))
    levdat6 <- levels(as.factor(as.character(datasub$TIMEnew6)))
    levdat7 <- levels(as.factor(as.character(datasub$TIMEnew7)))
    levdat8 <- levels(as.factor(as.character(datasub$TIMEnew8)))
    levdat9 <- levels(as.factor(as.character(datasub$TIMEnew9)))
    
      for (k in 1:length(fn$Time)){
        if(length(grep(fn$Time[k], levdat))==1) {
          gr<-grep(fn$Time[k], levdat) 
          Sec_variable<-"Sec" } else {
          if(length(grep(fn$Time[k], levdat2))==1) {
            gr<-grep(fn$Time[k], levdat2) 
            Sec_variable<-"Sec2" } else {
            if(length(grep(fn$Time[k], levdat3))==1) {
              gr<-grep(fn$Time[k], levdat3) 
              Sec_variable<-"Sec3" } else {
              if(length(grep(fn$Time[k], levdat4))==1) {
                gr<-grep(fn$Time[k], levdat4) 
                Sec_variable<-"Sec4" } else {
                if(length(grep(fn$Time[k], levdat5))==1) {
                  gr<-grep(fn$Time[k], levdat5) 
                  Sec_variable<-"Sec5" } else {
                  if(length(grep(fn$Time[k], levdat6))==1) { 
                    gr<-grep(fn$Time[k], levdat6) 
                    Sec_variable<-"Sec6" } else {
                    if(length(grep(fn$Time[k], levdat7))==1) {
                      gr<-grep(fn$Time[k], levdat7) 
                      Sec_variable<-"Sec7" } else {
                      if(length(grep(fn$Time[k], levdat8))==1) {
                        gr<-grep(fn$Time[k], levdat8) 
                        Sec_variable<-"Sec8" } else {
                        if(length(grep(fn$Time[k], levdat9))==1) {
                          gr<-grep(fn$Time[k], levdat9) 
                          Sec_variable<-"Sec9" 
                          }
                        }
                      }
                    }  
                  }
                }
              }
            }
          }
      
      reg_data <- datasub[(gr+5):(gr+115),]
      reg_fitCO2<-lm(CO2d_ppm~Sec_cont, data=reg_data)
      reg_fitCH4<-lm(CH4d_ppm~Sec_cont, data=reg_data)
      reg_fitH2O<-lm(H2O_ppm~Sec_cont, data=reg_data)

      #This equation is the same as in the Li-COR manual [i.e. CO2(t) ~ Cx + (CO2i-Cx)*exp(-a*(t-ti))  ] where Cx is the value at the assymptote, CO2i is the initial CO2 value, and ti is the initial time. 
      
# Nezha -> This is the only part of the code that doesn't work properly. For estimating flux measurements we need to fit a predefined equation. However, we need to provide initial parameters estimates. In mot of the cases, the exponetial flux cannot be calculated, and I guess it's because the initial parameters are not appropriate        
      #The nls functions is picky to the starting values of the parameters. We estimate the starting assymptote as the highest CO2 value, and the starting "a"  as -log(1/2)/"time when the concentration achieves 50% of the assymptote. Once we provide the starting values, the nls computes the final values.This model works for positive and negative fluxes.  

      reg_data$Sec_cont_0 <- reg_data$Sec_cont-reg_data$Sec_cont[1]+1 #The next function has problems finding the starting parameters when Sec_cont is too high. 
      exp_fitCO2 <- tryCatch(nls(CO2d_ppm ~ B + (reg_data$CO2d_ppm[1]- B)*exp(-a*(Sec_cont_0-(reg_data$Sec_cont_0[1]))), data = reg_data, start = list(B = reg_data$CO2d_ppm[length(reg_data$CO2d_ppm)], a = -log(1/2)/(mean(reg_data$Sec_cont_0)))), error = function(e) {  skip_to_next <<- TRUE})
      exp_fitCH4 <- tryCatch(nls(CH4d_ppm ~ B + (reg_data$CH4d_ppm[1]- B)*exp(-a*(Sec_cont_0-(reg_data$Sec_cont_0[1]))), data = reg_data, start = list(B = reg_data$CH4d_ppm[length(reg_data$CH4d_ppm)], a = -log(1/2)/(mean(reg_data$Sec_cont_0)))), error = function(e) {  skip_to_next <<- TRUE})
      exp_fitH2O <- tryCatch(nls(H2O_ppm ~ B + (reg_data$H2O_ppm[1]- B)*exp(-a*(Sec_cont_0-(reg_data$Sec_cont_0[1]))), data = reg_data, start = list(B = reg_data$H2O_ppm[length(reg_data$H2O_ppm)], a = -log(1/2)/(mean(reg_data$Sec_cont_0)))), error = function(e) {  skip_to_next <<- TRUE})
      
      fn$slopeCO2_lin[k]<-coef(reg_fitCO2)[2]
      fn$intCO2_lin[k]<-coef(reg_fitCO2)[1]
      fn$adjR2CO2_lin[k]<-summary(reg_fitCO2)$adj.r.squared
      fn$pvalueCO2_lin[k]<-summary(reg_fitCO2)$coefficients[2,4]
      fn$initial[k]<-gr
      
      #This equation is extracted from the Li-COR manual
      if(length(exp_fitCO2)==6) {
        fn$aCO2_exp[k]<-coef(exp_fitCO2)[2]
        fn$assymptCO2_exp[k]<-coef(exp_fitCO2)[1]
        fn$slopeCO2_exp[k]<-coef(exp_fitCO2)[2]*(coef(exp_fitCO2)[1]-predict(exp_fitCO2)[1])  
        fn$adjR2CO2_exp[k]<-summary(lm(reg_data$CO2d_ppm~predict(exp_fitCO2)))$adj.r.squared  #This is a way to calculate a pseudo-R2
        fn$pvalueCO2_exp[k]<-summary(exp_fitCO2)$coefficients[2,4]
      } 
      
      fn$slopeCH4_lin[k]<-coef(reg_fitCH4)[2]
      fn$intCH4_lin[k]<-coef(reg_fitCH4)[1]
      fn$adjR2CH4_lin[k]<-summary(reg_fitCH4)$r.squared
      fn$pvalueCH4_lin[k]<-summary(reg_fitCH4)$coefficients[2,4]
      
      if(length(exp_fitCH4)==6) {
        fn$aCH4_exp[k]<-coef(exp_fitCH4)[2]
        fn$assymptCH4_exp[k]<-coef(exp_fitCH4)[1]
        fn$slopeCH4_exp[k]<-coef(exp_fitCH4)[2]*(coef(exp_fitCH4)[1]-predict(exp_fitCH4)[1])
        fn$adjR2CH4_exp[k]<-summary(lm(reg_data$CH4d_ppm~predict(exp_fitCH4)))$adj.r.squared  #This is a way to calculate a pseudo-R2
        fn$pvalueCH4_exp[k]<-summary(exp_fitCH4)$coefficients[2,4]
      }
       
      fn$slopeH2O_lin[k]<-coef(reg_fitH2O)[2]
      fn$intH2O_lin[k]<-coef(reg_fitH2O)[1]
      fn$adjR2H2O_lin[k]<-summary(reg_fitH2O)$adj.r.squared
      fn$pvalueH2O_lin[k]<-summary(reg_fitH2O)$coefficients[2,4]
      
      if(length(exp_fitH2O)==6) {
        fn$aH2O_exp[k]<-coef(exp_fitH2O)[2]
        fn$assymptH2O_exp[k]<-coef(exp_fitH2O)[1]
        fn$slopeH2O_exp[k]<-coef(exp_fitH2O)[2]*(coef(exp_fitH2O)[1]-predict(exp_fitH2O)[1])
        fn$adjR2H2O_exp[k]<-summary(lm(reg_data$H2O_ppm~predict(exp_fitH2O)))$adj.r.squared  #This is a way to calculate a pseudo-R2 for the nls model
        fn$pvalueH2O_exp[k]<-summary(exp_fitH2O)$coefficients[2,4]
      }

# Nezha -> Could be possible to plot the data, the linear and exponential fits of each reg_data subset, including the R2 of each fit as well. It would be nice if this can be saved, so it would work as a quick quality conytrol check. 
        
      # See Warner et al 2017. Ecosystems 20, 1205:1216 for details in linear calculation
# Nezha -> if AtmPress is not provided in a meteorologial data file, we apply a constant
        
      fn$CO2_lin_flux[k]<- coef(reg_fitCO2)[2]*(vol/area)*(fn$AtmPress[k]/(glc*(fn$Temp[k]+273))) #micromol m-2 s-1
      fn$CH4_lin_flux[k]<- coef(reg_fitCH4)[2]*(vol/area)*(fn$AtmPress[k]/(glc*(fn$Temp[k]+273)))*1000 #nmol m-2 s-1
      fn$H2O_lin_flux[k]<- coef(reg_fitH2O)[2]*(vol/area)*(fn$AtmPress[k]/(glc*(fn$Temp[k]+273))) #micromol m-2 s-1
      
      if(length(exp_fitCO2)==6) fn$CO2_exp_flux[k]<- fn$slopeCO2_exp[k]*(vol/area)*(fn$AtmPress[k]/(glc*(fn$Temp[k]+273))) else(fn$CO2_exp_flux[k]<-NA) #micromol m-2 s-1
      if(length(exp_fitCH4)==6) fn$CH4_exp_flux[k]<- fn$slopeCH4_exp[k]*(vol/area)*(fn$AtmPress[k]/(glc*(fn$Temp[k]+273)))*1000 else(fn$CH4_exp_flux[k]<-NA) #nmol m-2 s-1
      if(length(exp_fitH2O)==6) fn$H2O_exp_flux[k]<- fn$slopeH2O_exp[k]*(vol/area)*(fn$AtmPress[k]/(glc*(fn$Temp[k]+273))) else(fn$H2O_exp_flux[k]<-NA) #micromol m-2 s-1
      
      
      # We select the best fit (exponential or linear) depending on the R2. We keep the flux with higher R2. 
      # However, when the expinential fit is very bad, the calculated flux could be really high or low (negative), so we selected an R2 threshold of 0.2 (empirically selected)
      # So when the R2 is higher than 0.2, we select the regression (linear or exponential) with higher R2, and when the R2 is lower than 0.2, we keep the linear fit (more conservative)
      # This is less important for CO2 than for CH4 or H2O, because in the first case, most of the fluxes have an R2 higher than 0.95  
# Nezha -> the problem with the L326-329 explanation is that, because exponential fit doesn't usually work because of the wrong initial parameters selection, linear fit is selected in most of the cases.        
      
      # Selected flux for CO2
      if(length(exp_fitCO2)==6){
        if(fn$adjR2CO2_exp[k] > 0.2){
          if(fn$adjR2CO2_exp[k]>fn$adjR2CO2_lin[k]) {
            fn$CO2_flux[k]<-fn$CO2_exp_flux[k]
            fn$CO2_adjR2[k]<-fn$adjR2CO2_exp[k]
            fn$CO2_fit[k]<-"exp" }
          else {
            fn$CO2_flux[k]<-fn$CO2_lin_flux[k]
            fn$CO2_adjR2[k]<-fn$adjR2CO2_lin[k]
            fn$CO2_fit[k]<-"lin"
          }
        }
        else {
          fn$CO2_flux[k]<-fn$CO2_lin_flux[k]
          fn$CO2_adjR2[k]<-fn$adjR2CO2_lin[k]
          fn$CO2_fit[k]<-"lin"
        }
      }
      else {
        fn$CO2_flux[k]<-fn$CO2_lin_flux[k]
        fn$CO2_adjR2[k]<-fn$adjR2CO2_lin[k]
        fn$CO2_fit[k]<-"lin"
      }
      
      # Selected flux for CH4
      if(length(exp_fitCH4)==6){
        if(fn$adjR2CH4_exp[k] > 0.2){
          if(fn$adjR2CH4_exp[k]>fn$adjR2CH4_lin[k]) {
            fn$CH4_flux[k]<-fn$CH4_exp_flux[k]
            fn$CH4_adjR2[k]<-fn$adjR2CH4_exp[k]
            fn$CH4_fit[k]<-"exp" }
          else {
            fn$CH4_flux[k]<-fn$CH4_lin_flux[k]
            fn$CH4_adjR2[k]<-fn$adjR2CH4_lin[k]
            fn$CH4_fit[k]<-"lin"
          }
        }
        else {
          fn$CH4_flux[k]<-fn$CH4_lin_flux[k]
          fn$CH4_adjR2[k]<-fn$adjR2CH4_lin[k]
          fn$CH4_fit[k]<-"lin"
        }
      }
      else {
        fn$CH4_flux[k]<-fn$CH4_lin_flux[k]
        fn$CH4_adjR2[k]<-fn$adjR2CH4_lin[k]
        fn$CH4_fit[k]<-"lin"
      }
      
      # Selected flux for H2O
      if(length(exp_fitH2O)==6){
        if(fn$adjR2H2O_exp[k] > 0.2){
          if(fn$adjR2H2O_exp[k]>fn$adjR2H2O_lin[k]) {
            fn$H2O_flux[k]<-fn$H2O_exp_flux[k]
            fn$H2O_adjR2[k]<-fn$adjR2H2O_exp[k]
            fn$H2O_fit[k]<-"exp" }
          else {
            fn$H2O_flux[k]<-fn$H2O_lin_flux[k]
            fn$H2O_adjR2[k]<-fn$adjR2H2O_lin[k]
            fn$H2O_fit[k]<-"lin"
          }
        }
        else {
          fn$H2O_flux[k]<-fn$H2O_lin_flux[k]
          fn$H2O_adjR2[k]<-fn$adjR2H2O_lin[k]
          fn$H2O_fit[k]<-"lin"
        }
      }
      else {
        fn$H2O_flux[k]<-fn$H2O_lin_flux[k]
        fn$H2O_adjR2[k]<-fn$adjR2H2O_lin[k]
        fn$H2O_fit[k]<-"lin"
      }
      }
    
    LGR_fluxes<-rbind(LGR_fluxes,fn)
  }

  LGR_fluxes$Date<-strptime(LGR_fluxes$Date,format='%m/%d/%Y')  
  write.table(LGR_fluxes,file="C:/Users/barbafej/Dropbox/SJ_stem_fluxes/SJ_studies/SJ_full_paper/txt/LGR_fluxes.txt", sep="\t",row.names=F,col.names=T) 
   
