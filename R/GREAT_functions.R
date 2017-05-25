



#------------------------------------------------------------------------------------------------------------------
#- function to download the zipfile containing all of the data from the library at Western Sydney University 
get_zipdata <- function(){
  

  #- check if the data and output directories exist. If they don't, create them.
  dir.create(file.path("Data"),showWarnings=F)
  dir.create(file.path("Output"),showWarnings=F)
  
  #- define the file name and the URL to the data
  zipfn <- "Glasshouse_DRAKE_EUTE_local-adaptation.zip"
  url <-   "http://research-data.westernsydney.edu.au/redbox/verNum1.8-SNAPSHOT/default/detail/baa6e7b38113a59b8e9d2e6b0fb4009a/Glasshouse_DRAKE_EUTE_THERMAL-NICHE.zip"
  
  
  #- download the data, if there is no local copy
  failureFlag <- 0
  if(!file.exists(zipfn)){
    failureFlag <- try(download.file(url, zipfn, mode="wb"))
  }
  
  #- unzip the data
  unzip(zipfn, exdir="Data", overwrite=TRUE)
  
  #- print an informative error message if downloading fails
  if(failureFlag !=0){
    message("Download failed. Perhaps try downloading the data manually by pointing a web-browser to http://doi.org/10.4225/35/57e4bf22dd3ec")
  }
  
}
#------------------------------------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- function to read and return the most recent size measurements of height and diameter
getSize <- function(path="Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data"){
  
  #- work out the path
  
  #- find the most recent file
  files <-  list.files(paste(path,"",sep=""),pattern="HEIGHTDIAMETER",full.names=T)
  files2 <- files[grep("20160108-20160229_L2.csv",files)] # only get the level 2 .csv file
  #files2 <- files2[nchar(files2)==109]# only get files with a filename of 109 characters (ignores the eary file)
  
  # dates <- c()
  # for (i in 1:length(files2)){
  #   dates[i] <- as.numeric(substr(files2[i],start=100,stop=102)) # gets the month and date as a single number
  # }
  # 
  #- read data, plot size over time
  hddata <- read.csv(files2)
  hddata$prov_trt <- as.factor(paste(hddata$Prov,hddata$Room,sep="-"))
  hddata$Date <- as.Date(hddata$Date,format="%d/%m/%Y")
  
  
  #- Bd-78 was mistakenly recorded on 2016-01-28 
  hddata[which(hddata$Code=="Bd-78" & hddata$Date==as.Date("2016-01-28")),"D1"] <- 2.68
  hddata[which(hddata$Code=="Bd-78" & hddata$Date==as.Date("2016-01-28")),"D2"] <- 2.75
  
  hddata$diam <- with(hddata,((D1+D2)/2))
  hddata$d2h <- with(hddata,(diam/10)^2*Height) #cm^3

  
  #- assign drought treatments
  hddata$W_treatment <- factor(hddata$W_treatment,levels=c("w","d"))
  
  #- work out the air temperature and new provenance keys
  key <- data.frame(Room=1:6,Tair= c(18,21.5,25,28.5,32,35.5)) # could be improved with real data
  hddata2 <- merge(hddata,key,by="Room")
  
  key2 <- data.frame(Prov=as.factor(LETTERS[1:3]),location= factor(c("Cold-edge","Warm-edge","Central"),
                                                                   levels=c("Cold-edge","Central","Warm-edge")))
  hddata3 <- merge(hddata2,key2,by="Prov")
  

  
  return(hddata3)
}
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- function to read and process the leaf number and leaf size datasets
getLA <- function(path="Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data"){
  
  la <-read.csv(paste(path,"GHS39_GREAT_MAIN_LEAFAREA_20160128_L2_V1.csv",sep="/"))
  la$Date <- as.Date("2016-1-28")
  
  la2 <-read.csv(paste(path,"GHS39_GREAT_MAIN_LEAFAREA_20160209_L2_V1.csv",sep="/"))
  la2$Date <- as.Date("2016-2-09")
  
  la <- rbind(la,la2)
  
  la$Room <- as.factor(la$Room)
  la$prov_trt <- as.factor(paste(la$Prov,la$Room,sep="-"))
  
  #- assign drought treatments
  la$W_treatment <- factor(la$W_treatment,levels=c("w","d"))
  
  return(la)
}
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#- function to read and process the leaf punch datasets
getPunches <- function(path="Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data"){
  dat1 <-read.csv(paste(path,"GHS39_GREAT_MAIN_LEAFAREA-PUNCHES_20160129_L2.csv",sep="/"))
  dat1$Date <- as.Date("2016-1-29")
  
  dat2 <-read.csv(paste(path,"GHS39_GREAT_MAIN_LEAFAREA-PUNCHES_20160209_L2.csv",sep="/"))
  dat2$Date <- as.Date("2016-2-9")
  
  dat <- rbind(dat1,dat2)
  
  dat$SLA <- with(dat,Puncharea/(Punchmass/1000))         # in cm2 g-1
  dat$LMA <- with(dat,(Punchmass/1000)/(Puncharea/10000)) # in g m-2
  
  #dat$prov2 <- dat$prov
  #dat$prov <- as.factor(substr(dat$pot,start=1,stop=1)) # overwrite "prov" to be A, B, or C. No Bw or Bd allowed.
  dat$Room <- as.factor(dat$Room)
  dat$prov_trt <- as.factor(paste(dat$Prov,dat$Room,sep="-"))
  
  #- assign drought treatments
  dat$W_treatment <- factor(dat$W_treatment,levels=c("w","d"))
  return(dat)
}  
#-----------------------------------------------------------------------------------------  



#-----------------------------------------------------------------------------------------
#- function to compile and return the three estimates of SLA from the GREAT experiment.
#  (1) leaf punches on 2016-1-29 and 2016-2-9,
#  (2) gas exchange leaves harvested on 2016-2-11 and 2016-2-29,
#  (3) whole-crown average SLA obtained by harvesting (total leaf area / total leaf mass)
#  Returns a list of these three elements
getSLA <- function(path="Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data"){
  
  #- get the punches
  punches <- getPunches()
  
  #---
  #- get the gas exchange leaves
  #- merge in the leaf mass data
  leaf1 <- read.csv(file=paste(path,"GHS39_GREAT_MAIN_GX-LEAVES_20160211_L2.csv",sep="/"))
  leaf2 <- read.csv(file=paste(path,"GHS39_GREAT_MAIN_GX-LEAVES_20160229_L2.csv",sep="/"))
  leaf1$Comment <- NULL
  leaf <- rbind(leaf1,leaf2)
  
  leaf$SLA <- with(leaf,Leafarea/(Leafmass))         # in cm2 g-1
  leaf$LMA <- with(leaf,(Leafmass)/(Leafarea/10000)) # in g m-2
  #names(leaf)[2] <- tolower(names(leaf)[2])
  
  # #- merge in a "key" from the punches dataset 
  # key <- subset(punches,Date==as.Date("2016-01-29"))[,c("Room","Prov","Code","W_treatment")]
  # potnumber <- unlist(strsplit(as.character(key$pot),split="-"))[2*(1:length(strsplit(as.character(key$pot),split="-")))  ]
  # potnumber2 <- sprintf("%02d",as.numeric(potnumber))
  # key$Pot <- key$pot
  # key$pot <- paste(key$prov2,potnumber2,sep="-")
  # 
  # leaf3 <- merge(leaf,key,by=("pot"))
  # leaf3$prov2 <- leaf3$Pot <- NULL
  # leaf3$Prov <- as.factor(substr(as.character(leaf3$Prov),start=1,stop=1))
  # names(leaf3)[2] <- tolower(names(leaf3)[2])
  #---
  
  
  #- get the harvest dataset
  harvest <- getHarvest()
  harvest$SLA <- with(harvest,leafarea/leafdm)           # in cm2 g-1
  harvest$LMA <- with(harvest,leafdm/(leafarea/10000))   # in g m-2
  harvest <- subset(harvest,Date != as.Date("2016-01-07")) # exclude the pre-planting harvest (SLA and LMA very different!)
  #harvest2 <- merge(harvest,key,by="Pot")
  return(list(punches,leaf,harvest)) # return a list of the three kinds of SLA measurements
}
#-----------------------------------------------------------------------------------------



  
#-----------------------------------------------------------------------------------------
#- function to read and process the Asat and AQ datasets
getAQ <- function(path="Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data"){
  
  #- read in the first set of measurements
  aq1 <-read.csv(paste(path,"GHS39_GREAT_MAIN_GX-AQ_20160202-20160203_L2.csv",sep="/"))
  aq1$campaign = 1
  
  #- read in the second set (prov B only!)
  aq2 <-read.csv(paste(path,"GHS39_GREAT_MAIN_GX-AQ_20160225-20160226_L2.csv",sep="/"))
  aq2$campaign = 2

  aq <- rbind(aq1,aq2)
  #names(aq)[1:2] <- tolower(names(aq)[1:2])
  #aq$prov <- as.factor(substr(aq$pot,start=1,stop=1))
  #aq$room <- as.factor(aq$room)
  aq$prov_trt <- as.factor(paste(aq$Prov,aq$Room,sep="-"))

  #- assign drought treatments
  #aq$Water_trt <- "wet"
  #aq$Water_trt[grep("Bd",aq$pot)] <- "dry"
  aq$W_treatment <- factor(aq$W_treatment,levels=c("w","d"))
  
  #- assign light levels to a factor variable
  aq$LightFac <- NA
  aq$LightFac[which(aq$PARi<150)] <- 1
  aq$LightFac[which(aq$PARi>400 & aq$PARi<600)] <- 2
  aq$LightFac[which(aq$PARi>800 & aq$PARi<1100)] <- 3
  aq$LightFac[which(aq$PARi>1200)] <- 4
  aq$LightFac <- as.factor(aq$LightFac)
  
  #- a few pots have strange data. UWS3 didn't seem to actually drop to low PARi on three cases.
  #-   so remove those bad data. This should remove 3 datapoints!
  aq <- aq[complete.cases(aq),]
  
  #- assign the temperature levels
  aq$TleafFac <- cut(aq$Tleaf,breaks=c(15,22,26,29,34,37,45),labels=1:6)
  
  #- merge in the location factor
  key2 <- data.frame(Prov=as.factor(LETTERS[1:3]),location= factor(c("Cold-edge","Warm-edge","Central"),
                                                                   levels=c("Cold-edge","Central","Warm-edge")))
  aq2 <- merge(aq,key2,by="Prov")
  
  #- average across replicate logs
  aq.means <- summaryBy(.~Room+Pot+Unit+Prov+prov_trt+location+W_treatment+LightFac+TleafFac+campaign,data=aq2,FUN=mean,keep.names=T)
  
  #- remove a crazy datapoint
  torm <- which(aq.means$prov_trt=="A-3" & round(aq.means$CTleaf,2)==28.75)
  aq.means[torm,"Photo"] <- NA 
  
  return(aq.means)
}
#-----------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#- function to read the soil moisture data
getVWC_AQ <- function(path="Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data"){
  
  #- read in the data
  vwc <- read.csv(paste(path,"/Share/Data/GHS39_GREAT_MAIN_SOILVWC_hydrosense_L1.csv",sep=""))
  names(vwc)[1:3] <- tolower(names(vwc)[1:3])
  vwc$prov <- as.factor(substr(vwc$treat,start=1,stop=1))
  vwc$room <- as.factor(vwc$room)
  vwc$prov_trt <- as.factor(paste(vwc$prov,vwc$room,sep="-"))
  
  #- fix up the "pot" variable to be the same as in the AQ datasets
  vwc$pot <- paste(vwc$treat,sprintf("%02d",vwc$pot),sep="-")
  
  #- assign drought treatments
  vwc$Water_trt <- "wet"
  vwc$Water_trt[grep("Bd",vwc$treat)] <- "dry"
  vwc$Water_trt <- factor(vwc$Water_trt,levels=c("wet","dry"))
  
  #- average across sub-replicate measurements
  vwc.m <- summaryBy(VWC1~pot+Water_trt,data=vwc,FUN=mean,keep.names=T,na.rm=T)
  names(vwc.m)[ncol(vwc.m)] <- "vwc"
  
  return(vwc.m)
}

#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- function to read and process the temperature response curves of photosynthesis
getAvT <- function(path="Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data"){
  
  avt <-read.csv(paste(path,"GHS39_GREAT_MAIN_GX-AVT_20160205_L2.csv",sep="/"))
  avt$Room <- as.factor(avt$Room)
  avt$prov_trt <- as.factor(paste(avt$Prov,avt$Room,sep="-"))
  
  #- assign drought treatments
  avt$W_treatment <- factor(avt$W_treatment,levels=c("w","d"))
  
  #- assign light levels to a factor variable
  avt$LightFac <- NA
  avt$LightFac[which(avt$PARi<150)] <- 1
  avt$LightFac[which(avt$PARi>1200)] <- 4
  avt$LightFac <- as.factor(avt$LightFac)
  
  #- assign the temperature levels
  avt$TleafFac <- cut(avt$Tleaf,breaks=c(15,22,26,29,34,37,45),labels=1:6)
  
  key2 <- data.frame(Prov=as.factor(LETTERS[1:3]),location= factor(c("Cold-edge","Warm-edge","Central"),
                                                                   levels=c("Cold-edge","Central","Warm-edge")))
  avt2 <- merge(avt,key2,by="Prov")
  
  #avt3 <- summaryBy(.~Room+Code+Unit+Prov+prov_trt+W_treatment+LightFac+TleafFac,data=avt2,FUN=mean,keep.names=T)
  return(avt2)
}
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#- function to read and process the temperature response curves of respiration
getRvT <- function(path="Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data"){
  
  rvt <-read.csv(paste(path,"GHS39_GREAT_MAIN_GX-RDARK_20160211_L3.csv",sep="/"))
  #rvt$prov <- as.factor(substr(rvt$pot,start=1,stop=1))
  #rvt$room <- as.factor(rvt$room)
  rvt$prov_trt <- as.factor(paste(rvt$Prov,rvt$Room,sep="-"))
  rvt$Rarea <- rvt$Photo*-1
  
  #- assign drought treatments
  rvt$W_treatment <- factor(rvt$W_treatment,levels=c("w","d"))
  
  #- assign the temperature levels
  rvt$TleafFac <- cut(rvt$Tleaf,breaks=c(10,15,20,25,27.5,35),labels=1:5)
  
  #- average across sub-replicate logs
  rvt2 <- summaryBy(.~Room+Pot+Prov+Code+prov_trt+W_treatment+TleafFac,data=rvt,FUN=mean,keep.names=T)
  #rvt2$Pot <- as.factor(paste(toupper(substr(as.character(rvt2$pot),start=1,stop=1)),substr(as.character(rvt2$pot),start=2,stop=5),sep=""))
  
  #- got them all? We're missing the fourth temperature for bw-26.
  xtabs(~Code,data=rvt2)
  
  #- merge in the leaf mass data
  leaf1 <- read.csv(file=paste(path,"GHS39_GREAT_MAIN_GX-LEAVES_20160211_L2.csv",sep="/"))
  leaf2 <- read.csv(file=paste(path,"GHS39_GREAT_MAIN_GX-LEAVES_20160229_L2.csv",sep="/"))
  leaf1$Comment <- NULL
  leaf <- rbind(leaf1,leaf2)
  #leaf$Code <- as.factor(leaf$Pot)
  #leaf$comment <- NULL
  leaf <- leaf[,c("Code","Leafarea","Leafmass")]
  
  rvt3 <- merge(rvt2,leaf,by=("Code"))

  #- calculate mass-specific leaf respiration rates
  rvt3$Rmass <- with(rvt3,Rarea*Leafarea/Leafmass/10000*1000) #- convert umol Co2 m-2 s-1 to nmol CO2 g-1 s-1
  
  #- merge in the location factor
  key2 <- data.frame(Prov=as.factor(LETTERS[1:3]),location= factor(c("Cold-edge","Warm-edge","Central"),
                                                                   levels=c("Cold-edge","Central","Warm-edge")))
  
  rvt4 <- merge(rvt3,key2,by="Prov")
  
  return(rvt4)
}
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- function to get the confidence intervals of an NLS fit
#https://quantitativeconservationbiology.wordpress.com/2013/07/02/confidence-interval-for-a-model-fitted-with-nls-in-r/

as.lm.nls <- function(object, ...) {
  if (!inherits(object, "nls")) {
    w <- paste("expected object of class nls but got object of class:", 
               paste(class(object), collapse = " "))
    warning(w)
  }
  
  gradient <- object$m$gradient()
  if (is.null(colnames(gradient))) {
    colnames(gradient) <- names(object$m$getPars())
  }
  
  response.name <- if (length(formula(object)) == 2) "0" else 
    as.character(formula(object)[[2]])
  
  lhs <- object$m$lhs()
  L <- data.frame(lhs, gradient)
  names(L)[1] <- response.name
  
  fo <- sprintf("%s ~ %s - 1", response.name, 
                paste(colnames(gradient), collapse = "+"))
  fo <- as.formula(fo, env = as.proto.list(L))
  
  do.call("lm", list(fo, offset = substitute(fitted(object))))
  
}
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- function to fit the June et al. (2004) FPB model for the temperature response of photosynthesis.
#- accepts a dataframe, returns a list with [1] named vector of parameter estiamtes and their se's,
#-   and [2] a dataframe with the predictions and 95% confidence intervals.
fitAvT <- function(dat){
  try(A_Topt <- nls(Photo~ Jref*exp(-1*((Tleaf-Topt)/theta)^2),data=dat,start=list(Jref=20,Topt=25,theta=20)))
  A_Topt2 <- summary(A_Topt)
  results <- A_Topt2$coefficients[1:6]
  names(results)[1:6] <- c("Aref","Topt","theta","Aref.se","Topt.se","theta.se")
  
  #- merge 95% CI into results dataframe
  confinterval <- confint(A_Topt)
  CI1 <- (sprintf("%.2f",round(unname(confinterval[1,]),2)))
  CI1a <- paste(CI1[1],CI1[2],sep="-")
  CI2 <- (sprintf("%.2f",round(unname(confinterval[2,]),2)))
  CI2a <- paste(CI2[1],CI2[2],sep="-")
  CI3 <- (sprintf("%.2f",round(unname(confinterval[3,]),2)))
  CI3a <- paste(CI3[1],CI3[2],sep="-")
  confinterval2 <- c(CI1a,CI2a,CI3a)
  
  
  
  TT <- seq(min(dat$Tleaf),max(dat$Tleaf),length=51)
  predicts <- predictNLS(A_Topt, newdata=data.frame(Tleaf = TT),interval="confidence",level=0.95)
  predicts.df <- data.frame(predicts$summary)
  predicts.df$Tleaf <- TT
  
  return(list(results,predicts.df,confinterval2))
}
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- function to fit a Q10 model of respiration
#- accepts a dataframe, returns a list with [1] named vector of parameter estiamtes and their se's,
#-   and [2] a dataframe with the predictions and 95% confidence intervals.
fitRvT <- function(dat,namex=Tleaf,namey=Rmass,lengthPredict=20,start=list(Rref=10,Q10=2)){
  
  dat$Yvar <- dat[[namey]]
  dat$Xvar <- dat[[namex]]
  
  try(R_Topt <- nls(Yvar~  Rref*Q10^((Xvar-22)/10),data=dat,start=start))
  R_Topt2 <- summary(R_Topt)
  results <- R_Topt2$coefficients[1:4]
  
  
  names(results)[1:4] <- c(paste(namey,"ref",sep=""),"Q10",paste(namey,"ref.se",sep=""),"Q10.se")
  
  
  #- merge 95% CI into results dataframe
  confinterval <- confint2(R_Topt)
  CI1 <- (sprintf("%.2f",round(unname(confinterval[1,]),2)))
  CI1a <- paste(CI1[1],CI1[2],sep="-")
  CI2 <- (sprintf("%.2f",round(unname(confinterval[2,]),2)))
  CI2a <- paste(CI2[1],CI2[2],sep="-")
  confinterval2 <- c(CI1a,CI2a)
  
  TT <- seq(min(dat$Tleaf),max(dat$Tleaf),length=lengthPredict)
  predicts <- predictNLS(R_Topt, newdata=data.frame(Xvar = TT),interval="confidence",level=0.95)
  predicts.df <- data.frame(predicts$summary)
  predicts.df$Tleaf <- TT
  
  return(list(results,predicts.df,confinterval2))
}
#-----------------------------------------------------------------------------------------










#-----------------------------------------------------------------------------------------
#-  A generic function to fit temperature response curves based on June 2004 FPB.
#-  The function accepts a dataframe and the quoted names of the x and y variables
#   in the dataframe. Returns a list with a named vector of parameter estimates and their se's,
#-   and a dataframe with the predictions and 95% confidence intervals.
fitJuneT <- function(dat,namex=Tleaf,namey,lengthPredict=21,start=list(Rref=25,Topt=20,theta=20)){
  dat$Yvar <- dat[[namey]]
  dat$Xvar <- dat[[namex]]
  
  try(G_Topt <- nls(Yvar~ Rref*exp(-1*((Xvar-Topt)/theta)^2),data=dat,start=start))
  G_Topt2 <- summary(G_Topt)
  results <- G_Topt2$coefficients[1:6]
  names(results)[1:6] <- c(paste(namey,"ref",sep=""),"Topt","theta",paste(namey,"ref.se",sep=""),"Topt.se","theta.se")
   
  #- merge 95% CI into results dataframe
  try(confinterval <- confint2(G_Topt))
  CI1 <- (sprintf("%.2f",round(unname(confinterval[1,]),2)))
  CI1a <- paste(CI1[1],CI1[2],sep="-")
  CI2 <- (sprintf("%.2f",round(unname(confinterval[2,]),2)))
  CI2a <- paste(CI2[1],CI2[2],sep="-")
  CI3 <- (sprintf("%.2f",round(unname(confinterval[3,]),2)))
  CI3a <- paste(CI3[1],CI3[2],sep="-")
  confinterval2 <- c(CI1a,CI2a,CI3a)
  
  #- predict the response variable across the full range of x-values
  TT <- seq(min(dat$Xvar),max(dat$Xvar),length=lengthPredict)
  predicts <- predictNLS(G_Topt, newdata=data.frame(Xvar = TT),interval="confidence",level=0.95,verbose=FALSE)
  predicts.df <- data.frame(predicts$summary)
  predicts.df$Tleaf <- TT
  
  return(list(results,predicts.df,confinterval2))
}
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#- function to return the harvest data as a dataframe
getHarvest <- function(path="Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data"){

    #- read in the raw data
  files <-  list.files(paste(path,"",sep=""),pattern="GHS39_GREAT_MAIN_BIOMASS",full.names=T)
  files2 <- files[grep(".csv",files)] # only get the .csv files
  files3 <- files2[grep("L2",files2)] # only get the level 2 files
  
  #- pull out the longer file name that has a different structure
  longfile <- files3[which.max(nchar(files3))]
  files4 <- files3[-which.max(nchar(files3))]
  
  dates <- c()
  dat.i <- list()
  for (i in 1:length(files4)){
    dat.i[[i]] <- read.csv(files4[i])
    
    if(i==1) dat.i[[i]]$W_treatment <- "w"
    
    if(i==1) dat.i[[i]]$Height <- dat.i[[i]]$Height/10 # on the first date, height was recorded in mm, rather than cm
    
    
    dat.i[[i]] <- dat.i[[i]][,c("Prov","Pot","W_treatment","Code","Height","D1","D2","Leafarea","Leafno","Leafmass","Stemmass","Rootmass")]
    dates[i] <- as.numeric(substr(files4[i],start=72,stop=79)) # extract the date from the file name
    dat.i[[i]]$Date <- base::as.Date(as.character(dates[i]),format="%Y%m%d")
    
    
    names(dat.i[[i]]) <- c("Prov","Pot","W_treatment","Code","h","d1","d2","leafarea","leafno","leafdm","stemdm","rootdm","Date")
  }
  dat <- do.call(rbind,dat.i)
  #dat$prov <- as.factor(substr(dat$Pot,start=1,stop=1))
  
  #- read in the longer datafile, fix up names for merging
  dat.long <- read.csv(longfile)
  dat.long$Date <- base::as.Date("2016-02-22")
  
  
  #- calculate total leaf and root dry mass
  dat.long$leafdm <- base::rowSums(dat.long[,c("Leafmass_sub","Leafmass")],na.rm=T)
  dat.long$rootdm <- base::rowSums(dat.long[,c("Rootmass_sub","Rootmass")],na.rm=T)
  

  dat.long <- dat.long[,c("Prov","Pot","W_treatment","Code","Height","D1","D2","Leafarea","Leafno",
                          "leafdm","Stemmass","rootdm","Date")]
  names(dat.long) <- c("Prov","Pot","W_treatment","Code","h","d1","d2","leafarea","leafno","leafdm","stemdm","rootdm","Date")
  #dat.long$prov <- as.factor(substr(dat.long$Pot,start=1,stop=1))
  
  #- these mass values are in mg. Convert to g.
  dat.long$leafdm <- dat.long$leafdm/1000
  dat.long$stemdm <- dat.long$stemdm/1000
  dat.long$rootdm <- dat.long$rootdm/1000
  
  
  dat <- rbind(dat,dat.long)
  #----------------------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------------------
  #- Do some data manipulation
  
  #- height was measured in mm for the first dataset. convert to cm
  #firsts <- which(dat$Date == as.Date("2016-01-07"))
  #dat[firsts,"h"] <- dat[firsts,"h"]/10
  
  #- do some simple math
  dat$diam <- base::rowMeans(cbind(dat$d1,dat$d2))
  dat$d2h <- with(dat,h*(diam/10)^2) # calculates in units of cubic centimeters
  dat$totdm <- base::rowSums(cbind(dat$leafdm,dat$stemdm,dat$rootdm))
  
  #- total leaf area was recorded incorrectly for C-33. It should be 1079, not 107.9 cm2. 
  dat[which(dat$Code=="C-33"),"leafarea"] <- 1079
  
  
  #- add a variable for water limitaiton treatment
  #dat$Water_trt <- "wet"
  #dat$Water_trt[grep("Bd",dat$Pot)] <- "dry"
  #dat$Water_trt <- factor(dat$Water_trt,levels=c("wet","dry"))
  
  
  #- log transform (base 10!)
  dat$logd2h <- log10(dat$d2h)
  dat$logtotdm <- log10(dat$totdm)
  
  
  key2 <- data.frame(Prov=as.factor(LETTERS[1:3]),location= factor(c("Cold-edge","Warm-edge","Central"),
                                                                   levels=c("Cold-edge","Central","Warm-edge")))
  dat2 <- merge(dat,key2,by="Prov")
  return(dat2)
}
#----------------------------------------------------------------------------------------------------------------




#- function to return an estimated mass based on tree size (d2h). Takes a vector of d2h (cm3), returns
#-  a vector of estimated total mass (g). Can later be expanded to include other predictors (provenance, etc),
#-  and other predictors (total leaf area, for example). Set droughtdat to F to exclude the water-limited provs
returnMassFromAllom <- function(d2hdat,plotson=T,droughtdat=F){
  
  #- get the harvest data
  dat <- getHarvest()
  
  if (droughtdat==F) {
    dat <- subset(dat,W_treatment=="w")
  }
  
  #----------------------------------------------------------------------------------------------------------------
  #- model the allometry, return predicted mass for the vector of d2h values
  lm1 <- lm(logtotdm~logd2h+I(logd2h^2),data=dat)
  predictions <- 10^predict(lm1,newdata=data.frame(logd2h=log10(d2hdat)))
  #----------------------------------------------------------------------------------------------------------------
  
  
  #----------------------------------------------------------------------------------------------------------------
  #- how many of the observations lie outside of the allometry?
  toolow <- which(d2hdat < min(dat$d2h))
  toohigh <- which(d2hdat > max(dat$d2h))
  
  outsides <- sum(length(toolow),length(toohigh))
  total <- length(d2hdat)
  percentage <- round(outsides/total*100,1)
  #print(paste(outsides," observations of ",total," outside of allometry (",percentage,"%)",sep=""))
  #----------------------------------------------------------------------------------------------------------------
  
  
  #----------------------------------------------------------------------------------------------------------------
  #- exploratory plotting
  if (plotson==T){
    palette(rev(brewer.pal(6,"Spectral")))
    COL=palette()[c(1,2,6)]
    #windows(12,12)
    pdf(file="output/FigureS2-Allometry.pdf",width=7.3,height=8)
    par(mar=c(6,6,1,1),cex.axis=1.2,cex.lab=2)
    plotBy(logtotdm~logd2h|location,data=dat,pch=16,xlab="",ylab="",axes=F,legend=F,col=COL)
    # legend(x=-1.2,y=1.1,legend=c("Cold-edge","Central","Warm-edge"),col=COL,
    #         title="Provenance",pch=16,cex=1.5,bg="white")
    legend("topleft",c("Cold-origin","Central","Warm-origin"),pch=16,col=COL,cex=1.2,title="Provenance",bty="n")
    coefs <- coef(lm1)
    xval <- seq(min(dat$logd2h),max(dat$logd2h),length=101)
    preds <- coefs[1]+xval*coefs[2]+xval^2*coefs[3]
    lines(preds~xval)
    magaxis(side=1:4,labels=c(1,1,0,0),las=1)
    title(xlab=expression(log[10]~(Plant~size~";"~d^2*h~";"~cm^3)),
          ylab=expression(log[10]~(Total~mass~";"~g)))
    
    dev.off()
 
  }
  #----------------------------------------------------------------------------------------------------------------
  
  #if(is.na(d2hdat)==T) dev.copy2pdf(file="output/FigureS3-Allometry.pdf")
  if(is.na(d2hdat)==F) return(predictions)
  
}




#- function to caculate and return growth metrics. Returns a list of two dataframes:
#     [1] RGR and AGR caculated for all available data.
#     [2] RGT and AGR merged with canopy leaf area and SLA for the intensive growth interval only
returnRGR <- function(path="Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data",plotson=F){
  
  #-----------------------------------------------------------------------------------------
  #- get the size data, estimate mass
  
  hddata <- getSize(path=path) 
  hddata$totmass <- returnMassFromAllom(d2hdat=hddata$d2h,plotson=plotson,droughtdat = F) #- predict mass from d2h allometry
  hddata <- hddata[with(hddata,order(W_treatment,Code,Date)),]
  hddata <- hddata[complete.cases(hddata),]                  # remove missing data
  hddata <- subset(hddata,W_treatment=="w")                  # remove drought data
  hddata$Code <- factor(hddata$Code)
  #-----------------------------------------------------------------------------------------
  
  
  
  #-----------------------------------------------------------------------------------------
  #- calculate RGR and AGR based on the estimated total mass
  hddata <- hddata[with(hddata,order(Code,Date)),]
  
  hddata.l <- split(hddata,hddata$Code)
  for(i in 1:length(hddata.l)){
    #print(i)
    crap <- hddata.l[[i]]
    hddata.l[[i]]$RGR <- c(NA,diff(log(hddata.l[[i]]$totmass)))/c(NA,diff(hddata.l[[i]]$Date)) # g g-1 day-1
    hddata.l[[i]]$AGR <- c(NA,diff(hddata.l[[i]]$totmass))/c(NA,diff(hddata.l[[i]]$Date))      # g day-1
    
  }
  hddata2 <- do.call(rbind,hddata.l)
  
  
  #- pull out the most important agr and rgr estimates (during our growth interval from Jan 28th to Feb 8th)
  rgrinterval1 <- subset(hddata2,Date %in% as.Date(c("2016-01-28","2016-02-08")))
  rgrinterval <- rgrinterval1[complete.cases(rgrinterval1),] # remove missing data
  #-----------------------------------------------------------------------------------------
  
  #------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------
  #- merge in the measurements of total plant leaf area to the RGR interval
  
  #- process the total plant leaf area data
  la <- getLA()
  
  #- calculate total plant leaf area. This method uses a different average leaf size for each plant
  la$canopy <- with(la,Leafno*Leafarea)
  
  
  #----------------------------------------------------------------------------------------------------
  #- calculate total plant leaf area using a room and date-specific mean value
  leaf_size <- summaryBy(Leafarea~Room+Leafsize+Date,data=la,FUN=mean,keep.names=F,na.rm=T)
  la2.1 <- merge(la,leaf_size,by=c("Room","Leafsize","Date"))
  la2.1$canopy2 <- with(la2.1,Leafno*Leafarea.mean)
  
  
  #-- average leaf size is temperature dependent, but not provenance dependent
  if(plotson==T){
    windows(40,50);par(mfrow=c(3,1),cex.lab=1.7,mar=c(7,7,2,1))
    boxplot(Leafno~Leafsize+Room,data=subset(la,W_treatment=="w" & Date==as.Date("2016-01-28")),las=2,ylab="Leaf number (n)",col=c("grey","red"))
    legend("topleft",c("small","large"),fill=c("grey","red"))
    boxplot(Leafarea~Leafsize+Room,data=subset(la,W_treatment=="w"& Date==as.Date("2016-01-28")),las=2,ylab="Average leaf size (cm2)",col=c("grey","red"))
    boxplot(canopy~Prov+Room,data=subset(la,W_treatment=="w" & Date==as.Date("2016-01-28")),las=2,ylim=c(0,1000),ylab="Total canopy size (cm2)",col=c("blue","orange","forestgreen"))
    legend("topleft",c("A","B","C"),fill=c("blue","orange","forestgreen"))
    
  }
  
  la2 <- summaryBy(canopy+canopy2~Room+Code+Prov+prov_trt+Date+W_treatment,data=la2.1,FUN=sum,keep.names=T)
  
  #- merge in total plant leaf number
  leaf_no <-summaryBy(Leafno~Code,data=la,FUN=sum,keep.names=T)
  la2 <- merge(la2,leaf_no,by="Code")
  la2$Date[which(la2$Date==as.Date("2016-02-09"))] <- as.Date("2016-02-08") # fix dates
  
  ##- average the leaf area data across the two dates, which bracket the RGR growth interval
  # no longer needed?
  la.m <- summaryBy(canopy+canopy2+Leafno~Room+Code+Prov+prov_trt+W_treatment,data=la2,FUN=mean,keep.names=T)
  
  #- average teh leaf area data, but keep the two dates separate! Changed on 12 July 2016.
  #- This gives an estimate of teh initial canopy size, at teh beginning of the interval.
  la.init <- summaryBy(canopy~Code,
                       data=subset(la2,Date==as.Date("2016-01-28")),FUN=mean,keep.names=T)[,c("Code","canopy")]
  names(la.init)[2] <- "canopy.init"
  
  #- merge total plant leaf area with tree size from the interval measurements
  la3 <- merge(la2,rgrinterval1,by=c("Room","Prov","prov_trt","Code","W_treatment","Date"))
  la3 <- la3[complete.cases(la3),]
  la3$logLA <- with(la3,log10(canopy))
  la3$logd2h <- with(la3,log10(d2h))
  
  la3 <- merge(la3,la.init,by="Code")
  
  #- plot log-log relation between d2h and leaf area, compare to allometry from GLAHD
  # windows()
  # plotBy(logLA~logd2h|Date,data=la3)
  # abline(a=1.889,b=0.7687) # allometry from GLAHD
  #--------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  #--------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------
  #---- get the SLA data from punches. SLA increase with temperature, to a point.
  sla1 <- getPunches()
  sla1$Date <- sla1$Date-1 # fix up the dates
  
  sla <- summaryBy(SLA+LMA~Room+Prov+Code+prov_trt+W_treatment+Date,FUN=mean,keep.names=T,data=sla1)
  rgrdat <- merge(la3,sla,by=c("Room","Prov","Code","prov_trt","W_treatment","Date"))

  
  
  
  #--------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------
  #- okay, so NOW we have a dataframe with all the bits in it for a formal RGR analysis. 
  #    We have growth, canopy leaf area, and SLA. These were measured INDEPENDENTLY.
  #head(rgrdat)
  rgrdat$logmass <- log10(rgrdat$totmass)
  
  #- calculate LAR (m2 kg-1)
  rgrdat$LAR <- with(rgrdat,canopy/10000/(totmass/1000))
  
  #- calculate leaf mass fraction (g g-1)
  rgrdat$LMF <- with(rgrdat,canopy/SLA/totmass)
  
  #- calculate net assimilation rate (NAR; g m-2 d-1) from absolute growth rate and total canopy leaf area
  rgrdat$NAR <- with(rgrdat,AGR/(canopy/10000))
  
  
  #- plot interrelationships
  if(plotson==T){
    windows()
    pairs(subset(rgrdat,W_treatment=="w")[,c("RGR","AGR","LAR","LMF","NAR","canopy","SLA","logmass","Room")],
          col=rev(brewer.pal(6,"RdYlGn"))[subset(rgrdat,W_treatment=="w")$Room])
  }
  
  
  #- get rid of some unwanted variabels to make things simpler
  rgrdat$canopy2 <- rgrdat$D1 <- rgrdat$D2 <- rgrdat$Comment <- rgrdat$Leafno <- NULL
  
  
  #- work out the air temperature and new provenance keys
  key <- data.frame(Room=1:6,Tair= c(18,21.5,25,28.5,32,35.5)) # could be improved with real data
  #hddata3 <- merge(hddata2,key,by="Room")
  rgrdat2 <- merge(rgrdat,key,by=c("Room","Tair"))
  
  #---------------------
  #--- put final dataset together
  
  #- AGR and RGR need to be extracted from the second date only (they were calculated by difference)
  rgrdat3 <- subset(rgrdat2,Date==as.Date("2016-02-08"))[,c("Code","AGR","RGR")]
  #- average RGR metrics across dates to provide interval averaged estimates
  rgrdat2.m <- summaryBy(LAR+NAR+LMF+SLA+canopy.init~Room+Tair+Prov+Code+W_treatment+location,
                         data=rgrdat2,FUN=mean,keep.names=T)
  
  rgrdat4 <- merge(rgrdat3,rgrdat2.m,by="Code")
  #---------------------
  #key2 <- data.frame(Prov=as.factor(LETTERS[1:3]),location= factor(c("Cold-edge","Warm-edge","Central"),
  #                                                                 levels=c("Cold-edge","Central","Warm-edge")))
 # hddata4 <- merge(hddata3,key2,by=c("Prov","location"))
  
  return(list(hddata2,rgrdat4,rgrdat2))


  
}
#--------------------------------------------------------------------------------------------------------------------







#--------------------------------------------------------------------------------------------------------------------
#-- function to interpret 2nd order log-polynomial curve fits
#--------------------------------------------------------------------------------------------------------------------
output.log_lin <- function(X, Y, params, times,Code){
  a <- params[1]; b <- params[2]; c <- params[3]
  #fitted <- (M0^(1-beta)   + r  * X *(1-beta))^(1/(1-beta))
  fitted <- exp(a + b*X + c*X^2)
  resid  <- Y - fitted
  data <- data.frame(time=X,observed = Y, fitted = fitted, resid = resid)
  #eq   <- bquote(paste((.(round(r * (1-beta), 3))*t)^.(round(1/(1-beta), 2))))
  eq <- bquote(a+bx+cx^2)
  mss  <- sum((fitted - mean(fitted))^2)
  rss  <- sum(resid^2)
  R2   <- mss/(mss + rss)
  rmse <- sqrt(rss)
  N <- length(X)
  logLik <- -N * (log(2 * pi) + 1 - log(N) + log(sum(resid^2)))/2
  AIC  <- -2 * logLik  + 2 * 3 # three parameters
  summary <- c(R2 = R2, AIC = AIC, RMSE = rmse)
  temp <- a + b*X + c*X^2 # fix from here down
  rates = data.frame(
    times = times,
    M    =  exp(a+b*times+c*times^2))                  # from Hunt- Plant Growth Curves
  rates$AGR  =  with(rates,M*(b+2*c*times))            # from Hunt- Plant Growth Curves
  rates$RGR <- rates$AGR/rates$M
  rates$Code <- Code
  out <- list(params = params[-4], summary = summary, equation = eq, data = data, rates = rates)
  return(out)
}
#--------------------------------------------------------------------------------------------------------------------





#--------------------------------------------------------------------------------------------------------------------
#- Function to plot map of Australia with circles showing points with measured provenances
#--------------------------------------------------------------------------------------------------------------------
plotAussie <- function(path="Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data",export=F){
  
  #read in provenance locations
  all.loc <- read.csv(paste(path,"GHS30_PCS_BIOG_PROV-LOCATIONS.csv",sep="/"))
  Eute.loc <- subset(all.loc,sp=="t")
  Eute.loc2 <- subset(Eute.loc,seedlot %in% c(17770,18589,20352))
  
  #- ala data?
  Eute.all <- read.csv(paste(path,"GHS30_GREAT_MAIN_EUTE-ALA.csv",sep="/"))
  
  #- remove mediana subspecies?
  #medianas <- grep("mediana",Eute.all$Subspecies...matched)
  #Eute.all1.5 <- Eute.all[-medianas,]
 
  
  Eute.all2 <- data.frame(Eute.all$Latitude...processed,Eute.all$Longitude...processed)
  names(Eute.all2) <- c("lat","long")
  Eute.all2 <- subset(Eute.all2,long>139&lat< -10.5)
  coordinates(Eute.all2) <- c("long","lat") #make Eute.all2 into a spatial points dataframe
  projection(Eute.all2) <- CRS('+proj=longlat') #give it a projection (from http://cran.at.r-project.org/web/packages/dismo/vignettes/sdm.pdf)
  
  # circles with a radius of 50 km
  
  x <- circles(Eute.all2, d=50000, lonlat=TRUE) #create circles around each point with a radius of 50km. This takes a long time
  #save(x,file="./output/Eutecircles")
  #load(file="./output/Eutecircles")
  pol <- gUnaryUnion(x@polygons) #"dissolve" the circles into each other
  
  
  
  
  #Tereticornis
  #create and export map with seed locations
  #windows(20,40);par(oma=c(5,5,1,1))
  map("worldHires", regions="australia", bg="white", fill=F, xlim=c(141.5, 155), ylim=c(-40, -10),mar=c(4,5,1,1),myboarder=0) 
  plot(pol,add=T,col="grey") #plot the joined circles\
  axis(2,line=0.5,at=c(-45,-35,-25,-15,0),cex.axis=1)
  axis(1,line=0.5,at=c(140,145,150,155),cex.axis=1)
  
  #clip to the sea boarder
  outline <- map("worldHires", regions="Australia", exact=TRUE, plot=FALSE) # returns a list of x/y coords
  xrange <- range(outline$x, na.rm=TRUE) # get bounding box
  yrange <- range(outline$y, na.rm=TRUE)
  xbox <- xrange + c(-2, 2)
  ybox <- yrange + c(-2, 2)
  subset <- !is.na(outline$x)
  polypath(c(outline$x[subset], NA, c(xbox, rev(xbox))),
           c(outline$y[subset], NA, rep(ybox, each=2)),
           col="white", rule="evenodd")
  points(x=Eute.loc$long,y=Eute.loc$lat,pch=21,col="black",bg="white",lwd=1.9,cex=1.5)
  points(x=Eute.loc2$long,y=Eute.loc2$lat,pch=21,col="black",bg="black",lwd=1.9,cex=1.5) # overly black points for the three provs studied
  if(export==T) dev.copy2pdf(file="output/FigS2_prov_map.pdf")
}
#--------------------------------------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#function to take vectors of "location", a response variable, and it's error term, and return a
#  bit of a table
mktable <- function(location,yvar,se,nchar1=1,nchar2=1,type="CI"){
  text1 <- ifelse(nchar1==1,"%.1f","%.2f")
  text2 <- ifelse(nchar2==1,"%.1f","%.2f")
  
  if(type == "SE"){
    vec <- paste(sprintf(text1,round(yvar,nchar1))," (",sprintf(text2,round(se,nchar2)),")",sep="")
    #names(vec) <- location
  }
  
  if(type == "CI"){
    vec <- paste(sprintf(text1,round(yvar,nchar1))," (",se,")",sep="")
    #names(vec) <- location
  }
  
  return(vec)
}
#-----------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------
#- compare the estimates of total canopy leaf area from destructive harvests
#    with the estimates from the leaf number and size measurements.
checkLeafAreaEst <- function(){
  size <- getHarvest()
  size2 <-subset(size,Date %in% as.Date(c("2016-01-29","2016-02-10")))
  size2$Date2 <- as.Date("2016-01-01")
  size2$Date2[which(size2$Date==as.Date("2016-01-29"))] <- as.Date("2016-01-28")
  size2$Date2[which(size2$Date==as.Date("2016-02-10"))] <- as.Date("2016-02-09")
  
  #------
  #- process the total plant leaf area data
  la <- getLA()
  
  #- calculate total plant leaf area. This method uses a different average leaf size for each plant
  la$canopy <- with(la,Leafno*Leafarea)
  
  
  #- calculate total plant leaf area using a room and date-specific mean value
  leaf_size <- summaryBy(Leafarea~Room+Leafsize+Date,data=la,FUN=mean,keep.names=F,na.rm=T)
  
  la2.1 <- merge(la,leaf_size,by=c("Room","Leafsize","Date"))
  la2.1$canopy2 <- with(la2.1,Leafno*Leafarea.mean)
  
  la2 <- summaryBy(canopy+canopy2~Room+Code+Prov+prov_trt+Date+W_treatment,data=la2.1,FUN=sum,keep.names=T)
  #------
  
  
  dat <- merge(la2[,c("Room","Prov","Code","Date","W_treatment","canopy","canopy2")],
               size2[,c("Code","W_treatment","leafarea","totdm","Date2")],
               by.x=c("Code","W_treatment","Date"),by.y=c("Code","W_treatment","Date2"))
  windows()
  plot(leafarea~canopy,data=dat,
       xlab="Canopy leaf area; estimated (cm2)",ylab="Canopy leaf area; destructively measured (cm2)")
  abline(0,1)
  lm1 <- lm(leafarea~canopy,data=dat)
  summary(lm1)
  abline(lm1,lty=2)
}
#-------------------------------------------------------------------------------------






#-------------------------------------------------------------------------------------
#- Function to return the respiratory component measurements,
#  made during the destructive harvests near the end of the experiment
#-------------------------------------------------------------------------------------
returnRcomponents <- function(path="Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data"){
  #- read in the respiration data
  #path <- "W://WORKING_DATA/GHS39/GREAT"
  Rdat1 <- read.csv(paste(path,"GHS39_GREAT_MAIN_GX-R_20160217-20160224_L2.csv",sep="/"))
  
  #- average over subreplicate logs
  Rdat2 <- summaryBy(dCO2+Photo+Cond+Ci+Trmmol+VpdL+Area~Date+Organ+Code+Pot+Prov+Unit+W_treatment,data=Rdat1,FUN=mean,keep.names=T)
  Rdat2$Date <- as.Date(Rdat2$Date,format="%Y-%m-%d")
  Rdat2$Code <- paste(Rdat2$Prov,sprintf("%02d",Rdat2$Pot),sep="-")
  
  # #- reformat the "Pot" variable. E.g, change "A-01" to "A-1" to match the size datasets.
  # crap <- unlist(strsplit(as.character(Rdat2$Pot), "-"))
  # Rdat2$prov <- as.factor(crap[seq(1,length(crap)-1,2)])
  # Rdat2$pot <- as.factor(paste(Rdat2$prov,as.numeric(crap[seq(0,length(crap),2)]),sep="-"))
  # Rdat2$prov <- NULL #- remove the "prov" variable. It gets added later with getSize().
  # Rdat2$Pot <- NULL
  
  #----------
  #- merge in the harvest mass data, to calculate mass-specific respiration rates
  finalHarvest <- read.csv(paste(path,"GHS39_GREAT_MAIN_BIOMASS_20160217-20160224_L2.csv",sep="/"))
  finalHarvest <- finalHarvest[,c("Code","W_treatment","Leafarea","Leafarea_sub","Leafmass","Leafmass_sub","Stemmass","Rootmass","Rootmass_sub")]
  finalHarvest$totalRoot <- rowSums(finalHarvest[,c("Rootmass","Rootmass_sub")],na.rm=T)
  finalHarvest$leafdm <- rowSums(finalHarvest[,c("Leafmass","Leafmass_sub")],na.rm=T)
  
  #names(finalHarvest)[2] <- "leafArea"
  
  Rdat3 <- merge(Rdat2,finalHarvest,by=c("Code","W_treatment"))
  
  #- calculate mass-based respiration rates
  Rdat3$Rmass <- NA
  leaves <- which(Rdat3$Organ=="leaf") #- find the indexes of the leaf measurements
  stems <- which(Rdat3$Organ=="stem") #- find the indexes of the stem measurements
  roots <- which(Rdat3$Organ=="root") #- find the indexes of the root measurements
  
  #- sort out the samples where the measured root mass was not different than the root total mass
  rootstofix <- which(is.na(Rdat3$Rootmass_sub)==T & is.na(Rdat3$Rootmass)==F)
  Rdat3$Rootmass_sub[rootstofix] <- Rdat3$Rootmass[rootstofix]  
  
  Rdat3$Rmass[leaves] <- Rdat3$Photo[leaves]*-1*Rdat3$Leafarea_sub[leaves]/10000/Rdat3$Leafmass_sub[leaves]*1000*1000 # nmol CO2 g-1 s-1
  Rdat3$Rmass[stems] <- Rdat3$Photo[stems]*-1*Rdat3$Area[stems]/10000/Rdat3$Stemmass[stems]*1000*1000 # nmol CO2 g-1 s-1
  Rdat3$Rmass[roots] <- Rdat3$Photo[roots]*-1*Rdat3$Area[roots]/10000/Rdat3$Rootmass_sub[roots]*1000*1000 # nmol CO2 g-1 s-1
  
  #----
  
  
  #----
  #- merge in the size data (just to get things like room numbers and drought treatments, etc)
  size <- getSize()
  size <- size[,c("Code","Room","Prov","W_treatment","location")]
  size <- unique(size)
  
  #- merge in room temperature key
  key <- data.frame(Room=1:6,Tair= c(18,21.5,25,28.5,32,35.5)) # could be improved with real data
  size2 <- merge(size,key)
  Rdat <- merge(Rdat3,size2,by=c("Code","W_treatment","Prov"))
  Rdat$Rarea <- -1*Rdat$Photo
  
  #- predict respiration at growth temperature
  Rdat$Rmass_insitu <- with(Rdat,Rmass*2^((Tair-25)/10))
  Rdat$Rarea_insitu <- with(Rdat,Rarea*2^((Tair-25)/10))
  return(Rdat)
}
#------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------
#- function to make a plot of average leaf size and total leaf number as a function of temperature
#------------------------------------------------------------------------------------------------
plot_leaf_area <- function(output=T){
  #- make a plot of the leaf
  palette(rev(brewer.pal(6,"Spectral")))
  COL=palette()[c(1,2,6)]
  
  #- process the total plant leaf area data
  la.raw <- getLA()
  
  #- work out the air temperature and new provenance keys
  key <- data.frame(Room=1:6,Tair= c(18,21.5,25,28.5,32,35.5)) # could be improved with real data
  key2 <- data.frame(Prov=as.factor(LETTERS[1:3]),location= factor(c("Cold-edge","Warm-edge","Central"),
                                                                   levels=c("Cold-edge","Central","Warm-edge")))
  la1 <- merge(la.raw,key,by="Room")
  la <- merge(la1,key2,by="Prov")
  
  #- calculate total plant leaf area. This method uses a different average leaf size for each plant
  la$canopy <- with(la,Leafno*Leafarea)
  
  
  #----------------------------------------------------------------------------------------------------
  #- calculate the average size of "large" leaves, along with the total number of leaves
  leaf_size <- summaryBy(Leafarea+Leafno+Tair~Room+location,data=subset(la,Leafsize=="large" & Date==as.Date("2016-01-28")),
                         FUN=c(mean,standard.error),keep.names=F,na.rm=T)
  
  leaf_no <- summaryBy(Leafno~Room+location+Tair+Date+Code,data=subset(la,W_treatment=="w"),
                       FUN=c(sum),keep.names=T,na.rm=T)
  leaf_no2 <- summaryBy(Leafno+Tair~Room+location,data=subset(leaf_no,Date==as.Date("2016-01-28")),
                        FUN=c(mean,standard.error),keep.names=F,na.rm=T)
  #----------------------------------------------------------------------------------------------------
  
  
  #----------------------------------------------------------------------------------------------------
  #- plot
  windows(40,50);par(mfrow=c(2,1),cex.lab=1.7,mar=c(1,1,1,1),oma=c(5,6,1,1))
  #- plot leaf size
  plotBy(Leafarea.mean~Tair.mean|location,data=leaf_size,pch=16,axes=F,xlab="",ylab="",legend=F,
         ylim=c(0,50),xlim=c(15,40),cex=1.5,col=COL,
         panel.first=adderrorbars(x=leaf_size$Tair.mean,y=leaf_size$Leafarea.mean,
                                  SE=leaf_size$Leafarea.standard.error,direction="updown"))
  magaxis(side=c(1,2),labels=c(1,1),frame.plot=T,las=1)
  title(ylab=expression(Leaf~size~(cm^2)),xpd=NA)
  legend("topright",levels(leaf_size$location),fill=COL,cex=0.8,title="Provenance")
  legend("topleft",legend=letters[1],bty="n")
  
  #- plot leaf number
  plotBy(Leafno.mean~Tair.mean|location,data=leaf_no2,pch=16,axes=F,xlab="",ylab="",legend=F,
         ylim=c(0,20),xlim=c(15,40),cex=1.5,col=COL,
         panel.first=adderrorbars(x=leaf_no2$Tair.mean,y=leaf_no2$Leafno.mean,
                                  SE=leaf_no2$Leafno.standard.error,direction="updown"))
  magaxis(side=c(1,2),labels=c(1,1),frame.plot=T,las=1)
  title(ylab=expression(Leaf~number~("#")),xpd=NA)
  legend("topleft",legend=letters[2],bty="n")
  title(xlab=expression(Growth~T[air]~(degree*C)),xpd=NA)
  
  #----------------------------------------------------------------------------------------------------
  if(output==T) dev.copy2pdf(file="output/Leaf_area_size.pdf")


}













#----------------------------------------------------------------------------------------------------------------
#' Function for smoothplots of GAMs. 
fitgam <- function(X,Y,dfr, k=-1, R=NULL){
  dfr$Y <- dfr[,Y]
  dfr$X <- dfr[,X]
  if(!is.null(R)){
    dfr$R <- dfr[,R]
    model <- 2
  } else model <- 1
  dfr <- droplevels(dfr)
  
  
  if(model ==1){
    g <- gam(Y ~ s(X, k=k), data=dfr)
  }
  if(model ==2){
    g <- gamm(Y ~ s(X, k=k), random = list(R=~1), data=dfr)
  }
  
  return(g)
}


#' Plot a generalized additive model
#' @param x Variable for X axis (unquoted)
#' @param y Variable for Y axis (unquoted)
#' @param data Dataframe containing x and y
#' @param kgam the \code{k} parameter for smooth terms in gam.
#' @param random An optional random effect (quoted)
#' @param log Whether to add log axes for x or y (but no transformations are done).
#' @param fitoneline Whether to fit only 
smoothplot <- function(x,y,g=NULL,data,
                       fittype=c("gam","lm"),
                       kgam=4,
                       random=NULL,
                       randommethod=c("lmer","aggregate"),
                       log="",
                       fitoneline=FALSE,
                       pointcols=NULL,
                       linecols=NULL, 
                       xlab=NULL, ylab=NULL,
                       polycolor=alpha("lightgrey",0.7),
                       axes=TRUE,
                       ...){
  
  fittype <- match.arg(fittype)
  randommethod <- match.arg(randommethod)
  
  if(!is.null(substitute(g))){
    data$G <- as.factor(eval(substitute(g),data))
  } else {
    fitoneline <- TRUE
    data$G <- 1
  }
  data$X <- eval(substitute(x),data)
  data$Y <- eval(substitute(y),data)
  data <- droplevels(data)
  
  data <- data[!is.na(data$X) & !is.na(data$Y) & !is.na(data$G),]
  
  if(is.null(pointcols))pointcols <- palette()
  if(is.null(linecols))linecols <- palette()
  
  if(is.null(xlab))xlab <- substitute(x)
  if(is.null(ylab))ylab <- substitute(y)
  
  # If randommethod = aggregate, average by group and fit simple gam.
  if(!is.null(random) && randommethod == "aggregate"){
    data$R <- data[,random]
    
    data <- summaryBy(. ~ R, FUN=mean, na.rm=TRUE, keep.names=TRUE, data=data,
                      id=~G)
    R <- NULL
  }
  
  
  if(!fitoneline){
    
    d <- split(data, data$G)
    
    if(fittype == "gam"){
      fits <- lapply(d, function(x)try(fitgam("X","Y",x, k=kgam, R=random)))
      if(!is.null(random))fits <- lapply(fits, "[[", "gam")
    } else {
      fits <- lapply(d, function(x)lm(Y ~ X, data=x))
    }
    hran <- lapply(d, function(x)range(x$X, na.rm=TRUE))
  } else {
    if(fittype == "gam"){
      fits <- list(fitgam("X","Y",data, k=kgam, R=random))
      if(!is.null(random))fits <- lapply(fits, "[[", "gam")
    } else {
      fits <- list(lm(Y ~ X, data=data))
    }
    hran <- list(range(data$X, na.rm=TRUE))
    
  }
  
  with(data, plot(X, Y, xaxt="n",yaxt="n", pch=16, col=pointcols[G],
                  xlab=xlab, ylab=ylab, ...))
  
  if(axes){
    if(log=="xy")magaxis(side=1:2, unlog=1:2)
    if(log=="x"){
      magaxis(side=1, unlog=1)
      axis(2)
      box()
    }
    if(log=="y"){
      magaxis(side=2, unlog=2)
      axis(1)
      box()
    }
    if(log==""){
      axis(1)
      axis(2)
      box()
    }
  }
  
  for(i in 1:length(fits)){
    
    if(fittype == "gam"){
      nd <- data.frame(X=seq(hran[[i]][1], hran[[i]][2], length=101))
      if(!inherits(fits[[i]], "try-error")){
        p <- predict(fits[[i]],nd,se.fit=TRUE)
        addpoly(nd$X, p$fit-2*p$se.fit, p$fit+2*p$se.fit, col=polycolor[i])
        lines(nd$X, p$fit, col=linecols[i], lwd=2)
      }
    }
    if(fittype == "lm"){
      pval <- summary(fits[[i]])$coefficients[2,4]
      LTY <- if(pval < 0.05)1 else 5
      predline(fits[[i]], col=linecols[i], lwd=2, lty=LTY)
    }
  }
  
  return(invisible(fits))
}


addpoly <- function(x,y1,y2,col=alpha("lightgrey",0.7),...){
  ii <- order(x)
  y1 <- y1[ii]
  y2 <- y2[ii]
  x <- x[ii]
  polygon(c(x,rev(x)), c(y1, rev(y2)), col=col, border=NA,...)
}

predline <- function(fit, from=NULL, to=NULL, col=alpha("lightgrey",0.7), ...){
  
  if(is.null(from))from <- min(fit$model[,2], na.rm=TRUE)
  if(is.null(to))to <- max(fit$model[,2], na.rm=TRUE)
  
  newdat <- data.frame(X = seq(from,to, length=101))
  names(newdat)[1] <- names(coef(fit))[2]
  
  pred <- as.data.frame(predict(fit, newdat, se.fit=TRUE, interval="confidence")$fit)
  
  addpoly(newdat[[1]], pred$lwr, pred$upr, col=col)
  
  #ablinepiece(fit, from=from, to=to, ...)
  lines(pred$fit~newdat[,1])
}

#'@title Add a line to a plot
#'@description As \code{abline}, but with \code{from} and \code{to} arguments. 
#'If a fitted linear regression model is used as asn argument, it uses the min and max values of the data used to fit the model.
#'@param a Intercept (optional)
#'@param b Slope (optional)
#'@param reg A fitted linear regression model (output of \code{\link{lm}}).
#'@param from Draw from this X value
#'@param to Draw to this x value
#'@param \dots Further parameters passed to \code{\link{segments}}
#'@export
ablinepiece <- function(a=NULL,b=NULL,reg=NULL,from=NULL,to=NULL,...){
  
  # Borrowed from abline
  if (!is.null(reg)) a <- reg
  
  if (!is.null(a) && is.list(a)) {
    temp <- as.vector(coefficients(a))
    from <- min(a$model[,2], na.rm=TRUE)
    to <- max(a$model[,2], na.rm=TRUE)
    
    if (length(temp) == 1) {
      a <- 0
      b <- temp
    }
    else {
      a <- temp[1]
      b <- temp[2]
    }
  }
  
  segments(x0=from,x1=to,
           y0=a+from*b,y1=a+to*b,...)
  
}

#----------------------------------------------------------------------------------------------------------------



