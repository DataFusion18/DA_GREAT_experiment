#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Script to analysis the raw GREAT experiment data. 
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------



###### R script to import and process the raw GREAT experiment data 
# to model the carbon pools and fluxes using DA

#-----------------------------------------------------------------------------------------
# Script to read and process the leaf datasets to Calculate leaf mass and daily leaf area
# inputs
rm(list=ls()) 
setwd("/Users/kashifmahmud/WSU/ARC_project_2/great_experiment")
source("R/load_packages_functions_CBM.R")

biomass.0129.raw = read.csv("Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data/GHS39_GREAT_MAIN_BIOMASS_20160129_L2.csv")
biomass.0210.raw = read.csv("Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data/GHS39_GREAT_MAIN_BIOMASS_20160210_L2.csv")

biomass.0129.raw = biomass.0129.raw[biomass.0129.raw$W_treatment %in% as.factor("w"),]
biomass.0210.raw = biomass.0210.raw[biomass.0210.raw$W_treatment %in% as.factor("w"),]
biomass.0129.raw = biomass.0129.raw[with(biomass.0129.raw, order(Code)), ]
biomass.0210.raw = biomass.0210.raw[with(biomass.0210.raw, order(Code)), ]
keeps <- c("Code", "Leafno", "Leafarea")
biomass.0129.raw = biomass.0129.raw[ , keeps, drop = FALSE]
biomass.0210.raw = biomass.0210.raw[ , keeps, drop = FALSE]
names(biomass.0129.raw) = c("Code", "Leafno", "total.LA")
names(biomass.0210.raw) = c("Code", "Leafno", "total.LA")
biomass.0129.melt <- melt(biomass.0129.raw, id.vars = "Code")
biomass.0210.melt <- melt(biomass.0210.raw, id.vars = "Code")
biomass.0129.melt$date = as.Date("2016-01-29")
biomass.0210.melt$date = as.Date("2016-02-10")

leaf.0128.raw = read.csv("Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data/GHS39_GREAT_MAIN_LEAFAREA_20160128_L2_V1.csv")
leaf.0209.raw = read.csv("Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data/GHS39_GREAT_MAIN_LEAFAREA_20160209_L2_V1.csv")

leaf.0128.raw = leaf.0128.raw[leaf.0128.raw$W_treatment %in% as.factor("w"),]
leaf.0209.raw = leaf.0209.raw[leaf.0209.raw$W_treatment %in% as.factor("w"),]

leaf.0128 = leaf.0128.raw[leaf.0128.raw$Code %in% biomass.0129.raw$Code,]
leaf.0209 = leaf.0209.raw[leaf.0209.raw$Code %in% biomass.0210.raw$Code,]

leaf.0128$total.LA = leaf.0128$Leafno * leaf.0128$Leafarea
leaf.0209$total.LA = leaf.0209$Leafno * leaf.0209$Leafarea

leaf.0128.mod = aggregate(. ~ Code, leaf.0128, sum)
leaf.0209.mod = aggregate(. ~ Code, leaf.0209, sum)

keeps <- c("Code", "Leafno", "total.LA")
leaf.0128.mod = leaf.0128.mod[ , keeps, drop = FALSE]
leaf.0209.mod = leaf.0209.mod[ , keeps, drop = FALSE]
leaf.0128.melt <- melt(leaf.0128.mod, id.vars = "Code")
leaf.0209.melt <- melt(leaf.0209.mod, id.vars = "Code")
leaf.0128.melt$date = as.Date("2016-01-28")
leaf.0209.melt$date = as.Date("2016-02-09")

leaf.0128.melt = rbind(leaf.0128.melt, biomass.0129.melt)
leaf.0209.melt = rbind(leaf.0209.melt, biomass.0210.melt)

#-----------------------------------------------------------------------------------------
# Plot the comparison grapgh from harvest and 
p1 = ggplot() +
  geom_line(data = leaf.0128.melt, aes(x = Code, y = value, group = interaction(variable,date), colour=factor(variable), linetype=factor(date))) +
  xlab("Code") +
  ylab("Leaf data") +
  ggtitle("Leaf data")
p1
ggsave(p1,filename=paste("Output/LA_LC_compare_1.png"))

p2 = ggplot() +
  geom_line(data = leaf.0209.melt, aes(x = Code, y = value, group = interaction(variable,date), colour=factor(variable), linetype=factor(date))) + 
  xlab("Code") +
  ylab("Leaf data") +
  ggtitle("Leaf data")
p2
ggsave(p2,filename=paste("Output/LA_LC_compare_2.png"))
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
################### Analyse stem height diameter to estimate Leaf, Stem and Root biomass
# Import weekly height diameter data for 3 months (Diameter is in mm; Height is in cm)
height.dia <- read.csv("Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data/GHS39_GREAT_MAIN_HEIGHTDIAMETER_20160108-20160229_L2.csv")
height.dia = height.dia[height.dia$W_treatment %in% as.factor("w"),]
height.dia$D = rowMeans(height.dia[,c("D1", "D2")], na.rm=TRUE)
height.dia = height.dia[complete.cases(height.dia), ]

initial.harvest = read.csv("Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data/GHS39_GREAT_MAIN_BIOMASS_20160107_L2.csv")
initial.harvest[ , c("Leafmass", "Stemmass", "Rootmass")] = initial.harvest[ , c("Leafmass", "Stemmass", "Rootmass")]
initial.harvest$Height = initial.harvest$Height/10 # Unit conversion from mm to cm
int.harvest.1 = read.csv("Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data/GHS39_GREAT_MAIN_BIOMASS_20160129_L2.csv")
int.harvest.1 = int.harvest.1[int.harvest.1$W_treatment %in% as.factor("w"),]
int.harvest.1[ , c("Leafmass", "Stemmass", "Rootmass")] = int.harvest.1[ , c("Leafmass", "Stemmass", "Rootmass")]
int.harvest.2 = read.csv("Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data/GHS39_GREAT_MAIN_BIOMASS_20160210_L2.csv")
int.harvest.2 = int.harvest.2[int.harvest.2$W_treatment %in% as.factor("w"),]
int.harvest.2[ , c("Leafmass", "Stemmass", "Rootmass")] = int.harvest.2[ , c("Leafmass", "Stemmass", "Rootmass")]

final.harvest = read.csv("Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data/GHS39_GREAT_MAIN_BIOMASS_20160217-20160224_L2.csv")
final.harvest = final.harvest[final.harvest$W_treatment %in% as.factor("w"),]
final.harvest$Room = NULL
final.harvest$Leafarea = rowSums(final.harvest[,c("Leafarea", "Leafarea_sub")], na.rm=T)
final.harvest$Leafmass = rowSums(final.harvest[,c("Leafmass", "Leafmass_sub")], na.rm=T)
final.harvest$Stemmass = rowSums(final.harvest[,c("Stemmass", "Stemmass_sub")], na.rm=T)
final.harvest$Rootmass = rowSums(final.harvest[,c("Rootmass", "Rootmass_sub")], na.rm=T)
final.harvest[c("Leafarea_sub","Leafmass_sub","Stemmass_sub","Rootmass_sub")] = NULL
final.harvest[ , c("Leafmass", "Stemmass", "Rootmass")] = final.harvest[ , c("Leafmass", "Stemmass", "Rootmass")]/1000 # Unit conversion from mg to g

harvest.data = rbind(int.harvest.1, int.harvest.1, final.harvest)
harvest.data$W_treatment = NULL
harvest.data = rbind(harvest.data, initial.harvest)
harvest.data$D = rowMeans(harvest.data[,c("D1", "D2")], na.rm=TRUE)
harvest.data = harvest.data[!(harvest.data$Rootmass==0),]
harvest.data = harvest.data[with(harvest.data, order(Rootmass)), ]

#-----------------------------------------------------------------------------------------
################### Linear regression model fitting [log(stem_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
# Fit the model with initial data (30) and intermediate (18*2=36) and final (90) harvest data for all seedlings (156 data in total)
# Units are: masses = g; height = cm; dia = mm
model.fit = data.frame(harvest.data$Code, harvest.data$Height, harvest.data$D, harvest.data$Leafarea, harvest.data$Leafmass, harvest.data$Stemmass, harvest.data$Rootmass)
names(model.fit) = c("Code", "Height", "D", "Leafarea", "Leafmass", "Stemmass", "Rootmass")

fit.sm <- lm(log(Stemmass) ~ log(D) + log(Height), data=model.fit)
summary(fit.sm) # show results
coefficients(fit.sm) # model coefficients
cat("Linear regression model fitting: log(stem_mass) = ", coefficients(fit.sm)[1], "+", coefficients(fit.sm)[2], 
    "* log(diameter) +", coefficients(fit.sm)[3], "* log(height)")

# Estimate the stemmass from the fitted linear regression equation
eq = function(x,y){exp(coefficients(fit.sm)[1] + coefficients(fit.sm)[2] * log(x)  + coefficients(fit.sm)[3] * log(y))}

# Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
height.dia$Stemmass = eq(height.dia$D,height.dia$Height)

#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
# Plotting observation and modelled data
png(file = "Output/Height_Stem mass.png")
plot(height.dia$Height,height.dia$Stemmass,col="red",main="Height vs Stemmass", pch=15, xlab="Height (cm)", ylab="Stem mass (g)")
lines(model.fit$Height,model.fit$Stemmass,type="p", col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
png(file = "Output/Diameter_Stemmass.png")
plot(height.dia$D,height.dia$Stemmass, col="red", main="Diameter vs Stem mass", pch=16, xlab="Diameter (mm)", ylab="Stem mass (g)")
lines(model.fit$D,model.fit$Stemmass,type="p",col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
# plot(y,x,col="red",main="Height vs Diameter", pch=17, xlab="Height (cm)", ylab="Diameter (cm)")
# lines(model.fit$Height,model.fit$D,type="p",col="green", pch=20)
# legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
################### Linear regression model fitting [log(root_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
# Fit the model with initial data (30) and intermediate (18*2=36) and final (90) harvest data for all seedlings (156 data in total)
# Units are: masses = g; height = cm; dia = mm
fit.rm <- lm(log(Rootmass) ~ log(D) + log(Height), data=model.fit)
summary(fit.rm) # show results
coefficients(fit.rm) # model coefficients
cat("Linear regression model fitting: log(root_mass) = ", coefficients(fit.rm)[1], "+", coefficients(fit.rm)[2], 
    "* log(diameter) +", coefficients(fit.rm)[3], "* log(height)")

# Estimate the stemmass from the fitted linear regression equation
eq = function(x,y){exp(coefficients(fit.rm)[1] + coefficients(fit.rm)[2] * log(x)  + coefficients(fit.rm)[3] * log(y))}

# Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
height.dia$Rootmass = eq(height.dia$D,height.dia$Height)

#-----------------------------------------------------------------------------------------
# Plotting observation and modelled data
png(file = "Output/Height_Rootmass.png")
plot(height.dia$Height,height.dia$Rootmass,col="red",main="Height vs Root mass", pch=15, xlab="Height (cm)", ylab="Root mass (g)")
lines(model.fit$Height,model.fit$Rootmass,type="p", col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
png(file = "Output/Diameter_Rootmass.png")
plot(height.dia$D,height.dia$Rootmass, col="red", main="Diameter vs Root mass", pch=16, xlab="Diameter (mm)", ylab="Root mass (g)")
lines(model.fit$D,model.fit$Rootmass,type="p",col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
# plot(y,x,col="red",main="Height vs Diameter", pch=17, xlab="Height (cm)", ylab="Diameter (cm)")
# lines(model.fit$Height,model.fit$D,type="p",col="green", pch=20)
# legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
################### Linear regression model fitting [log(leaf_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
# Fit the model with initial data (30) and intermediate (18*2=36) and final (90) harvest data for all seedlings (156 data in total)
# Units are: masses = g; height = cm; dia = mm
fit.lm <- lm(log(Leafmass) ~ log(D) + log(Height), data=model.fit)
summary(fit.lm) # show results
coefficients(fit.lm) # model coefficients
cat("Linear regression model fitting: log(leaf_mass) = ", coefficients(fit.lm)[1], "+", coefficients(fit.lm)[2], 
    "* log(diameter) +", coefficients(fit.lm)[3], "* log(height)")

# Estimate the stemmass from the fitted linear regression equation
eq = function(x,y){exp(coefficients(fit.lm)[1] + coefficients(fit.lm)[2] * log(x)  + coefficients(fit.lm)[3] * log(y))}

# Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
height.dia$Leafmass = eq(height.dia$D,height.dia$Height)

#-----------------------------------------------------------------------------------------
# Plotting observation and modelled data
png(file = "Output/Height_Leafmass.png")
plot(height.dia$Height,height.dia$Leafmass,col="red",main="Height vs Leaf mass", pch=15, xlab="Height (cm)", ylab="Leaf mass (g)")
lines(model.fit$Height,model.fit$Leafmass,type="p", col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
png(file = "Output/Diameter_Leafmass.png")
plot(height.dia$D,height.dia$Leafmass, col="red", main="Diameter vs Leaf mass", pch=16, xlab="Diameter (mm)", ylab="Leaf mass (g)")
lines(model.fit$D,model.fit$Leafmass,type="p",col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
# plot(y,x,col="red",main="Height vs Diameter", pch=17, xlab="Height (cm)", ylab="Diameter (cm)")
# lines(model.fit$Height,model.fit$D,type="p",col="green", pch=20)
# legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
################### Linear regression model fitting [log(leaf_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
# Fit the model with initial data (30) and intermediate (18*2=36) and final (90) harvest data for all seedlings (156 data in total)
# Units are: masses = g; height = cm; dia = mm; leaf area = cm2
fit.la <- lm(log(Leafarea) ~ log(D) + log(Height), data=model.fit)
summary(fit.la) # show results
coefficients(fit.la) # model coefficients
cat("Linear regression model fitting: log(leaf_area) = ", coefficients(fit.la)[1], "+", coefficients(fit.la)[2], 
    "* log(diameter) +", coefficients(fit.la)[3], "* log(height)")

# Estimate the stemmass from the fitted linear regression equation
eq = function(x,y){exp(coefficients(fit.la)[1] + coefficients(fit.la)[2] * log(x)  + coefficients(fit.la)[3] * log(y))}

# Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
height.dia$Leafarea = eq(height.dia$D,height.dia$Height)

#-----------------------------------------------------------------------------------------
# Plotting observation and modelled data
png(file = "Output/Height_Leafarea.png")
plot(height.dia$Height,height.dia$Leafarea,col="red",main="Height vs Leaf area", pch=15, xlab="Height (cm)", ylab="Leaf mass (g)")
lines(model.fit$Height,model.fit$Leafarea,type="p", col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
png(file = "Output/Diameter_Leafarea.png")
plot(height.dia$D,height.dia$Leafarea, col="red", main="Diameter vs Leaf area", pch=16, xlab="Diameter (mm)", ylab="Leaf mass (g)")
lines(model.fit$D,model.fit$Leafarea,type="p",col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
# plot(y,x,col="red",main="Height vs Diameter", pch=17, xlab="Height (cm)", ylab="Diameter (cm)")
# lines(model.fit$Height,model.fit$D,type="p",col="green", pch=20)
# legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
# setwd("Output")
# plots1 <- lapply(ll <- list.files(patt='.*[.]png'),function(x){
#   img <- as.raster(readPNG(x))
#   rasterGrob(img, interpolate = FALSE)
# })
# ggsave("Summary_plots.pdf", marrangeGrob(grobs=plots1, nrow=2, ncol=2))
# 
# 
# # Save the stem mass data for MCMC CBM
# height.dia$W_treatment = NULL; height.dia$Comment = NULL
# write.csv(height.dia, file = "Cleaf_Cstem_Croot.csv", row.names = FALSE)




