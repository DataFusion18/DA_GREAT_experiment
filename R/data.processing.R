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
# setwd("/Users/kashifmahmud/WSU/Final_projects/DA_GREAT_experiment")
source("R/load_packages_functions_CBM.R")

biomass.0129.raw = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160129_L2.csv")
biomass.0210.raw = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160210_L2.csv")

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

leaf.0128.raw = read.csv("Data/GHS39_GREAT_MAIN_LEAFAREA_20160128_L2_V1.csv")
leaf.0209.raw = read.csv("Data/GHS39_GREAT_MAIN_LEAFAREA_20160209_L2_V1.csv")

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

# # Plot measurement vs harvest LA to judge the data collection
# png(file = "Output/LA_harvest_vs_measurement.png")
# par(mfrow=c(2,1), mar=c(3, 4, 1, 1), mgp=c(2,1,0))
# plot(leaf.0128.mod$total.LA, biomass.0129.raw$total.LA, col="red", pch=16, xlab="Measured leaf area" ~ (cm^{2}), ylab="Harvested leaf area" ~ (cm^{2}))
# abline(0, 1) 
# plot(leaf.0209.mod$total.LA, biomass.0210.raw$total.LA, col="red", pch=16, xlab="Measured leaf area" ~ (cm^{2}), ylab="Harvested leaf area" ~ (cm^{2}))
# abline(0, 1) 
# dev.off()

# Find the residuals of harvest and measurements to adjust the LA measurements from standing trees
residuals1 = biomass.0129.raw$total.LA - leaf.0128.mod$total.LA
fit.res1 = lm(residuals1 ~ leaf.0128.mod$total.LA)
fit.res11 = lm(residuals1 ~ biomass.0129.raw$total.LA)
residuals2 = biomass.0210.raw$total.LA - leaf.0209.mod$total.LA
fit.res2 = lm(residuals2 ~ leaf.0209.mod$total.LA)
fit.res22 = lm(residuals2 ~ biomass.0210.raw$total.LA)

# leaf.0128.mod = leaf.0128.mod[with(leaf.0128.mod, order(total.LA)), ]
# leaf.0209.mod = leaf.0209.mod[with(leaf.0209.mod, order(total.LA)), ]
# biomass.0129.raw = biomass.0129.raw[with(biomass.0129.raw, order(total.LA)), ]
# biomass.0210.raw = biomass.0210.raw[with(biomass.0210.raw, order(total.LA)), ]
# mf1 = seq( (biomass.0129.raw$total.LA[1] / leaf.0128.mod$total.LA[1]), (biomass.0129.raw$total.LA[nrow(leaf.0128.mod)] / leaf.0128.mod$total.LA[nrow(leaf.0128.mod)]), length.out = nrow(leaf.0128.mod))
# mf2 = seq( (biomass.0210.raw$total.LA[1] / leaf.0209.mod$total.LA[1]), (biomass.0210.raw$total.LA[nrow(leaf.0209.mod)] / leaf.0209.mod$total.LA[nrow(leaf.0209.mod)]), length.out = nrow(leaf.0209.mod))
# mf = (mf1 + mf2) / 2

# Plot measurement with correction factor (cf) vs harvest LA to judge the data collection
png(file = "Output/LA_harvest_vs_measurement_cf.png")
par(mfrow=c(2,2), mar=c(3, 4, 1, 1), mgp=c(2,1,0))
plot(leaf.0128.mod$total.LA, biomass.0129.raw$total.LA, col="red", pch=16, xlab="Measured leaf area" ~ (cm^{2}), ylab="Harvested leaf area" ~ (cm^{2}))
abline(0, 1) 
rmse.1 = sqrt( mean( (leaf.0128.mod$total.LA - biomass.0129.raw$total.LA)^2) )
text(max(biomass.0129.raw$total.LA)*0.1, max(biomass.0129.raw$total.LA)*0.9, paste("RMSE =", format(round(rmse.1, 1))), pos = 4)

plot(leaf.0128.mod$total.LA + fitted(fit.res1), biomass.0129.raw$total.LA, col="red", pch=16, xlab="Measured leaf area" ~ (cm^{2}), ylab="Harvested leaf area" ~ (cm^{2}))
abline(0, 1) 
rmse.11 = sqrt( mean( ((leaf.0128.mod$total.LA + fitted(fit.res1)) - biomass.0129.raw$total.LA)^2) )
text(max(biomass.0129.raw$total.LA)*0.2, max(biomass.0129.raw$total.LA)*0.9, paste("RMSE =", format(round(rmse.11, 1))), pos = 4)

# plot(leaf.0128.mod$total.LA + fitted(fit.res1), biomass.0129.raw$total.LA, col="red", pch=16, xlab="Measured leaf area" ~ (cm^{2}), ylab="Harvested leaf area" ~ (cm^{2}))
# abline(0, 1) 
# rmse.111 = sqrt( mean( ((leaf.0128.mod$total.LA + fitted(fit.res11)) - biomass.0129.raw$total.LA)^2) )
# plot(leaf.0128.mod$total.LA * mf1, biomass.0129.raw$total.LA, col="red", pch=16, xlab="Measured leaf area" ~ (cm^{2}), ylab="Harvested leaf area" ~ (cm^{2}))
# abline(0, 1) 
plot(leaf.0209.mod$total.LA, biomass.0210.raw$total.LA, col="red", pch=16, xlab="Measured leaf area" ~ (cm^{2}), ylab="Harvested leaf area" ~ (cm^{2}))
abline(0, 1) 
rmse.2 = sqrt( mean( (leaf.0209.mod$total.LA - biomass.0210.raw$total.LA)^2) )
text(max(biomass.0210.raw$total.LA)*0.1, max(biomass.0210.raw$total.LA)*0.9, paste("RMSE =", format(round(rmse.2, 1))), pos = 4)

plot(leaf.0209.mod$total.LA + fitted(fit.res2), biomass.0210.raw$total.LA, col="red", pch=16, xlab="Measured leaf area" ~ (cm^{2}), ylab="Harvested leaf area" ~ (cm^{2}))
abline(0, 1) 
rmse.22 = sqrt( mean( ((leaf.0209.mod$total.LA + fitted(fit.res2)) - biomass.0210.raw$total.LA)^2) )
text(max(biomass.0210.raw$total.LA)*0.2, max(biomass.0210.raw$total.LA)*0.9, paste("RMSE =", format(round(rmse.22, 1))), pos = 4)

# plot(leaf.0209.mod$total.LA + fitted(fit.res2), biomass.0210.raw$total.LA, col="red", pch=16, xlab="Measured leaf area" ~ (cm^{2}), ylab="Harvested leaf area" ~ (cm^{2}))
# abline(0, 1) 
# rmse.222 = sqrt( mean( ((leaf.0209.mod$total.LA + fitted(fit.res22)) - biomass.0210.raw$total.LA)^2) )
# plot(leaf.0209.mod$total.LA * mf2, biomass.0210.raw$total.LA, col="red", pch=16, xlab="Measured leaf area" ~ (cm^{2}), ylab="Harvested leaf area" ~ (cm^{2}))
# abline(0, 1) 
dev.off()
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
################### Analyse stem height diameter to estimate Leaf, Stem and Root biomass
# Import weekly height diameter data for 3 months (Diameter is in mm; Height is in cm)
height.dia <- read.csv("Data/GHS39_GREAT_MAIN_HEIGHTDIAMETER_20160108-20160229_L2.csv")
height.dia = height.dia[height.dia$W_treatment %in% as.factor("w"),]
height.dia$D = rowMeans(height.dia[,c("D1", "D2")], na.rm=TRUE)
height.dia = height.dia[complete.cases(height.dia), ]
# height.dia$Date = as.Date(height.dia$Date)
# height.dia.sub = subset(height.dia, Date %in% as.factor("29/02/2016"))


initial.harvest = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160107_L2.csv")
initial.harvest[ , c("Leafmass", "Stemmass", "Rootmass")] = initial.harvest[ , c("Leafmass", "Stemmass", "Rootmass")]
initial.harvest$Height = initial.harvest$Height/10 # Unit conversion from mm to cm

int.harvest.1 = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160129_L2.csv")
int.harvest.1 = int.harvest.1[int.harvest.1$W_treatment %in% as.factor("w"),]
int.harvest.1[ , c("Leafmass", "Stemmass", "Rootmass")] = int.harvest.1[ , c("Leafmass", "Stemmass", "Rootmass")]
int.harvest.2 = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160210_L2.csv")
int.harvest.2 = int.harvest.2[int.harvest.2$W_treatment %in% as.factor("w"),]
int.harvest.2[ , c("Leafmass", "Stemmass", "Rootmass")] = int.harvest.2[ , c("Leafmass", "Stemmass", "Rootmass")]

final.harvest = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160217-20160224_L3_with date.csv")
final.harvest$Date = as.Date(final.harvest$Date, format = "%d/%m/%Y")
final.harvest = final.harvest[final.harvest$W_treatment %in% as.factor("w"),]
final.harvest$Room = NULL
final.harvest$Leafarea = rowSums(final.harvest[,c("Leafarea", "Leafarea_sub")], na.rm=T)
final.harvest$Leafmass = rowSums(final.harvest[,c("Leafmass", "Leafmass_sub")], na.rm=T)
final.harvest$Stemmass = rowSums(final.harvest[,c("Stemmass", "Stemmass_sub")], na.rm=T)
final.harvest$Rootmass = rowSums(final.harvest[,c("Rootmass", "Rootmass_sub")], na.rm=T)
final.harvest[c("Leafarea_sub","Leafmass_sub","Stemmass_sub","Rootmass_sub")] = NULL
final.harvest[ , c("Leafmass", "Stemmass", "Rootmass")] = final.harvest[ , c("Leafmass", "Stemmass", "Rootmass")]/1000 # Unit conversion from mg to g

harvest.data = rbind(int.harvest.1, int.harvest.2, subset(final.harvest, select = -Date))
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
# R = data.frame(D = model.fit$D, Height = model.fit$Height, Stemmass = model.fit$Stemmass, Fitted = exp(fitted(fit.sm)))

rmse.sm = sqrt( mean( (exp(fitted(fit.sm)) - model.fit$Stemmass)^2) )
percentage.error.sm = rmse.sm / mean(model.fit$Stemmass) * 100 #Percentage Error
# percentage.error.sm = (sqrt ( sum((model.fit$Stemmass - exp(fitted(fit.sm)))**2) / (length(fit.sm$residuals) - length(fit.sm$coefficients)))) / mean(model.fit$Stemmass) * 100 #Percentage Error
cat("Linear regression model fitting: log(stem_mass) =", coefficients(fit.sm)[1], "+", coefficients(fit.sm)[2], 
    "* log(diameter) +", coefficients(fit.sm)[3], "* log(height) \nwith percentage error =", percentage.error.sm, "%")

# Estimate the stemmass from the fitted linear regression equation
eq = function(x,y){exp(coefficients(fit.sm)[1] + coefficients(fit.sm)[2] * log(x)  + coefficients(fit.sm)[3] * log(y))}

# Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
height.dia$Stemmass = eq(height.dia$D,height.dia$Height)

#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
# Plotting observation and modelled data
png(file = "Output/1.Height_Stem mass.png")
plot(height.dia$Height,height.dia$Stemmass,col="red",main="Height vs Stemmass", pch=15, xlab="Height (cm)", ylab="Stem mass (g)")
lines(model.fit$Height,model.fit$Stemmass,type="p", col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
rmse.stemmass.height = sqrt(mean((eq(model.fit$D,model.fit$Height)-model.fit$Stemmass)^2,na.rm=TRUE))

png(file = "Output/1.Diameter_Stemmass.png")
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
rmse.rm = sqrt( mean( (exp(fitted(fit.rm)) - model.fit$Rootmass)^2) )
percentage.error.rm = rmse.rm / mean(model.fit$Rootmass) * 100 #Percentage Error
# percentage.error.rm = (sqrt ( sum((model.fit$Rootmass - exp(fitted(fit.rm)))**2) / (length(fit.rm$residuals) - length(fit.rm$coefficients)))) / mean(model.fit$Rootmass) * 100 #Percentage Error
cat("Linear regression model fitting: log(root_mass) = ", coefficients(fit.rm)[1], "+", coefficients(fit.rm)[2], 
    "* log(diameter) +", coefficients(fit.rm)[3], "* log(height) \nwith percentage error =", percentage.error.rm, "%")

# Estimate the stemmass from the fitted linear regression equation
eq = function(x,y){exp(coefficients(fit.rm)[1] + coefficients(fit.rm)[2] * log(x)  + coefficients(fit.rm)[3] * log(y))}

# Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
height.dia$Rootmass = eq(height.dia$D,height.dia$Height)

#-----------------------------------------------------------------------------------------
# Plotting observation and modelled data
png(file = "Output/2.Height_Rootmass.png")
plot(height.dia$Height,height.dia$Rootmass,col="red",main="Height vs Root mass", pch=15, xlab="Height (cm)", ylab="Root mass (g)")
lines(model.fit$Height,model.fit$Rootmass,type="p", col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
png(file = "Output/2.Diameter_Rootmass.png")
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
rmse.lm = sqrt( mean( (exp(fitted(fit.lm)) - model.fit$Leafmass)^2) )
percentage.error.lm = rmse.lm / mean(model.fit$Leafmass) * 100 #Percentage Error
# percentage.error.lm = (sqrt ( sum((model.fit$Leafmass - exp(fitted(fit.lm)))**2) / (length(fit.lm$residuals) - length(fit.lm$coefficients)))) / mean(model.fit$Leafmass) * 100 #Percentage Error
cat("Linear regression model fitting: log(leaf_mass) = ", coefficients(fit.lm)[1], "+", coefficients(fit.lm)[2], 
    "* log(diameter) +", coefficients(fit.lm)[3], "* log(height) \nwith percentage error =", percentage.error.lm, "%")

# Estimate the stemmass from the fitted linear regression equation
eq = function(x,y){exp(coefficients(fit.lm)[1] + coefficients(fit.lm)[2] * log(x)  + coefficients(fit.lm)[3] * log(y))}

# Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
height.dia$Leafmass = eq(height.dia$D,height.dia$Height)

#-----------------------------------------------------------------------------------------
# Plotting observation and modelled data
png(file = "Output/3.Height_Leafmass.png")
plot(height.dia$Height,height.dia$Leafmass,col="red",main="Height vs Leaf mass", pch=15, xlab="Height (cm)", ylab="Leaf mass (g)")
lines(model.fit$Height,model.fit$Leafmass,type="p", col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
png(file = "Output/3.Diameter_Leafmass.png")
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
# R = data.frame(D = model.fit$D, Height = model.fit$Height, Leafarea = model.fit$Leafarea, Fitted = exp(fitted(fit.la)))
rmse.la = sqrt( mean( (exp(fitted(fit.la)) - model.fit$Leafarea)^2) )
percentage.error.la = rmse.la / mean(model.fit$Leafarea) * 100 #Percentage Error
# percentage.error.la = (sqrt ( sum((model.fit$Leafarea - exp(fitted(fit.la)))**2) / (length(fit.la$residuals) - length(fit.la$coefficients)))) / mean(model.fit$Leafarea) * 100 #Percentage Error
cat("Linear regression model fitting: log(leaf_area) = ", coefficients(fit.la)[1], "+", coefficients(fit.la)[2], 
    "* log(diameter) +", coefficients(fit.la)[3], "* log(height)\nwith percentage error =", percentage.error.la, "%")

# Estimate the stemmass from the fitted linear regression equation
eq = function(x,y){exp(coefficients(fit.la)[1] + coefficients(fit.la)[2] * log(x)  + coefficients(fit.la)[3] * log(y))}

# Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
height.dia$Leafarea = eq(height.dia$D,height.dia$Height)

#-----------------------------------------------------------------------------------------
# Plotting observation and modelled data
png(file = "Output/4.Height_Leafarea.png")
plot(height.dia$Height,height.dia$Leafarea,col="red",main="Height vs Leaf area", pch=15, xlab="Height (cm)", ylab="Leaf mass (g)")
lines(model.fit$Height,model.fit$Leafarea,type="p", col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
png(file = "Output/4.Diameter_Leafarea.png")
plot(height.dia$D,height.dia$Leafarea, col="red", main="Diameter vs Leaf area", pch=16, xlab="Diameter (mm)", ylab="Leaf mass (g)")
lines(model.fit$D,model.fit$Leafarea,type="p",col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
dev.off()
# plot(y,x,col="red",main="Height vs Diameter", pch=17, xlab="Height (cm)", ylab="Diameter (cm)")
# lines(model.fit$Height,model.fit$D,type="p",col="green", pch=20)
# legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
#-----------------------------------------------------------------------------------------
# summerizing the model fits
model.summary = data.frame(Model = as.character(c("fit.sm","fit.rm","fit.lm","fit.la")), Adjusted.R.squared = c(summary(fit.sm)$adj.r.squared,summary(fit.rm)$adj.r.squared,
                                                                                                                summary(fit.lm)$adj.r.squared,summary(fit.la)$adj.r.squared), 
                           Residual.standard.error = c(sigma(fit.sm),sigma(fit.rm),sigma(fit.lm),sigma(fit.la)), percentage.error = c(percentage.error.sm,percentage.error.rm,percentage.error.lm,percentage.error.la))

# Plot measuremnts vs fitted points to judge the model fits
png(file = "Output/model_fit.png")
par(mfrow=c(2,2), mar=c(3, 4, 1, 1), mgp=c(2,1,0))
plot(exp(fitted(fit.sm)), model.fit$Stemmass, col="red", pch=16, xlab="Fitted stem mass (g)", ylab="Measured stem mass (g)")
abline(0, 1) 
text(min(exp(fitted(fit.sm))), max(model.fit$Stemmass)*0.9, paste("Adj R-squared =", format(round(summary(fit.sm)$adj.r.squared, 2)), 
                                                                  "\nResidual SE =", format(round(sigma(fit.sm), 2)), "\n% Error =", format(round(percentage.error.sm,1))), pos = 4)

plot(exp(fitted(fit.rm)), model.fit$Rootmass, col="green", pch=16, xlab="Fitted root mass (g)", ylab="Measured root mass (g)")
abline(0, 1) 
text(min(exp(fitted(fit.rm))), max(model.fit$Rootmass)*0.9, paste("Adj R-squared =", format(round(summary(fit.rm)$adj.r.squared, 2)), 
                                                                  "\nResidual SE =", format(round(sigma(fit.rm), 2)), "\n% Error =", format(round(percentage.error.rm,1))), pos = 4)
plot(exp(fitted(fit.lm)), model.fit$Leafmass, col="blue", pch=16, xlab="Fitted leaf mass (g)", ylab="Measured leaf mass (g)")
abline(0, 1) 
text(min(exp(fitted(fit.lm))), max(model.fit$Leafmass)*0.9, paste("Adj R-squared =", format(round(summary(fit.lm)$adj.r.squared, 2)), 
                                                                  "\nResidual SE =", format(round(sigma(fit.lm), 2)), "\n% Error =", format(round(percentage.error.lm,1))), pos = 4)
plot(exp(fitted(fit.la)), model.fit$Leafarea, col="black", pch=16, xlab = expression("Fitted leaf area" ~ (cm^{2})), ylab="Measured leaf area" ~ (cm^{2}))
abline(0, 1) 
text(min(exp(fitted(fit.la))), max(model.fit$Leafarea)*0.9, paste("Adj R-squared =", format(round(summary(fit.la)$adj.r.squared, 2)), 
                                                                  "\nResidual SE =", format(round(sigma(fit.la), 2)), "\n% Error =", format(round(percentage.error.la,1))), pos = 4)
dev.off()
#-----------------------------------------------------------------------------------------
# setwd("Output")
# plots1 <- lapply(ll <- list.files(pattern='.*[.]png'),function(x){
#   img <- as.raster(readPNG(x))
#   rasterGrob(img, interpolate = FALSE)
# })
# ggsave("Summary_plots.pdf", marrangeGrob(grobs=plots1, nrow=2, ncol=2))


# Save the stem mass data for MCMC CBM
height.dia$W_treatment = NULL; height.dia$Comment = NULL
write.csv(height.dia, file = "Output/Cleaf_Cstem_Croot.csv", row.names = FALSE)



#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- Plot the original biomass data and compare with predicted ones
# Plot the original biomass data
rooms = as.factor(c("1","2","3","4","5","6"))

# Initial harvest data
avg.harvest.data = data.frame(matrix(vector(), 0, 10,
                                     dimnames=list(c(), c("Date", "Room", "Leafarea", "Leafarea_SE", "Leafmass", "Leafmass_SE", "Stemmass", "Stemmass_SE", "Rootmass", "Rootmass_SE"))),
                              stringsAsFactors=F)
avg.harvest.data[1,c("Leafarea", "Leafmass", "Stemmass", "Rootmass")] = colMeans(initial.harvest[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], na.rm = TRUE) # R8 = Average of leaf counts
avg.harvest.data[1,c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = (apply(initial.harvest[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], 2, sd))/(nrow(initial.harvest)-1)^0.5 # R9 = Standard error of leaf counts
avg.harvest.data$Date = as.Date("2016-01-07")
avg.harvest.data = avg.harvest.data[rep(seq_len(nrow(avg.harvest.data)), each=length(rooms)),]
avg.harvest.data$Room = rooms

# Intermediate harvest data 1
int.harvest.1 = unique(merge(int.harvest.1, height.dia[,c("Room","Pot")]))
int.harvest.1$Date = as.Date("2016-01-29")
for(i in 1:length(rooms)) {
  int.harvest.1.idn = subset(int.harvest.1,Room==rooms[i]) 
  avg.harvest.data[nrow(avg.harvest.data)+1, c("Leafarea", "Leafmass", "Stemmass", "Rootmass")] = colMeans(int.harvest.1.idn[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], na.rm = TRUE) # R8 = Average of leaf counts
  avg.harvest.data[nrow(avg.harvest.data), c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = (apply(int.harvest.1.idn[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], 2, sd, na.rm = TRUE))/(nrow(int.harvest.1.idn))^0.5 # R9 = Standard error of leaf counts
  avg.harvest.data$Date[nrow(avg.harvest.data)] = int.harvest.1.idn$Date[1]
  avg.harvest.data$Room[nrow(avg.harvest.data)] = rooms[i]
}

# Intermediate harvest data 2
int.harvest.2 = unique(merge(int.harvest.2, height.dia[,c("Room","Pot")]))
int.harvest.2$Date = as.Date("2016-02-10")
for(i in 1:length(rooms)) {
  int.harvest.2.idn = subset(int.harvest.2,Room==rooms[i]) 
  avg.harvest.data[nrow(avg.harvest.data)+1, c("Leafarea", "Leafmass", "Stemmass", "Rootmass")] = colMeans(int.harvest.2.idn[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], na.rm = TRUE) # R8 = Average of leaf counts
  avg.harvest.data[nrow(avg.harvest.data), c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = (apply(int.harvest.2.idn[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], 2, sd, na.rm = TRUE))/(nrow(int.harvest.2.idn))^0.5 # R9 = Standard error of leaf counts
  avg.harvest.data$Date[nrow(avg.harvest.data)] = int.harvest.2.idn$Date[1]
  avg.harvest.data$Room[nrow(avg.harvest.data)] = rooms[i]
}

# Final harvest data
final.harvest = unique(merge(final.harvest, height.dia[,c("Room","Pot")]))
final.harvest = final.harvest[with(final.harvest, order(Room,Date)), ]
for(i in 1:length(rooms)) {
  final.harvest.idn = subset(final.harvest,Room==rooms[i]) 
  for(j in 1:length(unique(final.harvest.idn$Date))) {
    final.harvest.idn.date = subset(final.harvest.idn, Date == unique(final.harvest.idn$Date)[j])
    avg.harvest.data[nrow(avg.harvest.data)+1, c("Leafarea", "Leafmass", "Stemmass", "Rootmass")] = colMeans(final.harvest.idn.date[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], na.rm = TRUE) # R8 = Average of leaf counts
    avg.harvest.data[nrow(avg.harvest.data), c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = (apply(final.harvest.idn.date[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], 2, sd, na.rm = TRUE))/(nrow(final.harvest.idn.date))^0.5 # R9 = Standard error of leaf counts
    avg.harvest.data$Date[nrow(avg.harvest.data)] = final.harvest.idn.date$Date[1]
    avg.harvest.data$Room[nrow(avg.harvest.data)] = rooms[i]
  }
}

melted.harvest.data = melt(avg.harvest.data, id.vars=c("Date","Room"))
melted.harvest.data$Group = as.factor("Measured")


#-----------------------------------------------------------------------------------------
# Data predicted for all 15 replicates from regression analyses done with all available harvest data
height.dia$Date = as.Date(height.dia$Date, format = "%d/%m/%Y")
Keeps = c("Date","Room","Leafarea", "Leafmass", "Stemmass", "Rootmass")
height.dia.crop = height.dia[ , keeps, drop = FALSE]
pred.data = data.frame(matrix(vector(), 0, 10,
                                     dimnames=list(c(), c("Date", "Room", "Leafarea", "Leafarea_SE", "Leafmass", "Leafmass_SE", "Stemmass", "Stemmass_SE", "Rootmass", "Rootmass_SE"))),
                              stringsAsFactors=F)
pred.data$Date = as.Date
for(i in 1:length(rooms)) {
  height.dia.crop.idn = subset(height.dia.crop,Room==rooms[i]) 
  for(j in 1:length(unique(height.dia.crop.idn$Date))) {
    height.dia.crop.idn.date = subset(height.dia.crop.idn, Date == unique(height.dia.crop.idn$Date)[j])
    pred.data[nrow(pred.data)+1, c("Leafarea", "Leafmass", "Stemmass", "Rootmass")] = colMeans(height.dia.crop.idn.date[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], na.rm = TRUE) # R8 = Average of leaf counts
    pred.data[nrow(pred.data), c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = (apply(height.dia.crop.idn.date[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], 2, sd, na.rm = TRUE))/(nrow(height.dia.crop.idn.date))^0.5 # R9 = Standard error of leaf counts
    pred.data$Date[nrow(pred.data)] = height.dia.crop.idn.date$Date[1]
    pred.data$Room[nrow(pred.data)] = rooms[i]
  }
}
pred.data$Date = as.Date(pred.data$Date)
melted.pred.data = melt(pred.data, id.vars=c("Date","Room"))
melted.pred.data$Group = as.factor("Predicted")
melted.data = rbind(melted.harvest.data,melted.pred.data)

# plot all harvest data
meas = as.factor(c("Leafarea", "Leafmass", "Stemmass", "Rootmass"))
error = as.factor(c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE"))
pd <- position_dodge(1) # move the overlapped errorbars horizontally
for (p in 1:length(meas)) {
  summary.data.Cpool = subset(melted.data,variable %in% meas[p])
  summary.error.Cpool = subset(melted.data,variable %in% error[p])
  summary.error.Cpool$parameter = summary.data.Cpool$value
  
  p3 = ggplot(summary.error.Cpool, aes(x=Date, y=parameter, group = interaction(Room,Group), colour=as.factor(Room), shape=as.factor(Group))) + 
    geom_point(position=pd) +
    geom_errorbar(position=pd,aes(ymin=parameter-value, ymax=parameter+value), colour="grey", width=2) +
    geom_line(position=pd,data = summary.error.Cpool, aes(x = Date, y = parameter, group = interaction(Room,Group), colour=as.factor(Room), linetype=as.factor(Group))) +
    ylab(paste(as.character(meas[p]),"(g DM)")) + 
    # xlab("Month") +
    # coord_trans(y = "log10") + ylab(paste(as.character(meas[p]),"(g DM)")) +
    # ggtitle("C pools - Measured (points) vs Modelled (lines)") +
    labs(colour="Temperature Room",shape="Data Type",linetype="Data Type") +
    theme_bw() +
    # annotate("text", x = min(summary.error.Cpool$Date), y = max(summary.error.Cpool$value), size = 14, label = paste(title[p])) +
    # theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.title = element_text(colour="black", size=12)) +
    theme(legend.text = element_text(colour="black", size = 12)) +
    # theme(legend.key.height=unit(0.9,"line")) +
    theme(legend.position = c(0.2,0.80)) +
    theme(legend.key = element_blank()) +
    theme(text = element_text(size=12)) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 14, vjust=0.3)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  p3
  ggsave(p3,filename=paste("Output/Measured_",meas[p],".png",sep=""))
}



