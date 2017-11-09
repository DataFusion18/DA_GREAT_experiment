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
# Plot the comparison graph from harvest data and data from standing seedlings
plots = list()
plots[[1]] = ggplot() +
  geom_line(data = leaf.0128.melt, aes(x = Code, y = value, group = interaction(variable,date), colour=factor(variable), linetype=factor(date))) +
  xlab("Code") +
  ylab("Leaf data") +
  ggtitle("Leaf data")
# ggsave(plots[[1]],filename=paste("Output/LA_LC_compare_1.png"))

plots[[2]] = ggplot() +
  geom_line(data = leaf.0209.melt, aes(x = Code, y = value, group = interaction(variable,date), colour=factor(variable), linetype=factor(date))) + 
  xlab("Code") +
  ylab("Leaf data") +
  ggtitle("Leaf data")
# ggsave(plots[[2]],filename=paste("Output/LA_LC_compare_2.png"))

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
# png(file = "Output/LA_harvest_vs_measurement_cf.png")
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
plots[[3]] = recordPlot()
# plot(leaf.0209.mod$total.LA + fitted(fit.res2), biomass.0210.raw$total.LA, col="red", pch=16, xlab="Measured leaf area" ~ (cm^{2}), ylab="Harvested leaf area" ~ (cm^{2}))
# abline(0, 1) 
# rmse.222 = sqrt( mean( ((leaf.0209.mod$total.LA + fitted(fit.res22)) - biomass.0210.raw$total.LA)^2) )
# plot(leaf.0209.mod$total.LA * mf2, biomass.0210.raw$total.LA, col="red", pch=16, xlab="Measured leaf area" ~ (cm^{2}), ylab="Harvested leaf area" ~ (cm^{2}))
# abline(0, 1) 

pdf(file = "Output/LA_LC_compare.pdf")
grid.arrange(plots[[1]],plots[[2]])
plots[[3]]
dev.off()
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
################### Analyse stem height diameter to estimate Leaf, Stem and Root biomass
# Import weekly height diameter data for 3 months (Diameter is in mm; Height is in cm)
height.dia <- read.csv("Data/GHS39_GREAT_MAIN_HEIGHTDIAMETER_20160108-20160229_L2.csv")
height.dia = height.dia[height.dia$W_treatment %in% as.factor("w"),]
height.dia$D = rowMeans(height.dia[,c("D1", "D2")], na.rm=TRUE)
height.dia = height.dia[complete.cases(height.dia), ]
height.dia$Date = as.Date(height.dia$Date, format = "%d/%m/%Y")
# height.dia.sub = subset(height.dia, Date %in% as.factor("29/02/2016"))


initial.harvest = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160107_L2.csv")
initial.harvest[ , c("Leafmass", "Stemmass", "Rootmass")] = initial.harvest[ , c("Leafmass", "Stemmass", "Rootmass")]
initial.harvest$Height = initial.harvest$Height/10 # Unit conversion from mm to cm
initial.harvest$Date = as.Date("2016-01-07")
initial.harvest$Room = 0

int.harvest.1 = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160129_L2.csv")
int.harvest.1 = int.harvest.1[int.harvest.1$W_treatment %in% as.factor("w"),]
int.harvest.1[ , c("Leafmass", "Stemmass", "Rootmass")] = int.harvest.1[ , c("Leafmass", "Stemmass", "Rootmass")]
int.harvest.1$Date = as.Date("2016-01-29")
int.harvest.1 = unique(merge(int.harvest.1, height.dia[,c("Room","Pot")]))
int.harvest.2 = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160210_L2.csv")
int.harvest.2 = int.harvest.2[int.harvest.2$W_treatment %in% as.factor("w"),]
int.harvest.2[ , c("Leafmass", "Stemmass", "Rootmass")] = int.harvest.2[ , c("Leafmass", "Stemmass", "Rootmass")]
int.harvest.2$Date = as.Date("2016-02-10")
int.harvest.2 = unique(merge(int.harvest.2, height.dia[,c("Room","Pot")]))

final.harvest = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160217-20160224_L3_with date.csv")
final.harvest$Date = as.Date(final.harvest$Date, format = "%d/%m/%Y")
final.harvest = final.harvest[final.harvest$W_treatment %in% as.factor("w"),]
# final.harvest$Room = NULL
final.harvest$Leafarea = rowSums(final.harvest[,c("Leafarea", "Leafarea_sub")], na.rm=T)
final.harvest$Leafmass = rowSums(final.harvest[,c("Leafmass", "Leafmass_sub")], na.rm=T)
final.harvest$Stemmass = rowSums(final.harvest[,c("Stemmass", "Stemmass_sub")], na.rm=T)
final.harvest$Rootmass = rowSums(final.harvest[,c("Rootmass", "Rootmass_sub")], na.rm=T)
final.harvest[c("Leafarea_sub","Leafmass_sub","Stemmass_sub","Rootmass_sub")] = NULL
final.harvest[ , c("Leafmass", "Stemmass", "Rootmass")] = final.harvest[ , c("Leafmass", "Stemmass", "Rootmass")]/1000 # Unit conversion from mg to g
# final.harvest = unique(merge(final.harvest, height.dia[,c("Room","Pot")]))

harvest.data = rbind(int.harvest.1, int.harvest.2, final.harvest)
# harvest.data = rbind(int.harvest.1, int.harvest.2, subset(final.harvest, select = -Date))
harvest.data$W_treatment = NULL
# harvest.data = rbind(harvest.data, initial.harvest)
harvest.data$D = rowMeans(harvest.data[,c("D1", "D2")], na.rm=TRUE)
harvest.data = harvest.data[!(harvest.data$Rootmass==0),]
harvest.data = harvest.data[with(harvest.data, order(Rootmass)), ]


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
################### Linear regression model fitting [log(stem_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
height.dia = height.dia[with(height.dia, order(Room)), ]
harvest.data$temp = ifelse(harvest.data$Room == 1, 18, ifelse(harvest.data$Room == 2, 21.5, ifelse(harvest.data$Room == 3, 25,
                 ifelse(harvest.data$Room == 4, 28.5,ifelse(harvest.data$Room == 5, 32, 35.5)))))
height.dia$temp = ifelse(height.dia$Room == 1, 18, ifelse(height.dia$Room == 2, 21.5, ifelse(height.dia$Room == 3, 25,
                 ifelse(height.dia$Room == 4, 28.5,ifelse(height.dia$Room == 5, 32, 35.5)))))

# Fit a linear regression by stem dia and height (ignoring temperature variation)
sm1 <- lm(log(Stemmass) ~ log(D) + log(Height), data=harvest.data)
# Fit a linear regression by stem dia, height and their interaction with temperature (including temperature effect)
sm2 <- lm(log(Stemmass) ~ log(D) + log(Height) + log(D) : temp + log(Height) : temp, data=harvest.data)

# Fit a linear regression for Rootmass by stem dia and height (ignoring temperature variation)
rm1 <- lm(log(Rootmass) ~ log(D) + log(Height), data=harvest.data)
# Fit a linear regression by stem dia, height and their interaction with temperature (including temperature effect)
rm2 <- lm(log(Rootmass) ~ log(D) + log(Height) + log(D) : temp + log(Height) : temp, data=harvest.data)

# Fit a linear regression for Leafmass by stem dia and height (ignoring temperature variation)
lm1 <- lm(log(Leafmass) ~ log(D) + log(Height), data=harvest.data)
# Fit a linear regression by stem dia, height and their interaction with temperature (including temperature effect)
lm2 <- lm(log(Leafmass) ~ log(D) + log(Height) + log(D) : temp + log(Height) : temp, data=harvest.data)

# Fit a linear regression for Leafarea by stem dia and height (ignoring temperature variation)
la1 <- lm(log(Leafarea) ~ log(D) + log(Height), data=harvest.data)
# Fit a linear regression by stem dia, height and their interaction with temperature (including temperature effect)
la2 <- lm(log(Leafarea) ~ log(D) + log(Height) + log(D) : temp + log(Height) : temp, data=harvest.data)

#-----------------------------------------------------------------------------------------

# Plot predictions
layout(matrix(c(1,2,0,0,3,4),3,2,byrow=TRUE), widths=c(1,1), heights=c(10,1,10))
visreg(sm1, "D", overlay=TRUE)
visreg(sm1, "Height", overlay=TRUE)
visreg(sm2, "D", by="temp", overlay=TRUE,legend=FALSE)
visreg(sm2, "Height", by="temp", overlay=TRUE)
plots[[1]] = recordPlot()
  
layout(matrix(c(1,2,0,0,3,4),3,2,byrow=TRUE), widths=c(1,1), heights=c(10,1,10))
visreg(rm1, "D", overlay=TRUE)
visreg(rm1, "Height", overlay=TRUE)
visreg(rm2, "D", by="temp", overlay=TRUE,legend=FALSE)
visreg(rm2, "Height", by="temp", overlay=TRUE)
plots[[2]] = recordPlot()

layout(matrix(c(1,2,0,0,3,4),3,2,byrow=TRUE), widths=c(1,1), heights=c(10,1,10))
visreg(lm1, "D", overlay=TRUE)
visreg(lm1, "Height", overlay=TRUE)
visreg(lm2, "D", by="temp", overlay=TRUE,legend=FALSE)
visreg(lm2, "Height", by="temp", overlay=TRUE)
plots[[3]] = recordPlot()

layout(matrix(c(1,2,0,0,3,4),3,2,byrow=TRUE), widths=c(1,1), heights=c(10,1,10))
visreg(la1, "D", overlay=TRUE)
visreg(la1, "Height", overlay=TRUE)
visreg(la2, "D", by="temp", overlay=TRUE,legend=FALSE)
visreg(la2, "Height", by="temp", overlay=TRUE)
plots[[4]] = recordPlot()

pdf(file = "Output/3.model_comparison.pdf")
plots[[1]]; plots[[2]]; plots[[3]]; plots[[4]]
dev.off()


#-----------------------------------------------------------------------------------------
# Save model sumary and stat comparison
sink("Output/3.model_comparison.txt")
cat("Stemmass models:\n----------------\n### Linear regression ignoring temperature variation:"); summary(sm1)
cat("\n### Linear regression considering interaction with temperature:"); summary(sm2)
cat("### Comparison between both models:\n")
AIC(sm1, sm2); BIC(sm1, sm2)

cat("\n\nRootmass models:\n----------------\n### Linear regression ignoring temperature variation:");  summary(rm1)
cat("\n### Linear regression considering interaction with temperature:"); summary(rm2)
cat("### Comparison between both models:\n")
AIC(rm1, rm2); BIC(rm1, rm2)

cat("\n\nLeafmass models:\n----------------\n### Linear regression ignoring temperature variation:"); summary(lm1)
cat("\n### Linear regression considering interaction with temperature:"); summary(lm2)
cat("### Comparison between both models:\n")
AIC(lm1, lm2); BIC(lm1, lm2)

cat("\n\nLeafarea models:\n----------------\n### Linear regression ignoring temperature variation:"); summary(la1)
cat("\n### Linear regression considering interaction with temperature:"); summary(la2)
cat("### Comparison between both models:\n")
AIC(la1, la2); BIC(la1, la2)
sink()
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
################### Linear regression model fitting [log(stem_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
# Model fits for individual temperature treatments
# Units are: masses = g; height = cm; dia = mm, leafarea = cm^2
height.dia = height.dia[with(height.dia, order(Room)), ]
# model.summary = data.frame(Model = as.factor(c("sm","rm","lm","la","sm.comb","rm.comb","lm.comb","la.comb")), 
#                            RMSE = numeric(8), error = numeric(8))
model.rmse = data.frame(attributes = as.factor(c("sm","rm","lm","la")),
                        rmse.individual = numeric(4), rmse.combined = numeric(4))
model.error = data.frame(attributes = as.factor(c("sm","rm","lm","la")),
                         error.individual = numeric(4), error.combined = numeric(4))
# model.rmse<- model.rmse[seq(dim(model.rmse)[1],1),]
# model.error<- model.error[seq(dim(model.error)[1],1),]

fit.sm = list()
for (i in 1:(length(unique(harvest.data$Room)))) {
# for (i in 1:(length(unique(harvest.data$Room))-1)) {
    harvest.data.ind = subset(harvest.data, Room %in% i)
  height.dia.ind = subset(height.dia, Room %in% i)
  fit.sm[[i]] <- lm(log(Stemmass) ~ log(D) + log(Height), data=harvest.data.ind)
  # summary(fit.sm[[i]])
  # Estimate the stemmass from the fitted linear regression equation
  eq = function(x,y){exp(coefficients(fit.sm[[i]])[1] + coefficients(fit.sm[[i]])[2] * log(x)  + coefficients(fit.sm[[i]])[3] * log(y))}
  
  # Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
  height.dia$Stemmass[(1+(i-1)*nrow(height.dia.ind)):(i*nrow(height.dia.ind))] = eq(height.dia.ind$D,height.dia.ind$Height)
  if (i == 1) {
    rmse.sm = sum ((exp(fitted(fit.sm[[i]])) - harvest.data.ind$Stemmass)^2)
    }
  if (i > 1) {
    rmse.sm = rmse.sm + sum((exp(fitted(fit.sm[[i]])) - harvest.data.ind$Stemmass)^2)
    }
}
model.rmse$rmse.individual[1] = sqrt( ( rmse.sm ) / nrow(harvest.data))
model.error$error.individual[1] = model.rmse$rmse.individual[1] / mean(harvest.data$Stemmass) * 100 #Percentage Error

#-----------------------------------------------------------------------------------------

################### Linear regression model fitting [log(root_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
# Fit the model with initial data (30) and intermediate (18*2=36) and final (90) harvest data for all seedlings (156 data in total)
# Units are: masses = g; height = cm; dia = mm
fit.rm = list()
for (i in 1:(length(unique(harvest.data$Room)))) {
# for (i in 1:(length(unique(harvest.data$Room))-1)) {
  harvest.data.ind = subset(harvest.data, Room %in% i)
  height.dia.ind = subset(height.dia, Room %in% i)
  fit.rm[[i]] <- lm(log(Rootmass) ~ log(D) + log(Height), data=harvest.data.ind)
  # Estimate the stemmass from the fitted linear regression equation
  eq = function(x,y,t){exp(coefficients(fit.rm[[i]])[1] + coefficients(fit.rm[[i]])[2] * log(x)  + coefficients(fit.rm[[i]])[3] * log(y))}
  
  #-----------------------------
  # When we consider the temperature interactions
  # fit.rm[[i]] <- lm(log(Rootmass) ~ log(D) + log(Height) + log(D) : temp + log(Height) : temp, data=harvest.data.ind) # use temp interaction
  # 
  # # Estimate the stemmass from the fitted linear regression equation
  # eq = function(x,y,t){exp(coefficients(fit.rm[[i]])[1] + coefficients(fit.rm[[i]])[2] * log(x)  + coefficients(fit.rm[[i]])[3] * log(y) +
  #                          coefficients(fit.rm[[i]])[4] * log(x) * t + coefficients(fit.rm[[i]])[5] * log(y) * t)}
  #-----------------------------
  
  # Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
  height.dia$Rootmass[(1+(i-1)*nrow(height.dia.ind)):(i*nrow(height.dia.ind))] = eq(height.dia.ind$D,height.dia.ind$Height,height.dia.ind$temp)
  if (i == 1) {
    rmse.rm = sum ((exp(fitted(fit.rm[[i]])) - harvest.data.ind$Rootmass)^2)
  }
  if (i > 1) {
    rmse.rm = rmse.rm + sum((exp(fitted(fit.rm[[i]])) - harvest.data.ind$Rootmass)^2)
  }
}
model.rmse$rmse.individual[2] = sqrt( ( rmse.rm ) / nrow(harvest.data))
model.error$error.individual[2] = model.rmse$rmse.individual[2] / mean(harvest.data$Rootmass) * 100 #Percentage Error


#-----------------------------------------------------------------------------------------
################### Linear regression model fitting [log(leaf_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
# Fit the model with initial data (30) and intermediate (18*2=36) and final (90) harvest data for all seedlings (156 data in total)
# Units are: masses = g; height = cm; dia = mm
fit.lm = list()
for (i in 1:(length(unique(harvest.data$Room)))) {
# for (i in 1:(length(unique(harvest.data$Room))-1)) {
  harvest.data.ind = subset(harvest.data, Room %in% i)
  height.dia.ind = subset(height.dia, Room %in% i)
  fit.lm[[i]] <- lm(log(Leafmass) ~ log(D) + log(Height), data=harvest.data.ind)
  
  # Estimate the stemmass from the fitted linear regression equation
  eq = function(x,y){exp(coefficients(fit.lm[[i]])[1] + coefficients(fit.lm[[i]])[2] * log(x)  + coefficients(fit.lm[[i]])[3] * log(y))}
  
  # Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
  height.dia$Leafmass[(1+(i-1)*nrow(height.dia.ind)):(i*nrow(height.dia.ind))] = eq(height.dia.ind$D,height.dia.ind$Height)
  if (i == 1) {
    rmse.lm = sum ((exp(fitted(fit.lm[[i]])) - harvest.data.ind$Leafmass)^2)
  }
  if (i > 1) {
    rmse.lm = rmse.lm + sum((exp(fitted(fit.lm[[i]])) - harvest.data.ind$Leafmass)^2)
  }
}
model.rmse$rmse.individual[3] = sqrt( ( rmse.lm ) / nrow(harvest.data))
model.error$error.individual[3] = model.rmse$rmse.individual[3] / mean(harvest.data$Leafmass) * 100 #Percentage Error


#-----------------------------------------------------------------------------------------
################### Linear regression model fitting [log(leaf_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
# Fit the model with initial data (30) and intermediate (18*2=36) and final (90) harvest data for all seedlings (156 data in total)
# Units are: masses = g; height = cm; dia = mm; leaf area = cm2
fit.la = list()
for (i in 1:(length(unique(harvest.data$Room)))) {
# for (i in 1:(length(unique(harvest.data$Room))-1)) {
  harvest.data.ind = subset(harvest.data, Room %in% i)
  height.dia.ind = subset(height.dia, Room %in% i)
  fit.la[[i]] <- lm(log(Leafarea) ~ log(D) + log(Height), data=harvest.data.ind)
  
  # Estimate the stemmass from the fitted linear regression equation
  eq = function(x,y){exp(coefficients(fit.la[[i]])[1] + coefficients(fit.la[[i]])[2] * log(x)  + coefficients(fit.la[[i]])[3] * log(y))}
  
  # Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
  height.dia$Leafarea[(1+(i-1)*nrow(height.dia.ind)):(i*nrow(height.dia.ind))] = eq(height.dia.ind$D,height.dia.ind$Height)
  if (i == 1) {
    rmse.la = sum (((exp(fitted(fit.la[[i]])) - harvest.data.ind$Leafarea)/100)^2) # unit consersion from cm2 to dm2
  }
  if (i > 1) {
    rmse.la = rmse.la + sum(((exp(fitted(fit.la[[i]])) - harvest.data.ind$Leafarea)/100)^2) # unit consersion from cm2 to dm2
  }
}
model.rmse$rmse.individual[4] = sqrt( ( rmse.la ) / nrow(harvest.data))
model.error$error.individual[4] = model.rmse$rmse.individual[4] / (mean(harvest.data$Leafarea)/100) * 100 #Percentage Error


#-----------------------------------------------------------------------------------------
# Model fits for all combined temperature treatments
# Fit the model with initial data (30) and intermediate (18*2=36) and final (90) harvest data for all seedlings (156 data in total)
fit.sm.combined <- lm(log(Stemmass) ~ log(D) + log(Height), data=harvest.data)
# summary(fit.sm.combined) # show results
model.rmse$rmse.combined[1] = sqrt( mean( (exp(fitted(fit.sm.combined)) - harvest.data$Stemmass)^2) )
model.error$error.combined[1] = model.rmse$rmse.combined[1] / mean(harvest.data$Stemmass) * 100 #Percentage Error
# cat("Linear regression model fitting: log(stem_mass) =", coefficients(fit.sm.combined)[1], "+", coefficients(fit.sm.combined)[2],
#     "* log(diameter) +", coefficients(fit.sm.combined)[3], "* log(height) \nwith percentage error =", percentage.error.sm, "%")

# # Estimate the stemmass from the fitted linear regression equation
# eq = function(x,y){exp(coefficients(fit.sm.combined)[1] + coefficients(fit.sm.combined)[2] * log(x)  + coefficients(fit.sm.combined)[3] * log(y))}
# 
# # Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
# height.dia$Stemmass.comb = eq(height.dia$D,height.dia$Height)


fit.rm.combined <- lm(log(Rootmass) ~ log(D) + log(Height), data=harvest.data)
# summary(fit.rm) # show results
model.rmse$rmse.combined[2] = sqrt( mean( (exp(fitted(fit.rm.combined)) - harvest.data$Rootmass)^2) )
model.error$error.combined[2] = model.rmse$rmse.combined[2] / mean(harvest.data$Rootmass) * 100 #Percentage Error
# percentage.error.rm = (sqrt ( sum((harvest.data$Rootmass - exp(fitted(fit.rm)))**2) / (length(fit.rm$residuals) - length(fit.rm$coefficients)))) / mean(harvest.data$Rootmass) * 100 #Percentage Error
# cat("Linear regression model fitting: log(root_mass) = ", coefficients(fit.rm.combined)[1], "+", coefficients(fit.rm.combined)[2],
#     "* log(diameter) +", coefficients(fit.rm.combined)[3], "* log(height) \nwith percentage error =", percentage.error.rm, "%")

# # Estimate the stemmass from the fitted linear regression equation
# eq = function(x,y){exp(coefficients(fit.rm.combined)[1] + coefficients(fit.rm.combined)[2] * log(x)  + coefficients(fit.rm.combined)[3] * log(y))}
# 
# # Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
# height.dia$Rootmass.comb = eq(height.dia$D,height.dia$Height)

fit.lm.combined <- lm(log(Leafmass) ~ log(D) + log(Height), data=harvest.data)
# summary(fit.lm) # show results
model.rmse$rmse.combined[3] = sqrt( mean( (exp(fitted(fit.lm.combined)) - harvest.data$Leafmass)^2) )
model.error$error.combined[3] = model.rmse$rmse.combined[3] / mean(harvest.data$Leafmass) * 100 #Percentage Error
# percentage.error.lm = (sqrt ( sum((harvest.data$Leafmass - exp(fitted(fit.lm)))**2) / (length(fit.lm$residuals) - length(fit.lm$coefficients)))) / mean(harvest.data$Leafmass) * 100 #Percentage Error
# cat("Linear regression model fitting: log(leaf_mass) = ", coefficients(fit.lm.combined)[1], "+", coefficients(fit.lm.combined)[2],
#     "* log(diameter) +", coefficients(fit.lm.combined)[3], "* log(height) \nwith percentage error =", percentage.error.lm, "%")

# # Estimate the stemmass from the fitted linear regression equation
# eq = function(x,y){exp(coefficients(fit.lm.combined)[1] + coefficients(fit.lm.combined)[2] * log(x)  + coefficients(fit.lm.combined)[3] * log(y))}
# 
# # Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
# height.dia$Leafmass.comb = eq(height.dia$D,height.dia$Height)

fit.la.combined <- lm(log(Leafarea) ~ log(D) + log(Height), data=harvest.data)
# summary(fit.la) # show results
# R = data.frame(D = harvest.data$D, Height = harvest.data$Height, Leafarea = harvest.data$Leafarea, Fitted = exp(fitted(fit.la)))
model.rmse$rmse.combined[4] = sqrt( mean( ((exp(fitted(fit.la.combined)) - harvest.data$Leafarea)/100)^2) )
model.error$error.combined[4] = model.rmse$rmse.combined[4] / (mean(harvest.data$Leafarea)/100) * 100 #Percentage Error
# percentage.error.la = (sqrt ( sum((harvest.data$Leafarea - exp(fitted(fit.la)))**2) / (length(fit.la$residuals) - length(fit.la$coefficients)))) / mean(harvest.data$Leafarea) * 100 #Percentage Error
# cat("Linear regression model fitting: log(leaf_area) = ", coefficients(fit.la.combined)[1], "+", coefficients(fit.la.combined)[2],
#     "* log(diameter) +", coefficients(fit.la.combined)[3], "* log(height)\nwith percentage error =", percentage.error.la, "%")

# # Estimate the stemmass from the fitted linear regression equation
# eq = function(x,y){exp(coefficients(fit.la.combined)[1] + coefficients(fit.la.combined)[2] * log(x)  + coefficients(fit.la.combined)[3] * log(y))}
# 
# # Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
# height.dia$Leafarea.comb = eq(height.dia$D,height.dia$Height)
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Plotting observation and modelled data
plots = list() 
par(mfrow = c(2, 2))
plot(height.dia$Height,height.dia$Stemmass,col="red",main="Height vs Stemmass", pch=15, xlab="Height (cm)", ylab="Stemmass (g DM)")
lines(harvest.data$Height,harvest.data$Stemmass,type="p", col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
# dev.off()
# rmse.stemmass.height = sqrt(mean((eq(harvest.data$D,harvest.data$Height)-harvest.data$Stemmass)^2,na.rm=TRUE))

plot(height.dia$D,height.dia$Stemmass, col="red", main="Diameter vs Stemmass", pch=16, xlab="Diameter (mm)", ylab="Stemmass (g DM)")
lines(harvest.data$D,harvest.data$Stemmass,type="p",col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
# dev.off()
# plot(y,x,col="red",main="Height vs Diameter", pch=17, xlab="Height (cm)", ylab="Diameter (cm)")
# lines(harvest.data$Height,harvest.data$D,type="p",col="green", pch=20)
# legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
#-----------------------------------------------------------------------------------------

# Plotting observation and modelled data
# png(file = "Output/2.Height_Rootmass.png")
plot(height.dia$Height,height.dia$Rootmass,col="red",main="Height vs Rootmass", pch=15, xlab="Height (cm)", ylab="Rootmass (g DM)")
lines(harvest.data$Height,harvest.data$Rootmass,type="p", col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
# dev.off()
# png(file = "Output/2.Diameter_Rootmass.png")
plot(height.dia$D,height.dia$Rootmass, col="red", main="Diameter vs Rootmass", pch=16, xlab="Diameter (mm)", ylab="Rootmass (g DM)")
lines(harvest.data$D,harvest.data$Rootmass,type="p",col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
plots[[1]] = recordPlot()
# dev.off()
# plot(y,x,col="red",main="Height vs Diameter", pch=17, xlab="Height (cm)", ylab="Diameter (cm)")
# lines(harvest.data$Height,harvest.data$D,type="p",col="green", pch=20)
# legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)

#-----------------------------------------------------------------------------------------
# Plotting observation and modelled data
# png(file = "Output/3.Height_Leafmass.png")
par(mfrow = c(2, 2))
plot(height.dia$Height,height.dia$Leafmass,col="red",main="Height vs Leafmass", pch=15, xlab="Height (cm)", ylab="Leafmass (g DM)")
lines(harvest.data$Height,harvest.data$Leafmass,type="p", col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
# dev.off()
# png(file = "Output/3.Diameter_Leafmass.png")
plot(height.dia$D,height.dia$Leafmass, col="red", main="Diameter vs Leafmass", pch=16, xlab="Diameter (mm)", ylab="Leafmass (g DM)")
lines(harvest.data$D,harvest.data$Leafmass,type="p",col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
# dev.off()
# plot(y,x,col="red",main="Height vs Diameter", pch=17, xlab="Height (cm)", ylab="Diameter (cm)")
# lines(harvest.data$Height,harvest.data$D,type="p",col="green", pch=20)
# legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)

#-----------------------------------------------------------------------------------------
# Plotting observation and modelled data
# png(file = "Output/4.Height_Leafarea.png")
plot(height.dia$Height,height.dia$Leafarea,col="red",main="Height vs Leafarea", pch=15, xlab="Height (cm)", ylab="Leafarea" ~ (cm^{2}))
lines(harvest.data$Height,harvest.data$Leafarea,type="p", col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
# dev.off()
# png(file = "Output/4.Diameter_Leafarea.png")
plot(height.dia$D,height.dia$Leafarea, col="red", main="Diameter vs Leafarea", pch=16, xlab="Diameter (mm)", ylab="Leafarea" ~ (cm^{2}))
lines(harvest.data$D,harvest.data$Leafarea,type="p",col="green", pch=20)
legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)
plots[[2]] = recordPlot()
# dev.off()
# plot(y,x,col="red",main="Height vs Diameter", pch=17, xlab="Height (cm)", ylab="Diameter (cm)")
# lines(harvest.data$Height,harvest.data$D,type="p",col="green", pch=20)
# legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)

pdf(file = "Output/1.tree_attributes_over_time.pdf")
plots[[1]]
plots[[2]]
dev.off()
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
# summerizing the model fits for combined models
# model.summary = data.frame(Model = as.character(c("fit.sm","fit.rm","fit.lm","fit.la")), Adjusted.R.squared = c(summary(fit.sm)$adj.r.squared,summary(fit.rm)$adj.r.squared,
#                                                                                                                 summary(fit.lm)$adj.r.squared,summary(fit.la)$adj.r.squared),
#                            Residual.standard.error = c(sigma(fit.sm),sigma(fit.rm),sigma(fit.lm),sigma(fit.la)), percentage.error = c(percentage.error.sm,percentage.error.rm,percentage.error.lm,percentage.error.la))
# Plot measuremnts vs fitted points to judge the model fits
# png(file = "Output/model_fit.png")
par(mfrow=c(2,2), mar=c(3, 4, 1, 1), mgp=c(2,1,0))
plot(exp(fitted(fit.sm.combined)), harvest.data$Stemmass, col=rainbow(length(harvest.data$Room))[rank(harvest.data$Room)], pch=16, xlab="Fitted stem mass (g)", ylab="Measured stem mass (g)")
abline(0, 1)
text(min(exp(fitted(fit.sm.combined))), max(harvest.data$Stemmass)*0.9, paste("Adj R-squared =", format(round(summary(fit.sm.combined)$adj.r.squared, 2)),
                                                                     "\nResidual SE =", format(round(sigma(fit.sm.combined), 2))), pos = 4)
legend('bottomright',legend=sort(unique(harvest.data$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data$Room))),pch=19,
       bty="n",ncol=2,cex=0.8,pt.cex=1.2)

plot(exp(fitted(fit.rm.combined)), harvest.data$Rootmass, col=harvest.data$Room, pch=16, xlab="Fitted root mass (g)", ylab="Measured root mass (g)")
abline(0, 1)
text(min(exp(fitted(fit.rm.combined))), max(harvest.data$Rootmass)*0.9, paste("Adj R-squared =", format(round(summary(fit.rm.combined)$adj.r.squared, 2)),
                                                                     "\nResidual SE =", format(round(sigma(fit.rm.combined), 2))), pos = 4)
legend('bottomright',legend=sort(unique(harvest.data$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data$Room))),pch=19,
       bty="n",ncol=2,cex=0.8,pt.cex=1.2)

plot(exp(fitted(fit.lm.combined)), harvest.data$Leafmass, col=harvest.data$Room, pch=16, xlab="Fitted leaf mass (g)", ylab="Measured leaf mass (g)")
abline(0, 1)
text(min(exp(fitted(fit.lm.combined))), max(harvest.data$Leafmass)*0.9, paste("Adj R-squared =", format(round(summary(fit.lm.combined)$adj.r.squared, 2)),
                                                                     "\nResidual SE =", format(round(sigma(fit.lm.combined), 2))), pos = 4)
legend('bottomright',legend=sort(unique(harvest.data$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data$Room))),pch=19,
       bty="n",ncol=2,cex=0.8,pt.cex=1.2)

plot(exp(fitted(fit.la.combined)), harvest.data$Leafarea, col=harvest.data$Room, pch=16, xlab = expression("Fitted leaf area" ~ (cm^{2})), ylab="Measured leaf area" ~ (cm^{2}))
abline(0, 1)
text(min(exp(fitted(fit.la.combined))), max(harvest.data$Leafarea)*0.9, paste("Adj R-squared =", format(round(summary(fit.la.combined)$adj.r.squared, 2)),
                                                                     "\nResidual SE =", format(round(sigma(fit.la.combined), 2))), pos = 4)
legend('bottomright',legend=sort(unique(harvest.data$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data$Room))),pch=19,
       bty="n",ncol=2,cex=0.8,pt.cex=1.2)
plots[[3]] = recordPlot()
# dev.off()

# plot residuals against fitted values and quantile-quantile plot
# png(file = "Output/model_residuals.png", units="px", width=1500, height=2000, res=300)
par(mfrow=c(4,2), mar=c(3, 4, 1, 1), mgp=c(2,1,0))
plot(resid(fit.sm.combined) ~ exp(fitted(fit.sm.combined)), col=harvest.data$Room, xlab="Fitted stem mass (g)", ylab="Residual stem mass (g)")
abline(h=0)
qqPlot(residuals(fit.sm.combined), ylab="Residual stem mass (g)")
legend('bottomright',legend=sort(unique(harvest.data$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data$Room))),pch=19,
       bty="n",ncol=4,cex=0.8,pt.cex=1.2)

plot(resid(fit.rm.combined) ~ exp(fitted(fit.rm.combined)), col=harvest.data$Room, xlab="Fitted root mass (g)", ylab="Residual root mass (g)")
abline(h=0)
qqPlot(residuals(fit.rm.combined), ylab="Residual stem mass (g)")
legend('bottomright',legend=sort(unique(harvest.data$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data$Room))),pch=19,
       bty="n",ncol=4,cex=0.8,pt.cex=1.2)

plot(resid(fit.lm.combined) ~ exp(fitted(fit.lm.combined)), col=harvest.data$Room, xlab="Fitted leaf mass (g)", ylab="Residual leaf mass (g)")
abline(h=0)
qqPlot(residuals(fit.lm.combined), ylab="Residual leaf mass (g)")
legend('bottomright',legend=sort(unique(harvest.data$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data$Room))),pch=19,
       bty="n",ncol=4,cex=0.8,pt.cex=1.2)

plot(resid(fit.la.combined) ~ exp(fitted(fit.la.combined)), col=harvest.data$Room, xlab = expression("Fitted leaf area" ~ (cm^{2})), ylab="Residual leaf area" ~ (mm^{2}))
abline(h=0)
qqPlot(residuals(fit.la.combined), ylab="Residual leaf area" ~ (cm^{2}))
legend('bottomright',legend=sort(unique(harvest.data$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data$Room))),pch=19,
       bty="n",ncol=4,cex=0.8,pt.cex=1.2)
plots[[4]] = recordPlot()
# dev.off()

pdf(file = "Output/3.model_attributes.pdf")
plots[[3]]
plots[[4]]
dev.off()
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
# Plot measuremnts vs fitted points to judge the model fits
plots = list()
palette = c("#FF0000FF", "yellow3", "#00FF00FF", "#00FFFFFF", "#0000FFFF", "#FF00FFFF")
for (i in 1:(length(unique(harvest.data$Room)))) {
# for (i in 1:(length(unique(harvest.data$Room))-1)) {
  harvest.data.ind = subset(harvest.data, Room %in% i)
  
  par(mfrow=c(2,2), mar=c(3, 4, 1, 1), mgp=c(2,1,0), oma=c(0,0,1,0))
  plot(exp(fitted(fit.sm[[i]])), harvest.data.ind$Stemmass, col=palette[i], pch=16, xlab="Fitted stem mass (g)", ylab="Measured stem mass (g)")
  abline(0, 1) 
  text(min(exp(fitted(fit.sm[[i]]))), max(harvest.data.ind$Stemmass)*0.85, 
       paste("Adj R-squared =", format(round(summary(fit.sm[[i]])$adj.r.squared, 2)),
             "\nResidual SE =", format(round(sigma(fit.sm[[i]]), 2)),
             "\nlog(SM) =", format(round(coefficients(fit.sm[[i]])[1],2)), "+", format(round(coefficients(fit.sm[[i]])[2],2)),
             "* log(D)\n+", format(round(coefficients(fit.sm[[i]])[3],2)), "* log(H)"), pos = 4)
  # cat("Linear regression model fitting: log(leaf_area) = ", coefficients(fit.la.combined)[1], "+", coefficients(fit.la.combined)[2],
  #     "* log(diameter) +", coefficients(fit.la.combined)[3], "* log(height)\nwith percentage error =", percentage.error.la, "%")
  
  # legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Rooms",lty=NULL,col=palette[i],pch=19,
  #        bty="n",ncol=2,cex=0.8,pt.cex=1.2)

  plot(exp(fitted(fit.rm[[i]])), harvest.data.ind$Rootmass, col=palette[i], pch=16, xlab="Fitted root mass (g)", ylab="Measured root mass (g)")
  abline(0, 1) 
  text(min(exp(fitted(fit.rm[[i]]))), max(harvest.data.ind$Rootmass)*0.85, 
       paste("Adj R-squared =", format(round(summary(fit.rm[[i]])$adj.r.squared, 2)),
             "\nResidual SE =", format(round(sigma(fit.rm[[i]]), 2)),
             "\nlog(SM) =", format(round(coefficients(fit.rm[[i]])[1],2)), "+", format(round(coefficients(fit.rm[[i]])[2],2)),
             "* log(D)\n+", format(round(coefficients(fit.rm[[i]])[3],2)), "* log(H)"), pos = 4)
  # legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data.ind$Room))),pch=19,
  #        bty="n",ncol=2,cex=0.8,pt.cex=1.2)
  
  plot(exp(fitted(fit.lm[[i]])), harvest.data.ind$Leafmass, col=palette[i], pch=16, xlab="Fitted leaf mass (g)", ylab="Measured leaf mass (g)")
  abline(0, 1) 
  text(min(exp(fitted(fit.lm[[i]]))), max(harvest.data.ind$Leafmass)*0.85, 
       paste("Adj R-squared =", format(round(summary(fit.lm[[i]])$adj.r.squared, 2)),
             "\nResidual SE =", format(round(sigma(fit.lm[[i]]), 2)),
             "\nlog(SM) =", format(round(coefficients(fit.lm[[i]])[1],2)), "+", format(round(coefficients(fit.lm[[i]])[2],2)),
             "* log(D)\n+", format(round(coefficients(fit.lm[[i]])[3],2)), "* log(H)"), pos = 4)
  # legend('bottomright',legend=sort(unique(harvest.data$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data.ind$Room))),pch=19,
  #        bty="n",ncol=2,cex=0.8,pt.cex=1.2)
  
  plot(exp(fitted(fit.la[[i]])), harvest.data.ind$Leafarea, col=palette[i], pch=16, xlab = expression("Fitted leaf area" ~ (cm^{2})), ylab="Measured leaf area" ~ (cm^{2}))
  abline(0, 1) 
  text(min(exp(fitted(fit.la[[i]]))), max(harvest.data.ind$Leafarea)*0.85, 
       paste("Adj R-squared =", format(round(summary(fit.la[[i]])$adj.r.squared, 2)),
             "\nResidual SE =", format(round(sigma(fit.la[[i]]), 2)),
             "\nlog(SM) =", format(round(coefficients(fit.la[[i]])[1],2)), "+", format(round(coefficients(fit.la[[i]])[2],2)),
             "* log(D)\n+", format(round(coefficients(fit.la[[i]])[3],2)), "* log(H)"), pos = 4)
  legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Room",lty=NULL,col=palette[i],pch=19,
         bty="n",ncol=1,cex=0.8,pt.cex=1.2)
  mtext(paste("Model fits for individual temperature = room #",i), side = 3, line = -0.5, outer = TRUE)
  plots[[1+(i-1)*2]] = recordPlot()
  
  # plot residuals against fitted values and quantile-quantile plot
  # png(file = "Output/model_residuals.png", units="px", width=1500, height=2000, res=300)
  par(mfrow=c(4,2), mar=c(3, 4, 1, 1), mgp=c(2,1,0))
  plot(resid(fit.sm[[i]]) ~ exp(fitted(fit.sm[[i]])), col=palette[i], xlab="Fitted stem mass (g)", ylab="Residual stem mass (g)")
  abline(h=0)
  qqPlot(residuals(fit.sm[[i]]), ylab="Residual stem mass (g)")
  # legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data.ind$Room))),pch=19,
  #        bty="n",ncol=4,cex=0.8,pt.cex=1.2)
  
  plot(resid(fit.rm[[i]]) ~ exp(fitted(fit.rm[[i]])), col=palette[i], xlab="Fitted root mass (g)", ylab="Residual root mass (g)")
  abline(h=0)
  qqPlot(residuals(fit.rm[[i]]), ylab="Residual stem mass (g)")
  # legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data.ind$Room))),pch=19,
  #        bty="n",ncol=4,cex=0.8,pt.cex=1.2)
  
  plot(resid(fit.lm[[i]]) ~ exp(fitted(fit.lm[[i]])), col=palette[i], xlab="Fitted leaf mass (g)", ylab="Residual leaf mass (g)")
  abline(h=0)
  qqPlot(residuals(fit.lm[[i]]), ylab="Residual leaf mass (g)")
  # legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data.ind$Room))),pch=19,
  #        bty="n",ncol=4,cex=0.8,pt.cex=1.2)
  
  plot(resid(fit.la[[i]]) ~ exp(fitted(fit.la[[i]])), col=palette[i], xlab = expression("Fitted leaf area" ~ (cm^{2})), ylab="Residual leaf area" ~ (mm^{2}))
  abline(h=0)
  qqPlot(residuals(fit.la[[i]]), ylab="Residual leaf area" ~ (cm^{2}))
  legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Room",lty=NULL,col=palette[i],pch=19,
         bty="n",ncol=1,cex=0.8,pt.cex=1.2)
  mtext(paste("Model residuals for individual temperature = room #",i), side = 3, line = -0.5, outer = TRUE)
  plots[[i*2]] = recordPlot()
}
pdf(file = "Output/3.model_attributes_rooms.pdf")
plots
dev.off()
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
# Plot the comparison between both model structures
model.rmse.melt <- melt(model.rmse, id.vars = "attributes")
names(model.rmse.melt)[2] <- c("models")
model.rmse.melt$variables = as.factor("rmse")
model.error.melt <- melt(model.error, id.vars = "attributes")
names(model.error.melt)[2] <- c("models")
model.error.melt$variables = as.factor("error")
model.summary = rbind(model.rmse.melt,model.error.melt)
model.summary$models = ifelse(model.summary$models %in% as.factor(c("rmse.individual","error.individual")),"individual","combined")

png("Output/3.compare_models_ind_comb.png", units="px", width=2000, height=1600, res=220)

p1 = ggplot(data = model.summary, aes(x = attributes, y = value, group = models, fill = models)) +
  # ggplot(data = bic.melt, aes(x=factor(bic.melt$Treatment, levels=unique(as.character(bic.melt$Treatment))), y=BIC, fill = Model_setting)) +
  geom_bar(stat="identity", width = 0.5, position = "dodge") +
  facet_wrap(~ variables, scales = "free") +
  scale_fill_brewer(palette = "Set1", name = "Regression fitting") +
  xlab("Seedling attributes") +
  ylab("Model measures") +
  # ggtitle("BIC for various model settings") +
  theme_bw() +
  scale_x_discrete(breaks=c("la","lm","rm","sm"), labels=c("Leafarea","Leafmass","Rootmass","Stemmass")) +
  theme(legend.title = element_text(colour="black", size=10)) +
  theme(legend.text = element_text(colour="black", size=10)) +
  theme(legend.position = c(0.3,0.85)) +
  theme(legend.key.height=unit(1,"line")) +
  theme(legend.key = element_blank()) +
  theme(text = element_text(size=12)) +
  theme(axis.title.x = element_text(size = 12, vjust=-.2)) +
  theme(axis.title.y = element_text(size = 12, vjust=0.3)) +
  theme(axis.text.x = element_text(angle=45)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
print (p1)
dev.off()

#-----------------------------------------------------------------------------------------
# Print and save the model comparison summary
sink("Output/3.models_summary.txt")
cat("Stemmass models:")
cat("\n"); cat("Combined linear regression model:"); cat("\n")
cat("log(Stemmass) = ", coefficients(fit.sm.combined)[1], "+", 
    coefficients(fit.sm.combined)[2],"* log(Diameter) +", coefficients(fit.sm.combined)[3], "* log(Height)")
cat("\n"); cat("\n"); cat("Individual linear regression models:"); cat("\n")
for (i in 1:(length(unique(harvest.data$Room)))) {
  cat("log(Stemmass) = ", coefficients(fit.sm[[i]])[1], "+", 
    coefficients(fit.sm[[i]])[2],"* log(Diameter) +", coefficients(fit.sm[[i]])[3], "* log(Height)")
  cat("\n")
}

cat("\n"); cat("\n"); cat("Rootmass models:"); cat("\n")
cat("Combined linear regression model:"); cat("\n")
cat("log(Rootmass) = ", coefficients(fit.rm.combined)[1], "+", 
    coefficients(fit.rm.combined)[2],"* log(Diameter) +", coefficients(fit.rm.combined)[3], "* log(Height)")
cat("\n"); cat("\n"); cat("Individual linear regression models:"); cat("\n")
for (i in 1:(length(unique(harvest.data$Room)))) {
  cat("log(Stemmass) = ", coefficients(fit.rm[[i]])[1], "+", 
      coefficients(fit.rm[[i]])[2],"* log(Diameter) +", coefficients(fit.rm[[i]])[3], "* log(Height)")
  cat("\n")
}

cat("\n"); cat("\n"); cat("Leafmass models:"); cat("\n")
cat("Combined linear regression model:"); cat("\n")
cat("log(Leafmass) = ", coefficients(fit.lm.combined)[1], "+", 
    coefficients(fit.lm.combined)[2],"* log(Diameter) +", coefficients(fit.lm.combined)[3], "* log(Height)")
cat("\n"); cat("\n"); cat("Individual linear regression models:"); cat("\n")
for (i in 1:(length(unique(harvest.data$Room)))) {
  cat("log(Stemmass) = ", coefficients(fit.lm[[i]])[1], "+", 
      coefficients(fit.lm[[i]])[2],"* log(Diameter) +", coefficients(fit.lm[[i]])[3], "* log(Height)")
  cat("\n")
}

cat("\n"); cat("\n"); cat("Leafarea models:"); cat("\n")
cat("Combined linear regression model:"); cat("\n")
cat("log(Leafarea) = ", coefficients(fit.la.combined)[1], "+", 
    coefficients(fit.la.combined)[2],"* log(Diameter) +", coefficients(fit.la.combined)[3], "* log(Height)")
cat("\n"); cat("\n"); cat("Individual linear regression models:"); cat("\n")
for (i in 1:(length(unique(harvest.data$Room)))) {
  cat("log(Stemmass) = ", coefficients(fit.la[[i]])[1], "+", 
      coefficients(fit.la[[i]])[2],"* log(Diameter) +", coefficients(fit.la[[i]])[3], "* log(Height)")
  cat("\n")
}
sink()


#-----------------------------------------------------------------------------------------
# Save the biomass data for MCMC CBM
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
# int.harvest.1 = unique(merge(int.harvest.1, height.dia[,c("Room","Pot")]))
int.harvest.1$Date = as.Date("2016-01-29")
for(i in 1:length(rooms)) {
  int.harvest.1.idn = subset(int.harvest.1,Room==rooms[i]) 
  avg.harvest.data[nrow(avg.harvest.data)+1, c("Leafarea", "Leafmass", "Stemmass", "Rootmass")] = colMeans(int.harvest.1.idn[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], na.rm = TRUE) # R8 = Average of leaf counts
  avg.harvest.data[nrow(avg.harvest.data), c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = (apply(int.harvest.1.idn[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], 2, sd, na.rm = TRUE))/(nrow(int.harvest.1.idn))^0.5 # R9 = Standard error of leaf counts
  avg.harvest.data$Date[nrow(avg.harvest.data)] = int.harvest.1.idn$Date[1]
  avg.harvest.data$Room[nrow(avg.harvest.data)] = rooms[i]
}

# Intermediate harvest data 2
# int.harvest.2 = unique(merge(int.harvest.2, height.dia[,c("Room","Pot")]))
int.harvest.2$Date = as.Date("2016-02-10")
for(i in 1:length(rooms)) {
  int.harvest.2.idn = subset(int.harvest.2,Room==rooms[i]) 
  avg.harvest.data[nrow(avg.harvest.data)+1, c("Leafarea", "Leafmass", "Stemmass", "Rootmass")] = colMeans(int.harvest.2.idn[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], na.rm = TRUE) # R8 = Average of leaf counts
  avg.harvest.data[nrow(avg.harvest.data), c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = (apply(int.harvest.2.idn[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], 2, sd, na.rm = TRUE))/(nrow(int.harvest.2.idn))^0.5 # R9 = Standard error of leaf counts
  avg.harvest.data$Date[nrow(avg.harvest.data)] = int.harvest.2.idn$Date[1]
  avg.harvest.data$Room[nrow(avg.harvest.data)] = rooms[i]
}

# Final harvest data
# final.harvest = unique(merge(final.harvest, height.dia[,c("Room","Pot")]))
final.harvest = final.harvest[with(final.harvest, order(Room,Date)), ]
for(i in 1:length(rooms)) {
  final.harvest.idn = subset(final.harvest,Room==rooms[i]) 
  for(j in 1:2) {
    if (j==1) {
      final.harvest.idn.date = subset(final.harvest.idn, Date %in% as.Date(c("2016-02-17", "2016-02-18", "2016-02-19")))
    } else {
      final.harvest.idn.date = subset(final.harvest.idn, Date %in% as.Date(c("2016-02-22", "2016-02-23", "2016-02-24")))
    }
    avg.harvest.data[nrow(avg.harvest.data)+1, c("Leafarea", "Leafmass", "Stemmass", "Rootmass")] = colMeans(final.harvest.idn.date[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], na.rm = TRUE) # R8 = Average of leaf counts
    avg.harvest.data[nrow(avg.harvest.data), c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = (apply(final.harvest.idn.date[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], 2, sd, na.rm = TRUE))/(nrow(final.harvest.idn.date))^0.5 # R9 = Standard error of leaf counts
    avg.harvest.data$Date[nrow(avg.harvest.data)] = mean(final.harvest.idn.date$Date)
    avg.harvest.data$Room[nrow(avg.harvest.data)] = rooms[i]
  }
}

# for(j in 1:length(unique(final.harvest.idn$Date))) {
#   final.harvest.idn.date = subset(final.harvest.idn, Date == unique(final.harvest.idn$Date)[j])
#   avg.harvest.data[nrow(avg.harvest.data)+1, c("Leafarea", "Leafmass", "Stemmass", "Rootmass")] = colMeans(final.harvest.idn.date[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], na.rm = TRUE) # R8 = Average of leaf counts
#   avg.harvest.data[nrow(avg.harvest.data), c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = (apply(final.harvest.idn.date[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], 2, sd, na.rm = TRUE))/(nrow(final.harvest.idn.date))^0.5 # R9 = Standard error of leaf counts
#   avg.harvest.data$Date[nrow(avg.harvest.data)] = final.harvest.idn.date$Date[1]
#   avg.harvest.data$Room[nrow(avg.harvest.data)] = rooms[i]
# }

melted.harvest.data = melt(avg.harvest.data, id.vars=c("Date","Room"))
melted.harvest.data$Group = as.factor("Measured")


#-----------------------------------------------------------------------------------------
# Data predicted for all 15 replicates from regression analyses done with all available harvest data
keeps = c("Date","Room","Leafarea","Leafmass","Stemmass","Rootmass")
height.dia.crop = height.dia[ , keeps, drop = FALSE]
pred.data = data.frame(matrix(vector(), 0, 10,
                              dimnames=list(c(), c("Date", "Room", "Leafarea", "Leafarea_SE", "Leafmass", "Leafmass_SE", "Stemmass", "Stemmass_SE", "Rootmass", "Rootmass_SE"))),
                       stringsAsFactors=F)
pred.data$Date = as.Date(pred.data$Date)
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
plots = list()
meas = as.factor(c("Leafarea", "Leafmass", "Stemmass", "Rootmass"))
error = as.factor(c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE"))
pd <- position_dodge(1) # move the overlapped errorbars horizontally
for (p in 1:length(meas)) {
  summary.data.Cpool = subset(melted.data,variable %in% meas[p])
  summary.error.Cpool = subset(melted.data,variable %in% error[p])
  summary.error.Cpool$parameter = summary.data.Cpool$value
  
  plots[[p]] = ggplot(summary.error.Cpool, aes(x=Date, y=parameter, group = interaction(Room,Group), colour=as.factor(Room), shape=as.factor(Group))) + 
    geom_point(position=pd,size=2.5) +
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
  
  if (p==1) {
    plots[[p]] = plots[[p]] + ylab(expression(Leafarea~"("*cm^"2"*")"))
  }
  # ggsave(p3,filename=paste("Output/Measured_",meas[p],".png",sep=""))
}

pdf(file = "Output/2.tree_attributes_measured_vs_predicted.pdf",width=12, height=15)
print (do.call(grid.arrange,  plots))
# grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]])
dev.off() 

#-----------------------------------------------------------------------------------------
# Save the biomass data for MCMC CBM
write.csv(pred.data, file = "Output/modelled_data.csv", row.names = FALSE)
write.csv(avg.harvest.data, file = "Output/harvest_data.csv", row.names = FALSE)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- Plot all vs harvested (height and dia) data to explore the sampling effect on biomass prediction variation
height.dia$datatype = as.character("all")
harvest.data$datatype = as.character("harvested")
# harvest.data = unique(merge(harvest.data, height.dia[,c("Room","Pot")]))
harvest.data$Leafno = NULL
# height.dia$Room = NULL
harvest.data$temp = NULL
avg.harvest.data.sampling = rbind(height.dia,harvest.data)
keeps <- c("Date", "Room", "datatype", "Height", "D", "Leafarea", "Leafmass", "Stemmass", "Rootmass")
avg.harvest.data.sampling = avg.harvest.data.sampling[ , keeps, drop = FALSE]
unique(avg.harvest.data.sampling$Date)
avg.harvest.data.sampling.1 = subset(avg.harvest.data.sampling, Date %in% 
                                       as.Date(c("2016-01-28","2016-02-08")))
avg.harvest.data.sampling.1$Date = as.factor(avg.harvest.data.sampling.1$Date)
# avg.harvest.data.sampling.1$Date = as.factor(ifelse(avg.harvest.data.sampling.1$Date %in% as.factor("2016-01-28"),
#                                                   as.character("28~29-01-2016"),as.character("08~10-02-2016")))

avg.harvest.data.sampling.2 = subset(avg.harvest.data.sampling, Date %in% 
                                       as.Date(c("2016-01-29","2016-02-10")))
avg.harvest.data.sampling.2$Date = as.Date(ifelse(avg.harvest.data.sampling.2$Date %in% as.Date("2016-01-29"),
                                          as.Date("2016-01-28"),as.Date("2016-02-08")))
avg.harvest.data.sampling.2$Date = as.factor(avg.harvest.data.sampling.2$Date)
# avg.harvest.data.sampling.2$Date = as.factor(ifelse(avg.harvest.data.sampling.2$Date %in% as.factor("2016-01-28"),
#                                            as.character("28~29-01-2016"),as.character("08~10-02-2016")))

# avg.harvest.data.sampling.2 = rbind(avg.harvest.data.sampling.1,avg.harvest.data.sampling.2)

# avg.harvest.data.sampling.2 = subset(avg.harvest.data.sampling, !(Date %in%
#                                                                     as.Date(c("2016-01-28","2016-01-29","2016-02-08","2016-02-10"))))
plots = list() 
pd <- position_dodge(0.5)
# ann_text = data.frame(Room = 3, D = 4, Date = factor("2016-02-08", levels = c("2016-01-28","2016-02-08")))

plots[[1]] = ggplot(data = avg.harvest.data.sampling.1, aes(x=Room, y=D, group=interaction(Room,datatype), colour=as.factor(Room))) + 
  geom_boxplot() +
  geom_jitter(size=0.25) +
  geom_point(position=pd, data = avg.harvest.data.sampling.2, aes(x=Room, y=D, group=Room, colour=as.factor(Room)),size=2,shape=5,stroke=1.1) +
  labs(colour="Treatments") +
  facet_wrap( ~ Date, scales="free_x") +
  # scale_fill_manual(name = "", values = c("white", "gray85")) +
  # geom_text(data = ann_text,label = paste("Dots = All measured data", "\nTriangles = Harvest data") ) +
  annotate("text", x = 2.5, y = 5.2, size = 4, label = paste("Dots = All measured diameters", "\nDiamonds = Harvest diameters")) +
  xlab("Treatment room") + ylab("Diameter") + ggtitle("Diameters with treatments") +
  # guides(fill=guide_legend(title="Data type")) +
  theme_bw() + theme(legend.position = c(0.75,0.08),legend.direction = "horizontal",legend.box = "vertical") 

plots[[2]] = ggplot(data = avg.harvest.data.sampling.1, aes(x=Room, y=Height, group=interaction(Room,datatype), colour=as.factor(Room))) + 
  geom_boxplot() +
  geom_jitter(size=0.25) + 
  geom_point(position=pd, data = avg.harvest.data.sampling.2, aes(x=Room, y=Height, group=Room, colour=as.factor(Room)),size=3,shape=5,stroke=1.1) +
  labs(colour="Treatments") +
  facet_wrap( ~ Date, scales="free_x") +
  # scale_fill_manual(name = "Data type", values = c("white", "gray85")) +
  annotate("text", x = 2.5, y = 75, size = 4, label = paste("Dots = All measured heights", "\nDiamonds = Harvest heights")) +
  xlab("Treatment room") + ylab("Height") + ggtitle("Heights with treatments") +
  theme_bw() + theme(legend.position = c(0.75,0.08),legend.direction = "horizontal",legend.box = "vertical") 

pdf(file = "Output/4.data_sampling.pdf")
plots
dev.off() 
# plots[[1]] = ggplot(data = avg.harvest.data.sampling.1, aes(x=Room, y=D)) + geom_boxplot(aes(fill=datatype)) +
#   geom_jitter(size=0.25) +
#   facet_wrap( ~ Date, scales="free_x") +
#   scale_fill_manual(name = "Data type", values = c("white", "gray85")) +
#   xlab("Treatment room") + ylab("Diameter") + ggtitle("Diameter over time") +
#   guides(fill=guide_legend(title="Data type")) +
#   theme_bw() + theme(legend.position = c(0.75,0.9),legend.direction = "horizontal",legend.box = "vertical")
# 
# plots[[2]] = ggplot(data = avg.harvest.data.sampling.1, aes(x=Room, y=D, group=Room, colour=as.factor(Room))) + geom_boxplot(aes(fill=datatype)) +
#   geom_jitter(size=0.25) + 
#   labs(colour="Treatments") +
#   facet_wrap( ~ Date, scales="free_x") +
#   scale_fill_manual(name = "Data type", values = c("white", "gray85")) +
#   xlab("Treatment room") + ylab("Diameter") + ggtitle("Diameter over time with treatments") +
#   guides(fill=guide_legend(title="Data type")) +
#   theme_bw() + theme(legend.position = c(0.75,0.9),legend.direction = "horizontal",legend.box = "vertical")
# 
# plots[[3]] = ggplot(data = avg.harvest.data.sampling.1, aes(x=Room, y=Height)) + geom_boxplot(aes(fill=datatype)) +
#   geom_jitter(size=0.25) +
#   facet_wrap( ~ Date, scales="free_x") +
#   scale_fill_manual(name = "Data type", values = c("white", "gray85")) +
#   xlab("Treatment room") + ylab("Height") + ggtitle("Height over time") +
#   guides(fill=guide_legend(title="Data type")) +
#   theme_bw() + theme(legend.position = c(0.75,0.9),legend.direction = "horizontal",legend.box = "vertical")
# 
# plots[[4]] = ggplot(data = avg.harvest.data.sampling.1, aes(x=Room, y=Height, group=Room, colour=as.factor(Room))) + geom_boxplot(aes(fill=datatype)) +
#   geom_jitter(size=0.25) + labs(colour="Treatments") +
#   facet_wrap( ~ Date, scales="free_x") +
#   scale_fill_manual(name = "Data type", values = c("white", "gray85")) +
#   xlab("Treatment room") + ylab("Height") + ggtitle("Height over time with treatments") +
#   guides(fill=guide_legend(title="Data type")) +
#   theme_bw() + theme(legend.position = c(0.75,0.9),legend.direction = "horizontal",legend.box = "vertical")
# 
# # plots[[3]] = ggplot(data = avg.harvest.data.sampling.2, aes(x=Date, y=D)) + geom_boxplot(aes(fill=datatype)) +
# #   geom_jitter(size=0.5) +
# #   facet_wrap( ~ Date, scales="free_x") +
# #   xlab("Date") + ylab("Diameter") + ggtitle("Diameter over time") +
# #   guides(fill=guide_legend(title="Data type")) +
# #   theme_bw() + theme(legend.position = c(0.9,0.1))
# # plots[[4]] = ggplot(data = avg.harvest.data.sampling.2, aes(x=Date, y=Height)) + geom_boxplot(aes(fill=datatype)) +
# #   geom_jitter(size=0.5) +
# #   facet_wrap( ~ Date, scales="free_x") +
# #   xlab("Date") + ylab("Height") + ggtitle("Height over time") +
# #   guides(fill=guide_legend(title="Data type")) +
# #   theme_bw() + theme(legend.position = c(0.9,0.1))
# 
# pdf(file = "Output/4.data_sampling.pdf")
# plots
# dev.off() 

#-----------------------------------------------------------------------------------------
# Plot all H vs D data (harvested and measured) over time 
plots = list() 
plots[[1]] = ggplot(data = avg.harvest.data.sampling, aes(x=D, y=Height, group=Room, colour=as.factor(Room))) + 
  geom_point(size=0.5) + stat_smooth(method=lm) +
  labs(colour="Rooms") + xlab("Diameter (mm)") + ylab("Height (cm)") + 
  ggtitle("Height vs Diameter with treatments") +
  theme_bw() + theme(legend.position = c(0.85,0.25))

plots[[2]] = ggplot(data = avg.harvest.data.sampling, aes(x=D, y=Height, group=Room, colour=as.factor(Room))) + 
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method="lm",se=FALSE) + geom_point(size=0.5) + 
  facet_wrap(~Room, scales="free_x") +
  labs(colour="Rooms") + xlab("Diameter (mm)") + ylab("Height (cm)") + 
  theme_bw() + theme(legend.key.width=unit(0.9,"line")) +
  theme(legend.position = c(0.17,0.9),legend.direction = "horizontal",legend.box = "vertical")

pdf(file = "Output/7.height_vs_dia_treatments.pdf")
plots
dev.off() 
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
# Plot all data (harvested and predicted) in log scale over time 
plots = list() 
font.size = 10
# ggplot(data=avg.harvest.data.sampling, aes(x=D, y=Stemmass, group = interaction(Room,datatype), colour=as.factor(Room), shape=as.factor(datatype), size=as.factor(datatype))) +
#   geom_point() +
#   coord_trans(y = "log10") +
#   scale_size_manual(values=c(1,2))
plot.fun1 <- function(df1,df2,font.size){
  plots = ggplot() +
    geom_point(data=df1, aes(x=df1[,2], y=df1[,3], group = colnames(df1)[1], colour=as.factor(df1[,1])),size=0.1) +
    coord_trans(y = "log10") + ylab(paste(as.character(colnames(df1)[3] ), "(log scale)")) + 
    xlab(paste(as.character(colnames(df1)[2]))) +
    geom_point(data=df2, aes(x = df2[,2], y = df2[,3], group = colnames(df2)[3], colour=as.factor(df2[,1])),pch=2,size=1) +
    scale_color_manual(name=paste(as.character(colnames(df1)[1])), values = rainbow(14)) +
    theme_bw() + scale_size_manual(name="Data type", values=c(1,2)) +
    annotate("text", x = (min(df1[,2])*4), y = (max(df1[,3])*0.9), size = font.size-8, 
             label = paste("Dots = Predicted", "\nTriangles = Harvest")) +
    theme(legend.title = element_text(colour="black", size=font.size-3)) +
    theme(legend.text = element_text(colour="black", size = font.size-5)) +
    theme(legend.position = c(0.8,0.3)) + theme(legend.key = element_blank()) +
    theme(text = element_text(size=font.size)) +
    theme(axis.title.x = element_text(size = font.size)) + theme(axis.title.y = element_text(size = font.size)) +
    theme(legend.key.height=unit(0.5,"line")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  output = plots
}
plots[[1]] = plot.fun1(height.dia[,c("Date","D","Leafarea")], harvest.data[,c("Date","D","Leafarea")], font.size)
plots[[3]] = plot.fun1(height.dia[,c("Date","D","Leafmass")], harvest.data[,c("Date","D","Leafmass")], font.size)
plots[[5]] = plot.fun1(height.dia[,c("Date","D","Stemmass")], harvest.data[,c("Date","D","Stemmass")], font.size)
plots[[7]] = plot.fun1(height.dia[,c("Date","D","Rootmass")], harvest.data[,c("Date","D","Rootmass")], font.size)

plots[[2]] = plot.fun1(height.dia[,c("Date","Height","Leafarea")], harvest.data[,c("Date","Height","Leafarea")], font.size)
plots[[4]] = plot.fun1(height.dia[,c("Date","Height","Leafmass")], harvest.data[,c("Date","Height","Leafmass")], font.size)
plots[[6]] = plot.fun1(height.dia[,c("Date","Height","Stemmass")], harvest.data[,c("Date","Height","Stemmass")], font.size)
plots[[8]] = plot.fun1(height.dia[,c("Date","Height","Rootmass")], harvest.data[,c("Date","Height","Rootmass")], font.size)

pdf(file = "Output/5.tree_attributes_logscale_over_time.pdf")
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]])
grid.arrange(plots[[5]],plots[[6]],plots[[7]],plots[[8]])
dev.off()
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
# Plot all data  (harvested and predicted) with room (temperature) variation
plots = list() 
plots[[1]] = plot.fun1(height.dia[,c("Room","D","Leafarea")], harvest.data[,c("Room","D","Leafarea")], font.size)
plots[[3]] = plot.fun1(height.dia[,c("Room","D","Leafmass")], harvest.data[,c("Room","D","Leafmass")], font.size)
plots[[5]] = plot.fun1(height.dia[,c("Room","D","Stemmass")], harvest.data[,c("Room","D","Stemmass")], font.size)
plots[[7]] = plot.fun1(height.dia[,c("Room","D","Rootmass")], harvest.data[,c("Room","D","Rootmass")], font.size)

plots[[2]] = plot.fun1(height.dia[,c("Room","Height","Leafarea")], harvest.data[,c("Room","Height","Leafarea")], font.size)
plots[[4]] = plot.fun1(height.dia[,c("Room","Height","Leafmass")], harvest.data[,c("Room","Height","Leafmass")], font.size)
plots[[6]] = plot.fun1(height.dia[,c("Room","Height","Stemmass")], harvest.data[,c("Room","Height","Stemmass")], font.size)
plots[[8]] = plot.fun1(height.dia[,c("Room","Height","Rootmass")], harvest.data[,c("Room","Height","Rootmass")], font.size)

pdf(file = "Output/6.tree_attributes_logscale_with_temperature.pdf")
grid.arrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]])
grid.arrange(plots[[5]],plots[[6]],plots[[7]],plots[[8]])
dev.off()
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------


