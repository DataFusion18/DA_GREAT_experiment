#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Script to analysis the raw GREAT experiment draught data
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

###### R script to import and process the raw GREAT experiment draught data 
# to model the carbon pools and fluxes using DA

#-----------------------------------------------------------------------------------------
# Script to read and process the leaf datasets to Calculate leaf mass and daily leaf area
rm(list=ls()) 
source("R/load_packages_functions_CBM.R")

#-----------------------------------------------------------------------------------------
################### Analyse stem height diameter to estimate Leaf, Stem and Root biomass
# Import weekly height diameter data for 3 months (Diameter is in mm; Height is in cm)
height.dia <- read.csv("Data/GHS39_GREAT_MAIN_HEIGHTDIAMETER_20160108-20160229_L2.csv")
height.dia = height.dia[height.dia$Prov %in% as.factor("B"),]
height.dia$D = rowMeans(height.dia[,c("D1", "D2")], na.rm=TRUE)
height.dia = height.dia[complete.cases(height.dia), ]
height.dia$Date = as.Date(height.dia$Date, format = "%d/%m/%Y")

initial.harvest = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160107_L2.csv")
# initial.harvest[ , c("Leafmass", "Stemmass", "Rootmass")] = initial.harvest[ , c("Leafmass", "Stemmass", "Rootmass")]
initial.harvest$Height = initial.harvest$Height/10 # Unit conversion from mm to cm
initial.harvest$Date = as.Date("2016-01-07")
initial.harvest$Room = 0
initial.harvest$W_treatment = as.factor("i")

int.harvest.1 = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160129_L2.csv")
int.harvest.1 = int.harvest.1[int.harvest.1$Prov %in% as.factor("B"),]
int.harvest.1[ , c("Leafmass", "Stemmass", "Rootmass")] = int.harvest.1[ , c("Leafmass", "Stemmass", "Rootmass")]
int.harvest.1$Date = as.Date("2016-01-29")
int.harvest.1 = unique(merge(int.harvest.1, height.dia[,c("Room","Pot")]))
int.harvest.2 = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160210_L2.csv")
int.harvest.2 = int.harvest.2[int.harvest.2$Prov %in% as.factor("B"),]
int.harvest.2[ , c("Leafmass", "Stemmass", "Rootmass")] = int.harvest.2[ , c("Leafmass", "Stemmass", "Rootmass")]
int.harvest.2$Date = as.Date("2016-02-10")
int.harvest.2 = unique(merge(int.harvest.2, height.dia[,c("Room","Pot")]))

final.harvest = read.csv("Data/GHS39_GREAT_MAIN_BIOMASS_20160217-20160224_L3_with date.csv")
final.harvest$Date = as.Date(final.harvest$Date, format = "%d/%m/%Y")
final.harvest = final.harvest[final.harvest$Prov %in% as.factor("B"),]
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
# harvest.data$W_treatment = NULL
harvest.data = rbind(harvest.data, initial.harvest)
harvest.data$D = rowMeans(harvest.data[,c("D1", "D2")], na.rm=TRUE)
harvest.data = harvest.data[!(harvest.data$Rootmass==0),]
harvest.data = harvest.data[with(harvest.data, order(Rootmass)), ]


#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
# # Plot data
# harvest.data.crop = subset(harvest.data, Room %in% as.factor(c("0","4")) & W_treatment %in% as.factor(c("i","d")))
# plot(harvest.data$Height,harvest.data$Stemmass,col="red",main="Height vs Stemmass", pch=15, xlab="Height (cm)", ylab="Stemmass (g DM)")
# # lines(harvest.data$Height,harvest.data$Stemmass,type="p", col="green", pch=20)
# legend('topleft', c("Measurements", "Modelled"), lty=1, col=c('green','red'), bty='n', cex=0.75)


#-----------------------------------------------------------------------------------------
################### Linear regression model fitting [log(stem_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
# Model fits for individual temperature treatments
# Units are: masses = g; height = cm; dia = mm, leafarea = cm^2
height.dia = height.dia[with(height.dia, order(Room,W_treatment)), ]
fit.sm = list()
draught = as.factor(c("d","w"))
for (i in 1:(length(unique(harvest.data$Room))-1)) {
  for (j in 1:2) {
    harvest.data.ind = subset(harvest.data, Room %in% i & W_treatment %in% draught[j])
    height.dia.ind = subset(height.dia, Room %in% i & W_treatment %in% draught[j])
    fit.sm[[(i-1)*2+j]] <- lm(log(Stemmass) ~ log(D) + log(Height), data=harvest.data.ind)
    # summary(fit.sm[[i]])
    # Estimate the stemmass from the fitted linear regression equation
    eq = function(x,y){exp(coefficients(fit.sm[[(i-1)*2+j]])[1] + coefficients(fit.sm[[(i-1)*2+j]])[2] * log(x)  + coefficients(fit.sm[[(i-1)*2+j]])[3] * log(y))}
    
    # Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
    height.dia$Stemmass[(1+(i-1)*2*nrow(height.dia.ind)+(j-1)*nrow(height.dia.ind)):(((i-1)*2+j)*nrow(height.dia.ind))] = eq(height.dia.ind$D,height.dia.ind$Height)
    # if (i == 1 & j == 1) {
    #   rmse.sm = sum ((exp(fitted(fit.sm[[(i-1)*2+j]])) - harvest.data.ind$Stemmass)^2)
    # } else {
    #   rmse.sm = rmse.sm + sum((exp(fitted(fit.sm[[(i-1)*2+j]])) - harvest.data.ind$Stemmass)^2)
    # }
  }
  # if (j == 1) {
  #   model.rmse$rmse.draught[1] = sqrt( ( rmse.sm ) / nrow(harvest.data))
  # model.error$error.draught[1] = model.rmse$rmse.individual[1] / mean(harvest.data$Stemmass) * 100 #Percentage Error
  # } else {
  #   model.rmse$rmse.draught[1] = sqrt( ( rmse.sm ) / nrow(harvest.data))
  #   model.error$error.draught[1] = model.rmse$rmse.individual[1] / mean(harvest.data$Stemmass) * 100 #Percentage Error
  # }
}
# model.rmse$rmse.draught[1] = sqrt( ( rmse.sm ) / nrow(harvest.data))
# model.error$error.draught[1] = model.rmse$rmse.individual[1] / mean(harvest.data$Stemmass) * 100 #Percentage Error

#-----------------------------------------------------------------------------------------

################### Linear regression model fitting [log(root_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
# Fit the model with initial data (30) and intermediate (18*2=36) and final (90) harvest data for all seedlings (156 data in total)
# Units are: masses = g; height = cm; dia = mm
fit.rm = list()
for (i in 1:(length(unique(harvest.data$Room))-1)) {
  for (j in 1:2) {
    harvest.data.ind = subset(harvest.data, Room %in% i & W_treatment %in% draught[j])
    height.dia.ind = subset(height.dia, Room %in% i & W_treatment %in% draught[j])
    fit.rm[[(i-1)*2+j]] <- lm(log(Rootmass) ~ log(D) + log(Height), data=harvest.data.ind)
    # summary(fit.sm[[i]])
    # Estimate the stemmass from the fitted linear regression equation
    eq = function(x,y){exp(coefficients(fit.rm[[(i-1)*2+j]])[1] + coefficients(fit.rm[[(i-1)*2+j]])[2] * log(x)  + coefficients(fit.rm[[(i-1)*2+j]])[3] * log(y))}
    
    # Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
    height.dia$Rootmass[(1+(i-1)*2*nrow(height.dia.ind)+(j-1)*nrow(height.dia.ind)):(((i-1)*2+j)*nrow(height.dia.ind))] = eq(height.dia.ind$D,height.dia.ind$Height)
  }
}

#-----------------------------------------------------------------------------------------
################### Linear regression model fitting [log(leaf_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
# Fit the model with initial data (30) and intermediate (18*2=36) and final (90) harvest data for all seedlings (156 data in total)
# Units are: masses = g; height = cm; dia = mm
fit.lm = list()
for (i in 1:(length(unique(harvest.data$Room))-1)) {
  for (j in 1:2) {
    harvest.data.ind = subset(harvest.data, Room %in% i & W_treatment %in% draught[j])
    height.dia.ind = subset(height.dia, Room %in% i & W_treatment %in% draught[j])
    fit.lm[[(i-1)*2+j]] <- lm(log(Leafmass) ~ log(D) + log(Height), data=harvest.data.ind)
    # summary(fit.sm[[i]])
    # Estimate the stemmass from the fitted linear regression equation
    eq = function(x,y){exp(coefficients(fit.lm[[(i-1)*2+j]])[1] + coefficients(fit.lm[[(i-1)*2+j]])[2] * log(x)  + coefficients(fit.lm[[(i-1)*2+j]])[3] * log(y))}
    
    # Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
    height.dia$Leafmass[(1+(i-1)*2*nrow(height.dia.ind)+(j-1)*nrow(height.dia.ind)):(((i-1)*2+j)*nrow(height.dia.ind))] = eq(height.dia.ind$D,height.dia.ind$Height)
  }
}  

#-----------------------------------------------------------------------------------------
################### Linear regression model fitting [log(leaf_mass) = b(1) + b(2)*log(dia) + b(3)*log(height)]
# Fit the model with initial data (30) and intermediate (18*2=36) and final (90) harvest data for all seedlings (156 data in total)
# Units are: masses = g; height = cm; dia = mm; leaf area = cm2
fit.la = list()
for (i in 1:(length(unique(harvest.data$Room))-1)) {
  for (j in 1:2) {
    harvest.data.ind = subset(harvest.data, Room %in% i & W_treatment %in% draught[j])
    height.dia.ind = subset(height.dia, Room %in% i & W_treatment %in% draught[j])
    fit.la[[(i-1)*2+j]] <- lm(log(Leafarea) ~ log(D) + log(Height), data=harvest.data.ind)
    # summary(fit.sm[[i]])
    # Estimate the stemmass from the fitted linear regression equation
    eq = function(x,y){exp(coefficients(fit.la[[(i-1)*2+j]])[1] + coefficients(fit.la[[(i-1)*2+j]])[2] * log(x)  + coefficients(fit.la[[(i-1)*2+j]])[3] * log(y))}
    
    # Calculate all seedling stem mass from height and diameter using the linear model and then get the SEs from the 7 replicas
    height.dia$Leafarea[(1+(i-1)*2*nrow(height.dia.ind)+(j-1)*nrow(height.dia.ind)):(((i-1)*2+j)*nrow(height.dia.ind))] = eq(height.dia.ind$D,height.dia.ind$Height)
  }
}   

#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
# Plotting observation and modelled data
plots = list() 

# Stemmass
plots[[1]] = ggplot() + 
  geom_point(data=harvest.data, aes(x=Height, y=Stemmass, group = W_treatment, colour=W_treatment, shape=as.factor(4)), size=1.5) +
  geom_point(data=height.dia, aes(x=Height, y=Stemmass, group = W_treatment, colour=W_treatment, shape=as.factor(1)), size=1.5) +
  ylab("Stemmass (g DM)") + xlab("Height (cm)") +
  coord_trans(y = "log10") + 
  scale_colour_discrete(name="Treatments", breaks=c("d","w","i"),
                        labels=c("Draught", "Watered", "Initial")) +
  scale_shape_discrete(name="Data Type", breaks=c("4","1"),
                       labels=c("Harvest", "Predicted")) +
  theme_bw() + theme(legend.position = c(0.8,0.3), legend.key = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

plots[[2]] = ggplot() + 
  geom_point(data=harvest.data, aes(x=D, y=Stemmass, group = W_treatment, colour=W_treatment, shape=as.factor(4)), size=1.5) +
  geom_point(data=height.dia, aes(x=D, y=Stemmass, group = W_treatment, colour=W_treatment, shape=as.factor(1)), size=1.5) +
  ylab("Stemmass (g DM)") + xlab("Diameter (mm)") +
  coord_trans(y = "log10") + 
  scale_colour_discrete(name="Treatments", breaks=c("d","w","i"),
                        labels=c("Draught", "Watered", "Initial")) +
  scale_shape_discrete(name="Data Type", breaks=c("4","1"),
                       labels=c("Harvest", "Predicted")) +
  theme_bw() + theme(legend.position = c(0.8,0.3), legend.key = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
#-----------------------------------------------------------------------------------------
# Rootmass
plots[[3]] = ggplot() + 
  geom_point(data=harvest.data, aes(x=Height, y=Rootmass, group = W_treatment, colour=W_treatment, shape=as.factor(4)), size=1.5) +
  geom_point(data=height.dia, aes(x=Height, y=Rootmass, group = W_treatment, colour=W_treatment, shape=as.factor(1)), size=1.5) +
  ylab("Rootmass (g DM)") + xlab("Height (cm)") +
  coord_trans(y = "log10") + 
  scale_colour_discrete(name="Treatments", breaks=c("d","w","i"),
                        labels=c("Draught", "Watered", "Initial")) +
  scale_shape_discrete(name="Data Type", breaks=c("4","1"),
                       labels=c("Harvest", "Predicted")) +
  theme_bw() + theme(legend.position = c(0.8,0.3), legend.key = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

plots[[4]] = ggplot() + 
  geom_point(data=harvest.data, aes(x=D, y=Rootmass, group = W_treatment, colour=W_treatment, shape=as.factor(4)), size=1.5) +
  geom_point(data=height.dia, aes(x=D, y=Rootmass, group = W_treatment, colour=W_treatment, shape=as.factor(1)), size=1.5) +
  ylab("Rootmass (g DM)") + xlab("Diameter (mm)") +
  coord_trans(y = "log10") + 
  scale_colour_discrete(name="Treatments", breaks=c("d","w","i"),
                        labels=c("Draught", "Watered", "Initial")) +
  scale_shape_discrete(name="Data Type", breaks=c("4","1"),
                       labels=c("Harvest", "Predicted")) +
  theme_bw() + theme(legend.position = c(0.8,0.3), legend.key = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#-----------------------------------------------------------------------------------------
# Leafmass
plots[[5]] = ggplot() + 
  geom_point(data=harvest.data, aes(x=Height, y=Leafmass, group = W_treatment, colour=W_treatment, shape=as.factor(4)), size=1.5) +
  geom_point(data=height.dia, aes(x=Height, y=Leafmass, group = W_treatment, colour=W_treatment, shape=as.factor(1)), size=1.5) +
  ylab("Leafmass (g DM)") + xlab("Height (cm)") +
  coord_trans(y = "log10") + 
  scale_colour_discrete(name="Treatments", breaks=c("d","w","i"),
                        labels=c("Draught", "Watered", "Initial")) +
  scale_shape_discrete(name="Data Type", breaks=c("4","1"),
                       labels=c("Harvest", "Predicted")) +
  theme_bw() + theme(legend.position = c(0.8,0.3), legend.key = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

plots[[6]] = ggplot() + 
  geom_point(data=harvest.data, aes(x=D, y=Leafmass, group = W_treatment, colour=W_treatment, shape=as.factor(4)), size=1.5) +
  geom_point(data=height.dia, aes(x=D, y=Leafmass, group = W_treatment, colour=W_treatment, shape=as.factor(1)), size=1.5) +
  ylab("Leafmass (g DM)") + xlab("Diameter (mm)") +
  coord_trans(y = "log10") + 
  scale_colour_discrete(name="Treatments", breaks=c("d","w","i"),
                        labels=c("Draught", "Watered", "Initial")) +
  scale_shape_discrete(name="Data Type", breaks=c("4","1"),
                       labels=c("Harvest", "Predicted")) +
  theme_bw() + theme(legend.position = c(0.8,0.3), legend.key = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#-----------------------------------------------------------------------------------------
# Leafarea
plots[[7]] = ggplot() + 
  geom_point(data=harvest.data, aes(x=Height, y=Leafarea, group = W_treatment, colour=W_treatment, shape=as.factor(4)), size=1.5) +
  geom_point(data=height.dia, aes(x=Height, y=Leafarea, group = W_treatment, colour=W_treatment, shape=as.factor(1)), size=1.5) +
  ylab(expression(Leafarea~"("*cm^"2"*")")) + xlab("Height (cm)") +
  coord_trans(y = "log10") + 
  scale_colour_discrete(name="Treatments", breaks=c("d","w","i"),
                        labels=c("Draught", "Watered", "Initial")) +
  scale_shape_discrete(name="Data Type", breaks=c("4","1"),
                       labels=c("Harvest", "Predicted")) +
  theme_bw() + theme(legend.position = c(0.8,0.3), legend.key = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

plots[[8]] = ggplot() + 
  geom_point(data=harvest.data, aes(x=D, y=Leafarea, group = W_treatment, colour=W_treatment, shape=as.factor(4)), size=1.5) +
  geom_point(data=height.dia, aes(x=D, y=Leafarea, group = W_treatment, colour=W_treatment, shape=as.factor(1)), size=1.5) +
  ylab(expression(Leafarea~"("*cm^"2"*")")) + xlab("Diameter (mm)") +
  coord_trans(y = "log10") + 
  scale_colour_discrete(name="Treatments", breaks=c("d","w","i"),
                        labels=c("Draught", "Watered", "Initial")) +
  scale_shape_discrete(name="Data Type", breaks=c("4","1"),
                       labels=c("Harvest", "Predicted")) +
  theme_bw() + theme(legend.position = c(0.8,0.3), legend.key = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ml = marrangeGrob(plots, nrow=1, ncol=2)
ggsave("Output/1.tree_attributes_over_time_draught.pdf", ml)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
# Plot measuremnts vs fitted points to judge the model fits
plots = list()
palette = c("#FF0000FF", "yellow3", "#00FF00FF", "#00FFFFFF", "#0000FFFF", "#FF00FFFF")
for (i in 1:(length(unique(harvest.data$Room))-1)) {
  for (j in 1:2) {
    harvest.data.ind = subset(harvest.data, Room %in% i & W_treatment %in% draught[j])
    
    par(mfrow=c(2,2), mar=c(3, 4, 1, 1), mgp=c(2,1,0), oma=c(0,0,1,0))
    plot(exp(fitted(fit.sm[[(i-1)*2+j]])), harvest.data.ind$Stemmass, col=palette[i], pch=16, xlab="Fitted stem mass (g)", ylab="Measured stem mass (g)")
    abline(0, 1) 
    text(min(exp(fitted(fit.sm[[(i-1)*2+j]]))), max(harvest.data.ind$Stemmass)*0.85, 
         paste("Adj R-squared =", format(round(summary(fit.sm[[(i-1)*2+j]])$adj.r.squared, 2)),
               "\nResidual SE =", format(round(sigma(fit.sm[[(i-1)*2+j]]), 2)),
               "\nlog(SM) =", format(round(coefficients(fit.sm[[(i-1)*2+j]])[1],2)), "+", format(round(coefficients(fit.sm[[(i-1)*2+j]])[2],2)),
               "* log(D)\n+", format(round(coefficients(fit.sm[[(i-1)*2+j]])[3],2)), "* log(H)"), pos = 4)
    # cat("Linear regression model fitting: log(leaf_area) = ", coefficients(fit.la.combined)[1], "+", coefficients(fit.la.combined)[2],
    #     "* log(diameter) +", coefficients(fit.la.combined)[3], "* log(height)\nwith percentage error =", percentage.error.la, "%")
    
    # legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Rooms",lty=NULL,col=palette[i],pch=19,
    #        bty="n",ncol=2,cex=0.8,pt.cex=1.2)
    
    plot(exp(fitted(fit.rm[[(i-1)*2+j]])), harvest.data.ind$Rootmass, col=palette[i], pch=16, xlab="Fitted root mass (g)", ylab="Measured root mass (g)")
    abline(0, 1) 
    text(min(exp(fitted(fit.rm[[(i-1)*2+j]]))), max(harvest.data.ind$Rootmass)*0.85, 
         paste("Adj R-squared =", format(round(summary(fit.rm[[(i-1)*2+j]])$adj.r.squared, 2)),
               "\nResidual SE =", format(round(sigma(fit.rm[[(i-1)*2+j]]), 2)),
               "\nlog(SM) =", format(round(coefficients(fit.rm[[(i-1)*2+j]])[1],2)), "+", format(round(coefficients(fit.rm[[(i-1)*2+j]])[2],2)),
               "* log(D)\n+", format(round(coefficients(fit.rm[[(i-1)*2+j]])[3],2)), "* log(H)"), pos = 4)
    # legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data.ind$Room))),pch=19,
    #        bty="n",ncol=2,cex=0.8,pt.cex=1.2)
    
    plot(exp(fitted(fit.lm[[(i-1)*2+j]])), harvest.data.ind$Leafmass, col=palette[i], pch=16, xlab="Fitted leaf mass (g)", ylab="Measured leaf mass (g)")
    abline(0, 1) 
    text(min(exp(fitted(fit.lm[[(i-1)*2+j]]))), max(harvest.data.ind$Leafmass)*0.85, 
         paste("Adj R-squared =", format(round(summary(fit.lm[[(i-1)*2+j]])$adj.r.squared, 2)),
               "\nResidual SE =", format(round(sigma(fit.lm[[(i-1)*2+j]]), 2)),
               "\nlog(SM) =", format(round(coefficients(fit.lm[[(i-1)*2+j]])[1],2)), "+", format(round(coefficients(fit.lm[[(i-1)*2+j]])[2],2)),
               "* log(D)\n+", format(round(coefficients(fit.lm[[(i-1)*2+j]])[3],2)), "* log(H)"), pos = 4)
    # legend('bottomright',legend=sort(unique(harvest.data$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data.ind$Room))),pch=19,
    #        bty="n",ncol=2,cex=0.8,pt.cex=1.2)
    
    plot(exp(fitted(fit.la[[(i-1)*2+j]])), harvest.data.ind$Leafarea, col=palette[i], pch=16, xlab = expression("Fitted leaf area" ~ (cm^{2})), ylab="Measured leaf area" ~ (cm^{2}))
    abline(0, 1) 
    text(min(exp(fitted(fit.la[[(i-1)*2+j]]))), max(harvest.data.ind$Leafarea)*0.85, 
         paste("Adj R-squared =", format(round(summary(fit.la[[(i-1)*2+j]])$adj.r.squared, 2)),
               "\nResidual SE =", format(round(sigma(fit.la[[(i-1)*2+j]]), 2)),
               "\nlog(SM) =", format(round(coefficients(fit.la[[(i-1)*2+j]])[1],2)), "+", format(round(coefficients(fit.la[[(i-1)*2+j]])[2],2)),
               "* log(D)\n+", format(round(coefficients(fit.la[[(i-1)*2+j]])[3],2)), "* log(H)"), pos = 4)
    legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Room",lty=NULL,col=palette[i],pch=19,
           bty="n",ncol=1,cex=0.8,pt.cex=1.2)
    if (j == 1) {
      mtext(paste("Model fits for room #",i,"and draught treatment"), side = 3, line = -0.5, outer = TRUE)
    } else {
    mtext(paste("Model fits for room #",i,"and watered treatment"), side = 3, line = -0.5, outer = TRUE)
    }
    plots[[(i-1)*2+j]] = recordPlot()

    # # plot residuals against fitted values and quantile-quantile plot
    # # png(file = "Output/model_residuals.png", units="px", width=1500, height=2000, res=300)
    # par(mfrow=c(4,2), mar=c(3, 4, 1, 1), mgp=c(2,1,0))
    # plot(resid(fit.sm[[i]]) ~ exp(fitted(fit.sm[[i]])), col=palette[i], xlab="Fitted stem mass (g)", ylab="Residual stem mass (g)")
    # abline(h=0)
    # qqPlot(residuals(fit.sm[[i]]), ylab="Residual stem mass (g)")
    # # legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data.ind$Room))),pch=19,
    # #        bty="n",ncol=4,cex=0.8,pt.cex=1.2)
    # 
    # plot(resid(fit.rm[[i]]) ~ exp(fitted(fit.rm[[i]])), col=palette[i], xlab="Fitted root mass (g)", ylab="Residual root mass (g)")
    # abline(h=0)
    # qqPlot(residuals(fit.rm[[i]]), ylab="Residual stem mass (g)")
    # # legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data.ind$Room))),pch=19,
    # #        bty="n",ncol=4,cex=0.8,pt.cex=1.2)
    # 
    # plot(resid(fit.lm[[i]]) ~ exp(fitted(fit.lm[[i]])), col=palette[i], xlab="Fitted leaf mass (g)", ylab="Residual leaf mass (g)")
    # abline(h=0)
    # qqPlot(residuals(fit.lm[[i]]), ylab="Residual leaf mass (g)")
    # # legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Rooms",lty=NULL,col=rainbow(length(unique(harvest.data.ind$Room))),pch=19,
    # #        bty="n",ncol=4,cex=0.8,pt.cex=1.2)
    # 
    # plot(resid(fit.la[[i]]) ~ exp(fitted(fit.la[[i]])), col=palette[i], xlab = expression("Fitted leaf area" ~ (cm^{2})), ylab="Residual leaf area" ~ (mm^{2}))
    # abline(h=0)
    # qqPlot(residuals(fit.la[[i]]), ylab="Residual leaf area" ~ (cm^{2}))
    # legend('bottomright',legend=sort(unique(harvest.data.ind$Room)),title="Room",lty=NULL,col=palette[i],pch=19,
    #        bty="n",ncol=1,cex=0.8,pt.cex=1.2)
    # mtext(paste("Model residuals for individual temperature = room #",i), side = 3, line = -0.5, outer = TRUE)
    # plots[[i*2]] = recordPlot()
  }
}
pdf(file = "Output/3.model_attributes_rooms_draught.pdf")
plots
dev.off()
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
# Print and save the model comparison summary
sink("Output/3.models_summary_draught.txt")
cat("Stemmass models: Individual linear regression:")
cat("\n")
for (i in 1:(length(unique(harvest.data$Room))-1)) {
  cat("Room #",i,":"); cat("\n")
  for (j in 1:2) {
    cat("Treatment #",as.character(draught[j]),":")
    cat("log(Stemmass) = ", coefficients(fit.sm[[(i-1)*2+j]])[1], "+", 
      coefficients(fit.sm[[(i-1)*2+j]])[2],"* log(Diameter) +", coefficients(fit.sm[[(i-1)*2+j]])[3], "* log(Height)")
  cat("\n")
  }
}

cat("\n");cat("\n")
cat("Rootmass models: Individual linear regression:")
cat("\n")
for (i in 1:(length(unique(harvest.data$Room))-1)) {
  cat("Room #",i,":"); cat("\n")
  for (j in 1:2) {
    cat("Treatment #",as.character(draught[j]),":")
    cat("log(Rootmass) = ", coefficients(fit.rm[[(i-1)*2+j]])[1], "+", 
        coefficients(fit.rm[[(i-1)*2+j]])[2],"* log(Diameter) +", coefficients(fit.rm[[(i-1)*2+j]])[3], "* log(Height)")
    cat("\n")
  }
}

cat("\n");cat("\n")
cat("Leafmass models: Individual linear regression:")
cat("\n")
for (i in 1:(length(unique(harvest.data$Room))-1)) {
  cat("Room #",i,":"); cat("\n")
  for (j in 1:2) {
    cat("Treatment #",as.character(draught[j]),":")
    cat("log(Leafmass) = ", coefficients(fit.lm[[(i-1)*2+j]])[1], "+", 
        coefficients(fit.lm[[(i-1)*2+j]])[2],"* log(Diameter) +", coefficients(fit.lm[[(i-1)*2+j]])[3], "* log(Height)")
    cat("\n")
  }
}

cat("\n");cat("\n")
cat("Leafarea models: Individual linear regression:")
cat("\n")
for (i in 1:(length(unique(harvest.data$Room))-1)) {
  cat("Room #",i,":"); cat("\n")
  for (j in 1:2) {
    cat("Treatment #",as.character(draught[j]),":")
    cat("log(Leafarea) = ", coefficients(fit.la[[(i-1)*2+j]])[1], "+", 
        coefficients(fit.la[[(i-1)*2+j]])[2],"* log(Diameter) +", coefficients(fit.la[[(i-1)*2+j]])[3], "* log(Height)")
    cat("\n")
  }
}
sink()


#-----------------------------------------------------------------------------------------
# Save the stem mass data for MCMC CBM
height.dia$Comment = NULL
write.csv(height.dia, file = "Output/Cleaf_Cstem_Croot_draught.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- Plot the original biomass data and compare with predicted ones
# Plot the original biomass data
rooms = as.factor(c("1","2","3","4","5","6"))

# Initial harvest data
avg.harvest.data = data.frame(matrix(vector(), 0, 11,
                                     dimnames=list(c(), c("Date", "Room", "W_treatment","Leafarea", "Leafarea_SE", "Leafmass", "Leafmass_SE", "Stemmass", "Stemmass_SE", "Rootmass", "Rootmass_SE"))),
                              stringsAsFactors=F)
avg.harvest.data[1,c("Leafarea", "Leafmass", "Stemmass", "Rootmass")] = colMeans(initial.harvest[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], na.rm = TRUE) # R8 = Average of leaf counts
avg.harvest.data[1,c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = (apply(initial.harvest[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], 2, sd))/(nrow(initial.harvest)-1)^0.5 # R9 = Standard error of leaf counts
avg.harvest.data$Date = as.Date("2016-01-07")
avg.harvest.data = avg.harvest.data[rep(seq_len(nrow(avg.harvest.data)), each=length(rooms)),]
avg.harvest.data$Room = rooms
avg.harvest.data$W_treatment = as.character("i")

# Intermediate harvest data 1
int.harvest.1$W_treatment = as.character(int.harvest.1$W_treatment)
avg.harvest.data[(length(rooms)+1) : (length(rooms)+nrow(int.harvest.1)),c("Date","Room","W_treatment","Leafarea","Leafmass","Stemmass","Rootmass")] = 
  int.harvest.1[,c("Date","Room","W_treatment","Leafarea","Leafmass","Stemmass","Rootmass")]
avg.harvest.data[(length(rooms)+1) : (length(rooms)+nrow(int.harvest.1)), c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = c(0,0,0,0)

# Intermediate harvest data 2
int.harvest.2$W_treatment = as.character(int.harvest.2$W_treatment)
avg.harvest.data[(length(rooms)+nrow(int.harvest.1)+1) : (length(rooms)+nrow(int.harvest.1)+nrow(int.harvest.2)), c("Date","Room","W_treatment","Leafarea","Leafmass","Stemmass","Rootmass")] = 
  int.harvest.2[,c("Date","Room","W_treatment","Leafarea","Leafmass","Stemmass","Rootmass")]
avg.harvest.data[(length(rooms)+nrow(int.harvest.1)+1) : (length(rooms)+nrow(int.harvest.1)+nrow(int.harvest.2)), c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = c(0,0,0,0)

# Final harvest data
final.harvest = final.harvest[with(final.harvest, order(Room,Date)), ]
for(i in 1:length(rooms)) {
  for(j in 1:2) {
  # final.harvest.idn = subset(final.harvest,Room==rooms[i]) 
  final.harvest.idn = subset(final.harvest, Room %in% rooms[i] & W_treatment %in% draught[j])
  for(k in 1:2) {
    if (k==1) {
      final.harvest.idn.date = subset(final.harvest.idn, Date %in% as.Date(c("2016-02-17", "2016-02-18", "2016-02-19")))
    } else {
      final.harvest.idn.date = subset(final.harvest.idn, Date %in% as.Date(c("2016-02-22", "2016-02-23", "2016-02-24")))
    }
      avg.harvest.data[nrow(avg.harvest.data)+1, c("Leafarea", "Leafmass", "Stemmass", "Rootmass")] = colMeans(final.harvest.idn.date[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], na.rm = TRUE) # R8 = Average of leaf counts
      avg.harvest.data[nrow(avg.harvest.data), c("Leafarea_SE", "Leafmass_SE", "Stemmass_SE", "Rootmass_SE")] = (apply(final.harvest.idn.date[c("Leafarea", "Leafmass", "Stemmass", "Rootmass")], 2, sd, na.rm = TRUE))/(nrow(final.harvest.idn.date))^0.5 # R9 = Standard error of leaf counts
      avg.harvest.data$Date[nrow(avg.harvest.data)] = mean(final.harvest.idn.date$Date)
      avg.harvest.data$Room[nrow(avg.harvest.data)] = rooms[i]
      avg.harvest.data$W_treatment[nrow(avg.harvest.data)] = as.character(draught[j])
    }
  }
}

melted.harvest.data = melt(avg.harvest.data, id.vars=c("Date","Room","W_treatment"))
melted.harvest.data$Group = as.factor("Measured")
melted.harvest.data$W_treatment = as.factor(melted.harvest.data$W_treatment)

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


#-----------------------------------------------------------------------------------------
#- Plot all vs harvested (height and dia) data to explore the sampling effect on biomass prediction variation
height.dia$datatype = as.character("all")
harvest.data$datatype = as.character("harvested")
# harvest.data = unique(merge(harvest.data, height.dia[,c("Room","Pot")]))
harvest.data$Leafno = NULL
# height.dia$Room = NULL
avg.harvest.data.sampling = rbind(height.dia,harvest.data)
keeps <- c("Date", "Room", "datatype", "Height", "D", "Leafarea", "Leafmass", "Stemmass", "Rootmass")
avg.harvest.data.sampling = avg.harvest.data.sampling[ , keeps, drop = FALSE]
unique(avg.harvest.data.sampling$Date)
avg.harvest.data.sampling.1 = subset(avg.harvest.data.sampling, Date %in% 
                                       as.Date(c("2016-01-28","2016-01-29","2016-02-08","2016-02-10")))
avg.harvest.data.sampling.2 = subset(avg.harvest.data.sampling, !(Date %in%
                                                                    as.Date(c("2016-01-28","2016-01-29","2016-02-08","2016-02-10"))))
plots = list() 
plots[[1]] = ggplot(data = avg.harvest.data.sampling.1, aes(x=Room, y=D)) + geom_boxplot(aes(fill=datatype)) +
  geom_jitter(size=0.25) +
  facet_wrap( ~ Date, scales="free_x") +
  scale_fill_manual(name = "Data type", values = c("white", "gray85")) +
  xlab("Treatment room") + ylab("Diameter") + ggtitle("Diameter over time") +
  guides(fill=guide_legend(title="Data type")) +
  theme_bw() + theme(legend.position = c(0.75,0.9),legend.direction = "horizontal",legend.box = "vertical")

plots[[2]] = ggplot(data = avg.harvest.data.sampling.1, aes(x=Room, y=D, group=Room, colour=as.factor(Room))) + geom_boxplot(aes(fill=datatype)) +
  geom_jitter(size=0.25) + 
  labs(colour="Treatments") +
  facet_wrap( ~ Date, scales="free_x") +
  scale_fill_manual(name = "Data type", values = c("white", "gray85")) +
  xlab("Treatment room") + ylab("Diameter") + ggtitle("Diameter over time with treatments") +
  guides(fill=guide_legend(title="Data type")) +
  theme_bw() + theme(legend.position = c(0.75,0.9),legend.direction = "horizontal",legend.box = "vertical")

plots[[3]] = ggplot(data = avg.harvest.data.sampling.1, aes(x=Room, y=Height)) + geom_boxplot(aes(fill=datatype)) +
  geom_jitter(size=0.25) +
  facet_wrap( ~ Date, scales="free_x") +
  scale_fill_manual(name = "Data type", values = c("white", "gray85")) +
  xlab("Treatment room") + ylab("Height") + ggtitle("Height over time") +
  guides(fill=guide_legend(title="Data type")) +
  theme_bw() + theme(legend.position = c(0.75,0.9),legend.direction = "horizontal",legend.box = "vertical")

plots[[4]] = ggplot(data = avg.harvest.data.sampling.1, aes(x=Room, y=Height, group=Room, colour=as.factor(Room))) + geom_boxplot(aes(fill=datatype)) +
  geom_jitter(size=0.25) + labs(colour="Treatments") +
  facet_wrap( ~ Date, scales="free_x") +
  scale_fill_manual(name = "Data type", values = c("white", "gray85")) +
  xlab("Treatment room") + ylab("Height") + ggtitle("Height over time with treatments") +
  guides(fill=guide_legend(title="Data type")) +
  theme_bw() + theme(legend.position = c(0.75,0.9),legend.direction = "horizontal",legend.box = "vertical")
# plots[[3]] = ggplot(data = avg.harvest.data.sampling.2, aes(x=Date, y=D)) + geom_boxplot(aes(fill=datatype)) +
#   geom_jitter(size=0.5) +
#   facet_wrap( ~ Date, scales="free_x") +
#   xlab("Date") + ylab("Diameter") + ggtitle("Diameter over time") +
#   guides(fill=guide_legend(title="Data type")) +
#   theme_bw() + theme(legend.position = c(0.9,0.1))
# plots[[4]] = ggplot(data = avg.harvest.data.sampling.2, aes(x=Date, y=Height)) + geom_boxplot(aes(fill=datatype)) +
#   geom_jitter(size=0.5) +
#   facet_wrap( ~ Date, scales="free_x") +
#   xlab("Date") + ylab("Height") + ggtitle("Height over time") +
#   guides(fill=guide_legend(title="Data type")) +
#   theme_bw() + theme(legend.position = c(0.9,0.1))

pdf(file = "Output/4.data_sampling.pdf")
plots
dev.off() 

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


