# These packages are required to run this script. If you don't have these, use the following command 
# install.packages("pracma","signal","pspline","lme4","emmeans","ggplot2")
require(pracma)
require(signal)
require(pspline)
require(lme4)
require(emmeans)
require(ggplot2)

# Import the data file, a csv. file for either Eurycea or Plethodon
filename <- file.choose()
data <- (read.csv(filename, sep = ",", skip=0))

# Separates the data based on Species
dataPm <- data[which(data$Species=="Pm"),]
dataEl <- data[which(data$Species=="El"),]

# Define each variable
# Metadata
Species <- as.factor(data$species)
Name <- data$Name
Individual <- factor(data$individual)
Temperature <- data$temperature
PreyStart <- data$preystartdist

# Lunge Kinematics
MaxLunge <- data$maxlunge
LungeDuration <- data$timemaxlunge
AvLungeVel <- MaxLunge/LungeDuration/1000
PeakLungeVel <- data$peaklungevel
PeakLungeAcc <- data$peaklungeacc
PeakBMSLungePower <- data$peakbmslungepower

# Tongue Projection Kinematics
TimeMaxProj <- data$timemaxproj
TimeProjStart <- data$timeprojstart
MaxProj <- data$maxproj
ProjectionDuration <- TimeMaxProj - TimeProjStart
AvProjVel <- MaxProj/ProjectionDuration/1000
PeakProjVel <- data$peakprojvel
PeakProjAcc <- data$peakprojacc
PeakMMSProjPower <- data$peakmmsprojpower

# Coordination
LungeProjLat <- LungeDuration - TimeProjStart
TimeMaxReach <- data$timemaxreach
MaxReach <- data$maxreach

## CHANGE THE FOCUS VARIABLE IN THE LINE BELOW THEN
## RUN EVERYTHING AFTER IT
# Set the variable of interest
Focus <- MaxReach

data5 <- Focus[which(Temperature == 5)]
stats5 <- c(mean(data5), std(data5)/sqrt(length(data5)), min(data5), max(data5))

data10 <- Focus[which(Temperature == 10)]
stats10 <- c(mean(data10), std(data10)/sqrt(length(data10)), min(data10), max(data10))

data15 <- Focus[which(Temperature == 15)]
stats15 <- c(mean(data15), std(data15)/sqrt(length(data15)), min(data15), max(data15))

data20 <- Focus[which(Temperature == 20)]
stats20 <- c(mean(data20), std(data20)/sqrt(length(data20)), min(data20), max(data20))

data25 <- Focus[which(Temperature == 25)]
stats25 <- c(mean(data25), std(data25)/sqrt(length(data25)), min(data25), max(data25))

output <- c(stats5,stats10,stats15,stats20,stats25)
output




