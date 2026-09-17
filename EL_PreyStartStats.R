# These packages are required to run this script. If you don't have these, use the following command 
# install.packages("pracma","signal","pspline","lme4","emmeans","ggplot2")
require(pracma)
require(signal)
require(pspline)
require(lme4)
require(emmeans)
require(ggplot2)
require(lmerTest)

# Import the data file, a csv. file for Eurycea
filename <- file.choose()
data <- (read.csv(filename, sep = ",", skip=0))

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
AvLungeVel <- MaxLunge/LungeDuration
PeakLungeVel <- data$peaklungevel
PeakLungeAcc <- data$peaklungeacc
PeakBMSLungePower <- data$peakbmslungepower

# Tongue Projection Kinematics
TimeMaxProj <- data$timemaxproj
TimeProjStart <- data$timeprojstart
MaxProj <- data$maxproj
ProjectionDuration <- TimeMaxProj - TimeProjStart
AvProjVel <- MaxProj/ProjectionDuration
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

AOV_Focus <- lmer(Focus ~ PreyStart + (1|Individual))

# Create a line for Eurycea 5
Elnewdata5 <- data.frame(Temperature = c(5,5), PreyStart = c(1.336603,24.74493))
Elnewdata5$Focus <- predict(AOV_Focus, newdata = Elnewdata5, type = "response", re.form = NA)
Elnewdata5$RawFocus <- 10^Elnewdata5$Focus

# Create a line for Eurycea 10
Elnewdata10 <- data.frame(Temperature = c(10,10), PreyStart = c(1.336603,24.74493))
Elnewdata10$Focus <- predict(AOV_Focus, newdata = Elnewdata10, type = "response", re.form = NA)
Elnewdata10$RawFocus <- 10^Elnewdata10$Focus

# Create a line for Eurycea 15
Elnewdata15 <- data.frame(Temperature = c(15,15), PreyStart = c(1.336603,24.74493))
Elnewdata15$Focus <- predict(AOV_Focus, newdata = Elnewdata15, type = "response", re.form = NA)
Elnewdata15$RawFocus <- 10^Elnewdata15$Focus

# Create a line for Eurycea 20
Elnewdata20 <- data.frame(Temperature = c(20,20), PreyStart = c(1.336603,24.74493))
Elnewdata20$Focus <- predict(AOV_Focus, newdata = Elnewdata20, type = "response", re.form = NA)
Elnewdata20$RawFocus <- 10^Elnewdata20$Focus

# Create a line for Eurycea 25
Elnewdata25 <- data.frame(Temperature = c(25,25), PreyStart = c(1.336603,24.74493))
Elnewdata25$Focus <- predict(AOV_Focus, newdata = Elnewdata25, type = "response", re.form = NA)
Elnewdata25$RawFocus <- 10^Elnewdata25$Focus

#plot logistic regression curve
data1 <- data.frame(Temperature, Focus, PreyStart)
ggplot(data1, aes(x=PreyStart, y=Focus)) + 
  scale_color_manual(values = c("orange", "navy")) +
  geom_point() +
  geom_line(data = Elnewdata5, size=1, col = 'orange') +
  geom_line(data = Elnewdata10, size=1, col = 'orange') +
  geom_line(data = Elnewdata15, size=1, col = 'orange') +
  geom_line(data = Elnewdata20, size=1, col = 'orange') +
  geom_line(data = Elnewdata25, size=1, col = 'orange') 

summary(AOV_Focus)

