# These packages are required to run this script. If you don't have these, use the following command 
# install.packages("pracma","signal","pspline","lme4","emmeans","ggplot2","lmerTest")
require(pracma)
require(signal)
require(pspline)
require(lme4)
require(emmeans)
require(ggplot2)
require(lmerTest)

# Import the data file, a csv. file for Plethodon
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

# Create a line for Plethodon 5
Pmnewdata5 <- data.frame(Temperature = c(5,5), PreyStart = c(1.336603,33.67842))
Pmnewdata5$Focus <- predict(AOV_Focus, newdata = Pmnewdata5, type = "response", re.form = NA)
Pmnewdata5$RawFocus <- 10^Pmnewdata5$Focus

# Create a line for Plethodon 10
Pmnewdata10 <- data.frame(Temperature = c(10,10), PreyStart = c(1.336603,33.67842))
Pmnewdata10$Focus <- predict(AOV_Focus, newdata = Pmnewdata10, type = "response", re.form = NA)
Pmnewdata10$RawFocus <- 10^Pmnewdata10$Focus

# Create a line for Plethodon 15
Pmnewdata15 <- data.frame(Temperature = c(15,15), PreyStart = c(1.336603,33.67842))
Pmnewdata15$Focus <- predict(AOV_Focus, newdata = Pmnewdata15, type = "response", re.form = NA)
Pmnewdata15$RawFocus <- 10^Pmnewdata15$Focus

# Create a line for Plethodon 20
Pmnewdata20 <- data.frame(Temperature = c(20,20), PreyStart = c(1.336603,33.67842))
Pmnewdata20$Focus <- predict(AOV_Focus, newdata = Pmnewdata20, type = "response", re.form = NA)
Pmnewdata20$RawFocus <- 10^Pmnewdata20$Focus

# Create a line for Plethodon 25
Pmnewdata25 <- data.frame(Temperature = c(25,25), PreyStart = c(1.336603,33.67842))
Pmnewdata25$Focus <- predict(AOV_Focus, newdata = Pmnewdata25, type = "response", re.form = NA)
Pmnewdata25$RawFocus <- 10^Pmnewdata25$Focus

#plot logistic regression curve
data1 <- data.frame(Focus, PreyStart)
ggplot(data1, aes(x=PreyStart, y=Focus)) + 
  scale_color_manual(values = c("orange", "navy")) +
  geom_point() +
  geom_line(data = Pmnewdata5, size=1, col = 'navy') +
  geom_line(data = Pmnewdata10, size=1, col = 'navy') +
  geom_line(data = Pmnewdata15, size=1, col = 'navy') +
  geom_line(data = Pmnewdata20, size=1, col = 'navy') +
  geom_line(data = Pmnewdata25, size=1, col = 'navy') 
  
summary(AOV_Focus)
