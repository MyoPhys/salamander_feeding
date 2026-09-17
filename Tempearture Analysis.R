# These packages are required to run this script. If you don't have these, use the following command 
# install.packages("pracma","signal","pspline","lme4","emmeans","ggplot2","lmerTest")
require(pracma)
require(signal)
require(pspline)
require(lme4)
require(emmeans)
require(ggplot2)
require(lmerTest)

# Import the data file, a csv. file combining both species.
filename <- file.choose()
data <- (read.csv(filename, sep = ",", skip=0))

# Separates the data based on Species
dataPm <- data[which(data$Species=="Pm"),]
dataEl <- data[which(data$Species=="El"),]

# Define each variable
# Metadata
Species <- as.factor(data$species)
Name <- data$Name
AllIndividual <- factor(data$individual)
AllTemperature <- data$temperature
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
# Set the variable of interest. The expected relationship of any kinematic with temperature
# is exponential, so we always log10-transform the dependent variable
RawFocus <- AvProjVel

Focus <- log10(RawFocus[which(data$species=="Pm")])
Temperature <- AllTemperature[which(data$species=="Pm")]
Individual <- Name[which(data$species=="Pm")]
aNormPreyStart <- PreyStart[which(data$species=="Pm")]

PMdata525 <- data.frame(Focus,Temperature,Individual,aNormPreyStart)
PMAOV_Focus525 <- lmer(data = PMdata525, Focus ~ Temperature +  (1|Individual))

PMdata515 <- PMdata525[which(Temperature == c(5,10,15)),]
PMAOV_Focus515 <- lmer(data = PMdata515, Focus ~ Temperature+ (1|Individual))

PMdata1020 <- PMdata525[which(Temperature == c(10,15,20)),]
PMAOV_Focus1020 <- lmer(data = PMdata1020, Focus ~ Temperature+ (1|Individual))

PMdata1525 <- PMdata525[which(Temperature == c(15,20,25)),]
PMAOV_Focus1525 <- lmer(data = PMdata1525, Focus ~ Temperature+ (1|Individual))

# Create a line for Plethodon 5-25
PMnewdata525 <- data.frame(Individual = c("Pm1","Pm1"), Temperature = c(5,25),Species = c("Pm","Pm"),Success = c("S","S"),aNormPreyStart = c(11.92034,11.92034))
PMnewdata525$newFocus <- predict(PMAOV_Focus525, newdata = PMnewdata525, re.form = NA)
PMnewdata525$RawFocus <- 10^PMnewdata525$newFocus

# Create a line for Plethodon 5-15
PMnewdata515 <- data.frame(Individual = c("Pm1","Pm1"), Temperature = c(5,15),Species = c("Pm","Pm"),Success = c("S","S"),aNormPreyStart = c(11.92034,11.92034))
PMnewdata515$Focus <- predict(PMAOV_Focus515, newdata = PMnewdata515, type = "response", re.form = NA)
PMnewdata515$RawFocus <- 10^PMnewdata515$Focus

# Create a line for Plethodon 10-20
PMnewdata1020 <- data.frame(Individual = c("Pm1","Pm1"), Temperature = c(10,20),Species = c("Pm","Pm"),Success = c("S","S"),aNormPreyStart = c(11.92034,11.92034))
PMnewdata1020$Focus <- predict(PMAOV_Focus1020, newdata = PMnewdata1020, type = "response", re.form = NA)
PMnewdata1020$RawFocus <- 10^PMnewdata1020$Focus

# Create a line for Plethodon 15-25
PMnewdata1525 <- data.frame(Individual = c("Pm1","Pm1"), Temperature = c(15,25),Species = c("Pm","Pm"),Success = c("S","S"),aNormPreyStart = c(11.92034,11.92034))
PMnewdata1525$Focus <- predict(PMAOV_Focus1525, newdata = PMnewdata1525, type = "response", re.form = NA)
PMnewdata1525$RawFocus <- 10^PMnewdata1525$Focus

Focus <- log10(RawFocus[which(data$species=="El")])
Temperature <- AllTemperature[which(data$species=="El")]
Individual <- AllIndividual[which(data$species=="El")]
aNormPreyStart <- PreyStart[which(data$species=="El")]

Eldata525 <- data.frame(Focus,Temperature,Individual,aNormPreyStart)
ElAOV_Focus525 <- lmer(data = Eldata525, Focus ~ Temperature +  aNormPreyStart +(1|Individual))

Eldata515 <- Eldata525[which(Temperature == c(5,10,15)),]
ElAOV_Focus515 <- lmer(data = Eldata515, Focus ~ Temperature+ aNormPreyStart +(1|Individual))

Eldata1020 <- Eldata525[which(Temperature == c(10,15,20)),]
ElAOV_Focus1020 <- lmer(data = Eldata1020, Focus ~ Temperature+ aNormPreyStart +(1|Individual))

Eldata1525 <- Eldata525[which(Temperature == c(15,20,25)),]
ElAOV_Focus1525 <- lmer(data = Eldata1525, Focus ~ Temperature+  aNormPreyStart +(1|Individual))

# Create a line for Plethodon 5-25
Elnewdata525 <- data.frame(Individual = c("El2","El2"), Temperature = c(5,25),Species = c("El","El"),Success = c("S","S"),aNormPreyStart = c(12.41756,12.41756))
Elnewdata525$newFocus <- predict(ElAOV_Focus525, newdata = Elnewdata525, re.form = NA)
Elnewdata525$RawFocus <- 10^Elnewdata525$newFocus

# Create a line for Plethodon 5-15
Elnewdata515 <- data.frame(Individual = c("El2","El2"),Temperature = c(5,15),Species = c("El","El"),Success = c("S","S"),aNormPreyStart = c(12.41756,12.41756))
Elnewdata515$Focus <- predict(ElAOV_Focus515, newdata = Elnewdata515, type = "response", re.form = NA)
Elnewdata515$RawFocus <- 10^Elnewdata515$Focus

# Create a line for Plethodon 10-20
Elnewdata1020 <- data.frame(Individual = c("El2","El2"),Temperature = c(10,20),Species = c("El","El"),Success = c("S","S"),aNormPreyStart = c(12.41756,12.41756))
Elnewdata1020$Focus <- predict(ElAOV_Focus1020, newdata = Elnewdata1020, type = "response", re.form = NA)
Elnewdata1020$RawFocus <- 10^Elnewdata1020$Focus

# Create a line for Plethodon 15-25
Elnewdata1525 <- data.frame(Individual = c("El2","El2"),Temperature = c(15,25),Species = c("El","El"),Success = c("S","S"),aNormPreyStart = c(12.41756,12.41756))
Elnewdata1525$Focus <- predict(ElAOV_Focus1525, newdata = Elnewdata1525, type = "response", re.form = NA)
Elnewdata1525$RawFocus <- 10^Elnewdata1525$Focus


Individual <- Name
Temperature <- AllTemperature
#plot logistic regression curve
data1 <- data.frame(Individual, Temperature, RawFocus, Species)
ggplot(data1, aes(x=Temperature, y=RawFocus, col = Species, shape = Individual)) + 
  scale_y_continuous(trans='log10') +
  geom_jitter(width=0.5) +
  scale_color_manual(values = c("orange", "navy")) +
  scale_shape_manual(values = c(0,1,2,3,6,0,1,2,3,6,8)) +
  geom_line(data = PMnewdata525, size=1.5, col = 'navy') +
  geom_line(data = PMnewdata515, size=1, col = 'navy') +
  geom_line(data = PMnewdata1020, size=1, col = 'navy') +
  geom_line(data = PMnewdata1525, size=1, col = 'navy') +
  geom_line(data = Elnewdata525, size=1.5, col = 'orange') +
  geom_line(data = Elnewdata515, size=1, col = 'orange') +
  geom_line(data = Elnewdata1020, size=1, col = 'orange') +
  geom_line(data = Elnewdata1525, size=1, col = 'orange')

anova(PMAOV_Focus525)
summary(PMAOV_Focus525)
anova(PMAOV_Focus515)
summary(PMAOV_Focus515)
anova(PMAOV_Focus1020)
summary(PMAOV_Focus1020)
anova(PMAOV_Focus1525)
summary(PMAOV_Focus1525)

anova(ElAOV_Focus525)
summary(ElAOV_Focus525)
anova(ElAOV_Focus515)
summary(ElAOV_Focus515)
anova(ElAOV_Focus1020)
summary(ElAOV_Focus1020)
anova(ElAOV_Focus1525)
summary(ElAOV_Focus1525)
