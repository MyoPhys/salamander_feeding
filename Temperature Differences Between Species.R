# These packages are required to run this script. If you don't have these, use the following command 
# install.packages("pracma","signal","pspline","lme4","emmeans","ggplot2","lmerTest")
require(pracma)
require(signal)
require(pspline)
require(lme4)
require(emmeans)
require(pbkrtest)
require(ggplot2)
require(lmerTest)

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
# Set the variable of interest. The expected relationship of any kinematic with temperature
# is exponential, so we always log10-transform the dependent variable
RawFocus <- AvLungeVel
Focus <- log10(RawFocus)

data525 <- data.frame(Focus,Temperature,Species,Individual,PreyStart)
AOV_Focus525 <- lmer(data = data525, Focus ~ Temperature*Species + PreyStart+(1|Species:Individual))

data515 <- data525[which(Temperature == c(5,10,15)),]
AOV_Focus515 <- lmer(data = data515, Focus ~ Temperature*Species + PreyStart+(1|Species:Individual))

data1020 <- data525[which(Temperature == c(10,15,20)),]
AOV_Focus1020 <- lmer(data = data1020, Focus ~ Temperature*Species + PreyStart+(1|Species:Individual))

data1525 <- data525[which(Temperature == c(15,20,25)),]
AOV_Focus1525 <- lmer(data = data1525, Focus ~ Temperature*Species + PreyStart+(1|Species:Individual))


#Anovas for difference at each temperature
data525 <- data.frame(RawFocus,Temperature,Species,Individual,PreyStart)
data5 <- data525[which(Temperature == 5),]
AOV_Focus5 <- lmer(data = data5, RawFocus ~ Species+PreyStart+(1|Species:Individual))

data10 <- data525[which(Temperature == 10),]
AOV_Focus10 <- lmer(data = data10, RawFocus ~ Species+PreyStart+(1|Species:Individual))

data15 <- data525[which(Temperature == 15),]
AOV_Focus15 <- lmer(data = data15, RawFocus ~ Species+PreyStart+(1|Species:Individual))

data20 <- data525[which(Temperature == 20),]
AOV_Focus20 <- lmer(data = data20, RawFocus ~ Species+PreyStart+(1|Species:Individual))

data25 <- data525[which(Temperature == 25),]
AOV_Focus25 <- lmer(data = data25, RawFocus ~ Species+PreyStart+(1|Species:Individual))


# Create a line for Plethodon 5-25
PMnewdata525 <- data.frame(Individual = c("Pm1","Pm1"), Temperature = c(5,25),Species = c("Pm","Pm"), PreyStart = c(11.92034,11.92034))
PMnewdata525$newFocus <- predict(AOV_Focus525, newdata = PMnewdata525, re.form = NA)
PMnewdata525$RawFocus <- 10^PMnewdata525$newFocus

# Create a line for Plethodon 5-15
PMnewdata515 <- data.frame(Individual = c("Pm1","Pm1"), Temperature = c(5,15),Species = c("Pm","Pm"), PreyStart = c(11.92034,11.92034))
PMnewdata515$Focus <- predict(AOV_Focus515, newdata = PMnewdata515, type = "response", re.form = NA)
PMnewdata515$RawFocus <- 10^PMnewdata515$Focus

# Create a line for Plethodon 10-20
PMnewdata1020 <- data.frame(Individual = c("Pm1","Pm1"), Temperature = c(10,20),Species = c("Pm","Pm"), PreyStart = c(11.92034,11.92034))
PMnewdata1020$Focus <- predict(AOV_Focus1020, newdata = PMnewdata1020, type = "response", re.form = NA)
PMnewdata1020$RawFocus <- 10^PMnewdata1020$Focus

# Create a line for Plethodon 15-25
PMnewdata1525 <- data.frame(Individual = c("Pm1","Pm1"), Temperature = c(15,25),Species = c("Pm","Pm"), PreyStart = c(11.92034,11.92034))
PMnewdata1525$Focus <- predict(AOV_Focus1525, newdata = PMnewdata1525, type = "response", re.form = NA)
PMnewdata1525$RawFocus <- 10^PMnewdata1525$Focus

# Create a line for Eurycea 5-25
Elnewdata525 <- data.frame(Individual = c("El2","El2"), Temperature = c(5,25),Species = c("El","El"), PreyStart = c(12.41756,12.41756))
Elnewdata525$newFocus <- predict(AOV_Focus525, newdata = Elnewdata525, re.form = NA)
Elnewdata525$RawFocus <- 10^Elnewdata525$newFocus

# Create a line for Eurycea 5-15
Elnewdata515 <- data.frame(Individual = c("El2","El2"), Temperature = c(5,15),Species = c("El","El"), PreyStart = c(12.41756,12.41756))
Elnewdata515$Focus <- predict(AOV_Focus515, newdata = Elnewdata515, type = "response", re.form = NA)
Elnewdata515$RawFocus <- 10^Elnewdata515$Focus

# Create a line for Eurycea 10-20
Elnewdata1020 <- data.frame(Individual = c("El2","El2"), Temperature = c(10,20),Species = c("El","El"), PreyStart = c(12.41756,12.41756))
Elnewdata1020$Focus <- predict(AOV_Focus1020, newdata = Elnewdata1020, type = "response", re.form = NA)
Elnewdata1020$RawFocus <- 10^Elnewdata1020$Focus

# Create a line for Eurycea 15-25
Elnewdata1525 <- data.frame(Individual = c("El2","El2"), Temperature = c(15,25),Species = c("El","El"), PreyStart = c(12.41756,12.41756))
Elnewdata1525$Focus <- predict(AOV_Focus1525, newdata = Elnewdata1525, type = "response", re.form = NA)
Elnewdata1525$RawFocus <- 10^Elnewdata1525$Focus

#plot logistic regression curve
data1 <- data.frame(Temperature, RawFocus, Species, Individual)
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




anova(AOV_Focus5)
anova(AOV_Focus10)
anova(AOV_Focus15)
anova(AOV_Focus20)
anova(AOV_Focus25)




