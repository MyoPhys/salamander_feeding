## Set working directory to "Raw Eurycea Data"

ALLDATA <- NULL

filelist <- list.files()

# Import the calibration file, "EuryceaCalibrationData.csv"
filename <- file.choose()
caldata <- (read.csv(filename, sep = ",", skip=0))

for(i in 1:length(filelist)){
  
rm(list = setdiff(ls(), c("ALLDATA","filelist","i","caldata")))

require(pracma)
require(signal)
require(pspline)
require(lme4)
require(emmeans)
require(ggplot2)
require(svDialogs)
require(stringr)

# Import the data
filename <- filelist[i]
data <- (read.csv(filename, sep = ",", skip=0))

# Lookup the calibration for the trial
calibration <-  caldata[which(caldata$Filename==filename),2] #px/cm
calibration <- calibration #px/mm

# Ratio of SAR to tongue mass for Eurycea
SARratio <- 0.61

species <- str_sub(filename, start = 1, end = 3)
individual <- str_sub(filename, start = 5, end = 7)
temperature <- str_sub(filename, start = 10, end = 11)
date <- str_sub(filename, start = 14, end = 22)

# trim the data 
startframe <- which(is.finite(data[,3]))[1]

pefr <- length(which(is.finite(data[,5])))
pendframe <- which(is.finite(data[,5]))[pefr]

tefr <- length(which(is.finite(data[,1])))
tendframe <- which(is.finite(data[,1]))[tefr]

if(tendframe > pendframe){
  endframe <- pendframe
} else{
  endframe <- tendframe
}

ffr <- length(which(is.finite(data[,6])))
fendframe <- which(is.finite(data[,6]))[ffr]

# Assign the variables and convert from px to mm
tongueX <- data[startframe:endframe,1]/calibration
tongueY <- data[startframe:endframe,2]/calibration
noseX <- data[startframe:endframe,3]/calibration
noseY <- data[startframe:endframe,4]/calibration
preyX <- data[startframe:endframe,5]/calibration
preyY <- data[startframe:endframe,6]/calibration

# Filter each variable
# Assigning the filter specification variables
freq <- 3000 # Frequency of data
freqN <- freq/2 # Frequency normalized to Nyquist frequency
passbandFreqN <- 150/freqN  # Passband frequency AKA Wp; normalized to Nyquist frequency
stopbandFreqN <- 200/freqN  # Stopband frequency AKA Ws; normalized to Nyquist frequency
passbandRip <- 5 # Passband Ripple (dB) AKA Rp
stopbandAtt <- 25 # Stopband Attenuation (dB) AKA Rs

# Determing the order and cut-off frequency based on the filter specifications
buttOrderCut <- buttord(passbandFreqN, stopbandFreqN, passbandRip, stopbandAtt)

# Creating low pass Butterworth filter of order n
buttFiltLP <- butter(buttOrderCut)

# Normalized cut-off frequency
cutFreqN <- buttOrderCut$Wc/freqN

# Remove NaNs from Tongue variables
# Get the number of NaNs
sremoveNaN <- length(which(is.nan(tongueX))) + 1
eremoveNaN <- length(tongueX)

# Filter tongueX
filterdata <- tongueX[sremoveNaN:eremoveNaN]
filterdata1 <- c(rep(filterdata[1],500),filterdata,rep(filterdata[length(filterdata)],500))

# Using forward and reverse filtering to prevent phase shifts (i.e., making this a zero phase filter)
filterVPad <- filtfilt(buttFiltLP$b, buttFiltLP$a, filterdata1)
endplot <- length(filterVPad)-500
trimdata <- filterVPad[501:endplot]
plot(filterdata)
points(trimdata,col="red",type="l")

ftongueX <- c(rep(NaN,sremoveNaN-1),trimdata)

# Filter tongueY
filterdata <- tongueY[sremoveNaN:eremoveNaN]
filterdata1 <- c(rep(filterdata[1],500),filterdata,rep(filterdata[length(filterdata)],500))

# Using forward and reverse filtering to prevent phase shifts (i.e., making this a zero phase filter)
filterVPad <- filtfilt(buttFiltLP$b, buttFiltLP$a, filterdata1)
endplot <- length(filterVPad)-500
trimdata <- filterVPad[501:endplot]
plot(filterdata)
points(trimdata,col="red",type="l")

ftongueY <- c(rep(NaN,sremoveNaN-1),trimdata)

# Filter each variable
# Assigning the filter specification variables
freq <- 3000 # Frequency of data
freqN <- freq/2 # Frequency normalized to Nyquist frequency
passbandFreqN <- 50/freqN  # Passband frequency AKA Wp; normalized to Nyquist frequency
stopbandFreqN <- 100/freqN  # Stopband frequency AKA Ws; normalized to Nyquist frequency
passbandRip <- 5 # Passband Ripple (dB) AKA Rp
stopbandAtt <- 25 # Stopband Attenuation (dB) AKA Rs

# Determing the order and cut-off frequency based on the filter specifications
buttOrderCut <- buttord(passbandFreqN, stopbandFreqN, passbandRip, stopbandAtt)

# Creating low pass Butterworth filter of order n
buttFiltLP <- butter(buttOrderCut)

# Normalized cut-off frequency
cutFreqN <- buttOrderCut$Wc/freqN

# Filter noseX
filterdata <- noseX
filterdata1 <- c(rep(filterdata[1],500),filterdata,rep(filterdata[length(filterdata)],500))

# Using forward and reverse filtering to prevent phase shifts (i.e., making this a zero phase filter)
filterVPad <- filtfilt(buttFiltLP$b, buttFiltLP$a, filterdata1)
endplot <- length(filterVPad)-500
trimdata <- filterVPad[501:endplot]
plot(filterdata)
points(trimdata,col="red",type="l")

fnoseX <- trimdata

# Filter noseY
filterdata <- noseY
filterdata1 <- c(rep(filterdata[1],500),filterdata,rep(filterdata[length(filterdata)],500))

# Using forward and reverse filtering to prevent phase shifts (i.e., making this a zero phase filter)
filterVPad <- filtfilt(buttFiltLP$b, buttFiltLP$a, filterdata1)
endplot <- length(filterVPad)-500
trimdata <- filterVPad[501:endplot]
plot(filterdata)
points(trimdata,col="red",type="l")

fnoseY <- trimdata

# Filter preyX
# Get the number of NaNs
sremoveNaN <- length(which(is.nan(preyX))) + 1
eremoveNaN <- length(preyX)

filterdata <- preyX[sremoveNaN:eremoveNaN]
filterdata1 <- c(rep(filterdata[1],500),filterdata,rep(filterdata[length(filterdata)],500))

# Using forward and reverse filtering to prevent phase shifts (i.e., making this a zero phase filter)
filterVPad <- filtfilt(buttFiltLP$b, buttFiltLP$a, filterdata1)
endplot <- length(filterVPad)-500
trimdata <- filterVPad[501:endplot]
plot(filterdata)
points(trimdata,col="red",type="l")

fpreyX <- c(rep(NaN,sremoveNaN-1),trimdata)

# Filter preyX
# Get the number of NaNs
sremoveNaN <- length(which(is.nan(preyY))) + 1
eremoveNaN <- length(preyY)

filterdata <- preyY[sremoveNaN:eremoveNaN]
filterdata1 <- c(rep(filterdata[1],500),filterdata,rep(filterdata[length(filterdata)],500))

# Using forward and reverse filtering to prevent phase shifts (i.e., making this a zero phase filter)
filterVPad <- filtfilt(buttFiltLP$b, buttFiltLP$a, filterdata1)
endplot <- length(filterVPad)-500
trimdata <- filterVPad[501:endplot]
plot(filterdata)
points(trimdata,col="red",type="l")

fpreyY <- c(rep(NaN,sremoveNaN-1),trimdata)

# Calculate distance from nose to tongue tip
projectiondistance <- sqrt((fnoseX-ftongueX)^2+(fnoseY-ftongueY)^2)

# Calculate distance from nose to prey
preydistance <- sqrt((fpreyX-fnoseX)^2+(fpreyY-fnoseY)^2)

# Calculate distance from tongue to prey
tonguedistance <- sqrt((fpreyX-ftongueX)^2+(fpreyY-ftongueY)^2)

# Nose position at start
nosestartX <- na.omit(fnoseX)[1]
nosestartY <- na.omit(fnoseY)[1]

# Calculate move distance of the nose
lungedistance <- sqrt((fnoseX-nosestartX)^2+(fnoseY - nosestartY)^2)

# Calculate move distance of the prey
preymovedistance <- sqrt((fpreyX-nosestartX)^2+(fpreyY - nosestartY)^2)

## Calculate projection velocity and acceleration using quintic spline
smoothingcoef = 10^1
NNAprojdist <- na.omit(projectiondistance)
NNAprojdist <- NNAprojdist[1:100]
Time <- seq(from=1,to=100,by=1)*(1/3)
PointModel <- smooth.Pspline(Time,NNAprojdist,norder=5,method=1,spar=smoothingcoef)
proj_vel <- signif(predict(PointModel, Time, nderiv=1), digits=3)
proj_acc <- signif(predict(PointModel, Time, nderiv=2), digits=3)
proj_vel <- proj_vel
plot(proj_vel)
proj_acc <- proj_acc/(1/3000)
plot(Time, proj_acc, type="l")
points(Time, proj_vel, type="l")
msprojpower <- proj_acc*proj_vel
mmsprojpower <- msprojpower*SARratio
plot(mmsprojpower)
max(mmsprojpower)
max(proj_vel)
max(proj_acc)

## Calculate projection velocity and acceleration using quintic spline
smoothingcoef = 10^0.1
Time <- seq(from=1,to=length(lungedistance),by=1)
PointModel <- smooth.Pspline(Time,lungedistance,norder=5,method=1,spar=smoothingcoef)
lunge_vel <- signif(predict(PointModel, Time, nderiv=1), digits=3)
lunge_acc <- signif(predict(PointModel, Time, nderiv=2), digits=3)
lunge_vel <- lunge_vel
lunge_acc <- lunge_acc/(1/3000)
plot(Time, lunge_acc, type="l")
points(Time, lunge_vel, type="l")
bmslungepower <- lunge_acc*lunge_vel
bmslungepower <- bmslungepower
plot(bmslungepower)
max(bmslungepower)
max(lunge_vel)
max(lunge_acc)

# Calculate the prey distance at the start
preystartdist <- preydistance[is.finite(preydistance)][1]

# Calculate the time at which the projection starts
timeprojstart <- (length(which(is.nan(projectiondistance)))+1) 

# Calculate the prey distance when tongue projection starts
tonguestartdist <- preydistance[timeprojstart]

timeprojstart <- timeprojstart * (1/3000)

# Calculate the maximum projection distance
maxproj <- max(projectiondistance[is.finite(projectiondistance)])

# Calculate the time at which max projection distance occurs
timemaxproj <- which.max(projectiondistance) * (1/3000)

# Calculate the peak projection velocity
peakprojvel <- max(proj_vel)

# Calculate the time at which peak projection vel occurs
timemaxprojvel <- which.max(proj_vel) * (1/3000)+ timeprojstart

# Calculate the peak projection acceleration
peakprojacc <- max(proj_acc)

# Calculate the time at which peak projection acc occurs
timemaxprojacc <- which.max(proj_acc) * (1/3000)+ timeprojstart

# Calculate the peak mmspower of projection
peakmmsprojpower <- max(mmsprojpower)

# Calculate the time at which peak projection power occurs
timemaxprojpower <- which.max(mmsprojpower) * (1/3000)+ timeprojstart

# Calculate the maximum lunge distance
maxlunge <- max(lungedistance)

# Calculate the time at which max lunge distance occurs
timemaxlunge <- which.max(lungedistance) * (1/3000)

# Calculate the peak lunge velocity
peaklungevel <- max(lunge_vel)

# Calculate the time at which peak lunge vel occurs
timemaxlungevel <- which.max(lunge_vel) * (1/3000)

# Calculate the peak lunge acceleration
peaklungeacc <- max(lunge_acc)

# Calculate the time at which the peak lunge acc occurs 
timemmaxlungeacc<- which.max(lunge_acc) * (1/3000)

# Calculate the peak bmspower of the lunge
peakbmslungepower <- max(bmslungepower)

# Calculate the time at which the peak lunge mmp occurs
timemaxlungepower <- which.max(bmslungepower) * (1/3000)

# Calculate the combined reach of the lunge and tongue projection
reachdistance <- lungedistance + projectiondistance
maxreach <- max(reachdistance)

# Calculate the maximum combined reach distance
maxreach <- max(reachdistance[is.finite(reachdistance)])

# Calculate the time at which maximum reach occurs
timemaxreach <- which.max(reachdistance) * (1/3000)

# Print out the variables
output <- c(filename, species, individual, temperature, date, preystartdist, timeprojstart, maxproj, timemaxproj, peakprojvel, timemaxprojvel, peakprojacc, timemaxprojacc, peakmmsprojpower, timemaxprojpower,
  maxlunge, timemaxlunge, peaklungevel, timemaxlungevel, peaklungeacc, timemmaxlungeacc, peakbmslungepower, timemaxlungepower, maxreach, timemaxreach, tonguestartdist)

ALLDATA <- rbind(ALLDATA,output)
}

colnames(ALLDATA) <- c("filename", "species", "individual", "temperature", "date", "preystartdist", "timeprojstart", "maxproj", "timemaxproj", "peakprojvel", "timemaxprojvel", "peakprojacc", "timemaxprojacc", "peakmmsprojpower", "timemaxprojpower","maxlunge", "timemaxlunge", "peaklungevel", "timemaxlungevel", "peaklungeacc", "timemmaxlungeacc", "peakbmslungepower", "timemaxlungepower", "maxreach", "timemaxreach", "tonguestartdist")

write.csv(ALLDATA, "EuryceaData.csv", row.names = FALSE)

