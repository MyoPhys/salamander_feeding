README

Experimental data and analytical R codes supporting “Coordination of muscle- and spring-powered movements in salamander feeding across body temperatures”

This repository contains the raw data files and R scripts necessary to replicate the results of the study “Coordination of muscle- and spring-powered movements in salamander feeding across body temperatures”. This is a study of the effects of temperature on the coordination of thermally robust spring-powered tongue projection with thermally sensitive muscle-powered lunging in salamander feeding.

Data use license agreement: These data are provided only for reviewers and readers to check and replicate my analyses. For use of the data in other original research, please contact Jeffrey Olberding for permission.
Analyses were performed using RStudio V.2026.08.2 (Posit Software, PBC) running R V.4.4.1 (R Core Team).
1. “Raw Eurycea Data” – folder containing the digitized output of each feeding video for salamanders in genus Eurycea. File naming conventions include Genus Individual Temperature Date and Trial Number.
2. “Raw Plethodon Data” – folder containing the digitized output of each feeding video for salamanders in genus Plethodon. File naming conventions include Genus Individual Temperature Date and Trial Number.
3. “EuryceaCalibrationData.csv” - .csv file containing the calibration from pixels to mm used for each data file. 
4. “PlethodonCalibrationData.csv” - .csv file containing the calibration from pixels to mm used for each data file. 
5. “EuryceaExtraction_9_13_26.R” – R script that will process individual digitized data files for salamander in genus Eurycea to extract kinematic and kinetic variables. 
 - To run this file, set the working directory to “Raw Eurycea Data”.
 - This file will prompt you to select “EuryceaCalibrationData.csv”
- Creates an output file named “EuryceaData.csv
6. “PlethodonExtraction_9_13_26.R” – R script that will process individual digitized data files for salamander in genus Eurycea to extract kinematic and kinetic variables. 
 - To run this file, set the working directory to “Raw Plethodon Data”.
 - This file will prompt you to select “PlethodonCalibrationData.csv”
- Creates an output file named “PlethodonData.csv”
7. “EuryceaData_9_14_26.csv” – data files containing the extract kinematics and kinetics from the raw digitized files using “EuryceaExtraction_9_13_26.R”
8. “PlethodonData_9_14_26.csv” – data files containing the extract kinematics and kinetics from the raw digitized files using “PlethodonExtraction_9_13_26.R”
9. “CombinedData_9_14_26.csv” – data files containing the extract kinematics and kinetics from both “EuryceaData_9_14_26.csv” and “PlethodonData_9_14_26.csv”
10. “EL_PreyStartStats.R” – R script that will perform linear mixed-effects models of variables for Eurycea with prey starting distance as a fixed factor and individual as a random effect.
- requires “EuryceaData_9_14_26.csv”
11. “PM_PreyStartStats.R” – R script that will perform linear mixed-effects models of variables for Plethodon with prey starting distance as a fixed factor and individual as a random effect.
- requires “PlethodonData_9_14_26.csv”
12. “DescriptiveStats.R” – R script that extracts means+-SEM, minimum, and maximum for each variable. 
- requires either “EuryceaData_9_14_26.csv” or “PlethodonData_9_14_26.csv”
13. “TemperatureAnalysis.R” – R script that will perform linear mixed-effects models for each variable with temperature and prey starting distance as fixed effects and individual as a random effect. 
- requires either ““EuryceaData_9_14_26.csv” or “PlethodonData_9_14_26.csv”
14. “Temperature Differences Between Species.R” – R script that runs linear mixed-effects models for each variable with genus, temperature, prey starting distance, the interaction between genus and temperature as fixed effects and individual as a random effect. Also runs models at each discrete temperature with genus and prey starting distance as fixed effects and individual as a random effect. 
- requires “CombinedData_9_14_26.csv”







