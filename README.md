# CKME-136---Capstone

Provided below is the code used in RStudio for the data analysis of the Hospital Readmissions dataset:

#pulling in the data from the CSV file
library("plyr")
library("stats")
HRD <- read.csv("C:/Users/Alyssa/Downloads/HospitalReadmissions2016.csv")

#initial observations using the data as is
summary(HRD)

#As some of the attributes were factors, changed them to numeric values to determine the summary statistics
HRD$Hospital.Overall.Rating <- as.numeric(as.character(HRD$Hospital.Overall.Rating))
HRD$Excess.Readmission.Ratio <- as.numeric(as.character(HRD$Excess.Readmission.Ratio))
summary(HRD)

#initial look at relationship between ratio and overall rating
plot(HRD$Hospital.Overall.Rating,HRD$Excess.Readmission.Ratio,main = "Hospital Rating vs. Excess Readmission Ratio", xlab="Hospital Overall Rating", ylab ="Excess Readmission Ratio")

#cleaning the data to remove the values that are unavailable
HRD2<-HRD[!(HRD[,7]=="Not Available"),]
HRD3<-HRD2[!(HRD2[,13]=="Not Available"),]
HRD3<-HRD3[!(HRD3[,13]=="#N/A"),]

#new plot with the cleaned dataset
plot(HRD3$Hospital.Overall.Rating,HRD3$Excess.Readmission.Ratio,main = "Hospital Rating vs. Excess Readmission Ratio - Cleaned", xlab="Hospital Overall Rating", ylab ="Excess Readmission Ratio")
