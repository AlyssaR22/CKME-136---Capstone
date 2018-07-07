# CKME-136---Capstone

Provided below is the code used in RStudio for the data analysis of the Hospital Readmissions dataset:

#pulling in the data from the CSV file
library("plyr")
library("stats")
HRD <- read.csv("C:/Users/Alyssa/Downloads/HospitalReadmissions2016.csv")

#initial observations using the data as is
summary(HRD)
