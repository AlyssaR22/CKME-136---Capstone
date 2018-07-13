# CKME-136---Capstone

Provided below is the code used in RStudio for the data analysis of the Hospital Readmissions dataset:

#pulling in the data from the CSV file
library("plyr")
library("stats")
library("caret")
library("class")
library("gmodels")
HRD <- read.csv("C:/Users/Alyssa/Downloads/HospitalReadmissions2016.csv")

#initial observations using the data as is
summary(HRD)
#These are the results without changing the readmission ratio and quality measures to numeric values rather than factors
#this tells us that there were 5,419 excess readmission ratio measures that were "Not Available", 1,620 observations that had "Not Available" and 42 #N/A for hospital overall rating. 

#As some of the attributes were factors, changed them to numeric values to determine the summary statistics
HRD$Hospital.Overall.Rating <- as.numeric(as.character(HRD$Hospital.Overall.Rating))
HRD$Excess.Readmission.Ratio <- as.numeric(as.character(HRD$Excess.Readmission.Ratio))
summary(HRD)

#initial look at relationship between ratio and overall rating
plot(HRD$Hospital.Overall.Rating,HRD$Excess.Readmission.Ratio,main = "Hospital Rating vs. Excess Readmission Ratio", xlab="Hospital Overall Rating", ylab ="Excess Readmission Ratio")

#converting numeric values back to character values so that it can be recognized below
HRD <- read.csv("C:/Users/Alyssa/Downloads/HospitalReadmissions2016.csv")
str(HRD)

#cleaning the data to remove the observations that have the excess readmission ratio or hospital quality rating missing.
HRD2<-HRD[!(HRD[,7]=="Not Available"),]
HRD3<-HRD2[!(HRD2[,13]=="Not Available"),]
HRD3<-HRD3[!(HRD3[,13]=="#N/A"),]

str(HRD3)

HRD3$Hospital.Overall.Rating = as.numeric(as.character(HRD3$Hospital.Overall.Rating))
HRD3$Excess.Readmission.Ratio <- as.numeric(as.character(HRD3$Excess.Readmission.Ratio))

str(HRD3)
summary(HRD3)

#new plot with the cleaned dataset
plot(HRD3$Hospital.Overall.Rating,HRD3$Excess.Readmission.Ratio,main = "Hospital Rating vs. Excess Readmission Ratio - Cleaned", xlab="Hospital Overall Rating", ylab ="Excess Readmission Ratio")


#calculating the average readmission ratio for each hospital rather than per condition
HospMeans <- aggregate(HRD3$Excess.Readmission.Ratio, by=list(HRD3$Hospital.Name), FUN=mean)
HospRating <- aggregate(HRD3$Hospital.Overall.Rating, by=list(HRD3$Hospital.Name), FUN=max)

HospMeanRating<- merge(HospMeans, HospRating, by="Group.1")


#plot of the average excess readmission ratios vs the hospital overall rating
plot(HospMeanRating$x.y,HospMeanRating$x.x,main = "Hospital Rating vs. Average Hospital Readmission Ratio", xlab="Hospital Overall Rating", ylab = "Avg. Excess Readmission Ratio")


#frequency

plot(HRD3$Measure.Name)
count(HRD3$Measure.Name)

count(HRD3$Hospital.Overall.Rating)

plot(HRD3$Hospital.Ownership)
count(HRD3$Hospital.Ownership)

plot(HRD3$State)
count(HRD3$State)

#for the categorical variables this shows that the measure that had the highest number of observations were PN, HF & COPD. For hospital ownership, the types with the highest number are "Voluntary non-profit - Private", "proprietary" & "Voluntary non-profit - Other". The states with the highest number of observations are CA, TX, & FL.


#Correlation
cor(HRD3$Excess.Readmission.Ratio, HRD3$Hospital.Overall.Rating, method = "pearson")
#This correlation value means that as one value increases, the other value decreases. ie. as excess readmission ratio increases, the overall hospital quality rating decreases. This makes sense because the more readmissions there are, the lower the quality of healthcare supposedly. The only issue with this value is that it is low, which means it is not statistically significant.


#Calculate the average excess readmission ratio, sorted by condition.
aggregate(HRD3$Excess.Readmission.Ratio, by=list(HRD3$Measure.Name), FUN=mean)


#Calculate the average excess readmission ratio, sorted by State
aggregate(HRD3$Excess.Readmission.Ratio, by=list(HRD3$State), FUN=mean)


#Histograms of the two variables of interest to determine the distribution of the data
hist(HospMeanRating$x.x)
hist(HospMeanRating$x.y)


#outliers for the average readmission ratio portion of the data
boxplot(HospMeanRating[,2], main="Hosp. Readmission Ratio")
boxplot(HospMeanRating[,3], main="Hosp. Rating")


#outliers for the original readmission ratio portion of the data
boxplot(HRD3[,7],main = "Excess Readmission Ratio")
boxplot(HRD3[,13],main = "Hospital Overall Rating")
#outliers to be kept. Hospital overall rating is normally distributed. 
#do correlation, and relationship between two variables.


#logistic regression model using the overall average ratios
HospMeanRating$x.y <- as.numeric(HospMeanRating$x.y)

str(HospMeanRating)

HMR <- sample(nrow(HospMeanRating),floor(nrow(HospMeanRating)*0.7))
HMRTrain<- HospMeanRating[HMR,]
HMRTest<- HospMeanRating[-HMR,]
str(HMRTrain2)

qualitymodel2 <- glm(x.y ~ x.x, data = HMRTrain2)
summary(qualitymodel2)

qprediction2 <- round(predict(qualitymodel2,newdata = HMRTest2),0)

CrossTable(x=qprediction2,y= HMRTestLabels,prop.chisq = FALSE)

str(qprediction2)

curve(predict(qualitymodel2, data.frame(x.x=x) , type="resp"))






HRDm <- sample(nrow(HRD3),floor(nrow(HRD3)*0.7))
HRDmTrain<- HRD3[HRDm,]
HRDmTest<- HRD3[-HRDm,]
str(HRDmTrain)

qualitymodel <- lm(Excess.Readmission.Ratio ~ Hospital.Overall.Rating, data = HRDmTrain)
summary(qualitymodel)

qprediction <- predict(qualitymodel, interval = "prediction", newdata = HRDmTest)
curve(predict(qualitymodel, data.frame(HRDmTrain$Hospital.Overall.Rating=x) , type="resp"), add=TRUE)

error <-qprediction[,"fit"] - HRDmTest$Excess.Readmission.Ratio
hist(error)

rmse <- sqrt(sum((qprediction[,"fit"] - HRDmTest$Excess.Readmission.Ratio)^2)/(nrow(HRDmTest)))
change <- 1 - ((HRDmTest$Excess.Readmission.Ratio - abs(error)) / HRDmTest$Excess.Readmission.Ratio)
prctcases <- table(change<0.25)
prctcases

#Test and training sets using the 10-fold cross validation.
x <- HRD3[,1:12]
q<- HRD3[,13]
set.seed(1)
folds<-createFolds(q, k = 10, list = TRUE, returnTrain = TRUE)
str(folds)

q[folds[[1]]]
x[folds[[1]], 1:3]
