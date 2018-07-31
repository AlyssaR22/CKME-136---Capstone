# CKME-136---Capstone - Final Code

---
title: "CKME 136 v6"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown


```{r}
library("plyr")
library("stats")
library("caret")
library("class")
library("gmodels")
library("e1071")
library("MASS")
library("klaR")
HRD <- read.csv("C:/Users/Alyssa/Downloads/HospitalReadmissions2016.csv")
HRDi <- read.csv("C:/Users/Alyssa/Downloads/HospitalReadmissions2016.csv")

```

```{r}
summary(HRD)
#These are the results without changing the readmission ratio and quality measures to numeric values rather than factors
#this tells us that for the excess readmission ratio, there were 5,419 excess readmission ratio measures that were "Not Available" and for the hospital overall rating there were 1,620 observations that had "Not Available" and 42 #N/As. 

```


#converting factors to numbers to get the summary stats and impute the observations
```{r}
HRD$Hospital.Overall.Rating <- as.numeric(as.character(HRD$Hospital.Overall.Rating))
HRD$Excess.Readmission.Ratio <- as.numeric(as.character(HRD$Excess.Readmission.Ratio))
HRD$Cert..Bed.Count <- as.numeric(as.character(HRD$Cert..Bed.Count))
HRD$Number.of.Discharges <- as.numeric(as.character(HRD$Number.of.Discharges))
HRD$Number.of.Readmissions <- as.numeric(as.character(HRD$Number.of.Readmissions))
HRD$Predicted.Readmission.Rate <- as.numeric(as.character(HRD$Predicted.Readmission.Rate))
HRD$Expected.Readmission.Rate <- as.numeric(as.character(HRD$Expected.Readmission.Rate))

HRDi$Hospital.Overall.Rating <- as.numeric(as.character(HRDi$Hospital.Overall.Rating))
HRDi$Excess.Readmission.Ratio <- as.numeric(as.character(HRDi$Excess.Readmission.Ratio))
HRDi$Cert..Bed.Count <- as.numeric(as.character(HRDi$Cert..Bed.Count))
HRDi$Number.of.Discharges <- as.numeric(as.character(HRDi$Number.of.Discharges))
HRDi$Number.of.Readmissions <- as.numeric(as.character(HRDi$Number.of.Readmissions))
HRDi$Predicted.Readmission.Rate <- as.numeric(as.character(HRDi$Predicted.Readmission.Rate))
HRDi$Expected.Readmission.Rate <- as.numeric(as.character(HRDi$Expected.Readmission.Rate))
```

#Imputing the excess readmission ratios that are missing. 
```{r}
str(HRD)
str(HRDi)
mean(HRDi$Excess.Readmission.Ratio, na.rm = TRUE)
round(mean(HRDi$Hospital.Overall.Rating, na.rm = TRUE), digits = 0)

HRDi$Excess.Readmission.Ratio[is.na(HRDi$Excess.Readmission.Ratio)] = mean(HRDi$Excess.Readmission.Ratio, na.rm=TRUE)
HRDi$Hospital.Overall.Rating[is.na(HRDi$Hospital.Overall.Rating)] = round(mean(HRDi$Hospital.Overall.Rating, na.rm=TRUE), digits = 0)

summary(HRDi)
```


#5 number summary of the attributes
```{r}
str(HRD)
str(HRDi)
summary(HRD)
summary(HRDi)

#the minimum excess readmission ratio is 0.56 and the maximum is 1.73 when the values are imputed with the mean and when there are missing values.
```


```{r}
plot(HRD$Hospital.Overall.Rating,HRD$Excess.Readmission.Ratio,main = "Hospital Rating vs. Excess Readmission Ratio - Original Data ", xlab="Hospital Overall Rating", ylab ="Excess Readmission Ratio")
#these are the results with the ratios for all of the conditions at the hospitals

plot(HRDi$Hospital.Overall.Rating,HRDi$Excess.Readmission.Ratio,main = "Hospital Rating vs. Excess Readmission Ratio - Imputed Original", xlab="Hospital Overall Rating", ylab ="Excess Readmission Ratio")
#these are the results with the ratios for all of the conditions at the hospitals wirth the imputed values.

#There is no difference between the two plots

```

```{r}
boxplot(HRD[,7],main = "Excess Readmission Ratio - Original Data")
#this variable for each condition at each hospital is slightly right-skewed.

boxplot(HRDi[,7],main = "Excess Readmission Ratio -Imputed")
#this boxplot is more condensed than the one above. The median is the same for both boxplots. Q1 has decreased in this plot and Q3 has increased (the range has increased). The outliers are the same. The upper and lower fences have decreased (condensed) in this plot (the upper fence decreased and the lower fence increased).  

boxplot(HRD[,13],main = "Hospital Overall Rating - Original Data")

boxplot(HRDi[,13],main = "Hospital Overall Rating-Imputed")
#these boxplots are the same.
#outliers to be kept. Hospital overall rating is normally distributed. 
#do correlation, and relationship between two variables.
```


#converting numeric values back to character values so that it can be recognized below
```{r}
HRD <- read.csv("C:/Users/Alyssa/Downloads/HospitalReadmissions2016.csv")
str(HRD)
```

#analyzing the missing data
```{r}
missing <- HRD[(HRD[,7]=="Not Available"),]
missing2 <- HRD1[(HRD1[,13]=="Not Available"),]
missing3 <- HRD2[(HRD2[,13]=="#N/A"),]

missingdata<-rbind.data.frame(missing,missing2, missing3)

missingdata$Hospital.Overall.Rating = as.numeric(as.character(missingdata$Hospital.Overall.Rating))
missingdata$Excess.Readmission.Ratio <- as.numeric(as.character(missingdata$Excess.Readmission.Ratio))

summary(missingdata)
hist(missingdata$Hospital.Overall.Rating)
count(missingdata$Hospital.Overall.Rating)
hist(missingdata$Excess.Readmission.Ratio)
count(missingdata$Measure.Name)
plot(missingdata$Hospital.Name)
plot(missingdata$State)
count(missingdata$State)
#18% of the observations are were low quality ratings
#29% was high quality (4 & 5)
#47% was mediocre quality
#for the excess readmission ratio, the majority of the observations were between 0.9 and 1.1.
#733 observations were from TX and 579 were from CA. In the normal data, CA and TX had the most observations
#PN had the highest number of observations in the normal data, and CABG had the lowest. In this missing data, CABG is the measure that has the highest frequency.

#32% of the observations are from "Voluntary non-proft - Private" and 27% is "Proprietary"
#67% of the observations are from Urban hospitals, 33% is rural
```


#cleaning the dataset. We are removing the observations that have the excess readmission ratio missing, or the quality variable missing. We are not replacing the values as these measures are calculated using specific measures, replacement of the values can skew the measures.
```{r}
HRD1<-HRD[!(HRD[,7]=="Not Available"),]
HRD2<-HRD1[!(HRD1[,13]=="Not Available"),]
HRD3<-HRD2[!(HRD2[,13]=="#N/A"),]
```

```{r}
HRD3$Hospital.Overall.Rating = as.numeric(as.character(HRD3$Hospital.Overall.Rating))
HRD3$Excess.Readmission.Ratio <- as.numeric(as.character(HRD3$Excess.Readmission.Ratio))
HRD3$Cert..Bed.Count <- as.numeric(as.character(HRD3$Cert..Bed.Count))
HRD3$Number.of.Discharges <- as.numeric(as.character(HRD3$Number.of.Discharges))
HRD3$Number.of.Readmissions <- as.numeric(as.character(HRD3$Number.of.Readmissions))
HRD3$Predicted.Readmission.Rate <- as.numeric(as.character(HRD3$Predicted.Readmission.Rate))
HRD3$Expected.Readmission.Rate <- as.numeric(as.character(HRD3$Expected.Readmission.Rate))

str(HRD3)
summary(HRD3)
#This shows that the minimum excess readmission ratio was 0.62 and the maximum one was 1.73.
#The minimum number of discharges are 31 and the max is 8,355.
#The minimum number of readmissions is 11 and the max is 962, about 2,700 were "Too Few to Report"
#The minimum predicted readmission rate is 2.5 and the max is 31.86
#The minimum expected readmission rate is 2.72 and the max is 27.149
#The minimum bed count was 3, and the maximum was 2,449.

```

```{r}
plot(HRD3$Hospital.Overall.Rating,HRD3$Excess.Readmission.Ratio,main = "Hospital Rating vs. Excess Readmission Ratio - Cleaned Dataset", xlab="Hospital Overall Rating", ylab ="Excess Readmission Ratio")

#this plot shows every excess readmission ratio for each condition for each hospital, so we may have high ratios for high quality hospitals as it could be due to the measure for that specific condition (ie. one condition has a high ratio while the other conditions for the same hospital have low ratios which leads to the mean ratio for the hospital being low).We do see here that the frequency of high ratios does decrease as the overall rating increases.

plot(HRDi$Hospital.Overall.Rating,HRDi$Excess.Readmission.Ratio,main = "Hospital Rating vs. Excess Readmission Ratio - Imputed", xlab="Hospital Overall Rating", ylab ="Excess Readmission Ratio")

#The second plot has more observations. Some of the observations that are included are outliers (there are lower excess readmission ratios).

```

#creating the dataset with the average readmission ratio per hospital using the cleaned dataset.
```{r}
HospMeans <- aggregate(HRD3$Excess.Readmission.Ratio, by=list(HRD3$Hospital.Name), FUN=mean)
HospRating <- aggregate(HRD3$Hospital.Overall.Rating, by=list(HRD3$Hospital.Name), FUN=max)

HospMeanRating<- merge(HospMeans, HospRating, by="Group.1")


str(HospMeanRating)

plot(HospMeanRating$x.y,HospMeanRating$x.x,main = "Hospital Rating vs. Average Hospital Readmission Ratio", xlab="Hospital Overall Rating", ylab = "Avg. Excess Readmission Ratio")
#When the average excess readmission ratio is taken for each hospital using each of the medical conditions rather than looking at each condition, we see a more apparent trend in the increased frequencies of lower ratios for higher quality hospitals, and higher ratios for lower quality hospitals.

```

#creating the dataset with the average readmission ratio per hospital using the imputed dataset.
```{r}
str(HRDi)
HospMeansi <- aggregate(HRDi$Excess.Readmission.Ratio, by=list(HRDi$Hospital.Name), FUN=mean)
HospRatingi <- aggregate(HRDi$Hospital.Overall.Rating, by=list(HRDi$Hospital.Name), FUN=max)

HospMeanRatingi<- merge(HospMeansi, HospRatingi, by="Group.1")


str(HospMeanRatingi)

plot(HospMeanRatingi$x.y,HospMeanRatingi$x.x,main = "(Imputed) Hospital Rating vs. Avg. Hospital Readmission Ratio", xlab="Hospital Overall Rating", ylab = "Avg. Excess Readmission Ratio")
#We see that the range has increased for each of the Rating values

```


```{r}
boxplot(HospMeanRating[,2], main="Average Excess Readmission Ratio - Average Dataset")
boxplot(HospMeanRating[,3], main="Hospital Rating - Average Data")
#both of the above are normally distributed when we take the mean per hospital.

boxplot(HospMeanRatingi[,2], main="Average Excess Readmission Ratio - Imputed")
boxplot(HospMeanRatingi[,3], main="Hospital Overall Rating - Imputed - Average Dataset")
#the boxplot for the the imputed readmission ratio is slightly right-skewed compared to the above. This means that the median is less than the median in the results above. This shows that the imputed version has overall lower readmission ratios than the version above.

boxplot(HRD3[,7],main = "Excess Readmission Ratio")
#this variable for each condition at each hospital is slightly right-skewed.

boxplot(HRD3[,13],main = "Hospital Overall Rating")

#outliers to be kept. Hospital overall rating is normally distributed. 
#do correlation, and relationship between two variables.
```


```{r}
#frequency

plot(HRD3$Measure.Name)
count(HRD3$Measure.Name)
#from these results we see that PN is the condition with the highest frequency out of all medical conditions. The condition with the lowest frequency is CABG (coronary Artery bypass graft). 
#for the categorical variables this shows that the measure that had the highest number of observations were PN, HF & COPD.

plot(HRD3$Hospital.Overall.Rating)
count(HRD3$Hospital.Overall.Rating)
#The rating with the highest occurence was 3, which is a moderate rating. Next was 4 then 2, The two exremes, had a lower number of observations.

plot(HRD3$Hospital.Ownership)
count(HRD3$Hospital.Ownership)
#the hospital ownership wih the highest amount of occurences is "voluntary non-profit - Private". The one with the least was "Tribal"

plot(HRD3$Rural.Urban)
count(HRD3$Rural.Urban)
#this shows that 78% of the observations are from urban hospitals.

plot(HRD3$State)
count(HRD3$State)
 The states with the highest number of observations are CA, TX, & FL.
```


#Correlation
```{r}
cor(HospMeanRating$x.y,HospMeanRating$x.x, method = "spearman")
#This correlation value means that as one value increases, the other value decreases. ie. as excess readmission ratio increases, the overall hospital quality rating decreases. This makes sense because the more readmissions there are, the lower the quality of healthcare supposedly. As this value rounds to be 0.5 we see that it is statistically significant.

cor(HospMeanRatingi$x.y,HospMeanRatingi$x.x, method = "spearman")


str(HRD3)
HRD3$Hospital.Overall.Rating<-as.numeric(as.character(HRD3$Hospital.Overall.Rating))
cor(HRD3$Hospital.Overall.Rating,HRD3$Number.of.Readmissions, use="complete.obs", method = "pearson")
cor(HRD3$Hospital.Overall.Rating,HRD3$Cert..Bed.Count, method = "pearson")
#these two above provide negative statistically insignificant results. NEED TO TRY WITH MEAN DATA BY HOSP.

cor(HRD3$Excess.Readmission.Ratio, HRD3$Hospital.Overall.Rating, method = "pearson")
#as this correlation value was calculated using the excess readmission ratio per condition, we get a value that is still negative, but less significant.

cor(HRD3$Excess.Readmission.Ratio, HRD3$Predicted.Readmission.Rate, method = "pearson")

cor(HRD3$Excess.Readmission.Ratio, HRD3$Expected.Readmission.Rate, method = "pearson")
aov(Excess.Readmission.Ratio ~ Hospital.Ownership, data=HRD3)
pairwise.t.test(HRD3$Excess.Readmission.Ratio,HRD3$Hospital.Ownership, p.adjust="bonferroni")

cor(HRD3$Excess.Readmission.Ratio, HRD3$Number.of.Discharges,use = "complete.obs",method = "pearson")
```


###Histograms
```{r}
hist(HospMeanRating$x.x)
hist(HospMeanRating$x.y)
```



#logistic regression model using avg. 
```{r}

HospMeanRating$x.y <- factor(HospMeanRating$x.y, levels = c("1","2","3","4","5"), ordered = TRUE)

str(HospMeanRating)

HMR <- sample(nrow(HospMeanRating),floor(nrow(HospMeanRating)*0.7))
HMRTrain<- HospMeanRating[HMR,]
HMRTest<- HospMeanRating[-HMR,]

HMRTrain2<- HospMeanRating[HMR,2:3]
HMRTest2<- HospMeanRating[-HMR,2:3]
HMRTrainLabels<- HospMeanRating[HMR,3]
HMRTestLabels<- HospMeanRating[-HMR,3]

str(HMRTrain2)

qualitymodel2 <- polr(x.y ~ x.x, data = HMRTrain2, method = "logistic")
qualitymodel2
summary(qualitymodel2)

qprediction2 <- predict(qualitymodel2,newdata = HMRTest2)

table(HMRTest2$x.y,qprediction2)
mean(as.character(HMRTest2$x.y) !=as.character(qprediction2))


str(qprediction2)

error <-qprediction2[] - HMRTest2$x.y
hist(error)

```


#logistic regression model using avg and imputed values. 
```{r}

HospMeanRatingi$x.y <- factor(HospMeanRatingi$x.y, levels = c("1","2","3","4","5"), ordered = TRUE)

str(HospMeanRatingi)

HMRi <- sample(nrow(HospMeanRatingi),floor(nrow(HospMeanRatingi)*0.7))
HMRTraini<- HospMeanRatingi[HMRi,]
HMRTesti<- HospMeanRatingi[-HMRi,]

HMRTraini2<- HospMeanRatingi[HMRi,2:3]
HMRTesti2<- HospMeanRatingi[-HMRi,2:3]
HMRTrainLabelsi<- HospMeanRatingi[HMRi,3]
HMRTestLabelsi<- HospMeanRatingi[-HMRi,3]

str(HMRTraini2)

qualitymodeli2 <- polr(x.y ~ x.x, data = HMRTraini2, method = "logistic")
summary(qualitymodeli2)

qpredictioni2 <- predict(qualitymodeli2,newdata = HMRTesti2)

table(HMRTesti2$x.y,qpredictioni2)
mean(as.character(HMRTesti2$x.y) !=as.character(qpredictioni2))


str(qpredictioni2)

curve(predict(qualitymodel2, data.frame(x.x=x) , type="resp"))

error <-qprediction2[] - HMRTest2$x.y
hist(error)

```


#Test and training sets using the 10-fold cross validation.
```{r}

set.seed(1)
Tcontrol<- trainControl(method = "cv", number = 10)
Model1<- train(x.y ~ x.x, data = HMRTrain2, method = "polr", trControl = Tcontrol)
print(Model1)
#This model is for the normal data with the avg. readmission ratios for the cleaned dataset. The RMSE value here is 0.98.


set.seed(2)
Tcontrol2<- trainControl(method = "cv", number = 10)
Model2<- train(x.y ~ x.x, data = HMRTraini2, method = "polr", trControl = Tcontrol2)
print(Model2)
#This model is for the data with the avg. readmission ratios for the imputed dataset. The RMSE value here is 0.93.

``` 

#Naive-Bayes for the cleaned dataset
```{r}
str(HMRTrain2)

nbmodel <- naiveBayes(x.y ~ x.x, data = HMRTrain2)

nbmodel

nbpredictions <- predict(nbmodel, HMRTrain2)
table(nbpredictions, HMRTrain2$x.y)
```

#Naive-Bayes for the imputed dataset
```{r}
str(HMRTraini2)

nbmodel2 <- naiveBayes(x.y ~ x.x, data = HMRTraini2)

nbmodel

nbpredictions2 <- predict(nbmodel, HMRTraini2)
table(nbpredictions2, HMRTraini2$x.y)
```

#Test and training sets using the 10-fold cross validation for Naive-Bayes.
```{r}

set.seed(3)
Tcontrolnb<- trainControl(method = "cv", number = 10)
Modelnb1<- train(x.y ~ x.x, data = HMRTrain2, method = "nb", trControl = Tcontrolnb)
print(Modelnb1)
#This model is for the normal data with the avg. readmission ratios for the cleaned dataset. The RMSE value here is 0.98.


set.seed(4)
Tcontrolnb2<- trainControl(method = "cv", number = 10)
Modelnb2<- train(x.y ~ x.x, data = HMRTraini2, method = "nb", trControl = Tcontrolnb2)
print(Modelnb2)
#This model is for the data with the avg. readmission ratios for the imputed dataset. The RMSE value here is 0.93.
```


#Calculate the average excess readmission ratio, sorted by condition.
```{r}
aggregate(HRD3$Excess.Readmission.Ratio, by=list(HRD3$Measure.Name), FUN=mean)
#as all of the values are 1.00, this shows that there is not one condition that has a significantly higher or lower excess readmission ratio. All of the conditions seems to have on average the same amount of readmission

aggregate(HRD3$Hospital.Overall.Rating, by=list(HRD3$Measure.Name), FUN=mean)
#this shows that AMI has the lower overall mean rating and Hip/knee has highest. Does not line up exactly with results above.
```

###Calculate the average excess readmission ratio, sorted by State.
```{r}
aggregate(HRD3$Excess.Readmission.Ratio, by=list(HRD3$State), FUN=mean)

aggregate(HRD3$Hospital.Overall.Rating, by=list(HRD3$State), FUN=mean)
#The state with highest overall mean rating, does not line up exactly with results above, but are close.
```

###Calculate the average excess readmission ratio, sorted by urban/rural
```{r}
aggregate(HRD3$Excess.Readmission.Ratio, by=list(HRD3$Rural.Urban), FUN=mean)
#This shows that urban hospitals on average have a higher excess readmission ratio, but this dataset does not have very many rural observations.

aggregate(HRD3$Hospital.Overall.Rating, by=list(HRD3$Rural.Urban), FUN=mean)
#shows rural had higher rating which corresponds with results above, but once again, not meaningful.
```


