setwd("A:/Transportation Data Analytics/Term Project")
Mobility_Survey<-read.csv("A:/Transportation Data Analytics/Term Project/Citywide_Mobility_Survey_-_Trip_Diary_2018 (2).csv")
summary(Mobility_Survey)

Survey_Results<-subset(Mobility_Survey, select=-c(qDAY1TRIPTRAVELCODE24,qDAY1TRIPTRAVELCODE25,qDAY1TRIPTRAVELCODE26,qDAY1TRIPTRAVELCODE27,qDAY1TRIPTRANSITTO7,qDAY1TRIPTRANSITFROM7,qDISABILITY9,qWELFARE5,qshare8)) #Removes all variables coded as "refused response"
#is.na(Survey_Results) #view any NA results; add to subset removal
Survey_Results<-subset(Mobility_Survey, select=-c(qDAY1TRIPTRAVELCODE24,qDAY1TRIPTRAVELCODE25,qDAY1TRIPTRAVELCODE26,qDAY1TRIPTRAVELCODE27,qDAY1TRIPTRANSITTO7,qDAY1TRIPTRANSITFROM7,qDISABILITY9,qWELFARE5,qshare8,qday1typical,NTACODE_HOME,qDAY1TRIPSTARTAM,qDAY1TRIPSTARTNOON,qDAY1TRIPSTARTPM,qDAY1TRIP_OUTSIDENYC_START,qNTACODE_START,qNTACODE_END,qDAY1TRIPPARKPAY,qDAY1TRIPPARKPAY_AMOUNT,qDAY1TRIPBIKESTORE,qDAY1TRIPPARK,qDAY1TRIP_OUTSIDENYC_END,qDAY1TRIPTRANSITTO1,qDAY1TRIPTRANSITTO2,qDAY1TRIPTRANSITTO3,qDAY1TRIPTRANSITTO4, qDAY1TRIPTRANSITTO5,qDAY1TRIPTRANSITTO6,qDAY1TRIPTRANSITFROM1,qDAY1TRIPTRANSITFROM2,qDAY1TRIPTRANSITFROM3,qDAY1TRIPTRANSITFROM4,qDAY1TRIPTRANSITFROM5,qDAY1TRIPTRANSITFROM6)) #Removes Variables with High Amount of NA's (Contradictory and not relevent to model)
Survey_Results<-na.omit(Survey_Results)
summary(Survey_Results)
Survey_Results<-subset(Survey_Results, select = c(QDAY, qBOROUGH_HOME,qTRIPDAYTIME, qDAY1TRIPTRAVELCODE01,qDAY1TRIPTRAVELCODE02,qDAY1TRIPTRAVELCODE03,qDAY1TRIPTRAVELCODE04,qDAY1TRIPTRAVELCODE05,qDAY1TRIPTRAVELCODE06,qDAY1TRIPTRAVELCODE07,qDAY1TRIPTRAVELCODE08,qDAY1TRIPTRAVELCODE09,qDAY1TRIPTRAVELCODE10,qDAY1TRIPTRAVELCODE11,qDAY1TRIPTRAVELCODE12,qDAY1TRIPTRAVELCODE13,qDAY1TRIPTRAVELCODE14,qDAY1TRIPTRAVELCODE15,qDAY1TRIPTRAVELCODE16,qDAY1TRIPTRAVELCODE17,qDAY1TRIPTRAVELCODE18,qDAY1TRIPTRAVELCODE19,qDAY1TRIPTRAVELCODE20,qDAY1TRIPTRAVELCODE21, qDAY1TRIPTRAVELCODE22,qDAY1TRIPTRAVELCODE23,qDAY1TRIPPURPOSE,qDAY1TRIPLENGTH_CAT,qGENDER, qINCOME,qDISABILITY1,qDISABILITY2,qDISABILITY3,qDISABILITY4,qDISABILITY5,qDISABILITY6,qDISABILITY7, qAGECODE, qRACE,qHISPANIC,qEDUCATION,qLICENSE,qCARACCESS,qWELFARE1,qWELFARE2,qWELFARE3,qshare1,qshare2,qshare3,qshare4,qshare5,qshare6,qTEMPHIGH,qTEMPLOW,qPRECIPITATION,qModeGrouping))#Removing irrelevant variables

Survey_Analysis<-Survey_Results #Created Data Set for Analysis

Survey_Analysis['Disability'] <- 'Yes'#Create Disability Binary Variable; Yes:Disability: No:No Disability:
for (i in 1:nrow(Survey_Analysis)) {
  if(Survey_Analysis[i,"qDISABILITY7"] =="Yes"){
    Survey_Analysis[i,"Disability"]<-"No"
  }
}
Survey_Analysis<-subset(Survey_Analysis, select = -c(qDISABILITY1,qDISABILITY2,qDISABILITY3,qDISABILITY4,qDISABILITY5,qDISABILITY6,qDISABILITY7))

Survey_Analysis['Welfare']<-'Yes'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qWELFARE3"]=="Yes"){
    Survey_Analysis[i,"Welfare"]<-"No"
  }
}
Survey_Analysis<-subset(Survey_Analysis, select=-c(qWELFARE1,qWELFARE2,qWELFARE3))

Survey_Analysis['Rideshare']<-'Yes'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qshare6"]=="Yes"){
    Survey_Analysis[i,"Rideshare"]<-"No"
  }
}
Survey_Analysis<-subset(Survey_Analysis,select=-c(qshare1,qshare2,qshare3,qshare4,qshare5,qshare6))

#Converting Each Borough into Factor Variable
Survey_Analysis['Manhattan']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qBOROUGH_HOME"]=="Manhattan"){
    Survey_Analysis[i,'Manhattan']<-"1"
  }
}
Survey_Analysis['The Bronx']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qBOROUGH_HOME"]=="The Bronx"){
    Survey_Analysis[i,'The Bronx']<-"1"
  }
}
Survey_Analysis['Brooklyn']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qBOROUGH_HOME"]=="Brooklyn"){
    Survey_Analysis[i,'Brooklyn']<-"1"
  }
}
Survey_Analysis['Queens']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qBOROUGH_HOME"]=="Queens"){
    Survey_Analysis[i,'Queens']<-"1"
  }
}
Survey_Analysis['Staten Island']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qBOROUGH_HOME"]=="Staten Island"){
    Survey_Analysis[i,'Staten Island']<-"1"
  }
}
#Remove qBOROUGH_HOME varibles
Survey_Analysis<-subset(Survey_Analysis,select=-c(qBOROUGH_HOME))

#Recode the time of day variables to factors 
Survey_Analysis['AM']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qTRIPDAYTIME"]=="AM"){
    Survey_Analysis[i,'AM']<-"1"
  }
}
Survey_Analysis['PM']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qTRIPDAYTIME"]=="PM"){
    Survey_Analysis[i,'PM']<-"1"
  }
}
Survey_Analysis['NOON']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qTRIPDAYTIME"]=="NOON"){
    Survey_Analysis[i,'NOON']<-"1"
  }
}
#Remove qTRIPDAYTIME variable
Survey_Analysis<-subset(Survey_Analysis,select=-c(qTRIPDAYTIME))

#Recode Trip to to Weekend vs. Weekday 
Survey_Analysis['Weekend']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"QDAY"]=="Sunday"){
    Survey_Analysis[i,'Weekend']<-"1"
  }
  if(Survey_Analysis[i,"QDAY"]=="Saturday"){
    Survey_Analysis[i,"Weekend"]<-"1"
  }
} 
# Create a Weekday Variable 
Survey_Analysis['Weekday']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"Weekend"]=="0"){
    Survey_Analysis[i,'Weekday']<-"1"
  }
} 
#Remove QDAY Variable
Survey_Analysis<-subset(Survey_Analysis,select=-c(QDAY))

#Rename Travel Code To Fit Appropriate Mode
names(Survey_Analysis)[1] <- "Walk"
names(Survey_Analysis)[2]<-"Subway"
names(Survey_Analysis)[3]<-"LocalBus"
names(Survey_Analysis)[4]<-"BRT"
names(Survey_Analysis)[6]<-"Train"
names(Survey_Analysis)[9]<-"CommuterRail"
names(Survey_Analysis)[11]<-"PersonalCar"
names(Survey_Analysis)[12]<-"CarShare"
names(Survey_Analysis)[13]<-"Carpool"
names(Survey_Analysis)[14]<-"Motorcycle"
names(Survey_Analysis)[17]<-"RideHail"
names(Survey_Analysis)[18]<-"RideShare"
names(Survey_Analysis)[19]<-"CarService"
names(Survey_Analysis)[20]<-"DollarVan"
# Variable Name Changes; Please include in your code below"
#names(Survey_Analysis)[24]<-"TripPurpose"
#names(Survey_Analysis)[25]<-"TripLength"
#names(Survey_Analysis)[26]<-"Gender"
#names(Survey_Analysis)[27]<-"Income"
#names(Survey_Analysis)[28]<-"Age"
#names(Survey_Analysis)[29]<-"Race"
#names(Survey_Analysis)[30]<-"Hispanic"
#names(Survey_Analysis)[31]<-"Education"
#names(Survey_Analysis)[32]<-"LicenseAccess"
#names(Survey_Analysis)[33]<-"CarAccess"
#names(Survey_Analysis)[34]<-"TempLow"
#names(Survey_Analysis)[35]<-"TempHigh"
#names(Survey_Analysis)[36]<-"Precipitation"
#names(Survey_Analysis)[37]<-"Mode"

#Group Variables that Fall in Same Category 
Survey_Analysis['Ferry']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qDAY1TRIPTRAVELCODE07"]=="Yes"){
    Survey_Analysis[i,'Ferry']<-"1"
  }
  if (Survey_Analysis[i,"qDAY1TRIPTRAVELCODE08"]=="Yes"){
    Survey_Analysis[i,'Ferry']<-"1"
  }
}
Survey_Analysis<-subset(Survey_Analysis,select=-c(qDAY1TRIPTRAVELCODE08,qDAY1TRIPTRAVELCODE08))
Survey_Analysis['Taxi']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qDAY1TRIPTRAVELCODE15"]=="Yes"){
    Survey_Analysis[i,'Taxi']<-"1"
  }
  if (Survey_Analysis[i,"qDAY1TRIPTRAVELCODE16"]=="Yes"){
    Survey_Analysis[i,'Taxi']<-"1"
  }
}
Survey_Analysis['Bike']<-'0'
for (i in 1:nrow(Survey_Analysis)){
  if(Survey_Analysis[i,"qDAY1TRIPTRAVELCODE21"]=="Yes"){
    Survey_Analysis[i,'Bike']<-"1"
  }
  if (Survey_Analysis[i,"qDAY1TRIPTRAVELCODE22"]=="Yes"){
    Survey_Analysis[i,'Bike']<-"1"
  }
  if (Survey_Analysis[i,"qDAY1TRIPTRAVELCODE23"]== "Yes"){
    Survey_Analysis[i,'Bike']<-"1"
  }
}
Survey_Analysis<-subset(Survey_Analysis,select=-c(qDAY1TRIPTRAVELCODE15,qDAY1TRIPTRAVELCODE16,qDAY1TRIPTRAVELCODE21,qDAY1TRIPTRAVELCODE22,qDAY1TRIPTRAVELCODE23))
Survey_Analysis<-subset(Survey_Analysis,select=-c(qDAY1TRIPTRAVELCODE05,qDAY1TRIPTRAVELCODE10)) #Irrelevant Modes to Research

#Converting the variable Race into a factor variable 
unique(Survey_Analysis$qRACE)
class(Survey_Analysis[, "race"])
class(Survey_Analysis[, "qINCOME"])
frace<- factor(Survey_Analysis[,"qRACE",drop= FALSE])
race = factor(Survey_Analysis$qRACE,labels=c("1","2","3","4","5","6","7","8"))
Survey_Analysis<- cbind.data.frame(Survey_Analysis,race)
Survey_Analysis<- within.data.frame(Survey_Analysis,{rm(qRACE)})

#Converting the variable qIncome into a categorical variable
unique(Survey_Analysis$qINCOME)

fincome<- factor(Survey_Analysis[,"qINCOME"])
table(Survey_Analysis$qINCOME)
income= factor(Survey_Analysis$qINCOME,labels= c("1","2","3","4","5","6","7","8","9","10"))
Survey_Analysis<- cbind.data.frame(Survey_Analysis,income)


Trip_Purpose<- factor(Survey_Analysis$qDAY1TRIPPURPOSE)
table(Survey_Analysis$qDAY1TRIPPURPOSE)
Trip_Purpose<- factor(Survey_Analysis$qDAY1TRIPPURPOSE,labels = c("1","2","3","4","5","6","7","8","9","10","11"))
Survey_Analysis<- cbind.data.frame(Survey_Analysis,Trip_Purpose)

Trip_Length<- factor(Survey_Analysis$qDAY1TRIPLENGTH_CAT)
table(Survey_Analysis$qDAY1TRIPLENGTH_CAT)
Trip_Length<- factor(Survey_Analysis$qDAY1TRIPLENGTH_CAT,labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14"))
Survey_Analysis<- cbind.data.frame(Survey_Analysis,Trip_Length)

Age_Category<- factor(Survey_Analysis$qAGECODE)
table(Survey_Analysis$qAGECODE)
Age_Category<- factor(Survey_Analysis$qAGECODE,labels = c("1","2","3","4","5","6"))
Survey_Analysis<- cbind.data.frame(Survey_Analysis,Age_Category)

Hispanic<- factor(Survey_Analysis$qHISPANIC)
table(Survey_Analysis$qHISPANIC)
Hispanic<- factor(Survey_Analysis$qHISPANIC,labels = c("1","2","3","4","5","6"))
Survey_Analysis<- cbind.data.frame(Survey_Analysis,Hispanic)

Education<- factor(Survey_Analysis$qEDUCATION)
table(Survey_Analysis$qEDUCATION)
Education<- factor(Survey_Analysis$qEDUCATION,labels = c("1","2","3","4","5","6","7","8"))
Survey_Analysis<- cbind.data.frame(Survey_Analysis,Education)

Car_Access<- factor(Survey_Analysis$qCARACCESS)
table(Survey_Analysis$qCARACCESS)
Car_Access<- factor(Survey_Analysis$qCARACCESS,labels = c("1","2","3","4"))
Survey_Analysis<- cbind.data.frame(Survey_Analysis,Car_Access)

Mode_Grouping<- factor(Survey_Analysis$qModeGrouping)
table(Survey_Analysis$qModeGrouping)
Mode_Grouping<- factor(Survey_Analysis$qModeGrouping,labels = c("1","2","3","4","5","6","7","8","9"))
Survey_Analysis<- cbind.data.frame(Survey_Analysis,Mode_Grouping)

barplot(table(Survey_Analysis$qDAY1TRIPPURPOSE),space=1,beside = T,col =1:11, 
        main="Trip Purpose", cex.names = 0.8)
legend("topright", legend = c("Accompanying other traveler","Medical visit (doctor's office)","Commute to/from work","Personal errands","School","Social/recreation","Business "," Dining","Other","Refused","Shopping"),
       fill = 1:6, ncol = 2,
       cex = 0.6)

#install.packages("tidyverse")
#library(tidyverse)
#library(ggplot2)

#ggplot(Survey_Analysis, aes(x=factor(qDAY1TRIPPURPOSE)))+
 # geom_bar(stat="bin", width=0.7, fill="steelblue")+
  #theme_minimal()




write.csv(Survey_Analysis, "A:/Transportation Data Analytics/Term Project/Survey_Analysis.csv")

