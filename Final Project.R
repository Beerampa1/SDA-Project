Data<-read.csv("C:/Users/Saipa/Downloads/RadDat_5344.csv")
str(Data)
head(Data)
Data<- subset(Data, select = -c(Unique.Identifier))

head(Data)


Data <- read.csv(file = 'C:/Users/Saipa/DownloadsRadDat_5344.csv', stringsAsFactors = TRUE)
Data$In.Rad.Room<-as.numeric(Data$In.Rad.Room)

str(Data)
head(Data)
Data<- subset(Data, select = -c(Unique.Identifier))

head(Data)

Data<-na.omit(Data)


Model<-lm(Ordered.to.Complete...Mins~.,data = Data)
plot(Model)


plot(Model,4)

summary(Model)


ModelR<-lm(Ordered.to.Complete...Mins~.-In.Rad.Room, data= Data)
summary(ModelR)
plot(ModelR)
library(car)
vif(ModelR)

ModelR1<-lm(Ordered.to.Complete...Mins~.-In.Rad.Room-PatientAge, data= Data)
summary(ModelR1)
plot(ModelR1)
vif(ModelR1)

ModelR2<-lm(Ordered.to.Complete...Mins~.-In.Rad.Room-PatientAge-CatalogCode, data= Data)
summary(ModelR2)


###Backward direction stepwise regression###
Model1<-lm(Ordered.to.Complete...Mins~.,data= Data)

formula(Model1)
step(Model1,direction="backward")

##Call:
#lm(formula = Ordered.to.Complete...Mins ~ CatalogCode + PatientTypeMnemonic + 
 #    Priority + Loc.At.Exam.Complete + Exam.Completed.Bucket + 
  #   Exam.Room, data = Data)

vif(lm(formula = Ordered.to.Complete...Mins ~ CatalogCode + PatientTypeMnemonic + 
      Priority + Loc.At.Exam.Complete + Exam.Completed.Bucket + 
     Exam.Room, data = Data))

#                            GVIF  Df GVIF^(1/(2*Df))
#CatalogCode            24.406796 120        1.013401
#PatientTypeMnemonic     2.214866   3        1.141716
#Priority                3.111434   1        1.763926
#Loc.At.Exam.Complete   17.666907  24        1.061653
#Exam.Completed.Bucket   1.477875   2        1.102578
#Exam.Room             163.125771  12        1.236484




###Forward direction stepwise regression###

Model2<-lm(Ordered.to.Complete...Mins~1,data= Data)
formula(Model2)
step(Model2,scope~PatientAge+Radiology.Technician+In.Rad.Room+PatientTypeMnemonic+Priority+Loc.At.Exam.Complete,direction = "forward")

#lm(formula = Ordered.to.Complete...Mins ~ Loc.At.Exam.Complete + 
#           Priority + In.Rad.Room + PatientTypeMnemonic, data = Data)

vif(lm(formula = Ordered.to.Complete...Mins ~ Loc.At.Exam.Complete + 
                    Priority + In.Rad.Room + PatientTypeMnemonic, data = Data))

###vif(lm(formula = Ordered.to.Complete...Mins ~ Loc.At.Exam.Complete + 
         +                    # Priority + In.Rad.Room + PatientTypeMnemonic, data = Data))
#                         GVIF Df GVIF^(1/(2*Df))
#Loc.At.Exam.Complete 3.865685 24        1.028570
#Priority             2.835694  1        1.683952
#In.Rad.Room          1.067467  1        1.033183
#PatientTypeMnemonic  1.971144  3        1.119747

  
  
library(olsrr)
Model3<-lm(Ordered.to.Complete...Mins~.-In.Rad.Room-PatientAge-CatalogCode, data= Data)
summary.fit<-ols_step_best_subset(Model3)
summary.fit
plot(summary.fit)



mod<-lm(Ordered.to.Complete...Mins~., data = Data)
formula(mod)
step(mod,scope =PatientAge +In.Rad.Room+CatalogCode+ Radiology.Technician + PatientTypeMnemonic + Priority + 
       Loc.At.Exam.Complete + Exam.Completed.Bucket + Exam.Room ,direction = "both" )
