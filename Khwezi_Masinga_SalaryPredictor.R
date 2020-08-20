
custs <- read.csv("EMPData_2.csv")
#custs <- Cust
head(custs)

#K-Nearest Neighbour (KNN) Model


custs.subset<-subset(custs, select = c(Department.Name,LastPaycheckGroups,AgeGroup,city,
                                   state_province,Country_id,Education,SalaryQual))

head(custs.subset)  

str(custs.subset)

#converting factors to integers
custs.subset$Department.Name<-as.integer(custs.subset$Department.Name)
custs.subset$LastPaycheckGroups<-as.integer(custs.subset$LastPaycheckGroups)
custs.subset$AgeGroup<-as.integer(custs.subset$AgeGroup)
custs.subset$city<-as.integer(custs.subset$city)
custs.subset$state_province<-as.integer(custs.subset$state_province)
custs.subset$Country_id<-as.integer(custs.subset$Country_id)
custs.subset$Education<-as.integer(custs.subset$Education)
custs.subset$SalaryQual<-as.integer(as.character(custs.subset$SalaryQual))


#Normalize
normalize<-function(x) {
  return((x-min(x))/(max(x) - min(x)))
}

custs.subset.norm<-as.data.frame(lapply(custs.subset[,1:7],normalize))

tail(custs.subset.norm,50)
str(custs.subset.norm)
subset.backup<-custs.subset.norm

#Separating dataset into their training set, testing set, and validation set
sets<-sample(1:3,size=nrow(custs.subset.norm),replace = TRUE, prob = c(0.5,0.3,0.2))

#Entering values into the train tet
train_Set<-custs.subset.norm[sets==1,]

#Entering values into the test tst
test_Set<-custs.subset.norm[sets==2,]

#Entering values into the validation set
valid_Set<-custs.subset.norm[sets==3,]


#write.csv(train_Set,"Train_Set.csv")
#write.csv(test_Set,"Test_Set.csv")
#write.csv(valid_Set,"Valid_Set.csv")


#Creating dataframe for SalaryQual which is the column we are working on for predictions
train_Set.labels<-custs.subset[sets==1,8]
head(train_Set.labels)

test_Set.labels<-custs.subset[sets==2,8]
valid_Set.labels<-custs.subset[sets==3,8]

test_Set.labels

#use class library for knn
library(class)

#identifying the value of k. k = Square root the number of records in train_Set
nrow(train_Set)

#KNN 
knn.310<- knn(train = train_Set,test = test_Set,cl=train_Set.labels,k=310)
knn.311<- knn(train = train_Set,test = test_Set,cl=train_Set.labels,k=311)

#Testing the accuracy of KNN
sum_knn.310<-sum(test_Set.labels==knn.310)
sum_knn.310
Accurate.310<-round(100*(sum_knn.310/NROW(test_Set.labels)),3)
Accurate.311<-round(100*(sum(test_Set.labels == knn.311)/NROW(test_Set.labels)),3)

Accurate.310 #93.9% accuracy
Accurate.311 #93.9% accuracy

# Comparing the amount of correct and incorrect records in a table
table(knn.310,test_Set.labels) #Lift table

#CrossTable (Gain)
library(gmodels)

CrossTable(test_Set.labels,knn.310,prop.chisq = FALSE,
           prop.c = FALSE,prop.r = FALSE,
           dnn = c('Actual Salary Qualication','predicted Salary Qualification'))

#Validation Test

knn.310.valid<- knn(train = train_Set,test= valid_Set,cl=train_Set.labels,k=310)

Acc.310.valid<-round(100*(sum(valid_Set.labels == knn.310.valid)/NROW(valid_Set.labels)),3)

Acc.310.valid # 94% accuracy

