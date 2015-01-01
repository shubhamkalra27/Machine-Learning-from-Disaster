data.dir   <- 'D:/github/Machine-Learning-from-Disaster/data/'
  train.file <- paste0(data.dir, 'train.csv')
  test.file  <- paste0(data.dir, 'test.csv')
  
  trainData <- read.csv(train.file, stringsAsFactors=F)
  testData  <- read.csv(test.file, stringsAsFactors=F)
  
  summary(trainData) 
  #totalPassengers = 891
  nrow(trainData)
  
  #survuved = 342
  sum(trainData$Survived) 
  
  #plotting a few parameters to get feel of the distributions
  plot(density(trainData$Age, na.rm = TRUE))
  plot(density(trainData$Pclass, na.rm = TRUE))
  
  plot(density(trainData$Fare, na.rm = TRUE))
  
  #1. Surival and Sex
  counts <- table(trainData$Survived, trainData$Sex)
  barplot(counts, xlab = "Gender", ylab = "Number of People",main = "survived and deceased between male and female")
  
  #female survival rate  = 74.20%
  counts[2] / (counts[1] + counts[2])
  
  #male survival rate = 18.889%
  counts[4] / (counts[3] + counts[4])
  
  
  #2. Surival and class
  Pclass_survival <- table(trainData$Survived, trainData$Pclass)
  barplot(Pclass_survival, xlab = "Class", ylab = "Number of People", main = "survived and deceased In seating Class")
  
  #class 1 survival rate = 62.96%6
  Pclass_survival[2]/sum(Pclass_survival[1:2]) 
  
  #class 2 survival rate = 47%
  Pclass_survival[4]/sum(Pclass_survival[3:4]) 
  
  #class 3 survival rate = 24.23%
  Pclass_survival[6]/sum(Pclass_survival[5:6]) 
  
  
  #CLEANING DATA
  
  #replacing male with 0 female with 1
  trainData$Sex <- gsub("female", 1, trainData$Sex)
  trainData$Sex <- gsub("male", 0, trainData$Sex)
  
  #removed the unwanted variables from train data
  trainData <- (trainData[-c(1,9:12)])
  
"there are a lot of missing age values. How author has decided to
  nullify that effect is to estimate/ guess the values of ages based on 
  the title of the people. 
"

"there are roughty so many kind of titles"
master_vector <- grep("Master.",trainData$Name, fixed=TRUE)
miss_vector <- grep("Miss.", trainData$Name, fixed=TRUE)
mrs_vector <- grep("Mrs.", trainData$Name, fixed=TRUE)
mr_vector <- grep("Mr.", trainData$Name, fixed=TRUE)
dr_vector <- grep("Dr.", trainData$Name, fixed=TRUE)

"average of people in that category"
master_age <- round(mean(trainData$Age[master_vector], na.rm = TRUE), digits = 2)
miss_age <- round(mean(trainData$Age[miss_vector], na.rm = TRUE), digits =2)
mrs_age <- round(mean(trainData$Age[mrs_vector], na.rm = TRUE), digits = 2)
mr_age <- round(mean(trainData$Age[mr_vector], na.rm = TRUE), digits = 2)
dr_age <- round(mean(trainData$Age[dr_vector], na.rm = TRUE), digits = 2)

"replacing those missing values with the assumed average value"
for (i in 1:nrow(trainData)) {
  if (is.na(trainData[i,5])) {    
    if (i %in% master_vector) {
      trainData$Age[i] = master_age
      print("Master")
    } else if (i %in% miss_vector) {
      print("Miss")
      trainData$Age[i] = miss_age
    } else if (i %in% mrs_vector) {
      print("Mrs")
      trainData$Age[i] = mrs_age
    } else if (i %in% mr_vector) {
      print("Mr")
      trainData$Age[i] = mr_age
    } else if (i %in% dr_vector) {
      print("dr")
      trainData$Age[i] = dr_age
    } else {
      print("Uncaught Title")
    }
  }
}

"adding new variables to the table from using the existing ones"

#child
trainData["Child"] <- NA
for (i in 1:nrow(trainData)) {
  if (trainData$Age[i] <= 12) {
    trainData$Child[i] = 1
  } else {
    trainData$Child[i] = 2
  }
}

#family guy
trainData["Family"] = NA

for(i in 1:nrow(trainData)) {
  x = trainData$SibSp[i]
  y = trainData$Parch[i]
  trainData$Family[i] = x + y + 1
}

#Mom
trainData["Mother"] <- NA
for(i in 1:nrow(trainData)) {
  if(trainData$Name[i] == "Mrs" & trainData$Parch[i] > 0) {
    trainData$Mother[i] = 1
  } else {
    trainData$Mother[i] = 2
  }
}
"==========================================================="
"until now. we have worked on the trainData till, we repeat similar 
steps for testData which doesn't have the surived column"
"============================================================"

summary(testData) 
#totalPassengers = 418
nrow(testData)
 

#plotting a few parameters to get feel of the distributions
plot(density(testData$Age, na.rm = TRUE))
plot(density(testData$Pclass, na.rm = TRUE))

plot(density(testData$Fare, na.rm = TRUE))

  

#CLEANING DATA

#replacing male with 0 female with 1
testData$Sex <- gsub("female", 1, testData$Sex)
testData$Sex <- gsub("male", 0, testData$Sex)
head(testData)
#removed the unwanted variables from testData
testData <- (testData[-c(1,8:11)])

"there are a lot of missing age values. How author has decided to
nullify that effect is to estimate/ guess the values of ages based on 
the title of the people. 
"

"there are roughty so many kind of titles"
test_master_vector = grep("Master.",testData$Name)
test_miss_vector = grep("Miss.", testData$Name)
test_mrs_vector = grep("Mrs.", testData$Name)
test_mr_vector = grep("Mr.", testData$Name)
test_dr_vector = grep("Dr.", testData$Name)

"average of people in that category"
test_master_age <- round(mean(trainData$Age[test_master_vector], na.rm = TRUE), digits = 2)
test_miss_age <- round(mean(trainData$Age[test_miss_vector], na.rm = TRUE), digits =2)
test_mrs_age <- round(mean(trainData$Age[test_mrs_vector], na.rm = TRUE), digits = 2)
test_mr_age <- round(mean(trainData$Age[test_mr_vector], na.rm = TRUE), digits = 2)
test_dr_age <- round(mean(trainData$Age[test_dr_vector], na.rm = TRUE), digits = 2)

"replacing those missing values with the assumed average value"
for (i in 1:nrow(testData)) {
  if (is.na(testData[i,4])) {    
    if (i %in% test_master_vector) {
      testData$Age[i] = test_master_age
      print("Master")
    } else if (i %in% test_miss_vector) {
      print("Miss")
      testData$Age[i] = test_miss_age
    } else if (i %in% test_mrs_vector) {
      print("Mrs")
      testData$Age[i] = test_mrs_age
    } else if (i %in% test_mr_vector) {
      print("Mr")
      testData$Age[i] = test_mr_age
    } else if (i %in% test_dr_vector) {
      print("dr")
      testData$Age[i] = test_dr_age
    } else {
      print(paste("Uncaught title at: ", i, sep=""))
      print(paste("The title unrecognized was: ", testData[i,2], sep=""))
    }
  }
}

"adding new variables to the table from using the existing ones"

#child
#We do a manual replacement here, because we weren't able to programmatically figure out the title.
#We figured out it was 89 because the above print statement should have warned us.
testData[89, 4] = test_miss_age

testData["Child"] <- NA
for (i in 1:nrow(testData)) {
  if (testData$Age[i] <= 12) {
    testData$Child[i] = 1
  } else {
    testData$Child[i] = 2
  }
}

#family guy
testData["Family"] = NA

for(i in 1:nrow(testData)) {
  x = testData$SibSp[i]
  y = testData$Parch[i]
  testData$Family[i] = x + y + 1
}

#Mom
testData["Mother"] <- NA
for(i in 1:nrow(testData)) {
  if(testData$Name[i] == "Mrs" & testData$Parch[i] > 0) {
    testData$Mother[i] = 1
  } else {
    testData$Mother[i] = 2
  }
}

train.glm <- glm(Survived ~ Pclass + Sex + Age + Child +
                   Sex*Pclass + Family + Mother, family = binomial, data = trainData)
summary(train.glm)

p.hats <- predict.glm(train.glm, newdata = testData, type = "response")

survival <- NA
for(i in 1:length(p.hats)) {
  if(p.hats[i] > .5) {
    survival[i] <- 1
  } else {
    survival[i] <- 0
  }
}

sum(survival)
"131 people are saved out of 418"
