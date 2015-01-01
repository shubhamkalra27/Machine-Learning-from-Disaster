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

