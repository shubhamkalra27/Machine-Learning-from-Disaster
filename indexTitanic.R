data.dir   <- 'D:/github/Machine-Learning-from-Disaster/data/'(
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
  
  