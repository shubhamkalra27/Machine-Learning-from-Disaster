data.dir   <- 'C:/Users/shubham/SkyDrive/DataProjects/Titanic/data/'
train.file <- paste0(data.dir, 'train.csv')
test.file  <- paste0(data.dir, 'test.csv')



d.train <- read.csv(train.file, stringsAsFactors=F)


d.test  <- read.csv(test.file, stringsAsFactors=F)