
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)

# Find the indexes for the tile piece of the name
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

# Engineered variable: Title 
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch 

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build a new tree with new features
fit <- rpart(Survived ~ Pclass + Sex + Age  + Fare + Embarked + Title + FamilySize,
             data=train, method = "class")

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "/Users/yeyuan/Desktop/titanic/submission.csv", row.names = FALSE)


