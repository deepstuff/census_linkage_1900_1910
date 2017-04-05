library(randomForest)
library(dplyr)
library(mltools)
dir <- "R:/JoePriceResearch/record_linking/projects/deep_learning/training_sets"
setwd(dir)
df <- read.csv("ny_numeric.csv")
df <- df[complete.cases(df),]


#now lets mix up the test and training sets
rfSample <- function(ratio, trees, threshold = .5) {
N <- length(df$match)
training_size <- floor(ratio*N)
sample <- sample(1:N, N)
train <- df[sample[1:training_size],]
test <- df[sample[(training_size + 1):N],]
  
#rf
rf <- randomForest(match ~ racematch + yeardif + year + 
                      given_match + surname_match + 
                     statematch + countymatch + relationshipmatch + 
                     female + father_birth_match + mother_birth_match + marital_status_match, 
                   data = train, ntree=trees, type="prob", importance=TRUE)

test$rf_pred <- predict(rf, newdata = test)

#accuracy 
test$rf_match <- test$rf_pred >= i
accuracy <- sum(test$rf_match == test$match)/length(test$match)
return(list(test, rf, accuracy))
}

#grab sample testing set and model
sample <- rfSample(.8, 50,threshold=.5)
test <- sample[[1]]
model <- sample[[2]]
accuracy <- sample[[3]]
importance <- as.data.frame(importance(model))
importance <- importance[-2]
importance$`%IncMSE` <- round(importance$`%IncMSE`,3)
importance$features <- rownames(importance)

#make those 2x2 matricies
matrix_data <- c(nrow(filter(test,match & rf_match)),
                 nrow(filter(test,match & !rf_match)),
                 nrow(filter(test,!match & rf_match)),
                 nrow(filter(test,!match & !rf_match)))
evaluation_matrix <- as.data.frame(matrix(matrix_data,ncol=2))
names(evaluation_matrix) <- c("Actual Match", "Actual Non-match")
rownames(evaluation_matrix) <- c("Predicted Match", "Predicted Non-match")
false_neg <- nrow(filter(test,match & !rf_match))/nrow(test)
false_pos <- nrow(filter(test,!match & rf_match))/nrow(test)

evaluation_matrix
accuracy
false_neg
false_pos



ggplot(data=thresh_test,aes(x=threshold,y=accuracy)) + 
  #geom_smooth(se=FALSE,color="black") + 
  geom_point() + 
  theme_tufte()
