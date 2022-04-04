setwd("C:/Users/mitch/OneDrive/Desktop/School/UBC Okanagan/Year 3/DATA 410/project")
sales <- read.csv("Video_Games_Sales_as_at_22_Dec_2016_edited.csv", header = TRUE)
publishers <- read.csv("publishers.csv",header = T)

index <- 1
for(i in 1:nrow(sales)){
  if (publishers$Publisher[index] != sales$Publisher[i]){
    index <- index + 1
  }
  sales$Publisher[i] = publishers$EU[index]
}

# forward selection
sales1 <- cbind("Platform"=factor(sales$Platform),"Genre"=factor(sales$Genre),"Publisher"=sales$Publisher,"Critic_Score"=sales$Critic_Score,"User_Score"=sales$User_Score,"Rating"=factor(sales$Rating),"EU_Sales"=sales$EU_Sales)
sales1 <- as.data.frame(sales1)
sales1 <- sales1[sales1$EU_Sales < 0.5,]
model.full <- lm(sales1$EU_Sales~sales1$Platform+sales1$Publisher+sales1$Genre+sales1$Critic_Score+sales1$User_Score+sales1$Rating,data=sales1)
y <- sales1$EU_Sales
X <- data.matrix(sales1[,-7])
model <- lm(y~X)
model.empty <- lm(y~1)
model.fstep.aic <- step(model.empty,direction ="forward",
                       scope=list(lower=model.empty,upper=model))
summary(model.fstep.aic)
plot(model.fstep.aic)

# backward selection
model.bstep.aic <- step(model)
summary(model.bstep.aic)
plot(model.bstep.aic)

# best subset
library(bestglm)
model.best.bic <- bestglm(sales1,IC="BIC")
model.best.bic$BestModels
model.best.bic$BestModel
plot(model.best.bic$BestModel)

# Do games made domestically on average sell better than they do in the rest of the world?
sumDomestic <- 0
numDomestic <- 0
sumForeign <- 0
numForeign <- 0
for(j in 1:6902){
  if (is.na(sales$Publisher)){
    next
  } else if (sales$Publisher[j] == 1){
    sumDomestic <- sumDomestic + sales$EU_Sales[j]
    numDomestic <- numDomestic + 1
  } else{
    sumForeign <- sumForeign + sales$EU_Sales[j]
    numForeign <- numForeign + 1
  }
}
barplot(cbind(sumDomestic/numDomestic,sumForeign/numForeign),names.arg=c("Domestic Publisher","Foreign Publisher"))
