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

sales <- na.omit(sales)
sales <- sales[sales$EU_Sales>0,]
sales <- sales[sales$NA_Sales>0,]
sales$Platform <- factor(sales$Platform)
sales$Genre <- factor(sales$Genre)
sales$Rating <- factor(sales$Rating)
sales <- sales[-c(3503,3543,3813,4151),]
plot(model.full <- lm(log(sales$EU_Sales*1000000)~sales$Year_of_Release+sales$Platform+sales$Genre+sales$Publisher+sales$Critic_Score+sales$Critic_Count+sales$User_Score+sales$User_Count+sales$Rating),col=sales$Genre)
legend(x='right',legend=levels(factor(sales$Genre)),pch=16,col=unique(factor(sales$Genre)))
summary(model.full)

# forward selection
y <- log(sales[,6:10]*1000000)
X <- as.data.frame(cbind("Year"=sales$Year_of_Release,"Platform"=sales$Platform,"Genre"=sales$Genre,"Critic_Score"=sales$Critic_Score,"Critic_Count"=sales$Critic_Count,"User_Score"=sales$User_Score,"User_Count"=sales$User_Count,"Rating"=sales$Rating))
X$Platform <- factor(X$Platform)
X$Genre <- factor(X$Genre)
X$Rating <- factor(X$Rating)
y <- na.omit(y)
X <- na.omit(X)
model <- lm(y$EU_Sales~X$Year+X$Platform+X$Genre+X$Critic_Score+X$Critic_Count+X$User_Score+X$User_Count+X$Rating)
model.empty <- lm(y$EU_Sales~1)
model.fstep.aic <- step(model.empty,direction ="forward",scope=list(lower=model.empty,upper=model))
summary(model.fstep.aic)
model.fstep.bic <- step(model.empty,k=log(nrow(sales)),direction ="forward",scope=list(lower=model.empty,upper=model))
summary(model.fstep.bic)
plot(model.fstep.aic,col=X$Genre)

# backward selection
model.bstep.aic <- step(model)
summary(model.bstep.aic)
model.bstep.bic <- step(model,k=log(nrow(sales)))
plot(model.bstep.aic,col=X$Genre)

# best subset
library(bestglm)
model.best.aic <- bestglm(as.data.frame(cbind(X,y$EU_Sales)),IC="AIC")
model.best.aic$BestModels
model.best.aic$BestModel
summary(model.best.aic$BestModel)
model.best.bic <- bestglm(as.data.frame(cbind(X,y$EU_Sales)),IC="BIC")
model.best.bic$BestModels
model.best.bic$BestModel
summary(model.best.bic$BestModel)
plot(model.best.aic$BestModel)

# multicollinearity
cor(cbind(sales$Year_of_Release,sales$EU_Sales,sales$Critic_Score,sales$Critic_Count,sales$User_Score,sales$User_Count))

# Do games made domestically on average sell better than they do in the rest of the world?
sumDomestic <- 0
numDomestic <- 0
sumForeign <- 0
numForeign <- 0
for(j in 1:nrow(sales)){
  if (sales$Publisher[j] == 1){
    sumDomestic <- sumDomestic + sales$EU_Sales[j]
    numDomestic <- numDomestic + 1
  } else{
    sumForeign <- sumForeign + sales$EU_Sales[j]
    numForeign <- numForeign + 1
  }
}
barplot(cbind(sumDomestic/numDomestic,sumForeign/numForeign),names.arg=c("Domestic Publisher","Foreign Publisher"),ylab = "EU_Sales in Millions")
