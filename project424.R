library(leaps)

auto <- read.csv('auto.dat')

auto <- auto[complete.cases(auto),]

auto$logDisp <- log(auto$Displacement)
auto$logHorse <- log(auto$Horsepower)
auto$logWeight <- log(auto$Weight)

plot(auto[,-c(8)])

set.seed(0)
train <- sample(1:398, 250)

x.train <- auto[train,-c(1,8)]
y.train <- auto[train, c(1)]

x.test <- auto[-train, -c(1,8)]
y.test <- auto[-train, c(1)]

null <- lm(y.train~1, data=x.train)
full <- lm(y.train~., data=x.train)

step(null, scope=list(lower=null, upper=full), direction='both')

besttrain <- regsubsets(y.train~., data=x.train)
besttrain.sum <- summary(besttrain)
besttrain.sum$which
cbind(besttrain.sum$rsq, besttrain.sum$adjr2, besttrain.sum$cp)