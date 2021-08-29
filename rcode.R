#read data
dat=read.csv(file.choose()) #datv2
#compute log of sales
logSales=log(dat$Sales)
data=data.frame(dat, logSales)
names(data)
#removing unused variables
data=data[, !(names(data) %in% c("Console", "Title", "Publisher", "Genre", "Usedprice", "lnUsedPrice", "Handheld", "GBA", "GCN", "Infograme", "Sales", "Year.Released"))]
data2=data[, !(names(data) %in% c("Usedprice", "lnUsedPrice", "Handheld", "Y04",
                                  "Y05", "Y06", "Y07", "Y08", "Y09", "Y10", "GCN",
                                  "GBA", "Infograme"))]
#get variable names
names(data)

#RatingE, Xbox, Firstperson, Action, Infograme/Others as default
fullmodel=lm(data=data, logSales~Year.Released+Sequel+Re.release+Review+
                      RatingT+RatingM+MaxPlayers+Online+Licensed+Accessory+
                      Multiplatform+NDS+Wii+PS3+PSP+
                      Adventure+Educational+Racing+RPG+Simulation+Sports+Strategy+
                      Platform+Isometric+SideScrolling+TopDown+ThirdPerson+
                      X2K+Activision+Atari+Capcom+Disney+Eidos+EA+Konami+Microsoft+Midway+
                      Namco+Nintendo+Rockstar+Sony+Sega+THQ+SquareEnix+Ubisoft
                      )

summary(fullmodel)
plot(predict(fullmodel), resid(fullmodel), xlab = "Y-hat", ylab="Residual", main = "Residual Plot of Original Full Model")

#year as binary
fullmodel2=lm(data=data, logSales~Y05+Y06+Y07+Y08+Y09+Y10+Sequel+Re.release+Review+
                RatingT+RatingM+MaxPlayers+Online+Licensed+Accessory+
                Multiplatform+NDS+Wii+PS3+PSP+
                Adventure+Educational+Racing+RPG+Simulation+Sports+Strategy+
                Platform+Isometric+SideScrolling+TopDown+ThirdPerson+
                X2K+Activision+Atari+Capcom+Disney+Eidos+EA+Konami+Microsoft+Midway+
                Namco+Nintendo+Rockstar+Sony+Sega+THQ+SquareEnix+Ubisoft
)
summary(fullmodel2)
plot(predict(fullmodel2), resid(fullmodel2), xlab = "Y-hat", ylab="Residual", main = "Residual Plot of Full Model with Year as Binary Variables")
#residual plots

#VIF-adjusted model
novifmodel=lm(data=data, logSales~Y10+Sequel+Re.release+Review+
                RatingT+RatingM+MaxPlayers+Online+Licensed+Accessory+
                Multiplatform+NDS+Wii+PS3+PSP+
                Adventure+Educational+Racing+RPG+Simulation+Sports+Strategy+
                Platform+Isometric+SideScrolling+TopDown+ThirdPerson+
                X2K+Activision+Atari+Capcom+Disney+Eidos+EA+Konami+Microsoft+Midway+
                Namco+Nintendo+Rockstar+Sony+Sega+THQ+SquareEnix+Ubisoft
)
summary(novifmodel)
max(vif(novifmodel))

#selected model
selectmodel=lm(data=data, logSales~Y05+Y06+Y07+Y08+Y09+Y10+Sequel+Review+RatingM
              +Multiplatform+NDS+PS3+PSP+Adventure+Educational+Sports+Strategy+Platform
              +TopDown+ThirdPerson+Activision+Capcom+Disney+Eidos+EA+Microsoft+Nintendo+Rockstar
              +Sony+Sega+THQ)

summary(selectmodel)
#stepwise
library(MASS)
nullmodel=lm(logSales~1, data=data)
modelscope=list(lower=nullmodel, upper=fullmodel2)
back=stepAIC(fullmodel2, direction='backward', scope=modelscope)
back$coefficients

forward=stepAIC(nullmodel, direction='forward', scope=modelscope)
forward$coefficients

#random forest
set.seed(3132)
library(randomForest)
library(rsample)
library(ranger)
library(caret)
library(h2o)
rfmethod=randomForest(formula=logSales~., data=data)
rfmethod
plot(rfmethod)
which.min(rfmethod$mse)

#validation for rf
data_split=initial_split(data, prop=.7)
data_train=analysis(data_split)
data_test=assessment(data_split)
x_test=data_test[setdiff(names(data_test), "logSales")]
y_test=data_test$logSales
rfvalid=randomForest(formula=logSales~., data=data_train, xtest=x_test, ytest=y_test, ntree=300, importance=TRUE)
rfvalid
which.min(rfvalid$mse)
which.min(rfvalid$test$mse)
plot(rfvalid)
oob=sqrt(rfvalid$mse)
validation=sqrt(rfvalid$test$mse)
sqrt(rfvalid$mse[268])
sqrt(rfvalid$test$mse[268])
#optimal tree 298 (original, 218 at validation) RMSE .92 @original, .878@validation
rfvalid$test$rsq[298]

rftest=randomForest(formula=logSales~., data=data_train)


varImpPlot(rfvalid, type=2, n.var = 10, main="Random Forest Model Variance Importance")

#cross-validation
library(DAAG)
cvback=cv.lm(data, back, m=5)
#overall mse=.912
sqrt(.912)
#rmse=.955
cvforward=cv.lm(data, forward, m=5)

cvfull=cv.lm(data, fullmodel2, m=5)
sqrt(.93)
#rmse=.964

cvspecifed=cv.lm(data, selectmodel, m=5)
sqrt(.918)
#rmse=.958

cvnovif=cv.lm(data,novifmodel, m=5)
sqrt(.926)
#rmse=.962 

library(randomGLM)

#getting table in Latex
library(stargazer)
stargazer(fullmodel2, selectmodel, forward, back, title="Linear Regression Models", align=TRUE)

#getting basic number table
roundno=formatC(novifmodel$coef, digits=3, format="f")
roundno
write.table(roundno, file="testing.txt", sep=",", quote=FALSE, row.names=T)


shapiro.test(dat$Sales)
qqnorm(data$logSales, main="Normal Q-Q Plot of Transformed Sales")
qqline(data$logSales, col="red", lwd=2)
text(x=2, y=-4, labels="Wilk's Lambd =1, p<.001")
