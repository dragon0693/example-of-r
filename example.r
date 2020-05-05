# Identificar las variables que puedes excluir

library(glmnet)
library(ISLR)

#leemos los datos 
datos<-Hitters

str(datos)
summary(datos)

datos<-na.omit(datos)

nrow(datos)

max1 <- quantile(datos$CAtBat,.75)+1.5*(IQR(datos$CAtBat))
datos1<- datos[datos$CAtBat<max1,]

max2<-quantile(datos$CHits,.75)+1.5*(IQR(datos$CHits))
datos2<-datos1[datos1$CHits<max2,]

nrow(datos2)/nrow(datos)

x<-model.matrix(Salary~.,data=datos2)[,-1]
y<-as.matrix(datos2$Salary)
set.seed(100)
a<-sample(nrow(x),nrow(x)*0.7)
training_x<-x[a,]
testing_x<-x[-a,]

training_y<-y[a,]
testing_y<-y[-a,]

modelo_lasso<-cv.glmnet(training_x,training_y, alpha=1) #alpha = 0 para ridge, alpha=1 lasso
modelo_lasso
# Los df son los betas
str(modelo_lasso)

lambda_out<-modelo_lasso$lambda.min

plot(modelo_lasso)

#Prediciendo
prediccion_lasso<-predict(modelo_lasso,s=lambda_out, newx=testing_x)

#Error cuadratico medio mean((y-yest)^2)
ECM<-mean((testing_y-prediccion_lasso)^2)

modelo_lasso2<-glmnet(x,y,alpha = 1)
lasso.coef<-predict(modelo_lasso2,type="coefficients",s=lambda_out)[1:20,]
lasso.coef

#calcular coeficientes diferentes de cero
sum(lasso.coef!=0)