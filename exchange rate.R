df=read.csv("C:\\D\\---Education---\\Data Scientist\\datasets\\usd_inr_exchange_rate.csv")
library(ggplot2)
colnames(df)=c("year","usd","inr")
str(df)

#ploting the replationship to see whether replationship is simple or polinomial
ggplot()+
  geom_point(aes(x=df$year,y=df$inr),color="green")+
  geom_line(aes(x=df$year,y=df$inr),color="black")+
  ggtitle("EXchange Rates Past 50 year")+
  xlab("Year")+
  ylab("INR")+
  xlim(1947,2020)
df=df[,-2]

#splitting the dataset for simple regression
library(caTools)
split=sample.split(df$inr,SplitRatio = 0.8)

train=subset(df,split==T)
test=subset(df,split==F)

#train[,1]=scale(train[,1])
#test[,1]=scale(test[,1])



#simple linear regression
lreg=lm(formula = inr~.,data=train)
summary(lreg)

#predicted values of simple linear regression
y_pred=predict(lreg,newdata=test)

#plottin to see the fit
ggplot()+
  geom_point(aes(x=train$year,y=train$inr),color="green")+
  geom_line(aes(x=train$year,y=predict(lreg,newdata=train)))+
  xlim(1947,2019)


#plynomial linear regression
df2=df
df2$year2=df$year^2  #raising the power of the indep var for polinomial reg
df2$year3=df$year^3
y=df[,2]
df2$year4=df2$year^4
df2=df2[,-2]

#df2[,1:4]=scale(df2[,1:4])
df2$inr=y

#splitting train and test data
train_df2=subset(df2,split==T)
test_df2=subset(df2,split==F)

#fitting for polinomial reg
lreg2=lm(formula = inr~.,data=train_df2)
y_pred2=predict(lreg2,newdata=test_df2)


#seeing the fit of the poli reg
ggplot()+
  geom_point(aes(x=df$year,y=df$inr),color="green")+
  geom_line(aes(x=df$year,y=predict(lreg2,newdata=df2)))+
  xlim(1947,2019)

#predicting for year 2025
pre_2025=predict(lreg2,newdata=data.frame(year=2025,year2=2025^2,year3=2025^3,year4=2025^4))

#function for predicting
pred_year=function(x){
  pred_yr=predict(lreg2,newdata=data.frame(year=x,year2=x^2,year3=x^3,year4=x^4))
  print(pred_yr)
}

pred_year(2021)
