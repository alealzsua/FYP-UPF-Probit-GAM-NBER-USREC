# Predicting the NBER recession Probability Index
This markdown describes the R code employed for my FYP. It is far from perfect, so if you have any suggestion, feel free to let me know.

First, we call the libraries and load the data. I use the NBER recession probability index, Shadow rate, ACM Dataset, EBP Dataset. The first can be download from Fred, the other data series can be found in their author's webpages. 

```markdown
`setwd("C:/Users/Hewlett Packard/Desktop/TFG")
source('utilities.R')
library(plotly)
library(InformationValue)
library(bbplot)
library(rvest) 
library(dplyr)
library('quantmod')
library(sparklyr) 
library(mongolite) 
library(nodbi)
library(dbplot)
library(odbc)
library(RSQLite)
library(rjson)
library(tidyverse)
library(corrplot)
library(rgl)
library(readxl) 
library(ggplot2)
library(rms)
library(pdftools)
library(dbplyr)
library(sandwich)
library(lmtest)
library(xts)
library(plotly)
library(DataCombine)
library(tseries)


mydata <- read.csv("C:/Users/Hewlett Packard/Desktop/TFG/FRBNY-ACM_D.csv", header=TRUE)
USREC <- read.csv("C:/Users/Hewlett Packard/Desktop/TFG/USREC.csv")
EBPGZCS <- read.csv("C:/Users/Hewlett Packard/Desktop/TFG/EBPGZCS.csv")
shadowrate <- read_excel("C:/Users/Hewlett Packard/Desktop/TFG/shadowrate.xls", 
                         col_types = c("text", "numeric"))


dates <- seq(as.Date('1990-01-01'), as.Date('2015-11-01'), by='months')

dates

DATA <- data.frame(dates, USREC[1622:1932,2], mydata[344:654,2:31], 
                   shadowrate[360:670,2], EBPGZCS[205:515,2:4]) 

ShadowSpread <- DATA$ACMY10 - DATA$X3.9900000000000002
Rspread  <- DATA$ACMRNY10 - DATA$X3.9900000000000002
TPspread <- DATA$ACMTP10 - DATA$ACMTP01
MYSPREAD <- DATA$ACMRNY10 - DATA$ACMTP10

plot(dates,MYSPREAD)

DATA <- data.frame(dates, USREC[1622:1932,2], mydata[344:654,2:31], 
                   shadowrate[360:670,2], EBPGZCS[205:515,2:4],
                   ShadowSpread, Rspread, TPspread,MYSPREAD) 

plot_ly(DATA,x=~dates, y=~MYSPREAD)

View(DATA)`
```
Once we have our dataset we compute lagged variables for our different independent variables. We will need them in order to build our models. Here I also plot a dynamic multiplier in order to see the most significant lags. We may want to use them in our probits.


```markdown

`DATA <- slide(DATA, Var = "ShadowSpread", slideBy = -12)
DATA <- slide(DATA, Var = "ShadowSpread", slideBy = -14)
DATA <- slide(DATA, Var = "ShadowSpread", slideBy = -16)
DATA <- slide(DATA, Var = "ShadowSpread", slideBy = -24)

DATA <- slide(DATA, Var = "Rspread", slideBy = -14)
DATA <- slide(DATA, Var = "Rspread", slideBy = -24)
DATA <- slide(DATA, Var = "Rspread", slideBy = -18)
DATA <- slide(DATA, Var = "Rspread", slideBy = -10)

DATA <- slide(DATA, Var = "ebp", slideBy = -1)
DATA <- slide(DATA, Var = "ebp", slideBy = -2)
DATA <- slide(DATA, Var = "ebp", slideBy = -3)
DATA <- slide(DATA, Var = "ebp", slideBy = -6)
DATA <- slide(DATA, Var = "ebp", slideBy = -8)
DATA <- slide(DATA, Var = "ebp", slideBy = -12)


DATA <- slide(DATA, Var = "MYSPREAD", slideBy = -8)
DATA <- slide(DATA, Var = "MYSPREAD", slideBy = -12)
DATA <- slide(DATA, Var = "MYSPREAD", slideBy = -18)
DATA <- slide(DATA, Var = "MYSPREAD", slideBy = -20)
DATA <- slide(DATA, Var = "MYSPREAD", slideBy = -24)
DATA <- slide(DATA, Var = "MYSPREAD", slideBy = -28)
DATA <- slide(DATA, Var = "MYSPREAD", slideBy = -20)

#Plot shadow spread, Term premiums spreads and risk free rate spreads
ShadowSpread <- DATA$ACMY10 - DATA$X3.9900000000000002
plot_ly(DATA, x=~dates, y=~ShadowSpread, name='Shadow Rate Spread' ,type = 'scatter', mode = 'lines')%>%
  add_trace(y=~DATA$`Rspread`, name='Risk Free R Spread')%>%
  add_trace(y=~DATA$TPspread, name='Term Premium Spread')



#Dynamic multiplier
h    <- 50
mf   <- rep(0,h)
mf.c <- matrix(0,h,2)
T <- length(DATA$USREC.1622.1932..2.)

for( h in 1:h ){
  data.h  <- data.frame( y=DATA$USREC.1622.1932..2.[(1+h):T] , TermSpread=DATA$ShadowSpread[1:(T-h)])
  
  pred.reg.h <- lm(y ~ `TermSpread`, family=binomial(link=probit), data=data.h) 
  
  vc.nw.h <- NeweyWest(pred.reg.h)
  
  ct.h <- coeftest(pred.reg.h,vc.nw.h)
  
  mf[h]    <- ct.h['TermSpread','Estimate']
  mf.c[h,] <- mf[h] + ct.h['TermSpread','Std. Error']*c(-1,1)*qnorm(0.05)
}


par(mar=c(2,2,0.1,0.1))
plot(mf,t='b', lwd=3,col='darkred',ylim=c(-0.25,0.2),xaxs='i',tck=0.02,pch=19)
lines(mf.c[,1],lwd=1,col='darkred',pch=25,t='b')
lines(mf.c[,2],lwd=1,col='darkred',pch=24,t='b')
grid()
box()
abline(h=0,lwd=2)

plot_ly(DATA, x=~dates, y=~ShadowSpread, name='Shadow Rate Spread' ,type = 'scatter', mode = 'lines')%>%
  add_trace(y=~DATA$`Rspread`, name='Risk Free R Spread')%>%
  add_trace(y=~DATA$TPspread, name='Term Premium Spread') `
```

Know we take a look at different in sample specifications of our probit models. We also compute Newey West standard errors and plot all the fitted in sample predicted probabilities of recession.

```Markdown
`probit <- glm(DATA$USREC.1622.1932..2. ~ `ShadowSpread-14`,family=binomial(link="probit"), data=DATA)
coef(probit)        
summary(probit)

probit2 <- glm(DATA$USREC.1622.1932..2. ~ DATA$`ebp-6`,family=binomial(link="probit"), data=DATA)
coef(probit2)        
summary(probit2)

probit3 <- glm(DATA$USREC.1622.1932..2. ~ `ShadowSpread-14` + DATA$`ebp-6`,family=binomial(link="probit"), data=DATA)
coef(probit3)        
summary(probit3)


vc.nw.is1 <- NeweyWest(probit)
coeftest(probit, vc.nw.is1)
vc.nw.is2 <- NeweyWest(probit2)
coeftest(probit2, vc.nw.is2)
vc.nw.is3 <- NeweyWest(probit3)
coeftest(probit3, vc.nw.is3)
Box.test(a, lag=12)

pred1 <- predict(probit, DATA, type="response") 
pred2 <- predict(probit2, DATA, type="response") 
pred3 <- predict(probit3, DATA, type="response") 
pred4 <- predict(probit4, DATA, type="response") 
pred5 <- predict(probit5, DATA, type="response") 
pred6 <- predict(probit6, DATA, type="response") 

plot_ly(DATA,y=~pred1, x=~`ShadowSpread-14`, size=DATA$USREC.1622.1932..2., color= DATA$USREC.1418.1932..2.) 
plot_ly(DATA,y=~pred2, x=~`ebp-6`, size=DATA$USREC.1622.1932..2., color= DATA$USREC.1418.1932..2.) 
plot_ly(DATA,y=~pred3, x=~`ebp-6`, size= DATA$USREC.1622.1932..2., color= DATA$USREC.1418.1932..2.) 
plot_ly(DATA,y=~pred3, x=~`Rspread-14`, size= DATA$USREC.1622.1932..2.,color= DATA$USREC.1418.1932..2.) 

plot_ly(DATA, y=~DATA$USREC.1622.1932..2. , x=~`ShadowSpread-14`, color = ~pred1)
plot_ly(DATA, y=~DATA$USREC.1622.1932..2. , x=~`ebp-6`, color = ~pred2)
plot_ly(DATA, y=~DATA$USREC.1622.1932..2. , x=~`ShadowSpread-14`, color = ~pred3)
plot_ly(DATA, y=~DATA$USREC.1622.1932..2. , x=~`ebp-6`, color = ~pred3)


plot_ly(DATA, y=~pred1,x=~dates,name = 'Shadow Spread', type = 'scatter', mode = 'lines') %>%
  add_trace(y=~pred2,name = 'ebp')%>%
  add_trace(y=~pred3, name = 'model 3')%>%
  layout(title = "In Sample predicted probabilities",
         xaxis = list(title = "Dates"),
         yaxis = list (title = "US NBER Recession"))`
```
Now I run the models out of sample. Compute the estimated probabilities of recession and compare the results with the realized values. I compute R-squared and AUROC. I also plot Cummulative Sum of errors of every model and the receiver operating characteristic.

```Markdown
##OUTOF SAMPLE
`T <- length(DATA$dates)


is <- as.Date(DATA$dates) < as.Date('2006-01-01') 
os <- as.Date(DATA$dates) >= as.Date('2006-01-01') 

data.is <- DATA[is,]
data.os <- DATA[os,]

attach(data.is)

dates.is <- (as.Date(DATA$dates))[is]
dates.os <- (as.Date(DATA$dates))[os]


y.is     <- DATA[is,'USREC.1622.1932..2.']
y.os     <- DATA[os,'USREC.1622.1932..2.']


attach(data.is)

pred.reg.is1 <- glm(USREC.1622.1932..2. ~ 
                     `ShadowSpread-14`
                   ,family=binomial(link="probit"), data= data.is)

vc.nw.is1 <- NeweyWest(pred.reg.is1)
coeftest(pred.reg.is1, vc.nw.is1)
summary(pred.reg.is1)

pred.reg.is2 <- glm(USREC.1622.1932..2. ~  `ebp-6`
                    ,family=binomial(link="probit"), data= data.is)

vc.nw.is2 <- NeweyWest(pred.reg.is2)
coeftest(pred.reg.is2, vc.nw.is2)
summary(pred.reg.is2)

pred.reg.is3 <- glm(USREC.1622.1932..2. ~  `ebp-1` +
                      `ShadowSpread-14`
                    ,family=binomial(link="probit"), data= data.is)

vc.nw.is3 <- NeweyWest(pred.reg.is3)
coeftest(pred.reg.is3, vc.nw.is3)
summary(pred.reg.is3)


y.hat1  <- predict( pred.reg.is1,type='response',newdata= data.os) 
y.hat2  <- predict( pred.reg.is2,type='response',newdata= data.os) 
y.hat3  <- predict( pred.reg.is3,type='response',newdata= data.os) 

y.bench  <- rep( mean(y.is) , sum(os) )

e.0 <- y.os - y.bench
e.1 <- y.os - y.hat1 
e.2 <- y.os - y.hat2
e.3 <- y.os - y.hat3


mse.0 <- mean( e.0**2)
mse.1 <- mean( e.1**2)
mse.2 <- mean( e.2**2)
mse.3 <- mean( e.3**2)


R2.0  <- 1 - mse.0/var(y.os)
R2.1  <- 1 - mse.1/var(y.os)
R2.2  <- 1 - mse.2/var(y.os)
R2.3  <- 1 - mse.3/var(y.os)


# 

plot( dates.os, y.os, col='blue' , ylim = c(-2,2))
lines( dates.os , y.bench , lwd=2 , col='black')
lines( dates.os , y.hat1   , lwd=2 , col='red')
lines( dates.os , y.hat2   , lwd=2 , col='darkorange')
lines( dates.os , y.hat2   , lwd=2 , col='darkblue')

W <- data.frame(dates.os, y.bench, y.hat1,y.hat2,y.hat3, y.os,
                cumsum( e.0**2 )/sum( e.0**2)*100, cumsum( e.1**2 )/sum( e.0**2 ) * 100,
                cumsum( e.2**2 )/sum( e.0**2 ) * 100,
                cumsum( e.3**2 )/sum( e.0**2 ) * 100) 

plot_ly(W,y=~y.os, x=~dates.os , name = 'Out of Sample' ,  type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~y.bench, name = 'Benchmark') %>%
  add_trace(y = ~y.hat1, name = 'Shadow Spread') %>%
  add_trace(y = ~y.hat2, name = 'EBP') %>%
  add_trace(y = ~y.hat3, name = 'Model3') %>%
  layout(title = "Out of Sample predicted values ",
         xaxis = list(title = "Dates"),
         yaxis = list (title = "US NBER REC = 1"))


plot_ly(W,y=~cumsum.e.0.2..sum.e.0.2....100, x=~dates.os ,line = list(shape = "spline"), name = 'Out of Sample' ,  type = 'scatter', mode = 'lines') %>%
  add_trace(y=~cumsum.e.1.2..sum.e.0.2....100,line = list(shape = "spline"), name = 'Shadow Spread') %>%
  add_trace(y=~cumsum.e.2.2..sum.e.0.2....100,line = list(shape = "spline"), name = 'EBP') %>%
  add_trace(y=~cumsum.e.3.2..sum.e.0.2....100,line = list(shape = "spline"), name = 'Model 3') %>%
  layout(title = "Cummulative Sum of Errors",
         xaxis = list(title = "Dates"),
         yaxis = list (title = "Sum of Errors"))



#

plot( dates.os , cumsum( e.0**2 )/sum( e.0**2)*100 , lwd=2 , col='blue', ylim=c(0,130)) + 
  lines( dates.os , cumsum( e.1**2 )/sum( e.0**2 ) * 100 , lwd=2 , col='red') +
  lines( dates.os , cumsum( e.2**2 )/sum( e.0**2 ) * 100 , lwd=2 , col='darkorange')
  lines( dates.os , cumsum( e.3**2 )/sum( e.0**2 ) * 100 , lwd=2 , col='darkorange')


optCutOff <- optimalCutoff(data.os$`ShadowSpread-14`, pred.reg.is1$fitted.values)[1] 

predicted <- predict(probit, data.os, type="response")
  
library(ROCR)
library(pROC)
roc1 <- roc(data.os$USREC.1622.1932..2., y.hat1, plot=TRUE)
roc2 <- roc(data.os$USREC.1622.1932..2., y.hat2, plot=TRUE)
roc3 <- roc(data.os$USREC.1622.1932..2., y.hat3, plot=TRUE)
auc(roc1)
auc(roc2)
auc(roc3)

?roc()

roc_df1 <- data.frame(
  TPR=rev(roc1$sensitivities), 
  FPR=rev(1-roc1$specificities))

roc_df2 <- data.frame(
  TPR2=rev(roc2$sensitivities), 
  FPR2=rev(1-roc2$specificities))

roc_df3 <- data.frame(
  TPR3=rev(roc3$sensitivities), 
  FPR3=rev(1-roc3$specificities))


plot_ly(roc_df1, y=~roc_df1$TPR, x=~roc_df1$FPR, type = 'scatter', mode = 'lines' )%>%
  layout(title = "Receiver Operating Characteristic",
         xaxis = list(title = "FPR"),
         yaxis = list (title = "TPR"))

plot_ly(roc_df2, y=~roc_df2$TPR, x=~roc_df2$FPR, type = 'scatter', mode = 'lines' )%>%
  layout(title = "Receiver Operating Characteristic",
         xaxis = list(title = "FPR"),
         yaxis = list (title = "TPR"))

plot_ly(roc_df3, y=~roc_df3$TPR, x=~roc_df3$FPR, type = 'scatter', mode = 'lines' )%>%
  layout(title = "Receiver Operating Characteristic",
         xaxis = list(title = "FPR"),
         yaxis = list (title = "TPR"))`
```

We move into GAM models. I haven't plot the results for before and after the Great Moderation because I used to different R files and it could be confusing to put it all toghether because of the way I call the variables. But you can just look at the whole series or just at data after the 1990s, compare results and see how it changes. I let you do it as an exercise.

```Markdown
`
library(mgcv)
library(mgcViz)

gamobj1<- gam(DATA$USREC.1622.1932..2. ~ 
              s(DATA$`ShadowSpread-14`),
            data=DATA,method='REML', family=binomial(link=probit)) 
summary(gamobj1)
plot(gamobj1, all.terms = TRUE)
plot.gamViz(gamobj1, DATA$`ShadowSpread-14`,l_gridCheck1D(mean))
check1D(gamobj1, 'DATA$`ShadowSpread-14`')+l_gridCheck1D(mean)
check(gamobj1)

gamobj2<- gam(DATA$USREC.1622.1932..2. ~ 
                s(DATA$`ebp-6`),
              data=DATA,method='REML', family=binomial(link=probit)) 
summary(gamobj2)


gamobj3 <- gamV(DATA$USREC.1622.1932..2. ~ s(DATA$`ShadowSpread-14`) + s(DATA$`ebp-6`), data=DATA, method='REML', family=binomial(link=probit), aViz=list(nsim=50))
summary(gamobj3)
print(plot(gamobjV, allTerms = TRUE), pages =1)
plot.gamViz(gamobjV, DATA$`ShadowSpread-14`,l_gridCheck1D(mean))

check1D(gamobjV, 'DATA$`ShadowSpread-14`')+l_gridCheck1D(mean)
check(gamobjV)


A <- data.frame(DATA[15:311,],gamobj1$fitted.values[1:297], pred1[15:311],
                pred2[15:311],pred3[15:311], gamobj2$fitted.values[9:305],
                gamobj3$fitted.values[1:297])

dim(DATA)
length(pred1)
length(pred2)
length(pred3)
length(gamobj1$fitted.values)
length(gamobj2$fitted.values)
length(gamobj3$fitted.values)


plot_ly(A,x=~A$dates,y=~A$gamobj1.fitted.values.1.297.,name='GAM Model 3',
        type = 'scatter', mode = 'lines')%>%
  layout(title = "In Sample predicted probabilities",
         xaxis = list(title = "Dates"),
         yaxis = list (title = "US NBER Recession"))%>%
  add_trace(y=~A$pred1.15.311., name='Shadow Spread')%>%
  add_trace(y=~A$gamobj3.fitted.values.1.297., name='GAM Shadow Spread')%>%
  add_trace(y=~A$pred3.15.311., name='Model 3')`
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/alealzsua/FYP-UPF-Probit-GAM-NBER-USREC/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
