
##Download data
setwd('C://Users//silvia.passeri//Desktop//Joburg//Mozambique//R1 - May data') 
Mozdata<-read.csv("Final Mozambique_May 2017 - TeleDirect_HDDS_Analysis.csv", header =TRUE)

str(Mozdata)
attach(Mozdata)

#Install packages
install.packages("lme4")
install.packages("Matrix")
install.packages("Rcpp")
install.packages("sjPlot")
install.packages("arm")
install.packages("MASS")
library(Matrix)
library(Rcpp)
library(lme4)
library(sjPlot)
library(arm)
library(MASS)


#Density plot HDDS for the two modes of thedata collection
ggplot(Mozdata, aes(x=HDDS, fill=Group))                              +
  geom_density(position='identity', alpha=0.5)                    +
  theme_bw()                                                      +
  scale_x_continuous(name='HDDS',
                     breaks=seq(0,15,10),limits=c(0,15))          +
  scale_y_continuous(name='Density')    +
  ggtitle("Density plot for HDDS data (May)")

#Boxplot
ggplot(Mozdata, aes(x=Group, y=HDDS))                              +
  geom_boxplot(fill='grey80', colour="blue")                       +
  theme_bw()                                                       +
  scale_x_discrete()    +
  ggtitle("Box plot for HDDS data (May)")

#Boxplot
ggplot(Mozdata, aes(x=OperatorID, y=HDDS, fill=Group))                             +
  geom_boxplot()                    +
  theme_bw()                                                      +
  scale_x_discrete()    +
  ggtitle("Box plot for HDDS by operator (May)")

#Analysis
#percentage of food items consumed by group
food.perc<- function(y){
  freq<-table(Group,y)
  f<-ftable(freq)
  freq.perc <-round(prop.table(f,1)*100,1)
  
  return(freq.perc)
}

food.perc(Cereals)
food.perc(Roots)
food.perc(Pulses)
food.perc(Veg)
food.perc(Fruits)
food.perc(Meat)
food.perc(Eggs)
food.perc(Fish)
food.perc(Dairy)
food.perc(Fats)
food.perc(Sugars)
food.perc(Spice)

#Test
test <- function(y){
  
  Table<-table(Group,y)
  test_prop<-prop.test(Table, correct=FALSE)
  
  return(test_prop)
}
test(Cereals)
test(Roots)
test(Pulses)
test(Veg)
test(Fruits)
test(Meat)
test(Eggs)
test(Fish)
test(Dairy)
test(Fats)
test(Sugars)
test(Spice)

#Mean
mean<-tapply(HDDS,Group, mean, na.rm=T)
mean.Prov<-with(data, tapply(HDDS,list(Group, Province), mean, na.rm=T))

cum<-ecdf(HDDS)
plot(cum, lty=2)

#regression: simple regression
mod0.0<-lm(HDDS~Group+Province, data=Mozdata)
summary(mod0.0)

mod0.1<-lm(HDDS~Group+Province, data=Mozdata)
summary(mod0.1)

mod1<-lm(HDDS~Group, data=Mozdata)
anova(mod1)
confint(mod1)

mod1.1<-aov(HDDS~Group, data=Mozdata)
anova(mod1.1)
TukeyHSD(mod1.1)

mod1.2<-aov(HDDS~Group*Province, data=Mozdata)
anova(mod1.2)
TukeyHSD(mod1.2)

mod1.3<-aov(HDDS~Group+Province, data=Mozdata)
anova(mod1.3)
TukeyHSD(mod1.3)

HDDS.mod = data.frame(Fitted = fitted(mod1),
                       Residuals = resid(mod1), Treatment = Group)

ggplot(HDDS.mod, aes(Fitted, Residuals, colour = Treatment)) + geom_point()

#Random effect
mod.2<-lmer(HDDS~1 +(1|OperatorID))
summary(mod.2)
rsdDF <- as.data.frame(cbind(Group=as.factor(Mozdata$Group),Residual = resid(mod.2)))

ggplot(rsdDF, aes(x=Residual, fill=as.factor(Group)))                              +
  geom_density(position='identity', alpha=0.5)                    +
  theme_bw()                                                      +
  scale_x_continuous(name='HDDS',
                     breaks=seq(0,15,10),limits=c(0,15))          +
  scale_y_continuous(name='Density')    +
  ggtitle("Density plot for residuals (May)")

as.factor(Rsp.ID)
mod.2.1<-lmer(HDDS~Province +Group+Gender+(1|OperatorID))
summary(mod.2.1)

mod.2.3<-lmer(HDDS~Group-1+(1|OperatorID))
summary(mod.2.3)

#Diagnostic plots
#Residual plot  
plot(fitted(mod.2.2), residuals(mod.2.2)) 
abline(h=0)
plot(mod.2.2)

rsdHDDS<-resid(mod.2.2)
qqnorm(rsdHDDS)
qqline(rsdHDDS)


sjp.lmer(mod.2.2,
         facet.grid = FALSE)
        
sjp.lmer(mod.2.2,type="fe", title='rCSI')
sjp.lmer(mod.2.3)
sjp.lmer(mod.2.2,type="re.qq")
sjp.lmer(mod.2.2,type="re")
#sjp.lmer(mod.2.2, type='fe.cor')
  


#Export output tables from R to excel
#Try to think to create a matrix and then to export the output
#write.csv(list(cereals,roots,pulses,veg,fruit,), file='cereals.csv')
#?write.csv
