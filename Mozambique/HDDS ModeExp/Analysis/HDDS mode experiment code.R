
########################################
############ Mozambique HDDS data ######
############  Mode Experiment     ######
########################################################################
######  Script created by Silvia Passeri on the 5th of Octobr 2017######
########################################################################

#################################################################
#          Downloading packages                                 #
#################################################################
#Install packages
install.packages("lme4")
install.packages("Matrix")
install.packages("Rcpp")
install.packages("sjPlot")
install.packages("arm")
install.packages("MASS")
install.packages("RODBC")
install.packages("magrittr")
install.packages("dplyr")
install.packages('ggplot2')
install.packages('MatchIt')
install.packages('gridExtra')
#SvyDesign
install.packages('survey')
library(survey)
library(RODBC)
library(plyr)
library(Matrix)
library(Rcpp)
library(lme4)
library(sjPlot)
library(arm)
library(MASS)
library(ggplot2)
library(magrittr)
library(dplyr)
library(MatchIt)
library(gridExtra)
#######Downloading the SETSAN data
setwd('C://Users//silvia.passeri//Desktop//Joburg//Mozambique//HDDS test//VAC')
VAC<-read.csv("Moz VAC Jul17_Clean_V2.csv", header =TRUE)

#Changing names of the variables
VAC$ADM1_NAME<-VAC$Adm1_Name

#######Downloading the clean mVAM data directly from the DB
channel <- odbcConnect("mvam", uid="mvamuser", pwd="P4r21$3L", believeNRows=FALSE)
mVAM<- sqlQuery(channel,
                'SELECT T.*,Strata.Population1 as Population FROM (SELECT * FROM Obs_Mozambique WHERE SvyID=2301) T 
                   JOIN Strata ON T.ADM0_NAME=Strata.ADM0_NAME AND T.ADM1_NAME=Strata.STR1_NAME')
#mVAM only RURAL
mVAMRural<- mVAM[which(mVAM$UrbanRural=='R'),]

#Remove ADM1 not in mVAM
SETSAN <- subset(VAC, Adm1_Name %in% unique(mVAM$ADM1_NAME))

#Renaming the SETAN variables 
SETSAN$ADM1_NAME<-SETSAN$Adm1_Name

#Creating the unique Id for the F2F assessment
SETSAN$ID<-c(1:dim(SETSAN)[1])

#Adding the Population estimate for the rural population only in the mVAM data and in the SETSAN
mVAMRural$PopRural<-0
mVAMRural[mVAMRural$ADM1_NAME=="Gaza", 'PopRural']<-1467952
mVAMRural[mVAMRural$ADM1_NAME=="Inhambane", 'PopRural']<-1168156

SETSAN$PopRural<-0
SETSAN[SETSAN$ADM1_NAME=="Gaza", 'PopRural']<-1467952
SETSAN[SETSAN$ADM1_NAME=="Inhambane", 'PopRural']<-1168156
SETSAN$PopTot<-0
SETSAN[SETSAN$ADM1_NAME=="Inhambane"|SETSAN$ADM1_NAME=="Gaza" , 'PopTot']<-2636108


########Preliminary Analysis###############
###########################################
#Computing HDDS for the F2F
SETSAN$HDDS.F2F<-SETSAN$HDDS_Cereals+SETSAN$HDDS_Roots+SETSAN$HDDS_Pulses+SETSAN$HDDS_Veg+SETSAN$HDDS_Fruits+SETSAN$HDDS_Meat+SETSAN$HDDS_Eggs+SETSAN$HDDS_Fish+SETSAN$HDDS_Dairy+SETSAN$HDDS_Sugar+SETSAN$HDDS_Fats+SETSAN$HDDS_Condiments
SETSAN$HDDS<-SETSAN$HDDS_Cereals+SETSAN$HDDS_Roots+SETSAN$HDDS_Pulses+SETSAN$HDDS_Veg+SETSAN$HDDS_Fruits+SETSAN$HDDS_Meat+SETSAN$HDDS_Eggs+SETSAN$HDDS_Fish+SETSAN$HDDS_Dairy+SETSAN$HDDS_Sugar+SETSAN$HDDS_Fats+SETSAN$HDDS_Condiments
#Mean HDDS
mean<-tapply(mVAMRural$HDDS,mVAMRural$Approach, mean, na.rm=T)
#Density plot HDDS for the two modes of thedata collection in Rural areas
ggplot(mVAMRural, aes(x=HDDS, fill=Approach))                              +
  geom_vline(xintercept=mean, color='black', linetype='dashed', size=0.5) +
  geom_density(position='identity', alpha=0.5)                    +
  theme_bw()                                                      +
  scale_x_continuous(name='HDDS',
                     breaks=seq(0,15,10),limits=c(0,15))          +
  scale_y_continuous(name='Density')    +
  ggtitle("Density plot for HDDS data (June/July 2017)")


#Boxplot in Rural areas
ggplot(mVAMRural, aes(x=Approach, y=HDDS))                              +
  geom_boxplot(fill='grey80', colour="blue")                       +
  theme_bw()                                                       +
  scale_x_discrete()    +
  ggtitle("Box plot for HDDS data (June/July 2017)")


#Boxplot
ggplot(mVAMRural, aes(x=OperatorID, y=HDDS, fill=Approach))                             +
  geom_boxplot()                    +
  theme_bw()                                                      +
  scale_x_discrete()    +
  ggtitle("Box plot for HDDS by operator Rural areas (June/July 2017)")

#Mean of the HDDS for Rural respondents
mean<-tapply(mVAMRural$HDDS,mVAMRural$Approach, mean, na.rm=T)
mean.Prov<-with(mVAMRural, tapply(HDDS,list(mVAMRural$Approach, mVAMRural$ADM1_NAME), mean, na.rm=T))

cum<-ecdf(mVAM$HDDS)
plot(cum, lty=2)

#ANOVa for HDDS
HDDS.ANOVA<-aov(HDDS~Approach, data=mVAMRural)
an<-anova(HDDS.ANOVA)
an1<-anova(HDDS.ANOVA)
tuk<-TukeyHSD(HDDS.ANOVA)


#T.test among the 2 approaches
ttest_HDDS<- function(data){
  ttestHDDS<-t.test(data$HDDS~data$Approach,conf.level = 0.95,na.rm=T)
  return(ttestHDDS)
}

ttest_HDDS(mVAM)
ttest_HDDS(mVAMRural)


attach(mVAM)
#Analysis
#percentage of food items consumed by group
food.perc<- function(y){
  freq<-table(Approach,y)
  f<-ftable(freq)
  freq.perc <-round(prop.table(f,1)*100,1)
  
  return(freq.perc)
}

food.perc(HDDS_Cereals)
food.perc(HDDS_Roots)
food.perc(HDDS_Pulses)
food.perc(HDDS_Veg)
food.perc(HDDS_Fruits)
food.perc(HDDS_Meat)
food.perc(HDDS_Eggs)
food.perc(HDDS_Fish)
food.perc(HDDS_Dairy)
food.perc(HDDS_Fats)
food.perc(HDDS_Sugars)
food.perc(HDDS_Condiments)

#Test
test <- function(y){
  
  Table<-table(Approach,y)
  test_prop<-prop.test(Table, correct=FALSE)
  
  return(test_prop)
}
test(HDDS_Cereals)
test(HDDS_Roots)
test(HDDS_Pulses)
test(HDDS_Veg)
test(HDDS_Fruits)
test(HDDS_Meat)
test(HDDS_Eggs)
test(HDDS_Fish)
test(HDDS_Dairy)
test(HDDS_Fats)
test(HDDS_Sugars)
test(HDDS_Condiments)

table(mVAMRural$Language)
table(mVAMRural$ADM1_NAME, mVAMRural$Approach)


########### Survey ########
SETSAN$ID <- c(1:dim(SETSAN)[1])

svyF2F<-svydesign(ids=~ID, strata=~Adm1_Name, fpc=~PopRural, data=SETSAN, nest=TRUE)
summary(svyF2F)


prop.table(svytable(~ADM1_NAME+mean(HDDS.F2F),svyF2F),1)
#Weighted mean HDDS for F2F only rural
mean.HDDS.F2Fsvy<-svymean(~HDDS.F2F, svyF2F)
#UN-Weighted mean HDDS for F2F only rural
mean.HDDS.F2F<-mean(SETSAN$HDDS.F2F)
svyby(~HDDS.F2F, by=~ADM1_NAME, design=svyF2F, FUN=svymean)

pop<-read.csv('POPULATION.CSV',stringsAsFactors = FALSE)


########### Survey ########
svymVAM$prob
svymVAM<-svydesign(ids=~RspID, strata=~ADM1_NAME, fpc=~PopRural, data=mVAMRural, nest=TRUE)
summary(svymVAM)
install.packages('ddply')
library(ddply)
x <- ddply(svymVAM$variables, 'ADM1_NAME', transform, SelectWtNorm = scale(SelectWt,center = FALSE, scale = mean(SelectWt)))$SelectWtNorm
svymVAM$prob <- svymVAM$prob/x

#For Post-Stratification
postStrDF <- sqlQuery(channel,
                      "select STR1_NAME as ADM1_NAME, ISNULL(StrPop,Population1) Population  from strata where ADM0_NAME='Mozambique'")
svymVAM <- postStratify(des=svymVAM,strata =~ADM1_NAME,pop=subset(postStrDF,ADM1_NAME %in% unique(svyF2F$strata[,1])))

summary(svymVAM )
#For Calibration
totPop <- sum(weights(svymVAM))

calbrDFHoHSex <- round(prop.table(svytable(~ADM1_NAME+HoHSex,svyF2F))*totPop)
calbrDFHoHSex[,2]<- round(calbrDFHoHSex[,2]*totPop)

calbrDFGender<- round(prop.table(svytable(~ADM1_NAME+Gender,svyF2F))*totPop)
calbrDFGender[,2] <- round(calbrDFGender[,2]*totPop)

#Perform Calibration

#calibration performed by raking
svymVAM2 <- rake(svymVAM,
                    sample.margins=list(~ADM1_NAME+HoHSex),
                    population.margins=calbrDFHoHSex[,2])
                    control = list(maxit = 100, epsilon = 1, verbose=FALSE)

summary(svyYmnMVAM2)

###Propensity Score Matching###########
#Create a merged datasets based on the common variables among F2F and mVAM

mVAMSubset<-subset(mVAMRural, select=c('ADM1_NAME','HoHSex','Gender','IncomeSrc', 'HDDS','FoodAss_YN')) 
mVAMSubset$Participation<-1
F2FSubset<-subset(SETSAN, select=c('ADM1_NAME','HoHSex','Gender','IncomeSrc', 'HDDS','FoodAss_YN'))
F2FSubset$Participation<-0

#Merging the two datsets based on the common variables
merge_mVAM_F2F<-rbind(mVAMSubset,F2FSubset)
table(merge_mVAM_F2F$IncomeSrc)

merge_mVAM_F2F$IncomeSrc[merge_mVAM_F2F$IncomeSrc=='Agriculture and livestock']<-'AgFish'
merge_mVAM_F2F$IncomeSrc[merge_mVAM_F2F$IncomeSrc=='Fishing']<-'AgFish'
merge_mVAM_F2F$IncomeSrc[merge_mVAM_F2F$IncomeSrc=='Self employed']<-'SelfEmploy'
merge_mVAM_F2F$IncomeSrc[merge_mVAM_F2F$IncomeSrc=='Assistance']<-'Assistance_CasuaLabour'
merge_mVAM_F2F$IncomeSrc[merge_mVAM_F2F$IncomeSrc=='CasualLabour']<-'Assistance_CasuaLabour'


merge_mVAM_F2F$Gender[merge_mVAM_F2F$Gender=='Female']<-'F'
merge_mVAM_F2F$Gender[merge_mVAM_F2F$Gender=='Male']<-'M'

merge_mVAM_F2F$HoHSex[merge_mVAM_F2F$HoHSex=='Female']<-'F'
merge_mVAM_F2F$HoHSex[merge_mVAM_F2F$HoHSex=='Male']<-'M'

merge_mVAM_F2F$FoodAss_YN[merge_mVAM_F2F$FoodAss_YN=='No']<-'N'
merge_mVAM_F2F$FoodAss_YN[merge_mVAM_F2F$FoodAss_YN=='Yes']<-'Y'

#Removing levels that are not there anymore
merge_mVAM_F2F$IncomeSrc<-factor(merge_mVAM_F2F$IncomeSrc)
merge_mVAM_F2F$Gender<-factor(merge_mVAM_F2F$Gender)
merge_mVAM_F2F$HoHSex<-factor(merge_mVAM_F2F$HoHSex)
merge_mVAM_F2F$Participation<-as.factor(merge_mVAM_F2F$Participation)
merge_mVAM_F2F$FoodAss_YN<-as.factor(merge_mVAM_F2F$FoodAss_YN)
#Computing propensity Score
##################################
##### GLM : logistic regression
#################################

#assumes linearity between the logistic link function and the covariates
#looking for the best model given AIC

## Basic model
ps.glm<-glm(Participation ~ HoHSex+ ADM1_NAME+ Gender+IncomeSrc+FoodAss_YN, data=merge_mVAM_F2F, family=binomial())
#Computing the respondents's predicted probability of being TREATED given the estimates from the logit model
prf_df<-data.frame(pr_score<-predict(ps.glm,type="response") ,
                   Participation=ps.glm$model$Participation)# Attach predicted pscore
head(prf_df)
summary(ps.glm) # AIC 1633.6

#Region of COMMON SUPPORT
#After estimating the propensity score, it is useful to plot histograms of the estimated propensity scores by treatment status
labels<-paste('Type of survey:', c('CATI','F2F'))
prf_df%>%
  mutate(Participation=ifelse(Participation==1, labels[1],labels[2])) %>%
  ggplot(aes(x=pr_score))+
  geom_histogram(color='white')+
  facet_wrap(~Participation)+
  xlab("Probability to be selected for the CATI Survey")+
  stat_bin(binwidth = 0.05)+
  stat_bin(nbin = 30)+
  theme_bw()

    
#Computing a matching alghoritm
mod_match<-matchit(Participation~HoHSex+ ADM1_NAME+ IncomeSrc+FoodAss_YN, 
                   method='nearest', data=merge_mVAM_F2F)
summary(mod_match)
plot(mod_match, type='jitter')
plot(mod_match, type='hist')

mod_match.rep2<-matchit(Participation~HoHSex+ ADM1_NAME+ IncomeSrc+FoodAss_YN, 
                  method='nearest', replace=TRUE, data=merge_mVAM_F2F)
summary(mod_match.rep2)


#Creating a new data frame containing only the matche observations
dta_m<-match.data(mod_match)
str(dta_m)

dta_m.rep<-match.data(mod_match.rep)


mean<-tapply(dta_m$HDDS,dta_m$Participation, mean, na.rm=T)
ttestHDDS<-t.test(dta_m$HDDS~dta_m$Participation,conf.level = 0.95,na.rm=T)

mean.rep<-tapply(dta_m.rep$HDDS,dta_m.rep$Participation, mean, na.rm=T)
ttestHDDS.rep<-t.test(dta_m.rep$HDDS~dta_m.rep$Participation,conf.level = 0.95,na.rm=T)

fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$Participation <- as.factor(dta$Participation)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = Participation)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}
str(dta_m)
grid.arrange(
  fn_bal(dta_m, "HoHSex"),
  fn_bal(dta_m, "Gender"),
  fn_bal(dta_m, "IncomeSrc"),
  nrow = 3, widths = c(1, 0.8)
)

with(dta_m, t.test(HoHSex~Participation))
with(dta_m, t.test(HoHSex~Participation))