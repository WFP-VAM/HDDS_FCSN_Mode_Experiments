get_stats_numerical <- function(df1, col1, df2, col2) 
{
  # computes mean, CI and t-test for a numerical variable "col" in "df"
  ttest <- t.test(df1[,col1], df2[,col2])
  mean1 <- mean(df1[,col1])
  mean2 <- mean(df2[,col2])
  
  CI1 <- 1.96 * sd(df1[,col1])
  CI2 <- 1.96 * sd(df2[,col2])
  
  pval <- as.numeric(ttest[['p.value']])

  #return(list(mean1 = mean1, CI1 = CI1, mean2 = mean2, CI2 = CI2, pval = pval))
  
  dataset <- c(deparse(substitute(df1)), deparse(substitute(df2)))
  variable <- c(col1, col2)
  avg <- c(mean1, mean2)
  CI <- c(CI1, CI2)
  pval <- c(pval)
  
  results <- data.frame(dataset, variable, avg, CI, pval)
  
  return(results)
}

get_stats_z <- function(df1, col1, pop1, df2, col2, pop2)
{
  test <- prop.test(x = c(sum(df1[,col1]==pop1),       sum(df2[,col2]==pop2)), n = c(nrow(df1), nrow(df2)), correct=FALSE)
  
  dataset <- c(deparse(substitute(df1)), deparse(substitute(df2)))
  variable <- c(col1, col2)
  pop <- c(pop1, pop2)
  avg <- c(test['estimate'][0], test['estimate'][1])
  CI <- c(test['conf.int'][0], test['conf.int'][1])
  pval <- c(test['p.value'])
  results <- data.frame(dataset, variable, pop, avg, CI, pval)
  
  return(results)
  
}

# Silvia's
# test.fun<- function(data, col){
#   YN<-ifelse(((data[,col]=="F") | (data[,col]=="Female")) ,0,1)
#   TAB<-table(data$MODE,YN)
#   ttest<-prop.test(TAB, correct=FALSE)
#   
#   return(ttest)
# }
# d1 <- data.frame(Gender=F2F[,'Gender'])
# d1$MODE <- 'F2F'
# d2 <- data.frame(Gender=mVAM[,'Gender'])
# d2['MODE'] <- 'mVAM'
# dd <- rbind(d1,d2)
# 
# test.fun(dd, 'Gender')

