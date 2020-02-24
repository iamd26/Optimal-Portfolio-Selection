library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(tidyr)

#rm(list=ls())

#Read all relevant Excel files
fundamentals<-read.csv("fundamentals.csv")
#Use split-adjusted stock price to calculate returns
prices_sa<-read.csv("prices_sa.csv")
securities<-read.csv("securities.csv")
market_data <- read.csv(file="rf.csv", sep=",",header=TRUE, 
                        stringsAsFactors=FALSE)  
market_data1015<-market_data[22127:23637,]

head(prices_sa)
head(securities)
head(fundamentals)
head(market_data1015)

#Convert strings to dates for time series data
prices_sa$Date<-as.Date(prices_sa$Date,format = "%m/%d/%Y")

#Calculate DAILY risk-free return from 1-month US Treasury Bills
market_data1015$RF<-market_data1015$RF/30
str(market_data1015)
tail(market_data1015)

#Example: Simple visualization of AAPL stock price over time 2010-2016
AAPL<-prices_sa %>% filter(Symbol == "AAPL")
AAPL %>% ggplot(aes(x=Date,y=Close))+geom_line()+scale_x_date(date_breaks = "months") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Check for missing stock prices, 34 identified stocks were then removed 
missing_price<-prices_sa %>% group_by(Symbol) %>% summarize(count=n()) %>% filter(count != 1762)
prices_sa_new<-prices_sa %>% filter(!Symbol %in% missing_price$Symbol)

#Use prices_sa_new (contains Volume data) as the most updated price database
str(prices_sa_new)
head(prices_sa_new)

#Drop Volume data to prepare for table rearrangement
prices_only<-data.frame(prices_sa_new$Date,prices_sa_new$Symbol,prices_sa_new$Close)
colnames(prices_only)<-c("Date","Symbol", "Close")

#Final dataset that contain 467 stocks across 1762 tading days (each stock represents a column)
prices_rearrange<-print(prices_only %>% spread(Symbol,Close))
str(prices_rearrange)

#Calculate daily log of returns across all stocks
returns<-as.data.frame(sapply(prices_rearrange[2:468], function(x) diff(log(x))))
log_returns<-cbind(prices_rearrange$Date[2:1762],returns)
colnames(log_returns)[colnames(log_returns)=="prices_rearrange$Date[2:1762]"] <- "Date"

#Using log_returns as the cleaned dataset for return dataset
#Checking for correct log_returns output
log_returns[1:20,1:6]
str(log_returns)
head(log_returns)

daily_returns <-as.data.frame(sapply(log_returns[2:468], function(x) exp(x)-1))
daily_returns<-cbind.data.frame(log_returns[,1],daily_returns)
colnames(daily_returns)[1]<-"Date"
head(daily_returns)

###################################################################################################
###################################################################################################
#Cleaning of fundamentals dataset
fun_1216<-fundamentals %>% filter(!Year %in% c(2003,2004,2006,2007))
fun_1216$Year <- as.factor(fun_1216$Year)
fun_1216$Accounts.Payable <- as.numeric(fun_1216$Accounts.Payable)
fun_1216$Accounts.Receivable <- as.numeric(fun_1216$Accounts.Receivable)

#Simple regression model based on company attributes in fundamentals
lm1<-lm(Earnings.Before.Interest.and.Tax~Accounts.Payable+Accounts.Receivable+After.Tax.ROE+Capital.Expenditures+
          Cash.and.Cash.Equivalents+Common.Stocks,data=subset(fun_1216,Year==2014))
summary(lm1)

#Simple plot based on company attributes in fundamentals
fun_1216 %>% ggplot(aes(x=log(Cash.and.Cash.Equivalents),
  y=log(Earnings.Before.Interest.and.Tax),color=Year))+geom_point(alpha=0.2)+
  geom_smooth(se=F,method="lm")+xlab("Cash & Cash Equivalents")+ylab("EBIT")+
  ggtitle("Correlation between Companies' Cash and Earning Levels")+ylim(17,25)

###################################################################################################
#Sample kmeans clustering analysis using Cash.and.Cash.Equivalents and Earnings.Before.Interest.and.Tax
install.packages("ElemStatLearn")
library(ElemStatLearn)
install.packages("class")
library(class)

#Clean out fundamentals datasset by removing redundant columns and deleting missing information
fun_clean <- fun_1216 %>% filter(Earnings.Before.Interest.and.Tax>0 & Accounts.Payable != "" 
                                 & Accounts.Receivable != "" & Year != 2016)
fun_clean <- cbind.data.frame(fun_clean[,3:9],fun_clean[,11:33],fun_clean[,35:44])
str(fun_clean)

#Running regression models via stepwise selection
lm <- lm(Net.Cash.Flow.Operating~.,data=fun_clean)
step <- step(lm,direction="both")
summary(step)

step <- data.frame(summary(step)$coefficient)
rownames(step)

#Taking outcomes from stepwise selection, we further selected variabales based on accounting equations
#There are 11 varaiables left for the final regression model
lm <- lm(Net.Cash.Flow.Operating~Accounts.Payable                                   
         +Cost.of.Revenue                                  
         +Earnings.Before.Interest.and.Tax                
         +Gross.Profit                                 
         +Long.Term.Debt                                 
         +Long.Term.Investments                            
         +Net.Cash.Flows.Financing                         
         +Net.Cash.Flows.Investing        
         +Retained.Earnings                                
         +Short.Term.Debt...Current.Portion.of.Long.Term.Debt
         +Short.Term.Investments,
         data=fun_clean)
summary(lm)

#Selecting variables for kmeans clustering analysis
fun_kmeans <- cbind(fun_clean$Accounts.Payable,fun_clean$Cost.of.Revenue,fun_clean$Earnings.Before.Interest.and.Tax,
                    fun_clean$Gross.Profit,fun_clean$Long.Term.Debt,fun_clean$Long.Term.Investments,fun_clean$Net.Cash.Flows.Investing,
                    fun_clean$Net.Cash.Flows.Financing,fun_clean$Retained.Earnings,fun_clean$Short.Term.Debt...Current.Portion.of.Long.Term.Debt,
                    fun_clean$Short.Term.Investments)

colnames(fun_kmeans)<-c("Accounts.Payable","Cost.of.Revenue","Earnings.Before.Interest.and.Tax",
                        "Gross.Profit","Long.Term.Debt","Long.Term.Investments","Net.Cash.Flows.Investing",
                        "Net.Cash.Flows.Financing","Retained.Earnings","Short.Term.Debt...Current.Portion.of.Long.Term.Debt",
                        "Short.Term.Investments")

#Standardise each column of the dataset for kmeans clustering analysis
fun_kmeans <- data.frame(sapply(data.frame(fun_kmeans), function(x) scale(x)))
summary(fun_kmeans)

source("DataAnalyticsFunctions.R")
#set.seed(1)
### computing # of clusters based on the selected variables over 2012-2016
#kfit <- lapply(1:50, function(k) kmeans(fun_kmeans,k,nstart=10,iter.max=30))
# choose number of clusters based on the fit above
# we will use the  script kIC in DataAnalyticsFunctions.R
# We call the function kIC the performance of the various 
# kmeans for k=1,...50, that was stored in kfit.
# Then "A" for AICc (default) or "B" for BIC
kaic <- sapply(kfit,kIC)
kbic  <- sapply(kfit,kIC,"B")
kHDic  <- sapply(kfit,kIC,"C")
## Now we plot them, first we plot AICp
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
plot(kaic, main="Variable Selection 2012-2015",xlab="k (# of clusters)", ylab="IC (Deviance + Penalty)", 
     ylim=range(c(kaic,kbic,kHDic)), # get them on same page
     type="l", lwd=2)
# Vertical line where AIC is minimized
abline(v=which.min(kaic))
# Next we plot BIC
lines(kbic, col=4, lwd=2)
# Vertical line where BIC is minimized
abline(v=which.min(kbic),col=4)
# Next we plot HDIC
lines(kHDic, col=3, lwd=2)
# Vertical line where HDIC is minimized
abline(v=which.min(kHDic),col=3)

text(c(30,22,5),c(3000,5000,10000),c("AIC","BIC","HDIC"))

#Selection of K based on AIC, BIC and HDIC Information Criterion
which.min(kaic)
which.min(kbic)
which.min(kHDic)

#######################################################################################
#Using HDIC Information Criterion as calculated above for interpretation purpose
#Clustering Analysis

fun_cluster <- fun_1216 %>% filter(Earnings.Before.Interest.and.Tax>0 & Accounts.Payable != "" 
                                   & Accounts.Receivable != "" & Year != 2016)
fun_cluster <- data.frame(cbind(fun_cluster[,1:9],fun_cluster[,11:33],fun_cluster[,35:44]))

#######################################################################################
###Sub-cluster: Year 2012
fun_12 <- fun_cluster %>% filter(Year == 2012,Earnings.Before.Interest.and.Tax>0)
str(fun_12)

cluster12 <- cbind.data.frame(fun_12$Symbol,fun_12$Accounts.Payable,fun_12$Cost.of.Revenue,fun_12$Earnings.Before.Interest.and.Tax,
                              fun_12$Gross.Profit,fun_12$Long.Term.Debt,fun_12$Long.Term.Investments,fun_12$Net.Cash.Flows.Investing,
                              fun_12$Net.Cash.Flows.Financing,fun_12$Retained.Earnings,fun_12$Short.Term.Debt...Current.Portion.of.Long.Term.Debt,
                              fun_12$Short.Term.Investments)
summary(cluster12)

cluster12_1 <- sapply(data.frame(cluster12[,2:12]), function(x) scale(x))

cluster12_all<-cbind.data.frame(cluster12[,1],cluster12_1)
cluster12_all1<-cluster12_all[,2:12]

summary(cluster12_all1)

#set.seed(1)  
#cluster12_kmeans <- kmeans(cluster12_all1,which.min(kaic),nstart=25)

#Assessment of in-sample R squared
1 - sum(cluster12_kmeans$tot.withinss)/cluster12_kmeans$totss

cluster12_kmeans$centers
cluster12_kmeans$size

#Company list with cluster categorization
new_cluster12 <-cluster12_all %>% mutate(cluster=cluster12_kmeans$cluster)
companylist12<-cbind.data.frame(new_cluster12[,1],new_cluster12[,13])

#Dataset containing company symbols and clustering outcome
company_cluster12<-cbind.data.frame(new_cluster12[,1],cluster12_all1)
head(company_cluster12)

#Example of all companies belong to a certain cluster
colnames(companylist12)<-c("Symbol","Cluster")
companylist12

#################visualizing the 11-dimensional clustering for cluster 2012###############
install.packages("Rtsne")
library(Rtsne) 
tsne_model_1 = Rtsne(as.matrix(cluster12[,2:12]), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)
summary(tsne_model_1)
tsne_model_1
d_tsne_1 = as.data.frame(tsne_model_1$Y)
d_tsne_1_original=d_tsne_1
d_tsne_1_original$cl_kmeans = factor(cluster12_kmeans$cluster)
colourCount = length(unique(cluster12_kmeans$cluster))
library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(colourCount, "Set1"))
plot_cluster=function(data, var_cluster, palette)
{
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster),fill=getPalette(colourCount)) +
    geom_point(size=2) +
    guides(colour=guide_legend(override.aes=list(size=6))) +
    xlab("") + ylab("") +
    ggtitle("") +
    theme_light(base_size=20) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal") + 
    scale_fill_manual(values = getPalette(colourCount))
}
plot_k=plot_cluster(d_tsne_1_original, "cl_kmeans", "Accent")
library(gridExtra)
grid.arrange(plot_k,ncol=2)

##############################################################################################
#Calculate average returns for clusters in 2012
daily_returns12<-daily_returns %>% filter (Date > "2012-01-01" & Date <"2012-12-31")
companylist12

cluster_return12<-matrix(rep(0,2*which.min(kaic)),ncol=2)

for (j in companylist12$Cluster){
  a<-as.character(companylist12$Symbol[companylist12$Cluster==j])
  cluster_rt<-as.data.frame(daily_returns12[,names(daily_returns12) %in% a])
  mu_rt<-sapply(cluster_rt, function(x) mean(x))
#Bulding up the tangency portfolio
  one_vec<-rep(1,length(mu_rt))
  mu_rf<-mean(market_data1015[,5])
  mu_exe<-mu_rt-mu_rf*one_vec
  cov_cluster<-var(cluster_rt)
  top_mat<-solve(cov_cluster)%*%mu_exe
  bot_val<-as.numeric(one_vec%*%top_mat) 
  tan_vec12 <- top_mat[,1]/bot_val
  cluster_return12[j,1]<- crossprod(tan_vec12,mu_rt)
  cluster_return12[j,2] <- as.numeric(sqrt(t(tan_vec12)%*%cov_cluster%*%tan_vec12))
}
colnames(cluster_return12)<-c("2012cluster_rt","2012cluster_sd")
cluster_return12

##############################################################################################
#################Final Datasets##################
cluster12_all1
company_cluster12
companylist12
#################Final Datasets##################
#######################################################################################
###Sub-cluster: Year 2013
fun_13 <- fun_cluster %>% filter(Year == 2013,Earnings.Before.Interest.and.Tax>0)
head(fun_13)

cluster13 <- cbind.data.frame(fun_13$Symbol,fun_13$Accounts.Payable,fun_13$Cost.of.Revenue,fun_13$Earnings.Before.Interest.and.Tax,
                              fun_13$Gross.Profit,fun_13$Long.Term.Debt,fun_13$Long.Term.Investments,fun_13$Net.Cash.Flows.Investing,
                              fun_13$Net.Cash.Flows.Financing,fun_13$Retained.Earnings,fun_13$Short.Term.Debt...Current.Portion.of.Long.Term.Debt,
                              fun_13$Short.Term.Investments)
summary(cluster13)

cluster13_1 <- sapply(data.frame(cluster13[,2:12]), function(x) scale(x))

cluster13_all<-cbind.data.frame(cluster13[,1],cluster13_1)
cluster13_all1<-cluster13_all[,2:12]

summary(cluster13_all1)

#set.seed(1)  
#cluster13_kmeans <- kmeans(cluster13_all1,which.min(kaic),nstart=25,iter.max = 30)

#Assessment of in-sample R squared
1 - sum(cluster13_kmeans$tot.withinss)/cluster13_kmeans$totss

cluster13_kmeans$centers
cluster13_kmeans$size

#Company list with cluster categorization
new_cluster13 <-cluster13_all %>% mutate(cluster=cluster13_kmeans$cluster)
companylist13<-cbind.data.frame(new_cluster13[,1],new_cluster13[,13])

#Dataset containing company symbols and clustering outcome
company_cluster13<-cbind.data.frame(new_cluster13[,1],cluster13_all1)
head(company_cluster13)

#Example of all companies belong to a certain cluster
colnames(companylist13)<-c("Symbol","Cluster")
companylist13

#############visualizing the 11-dimensional clustering for cluster 2013
tsne_model_1 = Rtsne(as.matrix(cluster13[,2:12]), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)
summary(tsne_model_1)
tsne_model_1
d_tsne_1 = as.data.frame(tsne_model_1$Y)
d_tsne_1_original=d_tsne_1
d_tsne_1_original$cl_kmeans = factor(cluster13_kmeans$cluster)
colourCount = length(unique(cluster13_kmeans$cluster))
library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(colourCount, "Set1"))
plot_cluster=function(data, var_cluster, palette)
{
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster),fill=getPalette(colourCount)) +
    geom_point(size=2) +
    guides(colour=guide_legend(override.aes=list(size=6))) +
    xlab("") + ylab("") +
    ggtitle("") +
    theme_light(base_size=20) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal") + 
    scale_fill_manual(values = getPalette(colourCount))
}
plot_k=plot_cluster(d_tsne_1_original, "cl_kmeans", "Accent")
library(gridExtra)
grid.arrange(plot_k,ncol=2)

##############################################################################################
#Calculate average returns for clusters in 2013
daily_returns13<-daily_returns %>% filter (Date > "2013-01-01" & Date <"2013-12-31")
companylist13

cluster_return13<-matrix(rep(0,2*which.min(kaic)),ncol=2)

for (j in companylist13$Cluster){
  a<-as.character(companylist13$Symbol[companylist13$Cluster==j])
  cluster_rt<-as.data.frame(daily_returns13[,names(daily_returns13) %in% a])
  mu_rt<-sapply(cluster_rt, function(x) mean(x))
  #Bulding up the tangency portfolio
  one_vec<-rep(1,length(mu_rt))
  mu_rf<-mean(market_data1015[,5])
  mu_exe<-mu_rt-mu_rf*one_vec
  cov_cluster<-var(cluster_rt)
  top_mat<-solve(cov_cluster)%*%mu_exe
  bot_val<-as.numeric(one_vec%*%top_mat) 
  tan_vec <- top_mat[,1]/bot_val
  cluster_return13[j,1]<- crossprod(tan_vec,mu_rt)
  cluster_return13[j,2] <- as.numeric(sqrt(t(tan_vec)%*%cov_cluster%*%tan_vec))
}
colnames(cluster_return13)<-c("2013cluster_rt","2013cluster_sd")
cluster_return13

##############################################################################################
#################Final Datasets##################
cluster13_all1
company_cluster13
companylist13
#################Final Datasets##################
#######################################################################################
###Sub-cluster: Year 2014
fun_14 <- fun_cluster %>% filter(Year == 2014,Earnings.Before.Interest.and.Tax>0)
head(fun_14)

cluster14 <- cbind.data.frame(fun_14$Symbol,fun_14$Accounts.Payable,fun_14$Cost.of.Revenue,fun_14$Earnings.Before.Interest.and.Tax,
                   fun_14$Gross.Profit,fun_14$Long.Term.Debt,fun_14$Long.Term.Investments,fun_14$Net.Cash.Flows.Investing,
                   fun_14$Net.Cash.Flows.Financing,fun_14$Retained.Earnings,fun_14$Short.Term.Debt...Current.Portion.of.Long.Term.Debt,
                   fun_14$Short.Term.Investments)
summary(cluster14)

cluster14_1 <- sapply(data.frame(cluster14[,2:12]), function(x) scale(x))

cluster14_all<-cbind.data.frame(cluster14[,1],cluster14_1)
cluster14_all1<-cluster14_all[,2:12]

summary(cluster14_all1)

#set.seed(1)  
#cluster14_kmeans <- kmeans(cluster14_all1,which.min(kaic),nstart=25,iter.max = 30)

#Assessment of in-sample R squared
1 - sum(cluster14_kmeans$tot.withinss)/cluster14_kmeans$totss

cluster14_kmeans$centers
cluster14_kmeans$size

#Company list with cluster categorization
new_cluster14 <-cluster14_all %>% mutate(cluster=cluster14_kmeans$cluster)
companylist14<-cbind.data.frame(new_cluster14[,1],new_cluster14[,13])

#Dataset containing company symbols and clustering outcome
company_cluster14<-cbind.data.frame(new_cluster14[,1],cluster14_all1)
head(company_cluster14)

#Example of all companies belong to a certain cluster
colnames(companylist14)<-c("Symbol","Cluster")
companylist14

#############visualizing the 11-dimensional clustering for cluster 2014
tsne_model_1 = Rtsne(as.matrix(cluster14[,2:12]), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)
summary(tsne_model_1)
tsne_model_1
d_tsne_1 = as.data.frame(tsne_model_1$Y)
d_tsne_1_original=d_tsne_1
d_tsne_1_original$cl_kmeans = factor(cluster14_kmeans$cluster)
colourCount = length(unique(cluster14_kmeans$cluster))
library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(colourCount, "Set1"))
plot_cluster=function(data, var_cluster, palette)
{
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster),fill=getPalette(colourCount)) +
    geom_point(size=2) +
    guides(colour=guide_legend(override.aes=list(size=6))) +
    xlab("") + ylab("") +
    ggtitle("") +
    theme_light(base_size=20) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal") + 
    scale_fill_manual(values = getPalette(colourCount))
}
plot_k=plot_cluster(d_tsne_1_original, "cl_kmeans", "Accent")
library(gridExtra)
grid.arrange(plot_k,ncol=2)

##############################################################################################
#Calculate average returns for clusters in 2014
daily_returns14<-daily_returns %>% filter (Date > "2014-01-01" & Date <"2014-12-31")
companylist14

rm(cluster_return14)
cluster_return14<-matrix(rep(0,2*which.min(kaic)),ncol=2)

for (j in companylist14$Cluster){
  a<-as.character(companylist14$Symbol[companylist14$Cluster==j])
  cluster_rt<-as.data.frame(daily_returns14[,names(daily_returns14) %in% a])
  mu_rt<-sapply(cluster_rt, function(x) mean(x))
  #Bulding up the tangency portfolio
  one_vec<-rep(1,length(mu_rt))
  mu_rf<-mean(market_data1015[,5])
  mu_exe<-mu_rt-mu_rf*one_vec
  cov_cluster<-var(cluster_rt)
  top_mat<-solve(cov_cluster)%*%mu_exe
  bot_val<-as.numeric(one_vec%*%top_mat) 
  tan_vec <- top_mat[,1]/bot_val
  cluster_return14[j,1]<- crossprod(tan_vec,mu_rt)
  cluster_return14[j,2] <- as.numeric(sqrt(t(tan_vec)%*%cov_cluster%*%tan_vec))
}
colnames(cluster_return14)<-c("2014cluster_rt","2014cluster_sd")
cluster_return14

##############################################################################################
#################Final Datasets##################
cluster14_all1
company_cluster14
companylist14
#################Final Datasets##################
#######################################################################################
###Sub-cluster: Year 2015
fun_15 <- fun_cluster %>% filter(Year == 2015,Earnings.Before.Interest.and.Tax>0)
head(fun_15)

cluster15 <- cbind.data.frame(fun_15$Symbol,fun_15$Accounts.Payable,fun_15$Cost.of.Revenue,fun_15$Earnings.Before.Interest.and.Tax,
                              fun_15$Gross.Profit,fun_15$Long.Term.Debt,fun_15$Long.Term.Investments,fun_15$Net.Cash.Flows.Investing,
                              fun_15$Net.Cash.Flows.Financing,fun_15$Retained.Earnings,fun_15$Short.Term.Debt...Current.Portion.of.Long.Term.Debt,
                              fun_15$Short.Term.Investments)
summary(cluster15)

cluster15_1 <- sapply(data.frame(cluster15[,2:12]), function(x) scale(x))

cluster15_all<-cbind.data.frame(cluster15[,1],cluster15_1)
cluster15_all1<-cluster15_all[,2:12]

summary(cluster15_all1)

#set.seed(1)  
#cluster15_kmeans <- kmeans(cluster15_all1,which.min(kaic),nstart=25,iter.max = 30)

#Assessment of in-sample R squared
1 - sum(cluster15_kmeans$tot.withinss)/cluster15_kmeans$totss

cluster15_kmeans$centers
cluster15_kmeans$size

#Company list with cluster categorization
new_cluster15 <-cluster15_all %>% mutate(cluster=cluster15_kmeans$cluster)
companylist15<-cbind.data.frame(new_cluster15[,1],new_cluster15[,13])

#Dataset containing company symbols and clustering outcome
company_cluster15<-cbind.data.frame(new_cluster15[,1],cluster15_all1)
head(company_cluster15)

#Example of all companies belong to a certain cluster
colnames(companylist15)<-c("Symbol","Cluster")
companylist15

#############visualizing the 11-dimensional clustering for cluster 2015
tsne_model_1 = Rtsne(as.matrix(cluster15[,2:12]), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)
summary(tsne_model_1)
tsne_model_1
d_tsne_1 = as.data.frame(tsne_model_1$Y)
d_tsne_1_original=d_tsne_1
d_tsne_1_original$cl_kmeans = factor(cluster15_kmeans$cluster)
colourCount = length(unique(cluster15_kmeans$cluster))
library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(colourCount, "Set1"))
plot_cluster=function(data, var_cluster, palette)
{
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster),fill=getPalette(colourCount)) +
    geom_point(size=2) +
    guides(colour=guide_legend(override.aes=list(size=6))) +
    xlab("") + ylab("") +
    ggtitle("") +
    theme_light(base_size=20) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal") + 
    scale_fill_manual(values = getPalette(colourCount))
}
plot_k=plot_cluster(d_tsne_1_original, "cl_kmeans", "Accent")
library(gridExtra)
grid.arrange(plot_k,ncol=2)

##############################################################################################
#Calculate average returns for clusters in 2015
daily_returns15<-daily_returns %>% filter (Date > "2015-01-01" & Date <"2015-12-31")
companylist15

cluster_return15<-matrix(rep(0,2*which.min(kaic)),ncol=2)

for (j in companylist15$Cluster){
  a<-as.character(companylist15$Symbol[companylist15$Cluster==j])
  cluster_rt<-as.data.frame(daily_returns15[,names(daily_returns15) %in% a])
  mu_rt<-sapply(cluster_rt, function(x) mean(x))
  #Bulding up the tangency portfolio
  one_vec<-rep(1,length(mu_rt))
  mu_rf<-mean(market_data1015[,5])
  mu_exe<-mu_rt-mu_rf*one_vec
  cov_cluster<-var(cluster_rt)
  top_mat<-solve(cov_cluster)%*%mu_exe
  bot_val<-as.numeric(one_vec%*%top_mat) 
  tan_vec <- top_mat[,1]/bot_val
  cluster_return15[j,1]<- crossprod(tan_vec,mu_rt)
  cluster_return15[j,2] <- as.numeric(sqrt(t(tan_vec)%*%cov_cluster%*%tan_vec))
}
colnames(cluster_return15)<-c("2015cluster_rt","2015cluster_sd")
cluster_return15

##############################################################################################
#################Final Datasets##################
cluster15_all1
company_cluster15
companylist15
#################Final Datasets##################
########################################################################################
#########################PCA Analysis using all explanatory variables###################
########################################################################################
###performing PCA to find the principal components, and reduce dimensions
install.packages("plfm")
library(plfm)
##############################################
###for 2012
stock.labels12<-as.factor(fun_12[,1])
pca.12 <- prcomp(fun_12[,c(3:42)],scale=TRUE)
summary(pca.12)

plot(pca.12,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)

fun12_pc<-predict(pca.12)
fun12_all<-cbind.data.frame(fun_12$Symbol,fun12_pc[,1:6])
#Use the below dataset for PCA visualization
fun12_all

plot(fun12_pc[,1:2], pch=21,  main="")
text(fun12_pc[,1:2], label=stock.labels12, col="blue", cex=1)

plot(fun12_pc[,3:4], pch=21,  main="")
text(fun12_pc[,3:4], label=stock.labels12, col="blue", cex=1)

plot(fun12_pc[,5:6], pch=21,  main="")
text(fun12_pc[,5:6], label=stock.labels12, col="blue", cex=1)

#Look at the loadings for correlation of each factor with the original feature
loadings12 <- pca.12$rotation[,1:6]
loadings12

### For each factor lets display the top features that 
### are responsible for 1/2 of the squared norm of the loadings
#Loading1
v<-loadings12[order(abs(loadings12[,1]), decreasing=TRUE)[1:27],1]
loadingfit12 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit12)]

#Loading2
v<-loadings12[order(abs(loadings12[,2]), decreasing=TRUE)[1:27],2]
loadingfit12 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit12)]

#Loading3
v<-loadings12[order(abs(loadings12[,3]), decreasing=TRUE)[1:27],3]
loadingfit12 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit12)]

#Loading4
v<-loadings12[order(abs(loadings12[,4]), decreasing=TRUE)[1:27],4]
loadingfit12 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit13)]

#Loading5
v<-loadings12[order(abs(loadings12[,5]), decreasing=TRUE)[1:27],5]
loadingfit12 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit12)]

#Loading6
v<-loadings12[order(abs(loadings12[,6]), decreasing=TRUE)[1:27],6]
loadingfit12 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit12)]

##############################################
###for 2013
stock.labels13<-as.factor(fun_13[,1])
pca.13 <- prcomp(fun_13[,c(3:42)],scale=TRUE)
summary(pca.13)

plot(pca.13,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)

fun13_pc<-predict(pca.13)
fun13_all<-cbind.data.frame(fun_13$Symbol,fun13_pc[,1:6])
#Use the below dataset for PCA visualization
fun13_all

plot(fun13_pc[,1:2], pch=21,  main="")
text(fun13_pc[,1:2], label=stock.labels13, col="blue", cex=1)

plot(fun13_pc[,3:4], pch=21,  main="")
text(fun13_pc[,3:4], label=stock.labels13, col="blue", cex=1)

plot(fun13_pc[,5:6], pch=21,  main="")
text(fun13_pc[,5:6], label=stock.labels13, col="blue", cex=1)

#Look at the loadings for correlation of each factor with the original feature
loadings13 <- pca.13$rotation[,1:6]
loadings13

### For each factor lets display the top features that 
### are responsible for 1/2 of the squared norm of the loadings
#Loading1
v<-loadings13[order(abs(loadings13[,1]), decreasing=TRUE)[1:27],1]
loadingfit13 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit13)]

#Loading2
v<-loadings13[order(abs(loadings13[,2]), decreasing=TRUE)[1:27],2]
loadingfit13 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit13)]

#Loading3
v<-loadings13[order(abs(loadings13[,3]), decreasing=TRUE)[1:27],3]
loadingfit13 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit13)]

#Loading4
v<-loadings13[order(abs(loadings13[,4]), decreasing=TRUE)[1:27],4]
loadingfit13 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit13)]

#Loading5
v<-loadings13[order(abs(loadings13[,5]), decreasing=TRUE)[1:27],5]
loadingfit13 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit13)]

#Loading6
v<-loadings13[order(abs(loadings13[,6]), decreasing=TRUE)[1:27],6]
loadingfit13 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit13)]

##############################################
###for 2014
stock.labels14<-as.factor(fun_14[,1])
pca.14 <- prcomp(fun_14[,c(3:42)],scale=TRUE)
summary(pca.14)

plot(pca.14,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)

fun14_pc<-predict(pca.14)
fun14_all<-cbind.data.frame(fun_14$Symbol,fun14_pc[,1:6])
#Use the below dataset for PCA visualization
fun14_all

plot(fun14_pc[,1:2], pch=21,  main="")
text(fun14_pc[,1:2], label=stock.labels14, col="blue", cex=1)

plot(fun14_pc[,3:4], pch=21,  main="")
text(fun14_pc[,3:4], label=stock.labels14, col="blue", cex=1)

plot(fun14_pc[,5:6], pch=21,  main="")
text(fun14_pc[,5:6], label=stock.labels14, col="blue", cex=1)
#Look at the loadings for correlation of each factor with the original feature
loadings14 <- pca.14$rotation[,1:6]
loadings14

### For each factor lets display the top features that 
### are responsible for 1/2 of the squared norm of the loadings
#Loading1
v<-loadings14[order(abs(loadings14[,1]), decreasing=TRUE)[1:27],1]
loadingfit14 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit14)]

#Loading2
v<-loadings14[order(abs(loadings14[,2]), decreasing=TRUE)[1:27],2]
loadingfit14 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit14)]

#Loading3
v<-loadings14[order(abs(loadings14[,3]), decreasing=TRUE)[1:27],3]
loadingfit14 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit14)]

#Loading4
v<-loadings14[order(abs(loadings14[,4]), decreasing=TRUE)[1:27],4]
loadingfit14 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit14)]

#Loading5
v<-loadings14[order(abs(loadings14[,5]), decreasing=TRUE)[1:27],5]
loadingfit14 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit14)]

#Loading6
v<-loadings14[order(abs(loadings14[,6]), decreasing=TRUE)[1:27],6]
loadingfit14 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit14)]

##############################################
###for 2015
stock.labels15<-as.factor(fun_15[,1])
pca.15 <- prcomp(fun_15[,c(3:42)],scale=TRUE)
summary(pca.15)

plot(pca.15,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)

fun15_pc<-predict(pca.15)
fun15_all<-cbind.data.frame(fun_15$Symbol,fun15_pc[,1:6])
#Use the below dataset for PCA visualization
fun15_all

plot(fun15_pc[,1:2], pch=21,  main="")
text(fun15_pc[,1:2], label=stock.labels15, col="blue", cex=1)

plot(fun15_pc[,3:4], pch=21,  main="")
text(fun15_pc[,3:4], label=stock.labels15, col="blue", cex=1)

plot(fun15_pc[,5:6], pch=21,  main="")
text(fun15_pc[,5:6], label=stock.labels15, col="blue", cex=1)

#Look at the loadings for correlation of each factor with the original feature
loadings15 <- pca.15$rotation[,1:6]
loadings15

### For each factor lets display the top features that 
### are responsible for 1/2 of the squared norm of the loadings
#Loading1
v<-loadings15[order(abs(loadings15[,1]), decreasing=TRUE)[1:27],1]
loadingfit15 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit15)]

#Loading2
v<-loadings15[order(abs(loadings15[,2]), decreasing=TRUE)[1:27],2]
loadingfit15 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit15)]

#Loading3
v<-loadings15[order(abs(loadings15[,3]), decreasing=TRUE)[1:27],3]
loadingfit15 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)

v[1:which.min(loadingfit15)]

#Loading4
v<-loadings15[order(abs(loadings15[,4]), decreasing=TRUE)[1:27],4]
loadingfit15 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit15)]

#Loading5
v<-loadings15[order(abs(loadings15[,5]), decreasing=TRUE)[1:27],5]
loadingfit15 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit15)]

#Loading6
v<-loadings15[order(abs(loadings15[,6]), decreasing=TRUE)[1:27],6]
loadingfit15 <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 1/2 )^2)
v[1:which.min(loadingfit15)]

###################################################################################
#######Visualization of Network Data using 6-Dimensional PCA####################
###################################################################################
##########Calculate the euclidean distance between different companies##########
###################################################################################
##########2012
library(philentropy)
str(fun12_all)
fun12_all[,1]
distance12 <- data.frame(fun12_all[,1],distance(fun12_all[2:7], method = "euclidean"))
colnames(distance12)[1] <- "Symbol"
head(distance12)
##########2013
distance13 <- data.frame(fun13_all[,1],distance(fun13_all[2:7], method = "euclidean"))
colnames(distance13)[1] <- "Symbol"
head(distance13)
##########2014
head(fun14_all)
str(fun14_all)
distance14 <- data.frame(fun14_all[,1],distance(fun14_all[2:7], method = "euclidean"))
colnames(distance14)[1] <- "Symbol"
head(distance14)
##########2015
head(fun15_all)
str(fun15_all)
distance15 <- data.frame(fun15_all[,1],distance(fun15_all[2:7], method = "euclidean"))
colnames(distance15)[1] <- "Symbol"
head(distance15)
###########Distance Metrix
distance12[1:10,1:10] #year 2012
distance13[1:10,1:10] #year 2013
distance14[1:10,1:10] #year 2014
distance15[1:10,1:10] #year 2015
###########Transform data into 1 and 0 with median
###########if the distance is larger than median, then assigned 0
###########if the distance is smaller than median, then assigned 1
###########2012
distance12[distance12==0]=0
distance12[distance12<= 1 & distance12>0]=1
distance12[distance12>1]=0
distance12[1:10,1:10]
name12 <- as.vector(distance12[,1])
colnames(distance12) <- c('Symbol',name12)
distance12[1:10,1:10]
dis <- as.matrix(distance12)
dis[1:10,1:10]
class(dis)
class(distance12)
library(Matrix)
library(igraph)
install.packages('hashmap')
library(hashmap)

dis <- sapply(distance12, as.numeric)
g <- graph.adjacency(dis[,-1])
g
e <- get.edgelist(g)
e
dim(e)
timeLine <- c(-20 , +20)
par(mar=c(5,5,5,5))
tmp <- graph_from_edgelist(head(e,1000), directed=FALSE)
igraph.options(plot.layout=layout.graphopt, vertex.size=4)
colnames(companylist12)<-c("Symbol","Cluster")
head(companylist12)
h <- hashmap(c(as.vector(companylist12$Symbol)),c(as.vector(companylist12$Cluster)))
h[[as_ids(V(tmp))]]
dev.off()
V(tmp)$color <- h[[as_ids(V(tmp))]]
plot(tmp, vertex.size = 4, vertex.label.cex = 0.5, vertex.label.dist = 0.3)

###########2013
distance13[distance13==0]=0
distance13[distance13<=1 & distance13>0]=1
distance13[distance13>1]=0
distance13[1:10,1:10]
name13 <- as.vector(distance13[,1])
colnames(distance13) <- c('Symbol',name13)
distance13[1:10,1:10]
dis <- as.matrix(distance13)
dis[1:10,1:10]
class(dis)
class(distance13)
distance13[1:10,1:10]
library(Matrix)

dis <- sapply(distance13, as.numeric)

g <- graph.adjacency(dis[,-1])
g
e <- get.edgelist(g)
e
dim(e)
timeLine <- c(-20 , +20)
par(mar=c(5,5,5,5))
tmp <- graph_from_edgelist(head(e,1000), directed=FALSE)
igraph.options(plot.layout=layout.graphopt, vertex.size=4)
colnames(companylist13)<-c("Symbol","Cluster")
head(companylist13)
h <- hashmap(c(as.vector(companylist13$Symbol)),c(as.vector(companylist13$Cluster)))
h[[as_ids(V(tmp))]]
dev.off()
V(tmp)$color <- h[[as_ids(V(tmp))]]
plot(tmp, vertex.size = 4, vertex.label.cex = 0.5, vertex.label.dist = 0.3)

###########2014
distance14[distance14==0]=0
distance14[distance14 <= 1 & distance14>0]=1
distance14[distance14 > 1]=0
distance14[1:10,1:10]
name14 <- as.vector(distance14[,1])
colnames(distance14) <- c('Symbol',name14)
distance14[1:10,1:10]
dis <- as.matrix(distance14)
dis[1:10,1:10]
class(dis)
class(distance14)
library(Matrix)

dis <- sapply(distance14, as.numeric)
dis[,-1]
g <- graph.adjacency(dis[,-1])
g
e <- get.edgelist(g)
e
dim(e)
timeLine <- c(-20 , +20)
par(mar=c(5,5,5,5))
tmp <- graph_from_edgelist(head(e,1000), directed=FALSE)
igraph.options(plot.layout=layout.graphopt, vertex.size=4)
colnames(companylist14)<-c("Symbol","Cluster")
head(companylist14)
h <- hashmap(c(as.vector(companylist14$Symbol)),c(as.vector(companylist14$Cluster)))
h[[as_ids(V(tmp))]]
dev.off()
V(tmp)$color <- h[[as_ids(V(tmp))]]
plot(tmp, vertex.size = 4, vertex.label.cex = 0.5, vertex.label.dist = 0.3)

###########2015
distance15[distance15==0]=0
distance15[distance15<=1 & distance15>0]=1
distance15[distance15>1]=0
distance15[1:10,1:10]
name15 <- as.vector(distance15[,1])
colnames(distance15) <- c('Symbol',name15)
distance15[1:10,1:10]

dis <- as.matrix(distance15)
dis[1:10,1:10]
class(dis)
class(distance15)
library(Matrix)

dis <- sapply(distance15, as.numeric)

g <- graph.adjacency(dis[,-1])
g
e <- get.edgelist(g)
e
dim(e)
timeLine <- c(-20 , +20)
par(mar=c(5,5,5,5))
tmp <- graph_from_edgelist(head(e,1000), directed=FALSE)
igraph.options(plot.layout=layout.graphopt, vertex.size=4)
colnames(companylist15)<-c("Symbol","Cluster")
head(companylist15)
h <- hashmap(c(as.vector(companylist15$Symbol)),c(as.vector(companylist15$Cluster)))
h[[as_ids(V(tmp))]]
dev.off()
V(tmp)$color <- h[[as_ids(V(tmp))]]
plot(tmp, vertex.size = 4, vertex.label.cex = 0.5, vertex.label.dist = 0.3)

###################################################################################
##################Calculate average daily S&P 500 return across each year##########
###########Imformation for S&P 500#################################################
####full time
sp500 <- read.csv("SP_daily.csv")
head(sp500)
sp500 <- na.omit(sp500)
mean(sp500[,3])
sd(sp500[,3])
###########2012
sp500
sp500$Date<-as.Date(sp500$Date,format = "%m/%d/%Y")
sp500
sp500_12 <- sp500 %>%
  filter(Date >= "0012-01-01" & Date <= "0012-12-31")
mean(sp500_12[,3])
sd(sp500_12[,3])
###########2013
sp500_13 <- sp500 %>%
  filter(Date >= "0013-01-01" & Date <= "0013-12-31")
mean(sp500_13[,3])
sd(sp500_13[,3])
###########2014
sp500_14 <- sp500 %>%
  filter(Date >= "0014-01-01" & Date <= "0014-12-31")
mean(sp500_14[,3])
sd(sp500_14[,3])
###########2015
sp500_15 <- sp500 %>%
  filter(Date >= "0015-01-01" & Date <= "0015-12-31")
mean(sp500_15[,3])
sd(sp500_15[,3])
###########2016
sp500_16 <- sp500 %>%
  filter(Date >= "0016-01-01" & Date <= "0016-12-31")

mean(sp500_16[,3])
sd(sp500_16[,3])

###################################################################################
###################Choose Tangency Portfolio for each year#########################
###################################################################################
cluster_return12
cluster_return13
cluster_return14
cluster_return15
####tangent portfolio for 2012
####Calculate the expected value for optimal portfolio in 2012
cluster_return12
cluster12_kmeans$size
row.names(cluster_return12)<-1:nrow(cluster_return12)
as.data.frame(cluster_return12) %>% ggplot(aes(x=cluster_return12[,2],y=cluster_return12[,1],label=row.names(cluster_return12)))+
  geom_point(size=3,colour="#000099")+xlim(0.00001,0.05)+ylim(-0.005,0.01)+xlab(expression(sigma[p]))+ylab(expression(mu[p]))+
  geom_label() +geom_segment(aes(x = 0.00001, y =mu_rf , xend = cluster_return12[23,2], yend = cluster_return12[23,1], colour = "red"),size=1)+
  ggtitle("Portfolio Selection by Cluster - 2012")+geom_text(x=0, y=0, label="Rf",size=8)+
  geom_point(aes(x=sd(sp500_12[,3]), y=mean(sp500_12[,3])), colour="blue",size=5)+geom_text(x=sd(sp500_12[,3]), y=mean(sp500_12[,3]), label="S&P500",size=5)

####tangent portfolio for 2013
####Calculate the expected value for optimal portfolio in 2013
cluster_return13
cluster13_kmeans$size
row.names(cluster_return13)<-1:nrow(cluster_return13)
as.data.frame(cluster_return13) %>% ggplot(aes(x=cluster_return13[,2],y=cluster_return13[,1],label=row.names(cluster_return13)))+
  geom_point(size=3,colour="#000099")+xlim(0.00001,0.02)+ylim(-0.005,0.01)+xlab(expression(sigma[p]))+ylab(expression(mu[p]))+
  geom_label() +geom_segment(aes(x = 0.00001, y =mu_rf , xend = cluster_return13[3,2], yend = cluster_return13[3,1], colour = "red"),size=1)+
  ggtitle("Portfolio Selection by Cluster - 2013")+geom_text(x=0, y=0, label="Rf",size=8)+
  geom_point(aes(x=sd(sp500_13[,3]), y=mean(sp500_13[,3])), colour="blue",size=5)+geom_text(x=sd(sp500_13[,3]), y=mean(sp500_13[,3]), label="S&P500",size=5)

####tangent portfolio for 2014
####Calculate the expected value for optimal portfolio in 2014
cluster_return14
cluster14_kmeans$size
row.names(cluster_return14)<-1:nrow(cluster_return14)
as.data.frame(cluster_return14) %>% ggplot(aes(x=cluster_return14[,2],y=cluster_return14[,1],label=row.names(cluster_return14)))+
  geom_point(size=3,colour="#000099")+xlim(0.00001,0.026)+ylim(-0.005,0.015)+xlab(expression(sigma[p]))+ylab(expression(mu[p]))+
  geom_label() +geom_segment(aes(x = 0.00001, y =mu_rf , xend = cluster_return14[24,2], yend = cluster_return14[24,1], colour = "red"),size=1)+
  ggtitle("Portfolio Selection by Cluster - 2014")+geom_text(x=0, y=0, label="Rf",size=8)+
  geom_point(aes(x=sd(sp500_14[,3]), y=mean(sp500_14[,3])), colour="blue",size=5)+geom_text(x=sd(sp500_14[,3]), y=mean(sp500_14[,3]), label="S&P500",size=5)

####tangent portfolio for 2015
####Calculate the expected value for optimal portfolio in 2015
cluster_return15
cluster15_kmeans$size
row.names(cluster_return15)<-1:nrow(cluster_return15)
as.data.frame(cluster_return15) %>% ggplot(aes(x=cluster_return15[,2],y=cluster_return15[,1],label=row.names(cluster_return15)))+
  geom_point(size=3,colour="#000099")+xlim(0.00001,0.03)+ylim(-0.003,0.002)+xlab(expression(sigma[p]))+ylab(expression(mu[p]))+
  geom_label() +geom_segment(aes(x = 0.00001, y =mu_rf , xend = cluster_return15[8,2], yend = cluster_return15[8,1], colour = "red"),size=1)+
  ggtitle("Portfolio Selection by Cluster - 2015")+geom_text(x=0, y=0, label="Rf",size=8)+
  geom_point(aes(x=sd(sp500_15[,3]), y=mean(sp500_15[,3])), colour="blue",size=5)+geom_text(x=sd(sp500_15[,3]), y=mean(sp500_15[,3]), label="S&P500",size=5)

###################################################################################################
###################Calculation of 2016 Portfolio Return vs. S&P 500 Return#########################
###################################################################################################
daily_returns16<-daily_returns %>% filter (Date > "2016-01-01" & Date <"2016-12-31")
str(daily_returns16)

cluster_return12[23,]
cluster_return13[3,]
cluster_return14[24,]
cluster_return15[8,]

opt12<-companylist12$Symbol[companylist12$Cluster==23]
opt12
opt13<-companylist13$Symbol[companylist13$Cluster==3]
opt13
opt14<-companylist14$Symbol[companylist14$Cluster==24]
opt14
opt15<-companylist15$Symbol[companylist15$Cluster==8]
opt15
#############################################
#Construct daily portfolio return for 2012
col_num12 <- which(colnames(daily_returns16) %in% opt12)
portfolio12 <- daily_returns16[,col_num12]
cluster_return12[23,]
portfolio12

a12<-as.character(companylist12$Symbol[companylist12$Cluster==23])
cluster_rt12<-as.data.frame(daily_returns12[,names(daily_returns12) %in% a12])
mu_rt12<-sapply(cluster_rt12, function(x) mean(x))
#Bulding up the tangency portfolio
one_vec<-rep(1,length(mu_rt12))
mu_rf<-mean(market_data1015[,5])
mu_exe12<-mu_rt12-mu_rf*one_vec
cov_cluster12<-var(cluster_rt12)
top_mat12<-solve(cov_cluster12)%*%mu_exe12
bot_val12<-as.numeric(one_vec%*%top_mat12) 
tan_vec12 <- top_mat12[,1]/bot_val12
tan_vec12<-as.vector(tan_vec12)
tan_vec12

m12<-as.matrix(portfolio12) %*% tan_vec12
final_pt12<-portfolio12 %>% mutate(op12=m12)
final_pt12

#############################################
#Construct daily portfolio return for 2013
col_num13 <- which(colnames(daily_returns16) %in% opt13)
portfolio13 <- daily_returns16[,col_num13]
cluster_return13[3,]
portfolio13

a13<-as.character(companylist13$Symbol[companylist13$Cluster==3])
cluster_rt13<-as.data.frame(daily_returns13[,names(daily_returns13) %in% a13])
mu_rt13<-sapply(cluster_rt13, function(x) mean(x))
#Bulding up the tangency portfolio
one_vec<-rep(1,length(mu_rt13))
mu_rf<-mean(market_data1015[,5])
mu_exe13<-mu_rt13-mu_rf*one_vec
cov_cluster13<-var(cluster_rt13)
top_mat13<-solve(cov_cluster13)%*%mu_exe13
bot_val13<-as.numeric(one_vec%*%top_mat13) 
tan_vec13 <- top_mat13[,1]/bot_val13
tan_vec13<-as.vector(tan_vec13)
tan_vec13

m13<-as.matrix(portfolio13) %*% tan_vec13
final_pt13<-portfolio13 %>% mutate(op13=m13)
final_pt13

#############################################
#Construct daily portfolio return for 2014
col_num14 <- which(colnames(daily_returns16) %in% opt14)
portfolio14 <- daily_returns16[,col_num14]
cluster_return14[24,]
portfolio14

a14<-as.character(companylist14$Symbol[companylist14$Cluster==24])
cluster_rt14<-as.data.frame(daily_returns14[,names(daily_returns14) %in% a14])
mu_rt14<-sapply(cluster_rt14, function(x) mean(x))
#Bulding up the tangency portfolio
one_vec<-rep(1,length(mu_rt14))
mu_rf<-mean(market_data1015[,5])
mu_exe14<-mu_rt14-mu_rf*one_vec
cov_cluster14<-var(cluster_rt14)
top_mat14<-solve(cov_cluster14)%*%mu_exe14
bot_val14<-as.numeric(one_vec%*%top_mat14) 
tan_vec14 <- top_mat14[,1]/bot_val14
tan_vec14<-as.vector(tan_vec14)
tan_vec14

m14<-as.matrix(portfolio14) %*% tan_vec14
final_pt14<-portfolio14 %>% mutate(op14=m14)
final_pt14

#############################################
#Construct daily portfolio return for 2015
col_num15 <- which(colnames(daily_returns16) %in% opt15)
portfolio15 <- daily_returns16[,col_num15]
cluster_return15[8,]
portfolio15

a15<-as.character(companylist15$Symbol[companylist15$Cluster==8])
cluster_rt15<-as.data.frame(daily_returns15[,names(daily_returns15) %in% a15])
mu_rt15<-sapply(cluster_rt15, function(x) mean(x))
#Bulding up the tangency portfolio
one_vec<-rep(1,length(mu_rt15))
mu_rf<-mean(market_data1015[,5])
mu_exe15<-mu_rt15-mu_rf*one_vec
cov_cluster15<-var(cluster_rt15)
top_mat15<-solve(cov_cluster15)%*%mu_exe15
bot_val15<-as.numeric(one_vec%*%top_mat15) 
tan_vec15 <- top_mat15[,1]/bot_val15
tan_vec15<-as.vector(tan_vec15)
tan_vec15

m15<-as.matrix(portfolio15) %*% tan_vec15
final_pt15<-portfolio15
final_pt15

#########################################################################
#Construct final matrix for daily return plot configuration 2016
pt_perf<-cbind.data.frame(daily_returns16[,1],final_pt12$op12,final_pt13$op13,final_pt14$op14,final_pt15,sp500_16[,3])
colnames(pt_perf)<-c("Date","Portfolio Return (2012)","Portfolio Return (2013)","Portfolio Return (2014)","Portfolio Return (2015)","S&P500 (2016)")
pt_perf

final_plot<-pt_perf %>% ggplot()+geom_line(aes(x=as.Date(Date),y=`S&P500 (2016)`),color="Black",alpha=1,size=1.5)+
  geom_line(aes(x=as.Date(Date),y=`Portfolio Return (2012)`),color="Red",alpha=0.6)+
  geom_line(aes(x=as.Date(Date),y=`Portfolio Return (2013)`),color="Blue",alpha=0.2)+
  geom_line(aes(x=as.Date(Date),y=`Portfolio Return (2014)`),color="Green",alpha=0.4)+
  geom_line(aes(x=as.Date(Date),y=`Portfolio Return (2015)`),color="Orange",alpha=0.6)+
  xlab("Date")+ylab(expression(mu[p]))+ geom_text(aes(x = as.Date("2016-02-01"), y = -0.2, label = "Portfolio Return (2013)"))+
  geom_text(aes(x = as.Date("2016-02-15"), y = 0.1, label = "Portfolio Return (2012)"))+
  geom_text(aes(x = as.Date("2016-05-31"), y = 0.08, label = "Portfolio Return (2014)"))+
  geom_text(aes(x = as.Date("2016-07-25"), y = 0.05, label = "Portfolio Return (2015)"))+
  geom_text(aes(x = as.Date("2017-01-01"), y = 0.01, label = "S&P500 (2016)"))+
  ggtitle("Portfolio Performance vs. S&P500 (2016)")
final_plot

rt2016<-sapply(pt_perf[,2:6], function(x) mean(x))
rt2016
sd2016<-sapply(pt_perf[,2:6], function(x) sqrt(var(x)))
sd2016
perf_matrix<-rbind.data.frame(rt2016,sd2016)
colnames(perf_matrix)<-c("Portfolio (2012)","Portfolio (2013)","Portfolio (2014)","Portfolio (2015)","S&P500 (2016)")
row.names(perf_matrix)<-c("Return","St. Dev")
perf_matrix
