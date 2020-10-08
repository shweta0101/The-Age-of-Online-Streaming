# FACTOR ANALYSIS

#Install Libraries
library(corrplot)
library(psych)
library(GPArotation)

#Importing File
data <- read.csv(file.choose(), header = T)

#Checking Structure & Creating Summary
str(data)
summary(data)

#Checking For Missing Values
sapply(data, function(x) sum(is.na(x)))

#Giving Labels
data$Usr_Frnd = as.numeric(data$Usr_Frnd, levels = c('Not Very Important','Not Important','Neutral',
                                                     'Very Important','Important'),labels = c(1,2,3,5,4))

data$Org = as.numeric(data$Org, levels = c('Not Very Important','Not Important','Neutral',
                                           'Very Important','Important'),labels = c(1,2,3,5,4))

data$Str_Ab = as.numeric(data$Str_Ab, levels = c('Not Very Important','Not Important','Neutral',
                                                 'Very Important','Important'),labels = c(1,2,3,5,4))

data$Shr_Acc = as.numeric(data$Shr_Acc, levels = c('Not Very Important','Not Important','Neutral',
                                                 'Very Important','Important'),labels = c(1,2,3,5,4))

data$Whenever = as.numeric(data$Whenever, levels = c('Not Very Important','Not Important','Neutral',
                                                     'Very Important','Important'),labels = c(1,2,3,5,4))

data$Wherever = as.numeric(data$Wherever, levels = c('Not Very Important','Not Important','Neutral',
                                                     'Very Important','Important'),labels = c(1,2,3,5,4))

data$Buff_Sp = as.numeric(data$Buff_Sp, levels = c('Not Very Important','Not Important','Neutral',
                                                 'Very Important','Important'),labels = c(1,2,3,5,4))

data$Ads = as.numeric(data$Ads, levels = c('Not Very Important','Not Important','Neutral',
                                             'Very Important','Important'),labels = c(1,2,3,5,4))

data$Avb = as.numeric(data$Avb, levels = c('Not Very Important','Not Important','Neutral',
                                           'Very Important','Important'),labels = c(1,2,3,5,4))

data$Quality = as.numeric(data$Quality, levels = c('Not Very Important','Not Important','Neutral',
                                                   'Very Important','Important'),labels = c(1,2,3,5,4))

data$Variety = as.numeric(data$Variety, levels = c('Not Very Important','Not Important','Neutral',
                                           'Very Important','Important'),labels = c(1,2,3,5,4))

data$Amt_Og = as.numeric(data$Amt_Og, levels = c('Not Very Important','Not Important','Neutral',
                                                 'Very Important','Important'),labels = c(1,2,3,5,4))

data$Upd_Freq = as.numeric(data$Upd_Freq, levels = c('Not Very Important','Not Important','Neutral',
                                                     'Very Important','Important'),labels = c(1,2,3,5,4))

data$Trans_Md = as.numeric(data$Trans_Md, levels = c('Not Very Important','Not Important','Neutral',
                                                     'Very Important','Important'),labels = c(1,2,3,5,4))

data$Trans_Safety = as.numeric(data$Trans_Safety, levels = c('Not Very Important','Not Important','Neutral',
                                               'Very Important','Important'),labels = c(1,2,3,5,4))

data$Security_PI = as.numeric(data$Security_PI, levels = c('Not Very Important','Not Important','Neutral',
                                                           'Very Important','Important'),labels = c(1,2,3,5,4))

data$Service_Typ = as.numeric(data$Service_Typ, levels = c('Not Very Important','Not Important','Neutral',
                                                           'Very Important','Important'),labels = c(1,2,3,5,4))

data$Cost = as.numeric(data$Cost, levels = c('Not Very Important','Not Important','Neutral',
                                             'Very Important','Important'),labels = c(1,2,3,5,4))

data$Plans = as.numeric(data$Plans, levels = c('Not Very Important','Not Important','Neutral',
                                               'Very Important','Important'),labels = c(1,2,3,5,4))

data$Cust_Supp = as.numeric(data$Cust_Supp, levels = c('Not Very Important','Not Important','Neutral',
                                                       'Very Important','Important'),labels = c(1,2,3,5,4))

data$Recom_Eng = as.numeric(data$Recom_Eng, levels = c('Not Very Important','Not Important','Neutral',
                                                       'Very Important','Important'),labels = c(1,2,3,5,4))

data$Recom = as.numeric(data$Recom, levels = c('Not Very Important','Not Important','Neutral',
                                               'Very Important','Important'),labels = c(1,2,3,5,4))

data$Reviews = as.numeric(data$Reviews, levels = c('Not Very Important','Not Important','Neutral',
                                                   'Very Important','Important'),labels = c(1,2,3,5,4))

#Checking For Conversion
str(data)

#Checking Data Consistency using Cronbach's Alpha
alpha(data)

# Correlation 
data.cor = cor(data)
print(data.cor, digits = 2)
corrplot(data.cor, method = "circle")

#KMO And Bartlett's Test
KMO(data.cor)  
  #Overall MSA =  0.96 , sample data is adequate

cortest.bartlett(data.cor, n = 703)  
  # P-Value = 0 < 0.05 , data.cor matrix is not identity matrix

#Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(data)) # get eigenvalues
ap <- parallel(subject=nrow(data),var=ncol(data),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

#Using Common Factor Extraction Method
fit1 = factanal(x = data, factors= 5, rotation = "varimax")   
print(fit1$loadings, digits = 2, cutoff = .4, sort = TRUE)    
                                                              

#Using Principal Component Analysis
fit2 <- principal(data, nfactors=5, rotate="varimax")         
print(fit2$loadings, digits = 2, cutoff = .4, sort = TRUE)    
                                                              

#Using Maximum Likelihood Method
parallel<-fa.parallel(data, fm='minres', fa='fa')
fit3 = fa(data, nfactors=5, rotate="varimax", fm="minres")
print(fit3$loadings,digits = 2, cutoff=0.4, sort = TRUE)      

#Factor Plot
fa.diagram(fit2)
fa.diagram(fit3)

#Factor Scores
Scores = factor.scores(data.cor, fit3$loadings)
print(Scores, digits = 4)
