#CHI-SQUARE TEST OF INDEPENDENCE

#Used to find association (significant correlation) between two categorical variables.
#H0: The two variables are independent.
#H1: The two variables relate to each other.

#Import data
data = read.csv(file.choose(), header = T)
str(data)
summary(data)

#Independence between Gender & Awareness
xtabs(~ Awareness + Gender , data)
chisq.test(data$Gender, data$Awareness, correct = F)
## p-value = 0.03065 < 0.05
## Reject Ho

#Independence between Age & Awareness
xtabs(~ Awareness + Age , data)
chisq.test(data$Age, data$Awareness, correct = F)
## p-value = 2.931e-10 < 0.05
## Reject Ho

#Independence between Employment & Awareness
xtabs(~ Awareness + Employment , data)

#or

table(data$Employment, data$Awareness)

fisher.test(data$Employment, data$Awareness, simulate.p.value=TRUE)
## p-value = 0.0004998 < 0.05
## Reject Ho

#Independence between Education & Awareness
xtabs(~ Awareness + Education , data)
fisher.test(data$Education, data$Awareness, simulate.p.value=TRUE)
## p-value = 0.0004998 < 0.05
## Reject Ho

#Independence between Income & Awareness
xtabs(~ Awareness + Income , data)
chisq.test(data$Income, data$Awareness, correct = F)
## p-value = 3.536e-06 < 0.05
## Reject Ho

