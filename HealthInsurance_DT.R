# Derive DT for insurance data

library(rpart)
library(rpart.plot)
library(partykit)

indata=read.csv('insurance.csv')

indata$sex=as.factor(indata$sex)
indata$children=as.factor(indata$children)
indata$smoker=as.factor(indata$smoker)
indata$region=as.factor(indata$region)
indata$charge.range=as.factor(indata$charge.range)

t1=table(indata$Churn.)
t1
t1[1]/sum(t1)

head(indata)
#Data analysis
plot(indata$age, indata$charge.range) # directly correlated so can ignore one
plot(indata$sex, indata$charge.range) # directly correlated so can ignore one
plot(indata$bmi, indata$charge.range) # directly correlated so can ignore one
plot(indata$children, indata$charge.range) # directly correlated so can ignore one
plot(indata$smoker, indata$charge.range) # directly correlated so can ignore one
plot(indata$region, indata$charge.range) # directly correlated so can ignore one


# Decision Tree model
# With all parameters
dtmod1=rpart(charge.range ~ age+sex+bmi+children+smoker+region, data=indata, method='class',cp=0.001)


prp(dtmod1)

summary(dtmod1)

#get the rules
party_obj <- as.party.rpart(dtmod1, data = TRUE)
decisions <- partykit:::.list.rules.party(party_obj)
cat(paste(decisions, collapse = "\n"))


