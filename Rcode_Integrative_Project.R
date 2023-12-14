
#reading file
chrissy=read.csv("/Users/chrissycho/Desktop/IP final data/ChrissyIP0315_cleaned4 (1).csv")
head(chrissy)
str(chrissy)

#dummy coding
chrissy$HighAnxOrAvoid=as.numeric(chrissy$HighAnx==1 | chrissy$HighAvoid==1)
#another method
chrissy$HighAnxOrAvoid=ifelse(chrissy$HighAnx==1 | chrissy$HighAvoid==1, 1, 0)
#how many?
sum(chrissy$HighAnxOrAvoid==1)
#82 1s, 34 0s
82/116
34/116

#how many had better sleep function?
sum(chrissy$postsleep_inv-chrissy$presleep_inv>=0)

#correlation matrix to view basic correlations and decide covariates
#select non-factor variables to run (check str(chrissy))
round(cor(chrissy),2)

#making variables into factors
chrissy$HighAnxOrAvoid=as.factor(chrissy$HighAnxOrAvoid)
chrissy$HighAnx=as.factor(chrissy$HighAnx)
chrissy$HighAvoid=as.factor(chrissy$HighAvoid)

#linear regression, fitting basic models
model1=lm(postsleep_inv~HighAnx+HighAvoid+presleep_inv+predep_inv, data=chrissy)
summary(model1)

model2=lm(postsleep_inv~Anxiety+Avoidance+presleep_inv+predep_inv, data=chrissy)
summary(model2)

model3=lm(postsleep_inv~Anxiety+Avoidance+presleep_inv+predep_inv+postdep_inv, data=chrissy)
summary(model3)

model4=lm(postsleep_inv~HighAnx+HighAvoid+presleep_inv+predep_inv+HighAnx*HighAvoid, data=chrissy)
summary(model4)

model5=lm(postsleep_inv~HighAnxOrAvoid+presleep_inv+predep_inv, data=chrissy)
summary(model5)

#plotting pretest-posttest by group
plot(chrissy$presleep_inv, chrissy$postsleep_inv, col=c("blue", "green")[chrissy$HighAnxOrAvoid],
            xlab="Pretest Sleep Function", ylab="Posttest Sleep Function", main="Scatterplot and Regression of 
     Pretest on Posttest Sleep Function")
legend(x="bottomright", legend=c("Low Anx&Avoid", "High Anx/Avoid"), col=c("blue", "green"), pch=1)
abline(reg=lm(postsleep_inv~presleep_inv, data=subset(chrissy,HighAnxOrAvoid==1)), col="green")
abline(reg=lm(postsleep_inv~presleep_inv, data=subset(chrissy,HighAnxOrAvoid==0)), col="blue")


#t-test
leveneTest(presleep_inv~HighAnxOrAvoid, data=chrissy)
#p=.3479, homogen kept
t.test(presleep_inv~HighAnxOrAvoid, data=chrissy, alternative="greater", var.equal=TRUE)
#p=.81, not sig

leveneTest(postsleep_inv~HighAnxOrAvoid, data=chrissy)
#p=.747, homogen kept
t.test(postsleep_inv~HighAnxOrAvoid, data=chrissy, alternative="less", var.equal=TRUE)
#p=.114, not sig

#fitting ANCOVA model for Research Question 1, with adjusted regression line
rschq1=lm(postsleep_inv~presleep_inv+HighAnxOrAvoid+presleep_inv*HighAnxOrAvoid+predep_inv, data=chrissy)
summary(rschq1)

#multiple regression without covariate 
rschq3=lm(postsleep_inv~presleep_inv+HighAnxOrAvoid+presleep_inv*HighAnxOrAvoid, data=chrissy)
summary(rschq3)
#fitting keller advised model
rschqv2=lm(postsleep_inv~Anxiety+Avoidance+presleep_inv+predep_inv+Anxiety*Avoidance, data=chrissy)
summary(rschqv2)

rschqv3=lm(postsleep_inv~Anxiety+Avoidance, data=chrissy)
summary(rschqv3)

#interaction is not sig, run w/o interaction
followupmaineff=lm(postsleep_inv~presleep_inv+HighAnxOrAvoid+predep_inv, data=chrissy)
summary(followupmaineff)

#calculating model
library(car)
vif(rschq1)
vif(rschqv2)
vif(model2)

#calculating residual of the model, drawing qqplot
rschq1_residual=resid(rschq1)
qqnorm(rschq1_residual)
qqline(rschq1_residual)

rschqv2_residual=resid(rschqv2)
qqnorm(rschqv2_residual)
qqline(rschqv2_residual)

rschqv3_residual=resid(rschqv3)
qqnorm(rschqv3_residual)
qqline(rschqv3_residual)
#performing breuch pagan test
library(lmtest)
bptest(rschq1)
#BP(6)=12.349, p=.055
bptest(rschqv2)

#making apa tables
install.packages("apaTables")
library(apaTables)
apa.cor.table(chrissy[,c(4:11, 13)], filename="CorrTable.doc", table.number = 2)
apa.reg.table(rschq1, filename="RegTable.doc", table.number=3)
apa.reg.table(model4, filename="RegTable.doc", table.number=3)




