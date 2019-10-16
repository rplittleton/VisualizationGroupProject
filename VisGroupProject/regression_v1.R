library(dplyr)
library(ggplot2)
library(glmnet)
library(plyr)
#load data files
setwd("~/Documents/GitHub/VisualizationGroupProject/VisGroupProject/")
measures <- read.csv("22100001.csv")
reasons <- read.csv("22100056.csv")
incidents  <- read.csv("22100076.csv")
impacts <- read.csv("22100077.csv")
#police <- read.csv("22100078.csv")
freq <- read.csv("22100079.csv")
dat <- read.csv("JoinedTable.csv")

#data cleaning
#dat[is.na(dat)] <- 0
#colnames(dat)[1:2] <- c("Industry","Org.Size")
#Use better naming
#colnames(dat)[3:length(dat)] <- c("Industry","Disrupt.or.Deface","Steal.Information","Steal.Money","Steal.Intellectual.Property","Access.Unauthorized.Area","Monitor.and.Track.Activity","Unknown.Motive","Mobile.Security","Anti.Malware","Web.Security","Email.Security","Network.Security","Data.Protection","POS.Security","Software.Security","Hardware.Management","Identity.and.Access.Management","Physical.Access.Controls","No.Measures","Unknown","Business.was.Impacted","Disrupt.or.Deface","Steal.Information","Steal.Money","Steal.Intellectual.Property","Access.Unauthorized.Areas","Monitor.and.Track.activity","Unknown.Motive","Business.not.impacted","Loss.of.revenue","Loss.of.suppliers..customers..or.partners","Additional.repair.or.recovery.costs","Paid.ransom.payment","Prevented.the.use.of.resources.or.services..desktop..email.","Prevented.employees.from.carrying.out.day.to.day.work","Additional.time.required.by.employees.to.respond.to.the.cyber.security.incidents.","Damage.to.the.reputation.of.the.business","Fines.from.regulators.or.authorities","Discouraged.business.from.carrying.out.a.future.activity.that.was.planned","Minor.incident.s..impact.was.minimal.to.the.business","Business.was.not.impacted.in.any.of.the.ways.described","Do.not.know.or.do.not.know.the.full.extent.of.the.impact","Incidents.Reported.to.Police","Protect.Reputation","Protect.personal.information","Protect.trade.secrets","Compliance.with.laws","Past.Incidents","Past.Downtime","Prevent.Fraud","Secure.Continuity.of.Operations","Business.Does.not.spend.money.on.security")
#consider round averages to nearest whole number for prediction (?)
#rather do not use the dat (joined) table for prediction 


##### CLEAN DATA #####

#measures and impacts can be column joined as they contain the same data
#measures data: what security measures organizations have put in place to mitigate incidents (i.e. anti-malware)
#impacts data: the impact resulted in the incidents (i.e. Loss of revenue)
#use this dataframe for predicting measures and impacts risk questions 
measures_impacts <- cbind(measures,impacts$Impacts.of.cyber.security.incidents)
measures_impacts <- tbl_df(measures_impacts )
measures_impacts <- measures_impacts %>% select(Cyber.security.measures,North.American.Industry.Classification.System..NAICS.,Size.of.enterprise,VALUE,STATUS,`impacts$Impacts.of.cyber.security.incidents`)
colnames(measures_impacts)[grep("VALUE", colnames(measures_impacts))] <- "Percent"
colnames(measures_impacts)[ncol(measures_impacts)] <- "Impact.Type"
colnames(measures_impacts)[1:3] <- c("Cybersecurity.Measures","Industry","Org.Size")
colnames(measures_impacts)[5] <- "Rating"
measures_impacts$Percent <- measures_impacts$Percent/100

#incidents and reasons can be joined as they contain same data      
#incidents data: the actual incident/reason for the cyberattack
#reasons data: reason for investing in cybersecurity measures
#use this dataframe for predicting incidents and effect of investment reasons questions 
incidents_reasons <- cbind(incidents,reasons$Main.reasons.for.spending.time.or.money.on.cyber.security.measures.and.or.related.skills.training)
incidents_reasons <- tbl_df(incidents_reasons)
incidents_reasons <- incidents_reasons %>% select(Cyber.security.incidents,North.American.Industry.Classification.System..NAICS.,Size.of.enterprise,VALUE,STATUS,`reasons$Main.reasons.for.spending.time.or.money.on.cyber.security.measures.and.or.related.skills.training`)
colnames(incidents_reasons)[1:ncol(incidents_reasons)] <- c("Incident.Type","Industry","Org.Size","Percent","Rating","Investing.Reasons")
incidents_reasons$Percent <- incidents_reasons$P/100
#### PREDICTION ####
# Define question for Linear and Logistic Regression for presenting results in a visualization
# i.e. For a specific organization size, can we predict what cyberecurity measures to invest in  
# taking account all other variables for an infosec program?


#Data Wrangling - Cybersecurity.Measures is too long and should be factored
#print(levels(measures_impacts$Cybersecurity.Measures))

new_measures_levels <- c("Anti-malware Software","No Security Measures","None","Data protection",
                         "Email Security","Hardware Asset Management","Identity Asset Management","Mobile Security",
                         "Network Security","Physical Access Controls", "Point-Of-Sale (POS) Security",
                         "Sofware Application Security","Web Security")
levels(measures_impacts$Cybersecurity.Measures) <- new_measures_levels
measures_impacts$Cybersecurity.Measures <- as.character(measures_impacts$Cybersecurity.Measures)
#Create train/test set3
#train_mi <- measures_impacts[1:ceiling(nrow(measures_impacts)*2/3),]
#test_mi <- measures_impacts[ceiling(nrow(measures_impacts)*2/3) + 1:nrow(measures_impacts),]
set.seed(12345)
sample_size <- floor(0.70 * nrow(measures_impacts))
train_index <- sample(seq_len(nrow(measures_impacts)), size = sample_size)
train_mi <- measures_impacts[train_index,]
test_mi <- measures_impacts[-train_index,]

#Create logistic regression model
#mod.measures_impacts_q1 <- glm(Percent ~., data=train_mi, family=binomial)
#summary(mod.measures_impacts_q1) #we see that Cybersecurity.Measures has too high a p-value, so it's not a good predictor
mod.measures_impacts_q1 <- glm(Cybersecurity.Measures ~. , data=train_mi, family = binomial("logit"), maxit = 100)
summary(mod.measures_impacts_q1)
anova(mod.measures_impacts_q1, test="Chisq")
#We see model does not fair well for Org.Size or Rating, need to try different formulas
#Returns warning glm.fit: fitted probabilities numerically 0 or 1 occurred 
#Next step: try glm for regression model on each feature

#Visual for model 1:
mod.measures_impacts_q1.plot <- ggplot(train_mi, aes(Percent,Cybersecurity.Measures, color=Impact.Type)) +
  stat_smooth(method="glm", method.args = list(family = binomial("logit"), maxit = 100), formula=Cybersecurity.Measures ~.,
              alpha=0.2, size=2, aes(fill=Impact.Type)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Percent") + ylab("Cybersecurity.Measures") 
  #+ theme(axis.text.x = element_text(angle = 45))
mod.measures_impacts_q1.plot 

#For example, we see that investing in network security and anti-malware is a viable
#proposition to mitigate against the impact of reduced employee productivity and
#loss of customer trust

#Next steps: test new models and visuals, account for subset data (i.e. compare Total, all enterprises vs specific org type)

