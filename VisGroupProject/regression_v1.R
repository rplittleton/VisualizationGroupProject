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
  #stat_smooth(method="glm", method.args = list(family = binomial("logit"), maxit = 100), formula=Cybersecurity.Measures ~.,
              #alpha=0.2, size=2, aes(fill=Impact.Type)) 
  ggtitle("Percent of Cybersecurity Measures Invested Resulting From Cyber Impact") + 
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Percent") + ylab("Cybersecurity.Measures") + theme_linedraw()
  #+ theme(axis.text.x = element_text(angle = 45))
plot1 <- ggplotly(mod.measures_impacts_q1.plot)

#train_mi1_1 <- train_mi %>% filter(Org.Size == 'Small enterprises (10 to 49 employees)')
mod.measures_impacts_q1_1.plot <- ggplot(train_mi1_1, aes(Percent,Cybersecurity.Measures, color=Impact.Type)) +
  #stat_smooth(method="glm", method.args = list(family = binomial("logit"), maxit = 100), formula=Cybersecurity.Measures ~.,
  #alpha=0.2, size=2, aes(fill=Impact.Type)) 
  ggtitle("Small Size Firms - Percent of Cybersecurity Measures Invested Resulting From Cyber Impact") + 
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Percent") + ylab("Cybersecurity.Measures") + theme_linedraw()
#+ theme(axis.text.x = element_text(angle = 45))
plot1 <- ggplotly(mod.measures_impacts_q1.plot)

#Visual for model 2:
train_mi2 <- train_mi[complete.cases(train_mi), ]
mod.measures_impacts_q2.plot <- ggplot(train_mi2, aes(Cybersecurity.Measures,Impact.Type)) +
  geom_point(alpha=.5) +
  geom_smooth(method="glm", data = train_mi2, method.args = list(family = binomial("logit")), formula = Cybersecurity.Measures ~ Impact.Type) + 
  xlab("Cybersecurity.Measures") + ylab("Impact.Type") + ggtitle("Cybersecurity Measures by Impact Type Model") + 
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle = 45))
  theme_linedraw() + theme(axis.text.x = element_text(angle = 45,hjust = 1))
  #geom_jitter(height = 0.05)
  #geom_point(position=position_jitter(height=0.03, width=0)) +
mod.measures_impacts_q2.plot

#For example, we see that investing in network security and anti-malware is a viable
#proposition to mitigate against the impact of reduced employee productivity and
#loss of customer trust
#Next steps: test new models and visuals, account for subset data (i.e. compare Total, all enterprises vs specific org type)

#Visual for model 2:
vx <- sqldf('select "Industry", "Incident.Type", "Investing.Reasons",round(avg("Percent"),2) as "Percent" from incidents_reasons group by "Industry","Incident.Type", "Investing.Reasons" order by "Percent" DESC')
#vxx <- vx %>% filter(Industry == "Professional, scientific and technical services [54]")
plotel3 <- ggplot(vx,aes(reorder(Investing.Reasons, -Percent),Percent)) + 
  geom_bar(stat = "summary", fun.y = "mean", width=0.1,position = position_dodge(width = 0.2), aes(fill = Incident.Type)) + 
  theme(panel.background = element_blank(),aspect.ratio = 2/1) + 
  theme(axis.text.x = element_text(angle = 90,hjust = 0.5)) +
  xlab("Reasons To Invest (Investing.Reasons) ") + ylab("Average Percent of Organizations Invested") + ggtitle("Cybersecurity Investment Percentage by Investment Reason") + coord_flip()

#mod.measures_impacts_q3.plot <-  ggplot(incidents_reasons, aes(Incident.Type,Investing.Reasons, fill = Percent)) +
#  geom_bar(stat="identity") + 
#  stat_smooth(method="glm", data = incidents_reasons, method.args = list(family = binomial("logit")), formula = as.factor(incidents_reasons$Org.Size), as.factor(incidents_reasons$Investing.Reasons)) + 
#  xlab("Incident.Type") + ylab("Impact.Type") + ggtitle("Cybersecurity Measures by Impact Type Model") + 
#  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle = 45))
#  theme_linedraw() + theme(axis.text.x = element_text(angle = 45,hjust = 0.8))

#Sys.setenv("plotly_username"="jeching")
#Sys.setenv("plotly_api_key"="#####") #Removed for privacy

plot1 <- ggplotly(mod.measures_impacts_q1.plot) %>% layout(showlegend = FALSE)
api_create(plot1, filename = "Percent-Cybersecurity-Measures-By-Impact-Type")
  
plot2 <- ggplotly(mod.measures_impacts_q2.plot ) %>% layout(showlegend = FALSE)
api_create(plot2, filename = "Cybersecurity-Measures-by-Impact-Type-Model")

plot3 <- ggplotly(plotel3  )
api_create(plot3, filename = "Average-Percent-of-Organizations-Invested")

