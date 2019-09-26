library(dplyr)
library(ggplot2)
measures <- read.csv("22100001.csv")
reasons <- read.csv("22100056.csv")
incidents  <- read.csv("22100076.csv")
impacts <- read.csv("22100077.csv")
#police <- read.csv("22100078.csv")
freq <- read.csv("22100079.csv")
joinedtable <- read.csv("JoinedTable.csv")

#measures and impacts can be column joined as they contain the same data
#measures data: what security measures organizations have put in place to mitigate incidents (i.e. anti-malware)
#impacts data: the impact resulted in the incidents (i.e. Loss of revenue)
df1 <- cbind(measures,impacts$Impacts.of.cyber.security.incidents)
df1 <- tbl_df(df1)
df1 <- df1 %>% select(GEO,Cyber.security.measures,North.American.Industry.Classification.System..NAICS.,Size.of.enterprise,UOM,VALUE,STATUS,`impacts$Impacts.of.cyber.security.incidents`)

#incidents and reasons can be joined as they contain same data      
#incidents data: the actual incident/reason for the cyberattack
#reasons data: reason for investing in cybersecurity measures
df2 <- cbind(incidents,reasons$Main.reasons.for.spending.time.or.money.on.cyber.security.measures.and.or.related.skills.training)
df2 <- tbl_df(df2)
df2 <- df2 %>% select(GEO,Cyber.security.incidents,North.American.Industry.Classification.System..NAICS.,Size.of.enterprise,UOM,VALUE,STATUS,`reasons$Main.reasons.for.spending.time.or.money.on.cyber.security.measures.and.or.related.skills.training`)

freq_df <- tbl_df(freq)
freq_df <- freq_df %>% select(GEO,Frequency.of.cyber.security.incidents,North.American.Industry.Classification.System..NAICS.,Size.of.enterprise,UOM,VALUE,STATUS)

#example frequency plot of size of enterprise vs # of reported incidents
freq_df_agg <- freq_df %>% group_by(Size.of.enterprise) %>% summarise(total = sum(VALUE,na.rm = TRUE)) 
ggplot(data = freq_df_agg, aes(x = Size.of.enterprise,y = total)) + geom_bar(stat = "identity") + ggtitle("Size of Enterprise vs. Total Reported Incidents")


