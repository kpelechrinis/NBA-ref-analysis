library(vroom) 
library(reshape2)
library(ggplot2)

df <- vroom("https://raw.githubusercontent.com/atlhawksfanatic/L2M/master/1-tidy/L2M/L2M.csv")
df = df[which(!is.na(df$decision)),]


### Figures 1A-1B
## call recall per year
## CC/(CC+INC)
call_recall = data.frame(year=c(),rc = c())
for (s in c(2015,2016,2017,2018,2019,2020,2021,2022)){
  cc = dim(df[df$season==s & df$decision=="CC",])[1]
  inc = dim(df[df$season==s & df$decision=="INC",])[1]
  call_recall <- rbind(call_recall,data.frame(year=s,rc=cc/(inc+cc)))
}

## call precision per year 
## CC/(CC+IC)

call_precision = data.frame(year=c(),pc = c())
for (s in c(2015,2016,2017,2018,2019,2020,2021,2022)){
  cc = dim(df[df$season==s & df$decision=="CC",])[1]
  ic = dim(df[df$season==s & df$decision=="IC",])[1]
  call_precision = rbind(call_precision,data.frame(year=s,pc=cc/(ic+cc)))
}

call_pr = data.frame(year = call_precision$year,pc = call_precision$pc, rc = call_recall$rc)
melted_calls <- melt(call_pr, id = "year")
colnames(melted_calls)[2] = "Metric"
melted_calls$Metric = as.character(melted_calls$Metric)
melted_calls[melted_calls$Metric=="rc",]$Metric = "Call Recall"
melted_calls[melted_calls$Metric=="pc",]$Metric = "Call Precision"

#Figure 1A
ggplot(melted_calls, aes(x = year, y = value, color = Metric)) + geom_point()+geom_line() + xlab("Season")+ylab("Value")+theme_bw(base_size=16)+ylim(c(0.5,1))

## showcase that different types of calls have different precision and recall
call_types = c("Turnover: Traveling","Foul: Offensive", "Foul: Defense 3 Second")

call_precision_type = data.frame(year = c(), call_type = c(), pc = c())

for (s in c(2015,2016,2017,2018,2019,2020,2021,2022)){
  for (c in call_types){
    tmp = as.data.frame(table(df[df$season==s & df$call_type==c,]$decision))
    cc = tmp[which(tmp$Var1=="CC"),]$Freq
    if (length(cc) == 0){cc = 0}
    ic = tmp[which(tmp$Var1=="IC"),]$Freq
    if (length(ic)==0){ic = 0}
    call_precision_type = rbind(call_precision_type,data.frame(year=s,call_type = c,pc=cc/(ic+cc)))
  }
}

#Figure 1B (left)
ggplot(call_precision_type, aes(x = year, y = pc, color = call_type)) + geom_point()+geom_line() + xlab("Season")+ylab("Call Precision")+theme_bw(base_size=16)+ylim(c(0.5,1))

call_recall_type = data.frame(year = c(), call_type = c(), rc = c())

for (s in c(2015,2016,2017,2018,2019,2020,2021,2022)){
  for (c in call_types){
    tmp = as.data.frame(table(df[df$season==s & df$call_type==c,]$decision))
    cc = tmp[which(tmp$Var1=="CC"),]$Freq
    if (length(cc) == 0){cc = 0}
    inc = tmp[which(tmp$Var1=="INC"),]$Freq
    if (length(inc)==0){inc = 0}
    call_recall_type = rbind(call_recall_type,data.frame(year=s,call_type = c,rc=cc/(inc+cc)))
  }
}

#Figure 1B (right)
ggplot(call_recall_type, aes(x = year, y = rc, color = call_type)) + geom_point()+geom_line() + xlab("Season")+ylab("Call Recall")+theme_bw(base_size=16)

## plot the overall call rate precision/recall for each type of violation over the 7 year of data

violations = unique(df$call_type)
violation_rates.df = data.frame(Violation=c(),Recall=c(),Precision=c(),Sample = c())

for (v in violations){
  if (is.na(v)){
    next
  }
  tmp = df[which(df$call_type==v),]
  tmp_table = as.data.frame(table(tmp$decision))
  cc = tmp_table[which(tmp_table$Var1=="CC"),]$Freq
  if (length(cc) == 0){cc = 0}
  inc = tmp_table[which(tmp_table$Var1=="INC"),]$Freq
  if (length(inc) == 0){inc = 0}
  ic = tmp_table[which(tmp_table$Var1=="IC"),]$Freq
  if (length(ic) == 0){ic = 0}
  violation_rates.df <- rbind(violation_rates.df,data.frame(Violation=v,Recall=cc/(cc+inc), Precision = cc/(cc+ic),Sample=cc+ic+inc))
}

ggplot(violation_rates.df,aes(x=Precision,y=Recall))+geom_point(aes(size=Sample))+theme_bw(base_size=17)

# home court bias
source("hcb.R")
# player specific biases
source("player_bias.R")
# team specific biases
source("team_bias.R")
# racial bias
source("racial_bias.R")
