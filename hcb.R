library(vroom)
library(xtable)
## simulation function 

empirical_netsim2 <- function(name, df, league_recall_call, n = 100){
  LEAGUE_AVG_recall = 0.22 ## from data
  LEAGUE_AVG_precision = 0.31 ## from data
  df_tmp = df[which(df$committing_side==name),]
  df_tmp2 = df[which(df$disadvantaged_side==name),]
  sims <- rep(0,n)
  for (i in 1:n){
    s = 0
    s2 = 0
    for (v in 1:dim(df_tmp)[1]){
      if (length(which(league_recall_call$violation==df_tmp[v,]$call_type))>0){
        tmp_r = runif(1)
        if (tmp_r < league_recall_call[which(league_recall_call$violation==df_tmp[v,]$call_type),]$p1){
          s = s+1
        }else{
          if (tmp_r < league_recall_call[which(league_recall_call$violation==df_tmp[v,]$call_type),]$p2){
            s2 = s2+1
          }
        }
      }else{
        tmp_r = runif(1)
        if (tmp_r < LEAGUE_AVG_recall){
          s = s + 1
        }else{
          if (tmp_r < LEAGUE_AVG_precision){
            s2 = s2 + 1
          }
        }
      }
    }
    #s2 = 0 
    for (v in 1:dim(df_tmp2)[1]){
      if (length(which(league_recall_call$violation==df_tmp2[v,]$call_type))>0){
        tmp_r = runif(1)
        if (tmp_r < league_recall_call[which(league_recall_call$violation==df_tmp2[v,]$call_type),]$p1){
          s2 = s2+1
        }else{
          if (tmp_r < league_recall_call[which(league_recall_call$violation==df_tmp2[v,]$call_type),]$p2){
            s = s+1
          }
        }
      }else{
        tmp_r = runif(1)
        if (runif(1) < LEAGUE_AVG_recall){
          s2 = s2 + 1
        }else{
          if (tmp_r < LEAGUE_AVG_precision){
            s= s+1
          }
        }
      }
    }
    sims[i] = s-s2
  }
  return(sims)
}

## 

df_ <- vroom("https://raw.githubusercontent.com/atlhawksfanatic/L2M/master/1-tidy/L2M/L2M.csv")
df_ = df_[which(!is.na(df_$decision)),]
df_ = df_[as.character(df_$decision)!="CNC",]
results_home <- data.frame(team=c(),season=c(),pval=c(),effect = c(), size=c())
for (y in c("reg","playoffs","all")){
  for (p in c("home")){
    league_recall_call = data.frame(violation=c(),p1=c(), p2=c())
    if (y=="reg"){df_3y = df_[df_$playoff==FALSE ,]}
    if (y == "playoffs"){df_3y = df_[df_$playoff==TRUE ,]}
    if (y== "all"){df_3y = df_}
    for (v in 1:length(unique(df_$call_type))){
      tmp = df_[which(df_$call_type==unique(df_$call_type)[v]),]
      if (dim(tmp)[1]>0){
        tmptable = as.data.frame(table(tmp$decision))
        cc = tmptable[which(tmptable$Var1=="CC"),]$Freq
        if (length(cc) == 0){cc = 0}
        inc = tmptable[which(tmptable$Var1=="INC"),]$Freq
        if (length(inc)==0){inc = 0}
        ic = tmptable[which(tmptable$Var1=="IC"),]$Freq
        if (length(ic)==0){ic = 0}
        league_recall_call <- rbind(league_recall_call,data.frame(violation=unique(df_$call_type)[v],p1=inc/(inc+cc+ic),p2 = (ic+inc)/(ic+cc+inc)))
      }
    }
    t_real_tmp1 = as.data.frame(table(df_3y[which(df_3y$committing_side == p),]$decision))
    inc1 = t_real_tmp1[which(t_real_tmp1$Var1=="INC"),]$Freq
    if (length(inc1)==0){inc1 = 0}
    t_real_tmp2 = as.data.frame(table(df_3y[which(df_3y$disadvantaged_side == p),]$decision))
    inc2 = t_real_tmp2[which(t_real_tmp2$Var1=="INC"),]$Freq
    if (length(inc2)==0){inc2 = 0}
    ic1 = t_real_tmp2[which(t_real_tmp2$Var1=="IC"),]$Freq
    if (length(ic1) ==0){ic1 =0}
    ic2 = t_real_tmp1[which(t_real_tmp1$Var1=="IC"),]$Freq
    if (length(ic2) ==0){ic2 =0}
    t_real = (inc1+ic1) - (inc2+ic2)
    t_sim <- empirical_netsim2(p,df=df_3y,league_recall_call,n=100)
    emp_pval = length(which(t_sim>=t_real))/100
    results_home <- rbind(results_home,data.frame(team=p,season=y,pval=emp_pval,effect=t_real-mean(t_sim),size=dim(df_3y)[1]))
  }
}

xtable(results_home)
