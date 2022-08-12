library(vroom)
library(xtable)

## simulation function

empirical_netsim <- function(name, df, league_recall_call, n = 100){
  LEAGUE_AVG_recall = 0.22 ## from data
  LEAGUE_AVG_precision = 0.31
  df_tmp = df[which(df$committing==name),]
  df_tmp2 = df[which(df$disadvantaged==name),]
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
          if (tmp_r<  LEAGUE_AVG_precision){
            s2 = s2+1
        }
      }
      }
    }
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
        if (tmp_r < LEAGUE_AVG_recall){
          s2 = s2 + 1
        }else{
          if (tmp_r < LEAGUE_AVG_precision){
            s = s+1
          }
        }
      }
    }
    sims[i] = s-s2
  }
  return(sims)
}

df_3y <- vroom("https://raw.githubusercontent.com/atlhawksfanatic/L2M/master/1-tidy/L2M/L2M.csv")
df_3y = df_3y[which(!is.na(df_3y$decision)),]
df_3y = df_3y[as.character(df_3y$decision)!="CNC",]

## estimate the net benefit above expectation on infraction call
## when committing and missed is +1 when disadvantaged and missed is -1
## keep only those with 100 data points in total (either committing or disadvantaged) -- top 10th percentile
## quantile(table(c(df_3y$committing,df_3y$disadvantaged)))
players = unique(c(df_3y$disadvantaged,df_3y$committing))

results.df <- data.frame(player=c(),pval = c(), effect = c(), size = c())

for (p in 1:length(players)){
  if (dim(df_3y[which(df_3y$committing == players[p]),])[1]+dim(df_3y[which(df_3y$disadvantaged == players[p]),])[1]>100){
    league_recall_call = data.frame(violation=c(),p1=c(),p2=c())
    
    for (v in 1:length(unique(df_3y$call_type))){
      tmp = df_3y[which(df_3y$call_type==unique(df_3y$call_type)[v] & (df_3y$committing!=players[p] & df_3y$disadvantaged!=players[p])),]
      if (dim(tmp)[1]>0){
        tmptable = as.data.frame(table(tmp$decision))
        cc = tmptable[which(tmptable$Var1=="CC"),]$Freq
        if (length(cc) == 0){cc = 0}
        inc = tmptable[which(tmptable$Var1=="INC"),]$Freq
        if (length(inc)==0){inc = 0}
        ic = tmptable[which(tmptable$Var1=="IC"),]$Freq
        if (length(ic)==0){ic = 0}
        league_recall_call <- rbind(league_recall_call,data.frame(violation=unique(df_3y$call_type)[v],p1=inc/(inc+ic+cc), p2 = (inc+ic)/(inc+cc+ic)))
      }
    }
    t_real_tmp1 = as.data.frame(table(df_3y[which(df_3y$committing == players[p]),]$decision))
    inc1 = t_real_tmp1[which(t_real_tmp1$Var1=="INC"),]$Freq
    if (length(inc1)==0){inc1 = 0}
    t_real_tmp2 = as.data.frame(table(df_3y[which(df_3y$disadvantaged == players[p]),]$decision))
    inc2 = t_real_tmp2[which(t_real_tmp2$Var1=="INC"),]$Freq
    if (length(inc2)==0){inc2 = 0}
    ic1 = t_real_tmp2[which(t_real_tmp2$Var1=="IC"),]$Freq
    if (length(ic1) ==0){ic1 =0}
    ic2 = t_real_tmp1[which(t_real_tmp1$Var1=="IC"),]$Freq
    if (length(ic2) ==0){ic2 =0}
    t_real = (inc1+ic1) - (inc2+ic2)
    t_real = inc1 - inc2
    t_sim <- empirical_netsim(players[p],df=df_3y,league_recall_call,n=100)
    emp_pval = length(which(t_sim>=t_real))/100
    results.df <- rbind(results.df,data.frame(player=players[p],pval=emp_pval,effect=t_real-mean(t_sim), size=dim(df_3y[which(df_3y$committing == players[p]),])[1]+dim(df_3y[which(df_3y$disadvantaged == players[p]),])[1]))
  }
}

xtable(results.df)
