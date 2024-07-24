pbp <- read.csv("pbp.csv")
technicals <- c("Hanging Technical","Taunting Technical","Technical","Unsportsmanlike Technical")
refs_race.df <- read.csv("refs.csv")
players_race.df <- read.csv("player_race2.csv")
pids.df <- read.csv("nba_playerIds.csv")
boxscores <- read.csv("boxscores.csv")
boxscores$MIN = gsub(":"," ",boxscores$MIN)
boxscores <- boxscores %>% separate(MIN, c('minutes', 'seconds'))
boxscores$toc = as.numeric(boxscores$minutes)+(as.numeric(boxscores$seconds)/60)
boxscores[boxscores$minutes=="",]$toc = 0
boxscore_merge = merge(boxscores,players_race.df[,c("PLAYER_NAME","Race")],by="PLAYER_NAME",all.x=TRUE)
tech.df = pbp[pbp$call_type %in% technicals,]

tech.df$ref_race = rep("",dim(tech.df)[1])
for (i in 1:dim(tech.df)[1]){
  if (tech.df[i,]$ref!=""){
    tech.df[i,]$ref_race = refs_race.df[refs_race.df$Ref==tech.df[i,]$ref,]$race
  }
}

pids.df$player_race = rep("",dim(pids.df)[1])
for (i in 1:dim(pids.df)[1]){
  tmp = players_race.df[players_race.df$PLAYER_NAME==pids.df[i,]$PLAYER_NAME,]
  if (dim(tmp)[1]>0){
    pids.df[i,]$player_race = tmp[1,]$Race
  }
}

tech.df$p_race = rep("",dim(tech.df)[1])

for (i in 1:dim(tech.df)[1]){
  if (dim(pids.df[pids.df$id==tech.df[i,]$pid,])[1]>0){
    tech.df[i,]$p_race = pids.df[pids.df$id==tech.df[i,]$pid,]$player_race
  }
}

## create two columns in the pbp.df that have the race of the player getting the tech and the ref
games = unique(pbp$game_id)

## only use games that we know all three referees
games_added <- c()
for (g in 1:length(games)){
  gid = games[g]
  refs = unique(pbp[pbp$game_id==gid & pbp$ref!="",]$ref)
  if (length(refs)!=3){
    next
  }
  games_added <- append(games_added,gid)
}


wb_n = length(which(tech.df$ref_race == "white" & tech.df$p_race == "black" & tech.df$game_id %in% games_added))
ww_n = length(which(tech.df$ref_race == "white" & tech.df$p_race == "white" & tech.df$game_id %in% games_added))
bb_n = length(which(tech.df$ref_race=="black" & tech.df$p_race == "black" & tech.df$game_id %in% games_added))
bw_n = length(which(tech.df$ref_race=="black" & tech.df$p_race == "white" & tech.df$game_id %in% games_added))

ww_min = 0
wb_min=0
bb_min=0
bw_min = 0
for (i in 1:dim(refs_race.df)[1]){
  games_f= unique(pbp[pbp$ref == refs_race.df[i,]$Ref & pbp$game_id %in% games_added,]$game_id)
  for (g in 1:length(games_f)){
    bgame = boxscore_merge[boxscore_merge$GAME_ID==games_f[g],]
    if (refs_race.df[i,]$race=="white"){
      ww_min = ww_min+sum(as.numeric(bgame[bgame$Race=="white",]$toc),na.rm=T)
      wb_min = wb_min +sum(as.numeric(bgame[bgame$Race=="black",]$toc),na.rm=T)
    }
    if (refs_race.df[i,]$race=="black"){
      bw_min = bw_min+sum(as.numeric(bgame[bgame$Race=="white",]$toc),na.rm=T)
      bb_min = bb_min +sum(as.numeric(bgame[bgame$Race=="black",]$toc),na.rm=T)
    }
  }
}


print("White player")
print(paste0("White referee: ",as.numeric(48*ww_n/ww_min)))
print(paste0("African American referee: ", as.numeric(48*bw_n/bw_min)))
print("%%%%%%%%%%%%%")
print("African American player")
print(paste0("White referee: ",as.numeric(48*wb_n/wb_min)))
print(paste0("African American referee: ", as.numeric(48*bb_n/bb_min)))


print("%%%%%%%%%%%%%%%%%%%%")
print(paste0("Tech call rate per 48mins (same race): ",as.numeric(48*(bb_n+ww_n)/(bb_min+ww_min))))
print(paste0("Tech call rate per 48mins (opp race): ",as.numeric(48*(bw_n+wb_n)/(bw_min+wb_min))))

## simulate the techs 

refs_race.df$ngames = rep(1,dim(refs_race.df)[1])
refs_race.df$ntechs = rep(0,dim(refs_race.df)[1])

tech_counts = as.data.frame(table(tech.df$ref))

for (i in 1:dim(refs_race.df)[1]){
  tmp = tech_counts[tech_counts$Var1 == refs_race.df[i,]$Ref,]
  if (dim(tmp)[1] > 0){
    refs_race.df[i,]$ntechs = tmp[1,]$Freq
    refs_race.df[i,]$ngames = length(unique(pbp[pbp$ref == refs_race.df[i,]$Ref,]$game_id))
  }
}
refs_race.df$tech_rate = refs_race.df$ntechs/refs_race.df$ngames


ww_sim = rep(0,100)
wb_sim = rep(0,100)
bb_sim = rep(0,100)
bw_sim = rep(0,100)
same_sim = rep(0,100)
diff_sim = rep(0,100)
for (b in 1:100){
  tech_sim.df = data.frame(ref_race=c(),p_race=c(),game_id = c())
  for (g in 1:length(games_added)){
    gid = games_added[g]
    box_tmp = boxscore_merge[boxscore_merge$GAME_ID==gid,]
    if (dim(box_tmp)[1] == 0){
      next
    }
    refs = unique(pbp[pbp$game_id==gid & pbp$ref!="",]$ref)
    for (r in 1:length(refs)){
      if (runif(1) < refs_race.df[refs_race.df$Ref==refs[r],]$tech_rate){
        tech_sim.df <- rbind(tech_sim.df,data.frame(ref_race = refs_race.df[refs_race.df$Ref==refs[r],]$race , p_race = sample(size= 1, box_tmp$Race,prob=box_tmp$toc),game_id=gid))
      }
    }
  }
  wb_ns = length(which(tech_sim.df$ref_race == "white" & tech_sim.df$p_race == "black" & tech_sim.df$game_id %in% games_added))
  ww_ns = length(which(tech_sim.df$ref_race == "white" & tech_sim.df$p_race == "white" & tech_sim.df$game_id %in% games_added))
  bb_ns = length(which(tech_sim.df$ref_race=="black" & tech_sim.df$p_race == "black" & tech_sim.df$game_id %in% games_added))
  bw_ns = length(which(tech_sim.df$ref_race=="black" & tech_sim.df$p_race == "white" & tech_sim.df$game_id %in% games_added))


  ww_sim[b] = 48*ww_ns/ww_mins
  wb_sim[b] = 48*wb_ns/wb_mins
  bb_sim[b] = 48*bb_ns/bb_mins
  bw_sim[b] = 48*bw_ns/bw_mins
  same_sim[b] = 48*(bb_ns+ww_ns)/(bb_mins+ww_mins)
  diff_sim[b] = 48*(bw_ns+wb_ns)/(bw_mins+wb_mins)
}

dat = data.frame(g=diff_sim-same_sim)
observed_diff = as.numeric(48*(bw_n+wb_n)/(bw_min+wb_min))-as.numeric(48*(bb_n+ww_n)/(bb_min+ww_min))
## figure 1E
ggplot(dat, aes(x=g)) + geom_histogram(aes(y=..density..), binwidth=.5,colour="black", fill="white") +geom_density(alpha=.2, fill="#FF6666") +xlim(-0.001,0,0.004) + geom_vline(xintercept = observed_diff,color="red")+labs(x="g")
