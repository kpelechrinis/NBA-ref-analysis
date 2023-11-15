import time
import pandas as pd
from nba_api.stats.endpoints import boxscoreadvancedv2
from nba_api.stats.static import players

nba_players = players.get_players()

nba_players_df = pd.DataFrame(columns = ['id','full_name','first_name','last_name'])

for p in nba_players:
    nba_players_df.loc[len(nba_players_df.index)] = [p['id'],p['full_name'],p['first_name'],p['last_name']] 
    
nba_players_df.to_csv("nba_playerIds.csv",index=False)

f = open("game_ids_nba.csv","r")

i = 0

for line in f:
    if i <=1056:
        i+=1
        continue
    if i%1000 ==0:
        print(i)
    gid = line.rstrip()
    boxscore = boxscoreadvancedv2.BoxScoreAdvancedV2(game_id='00'+gid).player_stats.get_data_frame()
    if i == 0:
        boxes = boxscore
        i += 1
    else:
        boxes = pd.concat([boxes,boxscore],ignore_index=True, sort=False)
        time.sleep(1)
        i+=1

boxes.to_csv("boxscores.csv",index=False)
