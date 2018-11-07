library(tidyr)
library(rjson)
library(purrr)
library(magrittr)
library(dplyr)
load("data/matches.rda")
load("data/picks.rda")

ids = extract_numeric(matches_df$id)
ids = ids[!ids %in% picks_df$match_id]
start = "https://www.dotabuff.com/matches/"

#picks_df = data.frame()
#ids = ids[ids == "4153695159"]
for(id in ids){
   print(id)
  tryCatch({
   req = str_c("https://api.opendota.com/api/matches/",id)
   json = fromJSON(file=req)
   tryCatch({
   picks = json$picks_bans %>% map_df(extract,c("is_pick","team","hero_id"))
   who_won = ifelse(json$radiant_win == TRUE,0,1)
   picks$is_winner = ifelse(picks$team == who_won,"Winner","Loser") 
   picks$match_id = id
   picks_df = rbind(picks_df,picks)
   s},error=function(e){})
   Sys.sleep(0.6)
  },error=function(e){})
}

#0 - радиент
#1 - даер


save(picks_df,file="~/elo/data/picks.rda")
