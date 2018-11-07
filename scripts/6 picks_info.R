library(dplyr)
library(tidyr)
library(tidytext)

load(file="/data/picks.rda")
load(file="/data/matches.rda")


picks_df = filter(picks_df,id %in% extract_numeric(matches_df$id))

teams_outcomes = select(matches_df,id,winner,loser)
teams_outcomes = gather(teams_outcomes,"is_winner","team_name",2:3)
teams_outcomes$id = extract_numeric(teams_outcomes$id)
names(teams_outcomes)[1] = "match_id"

picks_df$is_winner = tolower(picks_df$is_winner)

teams_outcomes = left_join(teams_outcomes,picks_df)
teams_outcomes = na.omit(teams_outcomes)

hero_cnts = teams_outcomes %>% group_by(hero_id) %>% count()
hero_cnts = filter(hero_cnts,n > 10)
teams_outcomes = filter(teams_outcomes,hero_id %in% hero_cnts$hero_id)


picks = filter(teams_outcomes,is_pick == TRUE)

picks_cnt = picks %>% group_by(team_name,hero_id) %>% count()
picks_cnt = picks_cnt %>%
  bind_tf_idf(hero_id, team_name, n)
picks_cnt = arrange(picks_cnt,team_name,desc(tf_idf))

team_counts = picks_cnt %>% group_by(team_name) %>% count()
picks_cnt$hero_position = sequence(team_counts$nn)
picks_cnt = filter(picks_cnt,hero_position <= 4)





bans = filter(teams_outcomes,is_pick == FALSE)

bans_cnt = bans %>% group_by(team_name,hero_id) %>% count()
bans_cnt = bans_cnt %>%
  bind_tf_idf(hero_id, team_name, n)
bans_cnt = arrange(bans_cnt,team_name,desc(tf_idf))

team_counts = bans_cnt %>% group_by(team_name) %>% count()
bans_cnt$hero_position = sequence(team_counts$nn)
bans_cnt = filter(bans_cnt,hero_position <= 4)





losers = filter(teams_outcomes,is_winner=="loser")
losers = select(losers,1,3)
names(losers)[2] = "loser"
losers = losers[!duplicated(losers),]
losers = left_join(teams_outcomes,losers)
losers = losers %>% group_by(loser,hero_id)  %>% count()
names(losers)[1] = "team_name"

losers_cnt = losers %>% group_by(team_name,hero_id) %>% count()
losers_cnt = losers_cnt %>%
  bind_tf_idf(hero_id, team_name, nn)
losers_cnt = arrange(losers_cnt,team_name,desc(tf_idf))

team_counts = losers_cnt %>% group_by(team_name) %>% count()
losers_cnt$hero_position = sequence(team_counts$n)
losers_cnt = filter(losers_cnt,hero_position <= 4)

picks_cnt$cat = "pick"
bans_cnt$cat = "ban"
losers_cnt$cat = "lose"
picks_cnt = select(picks_cnt,team_name,hero_id,tf_idf,hero_position,cat)
bans_cnt = select(bans_cnt,team_name,hero_id,tf_idf,hero_position,cat)
losers_cnt = select(losers_cnt,team_name,hero_id,tf_idf,hero_position,cat)

top = rbind(picks_cnt,bans_cnt,losers_cnt)
top$cat = factor(top$cat,levels=c("pick","ban","lose"))
top = arrange(top,team_name,cat)
top$position = sequence(14)[c(-5,-10)]

save(top,file="~/elo/data/top_hrs.rda")
