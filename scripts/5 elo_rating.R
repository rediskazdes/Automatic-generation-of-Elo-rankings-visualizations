library(EloRating)
library(googlesheets)

load("data/matches.rda")
tms_cnts = matches_df %>% gather(cat,team,3:4)
tms_cnts = tms_cnts %>% group_by(team) %>% count()
tms_cnts = filter(tms_cnts,n > 3)
matches_df = filter(matches_df,loser %in% tms_cnts$team & winner %in% tms_cnts$team)

res <- elo.seq(winner=matches_df$winner, loser=matches_df$lose,Date=matches_df$date_2,runcheck=F)
elllo <- extract.elo(res)
elllo <- as.data.frame(elllo)
elllo$name = rownames(elllo)

before_date = filter(matches_df,date < "2018-10-23")

res <- elo.seq(winner=before_date$winner, loser=before_date$lose,Date=before_date$date_2,runcheck=F)
elllo1 <- extract.elo(res)
elllo1 <- as.data.frame(elllo1)
elllo1$name = rownames(elllo1)

compare = left_join(elllo,elllo1,by="name")
compare$elllo1[is.na(compare$elllo1)] = 1000
compare = select(compare,2,3,1)
compare$diff = compare$elllo - compare$elllo1

top_teams <- gs_title("Teams Info") %>% gs_read(1)
top_teams = na.omit(top_teams)
compare = left_join(compare,select(top_teams,1,4))

compare$pos_before_date = rank(compare$elllo1) %>% floor()
compare$pos_after_date = rank(compare$elllo) %>% floor()
compare$pos_before_date = max(compare$pos_before_date) - compare$pos_before_date+1
compare$pos_after_date = max(compare$pos_after_date) - compare$pos_after_date+1
compare$pos_diff = compare$pos_before_date - compare$pos_after_date

compare$coor = nrow(compare):1
compare$region = factor(compare$region,levels=(c("CIS","Europe","China","Southeast Asia","South America","North America"))) 
compare$diff_dir = ifelse(compare$pos_diff > 0,"Positive",ifelse(compare$pos_diff < 0,"Negative","-"))
compare$diff_dir = factor(compare$diff_dir,levels=unique(compare$diff_dir))
compare$pos_diff = ifelse(compare$diff_dir == "Positive",str_c("+",compare$pos_diff),as.character(compare$pos_diff))
compare$pos_diff[compare$pos_diff==0] = "—"

###### dpc
load('~/elo/data/dpc.rda')

compare = left_join(compare,dpc)
compare$Points[is.na(compare$Points)] = "—"

save(compare,file="~/elo/data/elo_recent.rda")
