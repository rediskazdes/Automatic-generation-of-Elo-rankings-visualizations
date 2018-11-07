#compare after modifications in visual
elo_rank = gs_new("Elo ranking", ws_title = "ranks", input = compare,trim = TRUE, verbose = FALSE)

#load matches_df
write.csv(matches_df,file="data/matches.csv")
