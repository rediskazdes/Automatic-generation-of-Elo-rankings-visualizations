library(rvest)
url = "https://liquipedia.net/dota2/Portal:Teams"
page = read_html(url)

team_names = page %>% html_nodes(xpath='//*[@id="mw-content-text"]/div/div[3]//span[2]/a') %>% html_text()
links = page %>% html_nodes(xpath='//*[@id="mw-content-text"]/div/div[3]//span[2]/a') %>% html_attr("href")

top_teams = data.frame(name=team_names,link=links,stringsAsFactors = F)

start = "https://liquipedia.net"

top_teams$dotabuff_link = NA
top_teams$region = NA
for (i in 1:nrow(top_teams)){
  end = top_teams$link[i]
  url = str_c(start,end)
  page = read_html(url)
  if (page %>% html_nodes(xpath='//*[@target="_blank"]') %>%
      html_attr("href") %>% str_detect(regex("https://www.dotabuff.com/esports/teams/.*")) %>% any ){
    top_teams$dotabuff_link[i] = page %>% html_nodes(xpath='//*[@target="_blank"]') %>%
      html_attr("href") %>% str_extract(regex("https://www.dotabuff.com/esports/teams/.*")) %>%
      as.character() %>% na.omit()
  }
  tryCatch({
    top_teams$region[i] = page %>% html_nodes(xpath='//div[5]//*[@class="infobox-cell-2"]//a[1]') %>% html_attr("title")
  },error=function(e){})
  Sys.sleep(0.6)
  print(top_teams$name[i])
}

#library(googlesheets)
#manual_edit <- gs_new("Teams Info", ws_title = "teams", input = top_teams,
#                      trim = TRUE, verbose = FALSE)

#top_teams <- gs_title("Teams Info") %>% gs_read(1)
top_teams = na.omit(top_teams)

teams_performance = data.frame()
end = "/matches"
for (i in 1:nrow(top_teams)){
  start = top_teams$dotabuff_link[i]
  url = str_c(start,end)
  page = read_html(url)
  outcome = page %>% html_nodes(xpath="//div[1]/div[8]/div[2]/section[2]//tr/td[2]/div/a") %>% html_text
  id = page %>% html_nodes(xpath="//div[1]/div[8]/div[2]/section[2]//tr/td[2]/div/a") %>% html_attr("href")
  date = page %>% html_nodes(xpath="//div[1]/div[8]/div[2]/section[2]//tr/td[2]//span")  %>% html_text
  enemy= page %>% html_nodes(xpath='//*[@class="team-text team-text-full"]')  %>% html_text
  team_perf = data.frame(team=top_teams$name[i],outcome,date,enemy,id,stringsAsFactors = F)
  teams_performance = rbind(teams_performance,team_perf)
  str_l = length(page %>% html_nodes(xpath='//*[@class="pagination"]/*[@class="next"]/a'))
  while(min(date) > ymd("2018-08-26") & str_l > 0){
    url = page %>% html_nodes(xpath='//*[@class="pagination"]/*[@class="next"]/a') %>% html_attr("href") %>% str_c("https://dotabuff.com",.)
    page = read_html(url)
    outcome = page %>% html_nodes(xpath="//div[1]/div[8]/div[2]/section[2]//tr/td[2]/div/a") %>% html_text
    id = page %>% html_nodes(xpath="//div[1]/div[8]/div[2]/section[2]//tr/td[2]/div/a") %>% html_attr("href")
    date = page %>% html_nodes(xpath="//div[1]/div[8]/div[2]/section[2]//tr/td[2]//span")  %>% html_text
    enemy= page %>% html_nodes(xpath='//*[@class="team-text team-text-full"]')  %>% html_text
    team_perf = data.frame(team=top_teams$name[i],outcome,date,enemy,id,stringsAsFactors = F)
    teams_performance = rbind(teams_performance,team_perf)
    str_l = length(page %>% html_nodes(xpath='//*[@class="pagination"]/*[@class="next"]/a'))
  }
  
  print(top_teams$name[i])
  Sys.sleep(0.4)
}

teams_performance1 = teams_performance
teams_performance = filter(teams_performance1,date > ymd("2018-08-26"))

teams_performance$winner = ifelse(teams_performance$outcome == "Won Match",as.character(teams_performance$team),as.character(teams_performance$enemy))
teams_performance$loser = ifelse(teams_performance$winner == teams_performance$team,as.character(teams_performance$enemy),as.character(teams_performance$team))
teams_performance = filter(teams_performance,team  %in% top_teams$name & enemy %in% top_teams$name)
matches_df = select(teams_performance,-1,-2,-4)

matches_df$date = ymd(matches_df$date)
matches_df = matches_df[!duplicated(matches_df$id),]
matches_df = arrange(matches_df,date)
dates = sample(seq(as.Date('2001/01/01'), as.Date('2018/10/10'), by="day"), nrow(matches_df))
dates = sort(dates)
matches_df$date_2 = dates
View(table(dates))
library(EloRating)
#seqcheck(winner=matches_df$winner, loser=matches_df$loser, Date=matches_df$date)

#matches_df$date = as.character(matches_df$date)
#matches_df

res <- elo.seq(winner=matches_df$winner, loser=matches_df$lose,Date=matches_df$date_2,runcheck=F)
elllo <- extract.elo(res)
elllo <- as.data.frame(elllo)
elllo$name = rownames(elllo)
