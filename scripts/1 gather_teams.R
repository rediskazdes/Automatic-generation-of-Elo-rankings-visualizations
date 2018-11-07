library(rvest)
library(stringr)
url = "https://liquipedia.net/dota2/Portal:Teams"
page = read_html(url)

team_names = page %>% html_nodes(xpath='//*[@id="mw-content-text"]/div/div//span[2]/a') %>% html_text()
links = page %>% html_nodes(xpath='//*[@id="mw-content-text"]/div/div//span[2]/a') %>% html_attr("href")

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

library(googlesheets)
manual_edit <- gs_new("Teams Info", ws_title = "teams", input = top_teams,
                      trim = TRUE, verbose = FALSE)
