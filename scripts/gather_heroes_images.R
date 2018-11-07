library(rvest)

url = "https://dota2.gamepedia.com/Dota_2_Wiki"
page = read_html(url)
pic_name = page %>% html_nodes(xpath='//*[@id="heroes"]/div[2]//tr/td/div/div[1]/a/img') %>%  html_attr("alt")
pic_link = page %>% html_nodes(xpath='//*[@id="heroes"]/div[2]//tr/td/div/div[1]/a/img') %>% html_attr("src")
localized_name = page %>% html_nodes(xpath='//*[@id="heroes"]/div[2]//tr/td/div/div[1]/a') %>% html_attr("title")

heroes_pics = data.frame(localized_name,pic_name,pic_link,stringsAsFactors = F)
save(heroes_pics,file="~/elo/heroes_pics.rda")

download.file(heroes_pics$pic_link, destfile = str_c("~/elo/pics/",heroes_pics$pic_name))


req = "https://api.opendota.com/api/heroes"
heroes = fromJSON(file=req) %>% map_df(extract,c("id","localized_name"))
names(heroes)[1] = "hero_id"

heroes_pics = left_join(heroes_pics,heroes)

save(heroes_pics,file="data/heroes.rda")
