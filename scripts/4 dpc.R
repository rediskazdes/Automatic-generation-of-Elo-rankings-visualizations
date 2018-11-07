library(rvest)

url = "https://liquipedia.net/dota2/Dota_Pro_Circuit/2018-19/Rankings/Full"

page = read_html(url)
dpc = page %>% html_nodes(xpath='//*[@id="mw-content-text"]/div/div[2]/div[2]/table') %>% html_table(header=TRUE) %>% as.data.frame()
dpc = select(dpc,2,3)
dpc = dpc[-1,]
names(dpc)[1] = "name"
dpc$Points = as.numeric(dpc$Points)
save(dpc,file="/data/dpc.rda")
