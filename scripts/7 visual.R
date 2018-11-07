library(ggplot2)
library(grid)
library(stringr)
library(dplyr)
library(magick)
library(png)
library(grid)

load("/data/elo_recent.rda")
load("/data/top_hrs.rda")
load("/data/heroes.rda")

heroes_relev = filter(heroes_pics,hero_id %in% top$hero_id)
coors = select(compare,name,coor,region)
names(coors)[1] = "team_name"
top = left_join(top,coors)
top = na.omit(top)


###### Картинка
change = c("#10D636","#FA3433","#E8E4D9")
change_scheme = data.frame(diff_dir = levels(compare$diff_dir),color=change,stringsAsFactors = FALSE)

#color_scheme = c("#6CFF4A","#FCF04B","#FF4233","#FF8D31","#51A891", "#424BBF")
#color_scheme = c("#5EDB1E","#238F14","#FF1A1C","#B11A1C","#24BEC9", "#146A70")
color_scheme = c("#1CA356","#4FBD37","#EB6560","#EDA468", "#4575b4","#7DA5BD")
background_color = "#140324"

my_theme <- function() {
  theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid  = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill = background_color),
      legend.position="top",
      legend.background = element_rect(fill = background_color),
      plot.background = element_rect(fill = background_color),
      plot.title = element_text(size = rel(2),color="white",hjust=0.5,family = "AvantGarde"),
      plot.subtitle = element_text(size=rel(1),color="white",hjust=0.7,family="AvantGarde"),
      legend.direction = "horizontal",
      legend.text = element_text(colour="white",margin = margin(r = 30, unit = "pt"),size=14),
      legend.title = element_blank(),
      legend.spacing.x = unit(0.1, 'cm'),
      legend.key = element_rect(colour = background_color),
      legend.key.height =  unit(0.5, "cm"),
      legend.key.width = unit(1.5, "cm"),
      axis.text.y = element_text(lineheight = 0.5)
      )
}
aspect_ratio <- 2.3

title_text = "Elo ranking by Spectral Alliance\nESL One Hamburg\nDreamleague S10 Minor"
subtitle_text = "And minor local tournaments"

x_nms = c(" ","Previous\nRank","World\nRank","Rank\nChange","Elo\nPoints","DPC\nPoints")
x_space = 0.33
x_first = 1.6
x_coors = c(1,x_first,x_first+x_space,x_first+x_space*2,x_first+x_space*3,x_first+x_space*4)

g = ggplot() + geom_tile(data=compare, 
                         mapping=aes(x=x_coors[1],y=coor-0.065,height=0.85,width=0.95,fill=region), alpha=0.9)+ 
               geom_text(data=compare,aes(y=coor,x=x_coors[1],label=name),family = "AvantGarde", colour = "white") +
               geom_text(data=compare,aes(y=coor,x=x_coors[2],label=pos_before_date),family = "AvantGarde", colour = "white") +  
               geom_text(data=compare,aes(y=coor,x=x_coors[3],label=pos_after_date),family = "AvantGarde", colour = "white") +  
               geom_text(data=compare,aes(y=coor,x=x_coors[4],label=pos_diff,colour=diff_dir),family = "AvantGarde")+
               geom_text(data=compare,aes(y=coor,x=x_coors[5],label=elllo),family = "AvantGarde", colour = "white") +
               geom_text(data=compare,aes(y=coor,x=x_coors[6],label=Points),family = "AvantGarde", colour = "white") +
               scale_colour_manual(values=change_scheme$color) +
               annotate("text",x=x_coors,y=max(compare$coor+2),label=x_nms,color="white",size=4.3,family = "AvantGarde") +
               scale_fill_manual(values=color_scheme) +
               xlim(min(x_coors)-0.5,max(x_coors)+1.8)+
               guides(colour =FALSE)  +
               my_theme() +
               ggtitle(title_text,subtitle=subtitle_text)

xl = 0.05
yl = 0.5
space = 0.11
space_table = 0.15

mlprs = c(2,7,12)
xh_coors = max(x_coors)+space_table + space*mlprs+space/2
xh_names = c("Favorable\nPicks","Favorable\nBans","Bad\nAgainst")

g = g + annotate("text",x=xh_coors,y=max(compare$coor+2),label=xh_names,color="white",size=4.3,family = "AvantGarde") +
        geom_tile(mapping=aes(x=xh_coors,y=median(compare$coor)+1.5,height=median(compare$coor)*2+2,width=0.49),fill=c("#19FF2F","#FF4023","yellow"), alpha=0.15)
#0061FF
top1 = filter(top,team_name == "Team Secret")
for(id in heroes_relev$hero_id){
    print(id)
    path = str_c("~/elo/data/pics/",heroes_relev$pic_name[heroes_relev$hero_id == id])
    h = image_read(path) %>% rasterGrob(interpolate=TRUE)
    df1 = filter(top,hero_id == id)
    
    if(nrow(df1) > 0){
        x1 = max(x_coors)+space_table+space*df1$position-xl
        x2 = max(x_coors)+space_table+space*df1$position+xl
        y1 = df1$coor-yl
        y2 = df1$coor+yl
        
        for (i in 1:nrow(df1)){
            g = g +
            annotation_custom(h, xmin=x1[i], xmax=x2[i], ymin=y1[i], ymax=y2[i]) +
            geom_point()
        }
    }
}

ggsave("~/elo/eng/overview.png",g, height = 7 * aspect_ratio, width = 9.3)


#### Regions
for (i in 1:length(levels(compare$region))){
    print(levels(compare$region)[i])
    compare1 = filter(compare,region==levels(compare$region)[i])
    compare1$coor = nrow(compare1):1
    change_scheme1 = filter(change_scheme,diff_dir %in% levels(compare1$diff_dir))
    
    g1 = ggplot() + geom_tile(data=compare1, 
                             mapping=aes(x=x_coors[1],y=coor-0.065,height=0.85,width=0.95),fill=color_scheme[i], alpha=0.9)+ 
      geom_text(data=compare1,aes(y=coor,x=x_coors[1],label=name),family = "AvantGarde", colour = "white") +
      geom_text(data=compare1,aes(y=coor,x=x_coors[2],label=pos_before_date),family = "AvantGarde", colour = "white") +  
      geom_text(data=compare1,aes(y=coor,x=x_coors[3],label=pos_after_date),family = "AvantGarde", colour = "white") +  
      geom_text(data=compare1,aes(y=coor,x=x_coors[4],label=pos_diff,colour=diff_dir),family = "AvantGarde")+
      geom_text(data=compare1,aes(y=coor,x=x_coors[5],label=elllo),family = "AvantGarde", colour = "white") +
      geom_text(data=compare1,aes(y=coor,x=x_coors[6],label=Points),family = "AvantGarde", colour = "white") +
      scale_colour_manual(values=change_scheme1$color) +
      annotate("text",x=x_coors,y=max(compare1$coor+2),label=x_nms,color="white",size=4.3,family = "AvantGarde") +
      scale_fill_manual(values=color_scheme) +
      xlim(min(x_coors)-0.5,max(x_coors)+1.8)+
      guides(colour =FALSE) +
      my_theme() + theme(legend.position="none")+
      ggtitle(title_text,subtitle=subtitle_text) +
      annotate("text",x=xh_coors,y=max(compare1$coor+2),label=xh_names,color="white",size=4.3,family = "AvantGarde") +
      geom_tile(mapping=aes(x=xh_coors,y=median(compare1$coor)+1.5,height=median(compare1$coor)*2+2,width=0.49),fill=c("#19FF2F","#FF4023","yellow"), alpha=0.15)
    
    top1 = filter(top, region %in% levels(compare$region)[i])
    coors1 = select(compare1,name,coor,region)
    names(coors1)[1] = "team_name"
    top1 = select(top1,-coor)
    top1 = left_join(top1,coors1)
    
    for(id in heroes_relev$hero_id){
      print(id)
      path = str_c("~/elo/data/pics/",heroes_relev$pic_name[heroes_relev$hero_id == id])
      h = image_read(path) %>% rasterGrob(interpolate=TRUE)
      df1 = filter(top1,hero_id == id)
      
      if(nrow(df1) > 0){
        x1 = max(x_coors)+space_table+space*df1$position-xl
        x2 = max(x_coors)+space_table+space*df1$position+xl
        y1 = df1$coor-yl
        y2 = df1$coor+yl
        
        for (k in 1:nrow(df1)){
          g1 = g1 +
            annotation_custom(h, xmin=x1[k], xmax=x2[k], ymin=y1[k], ymax=y2[k]) +
            geom_point()
        }
      }
    }
    
path = str_c("~/elo/eng/",levels(compare1$region)[i],".png")
heigh = 3 + nrow(compare1)/4
ggsave(path,g1, height = heigh, width = 9.3)
}


#### parts of table
compare$part = rep(1:6,each=11)

for (i in unique(compare$part)){
compare2 = filter(compare,part ==i)
change_scheme1 = filter(change_scheme,diff_dir %in% levels(compare2$diff_dir))
g3 = ggplot() + geom_tile(data=compare2, 
                         mapping=aes(x=x_coors[1],y=coor-0.065,height=0.85,width=0.4,fill=region), alpha=0.9)+ 
  geom_text(data=compare2,aes(y=coor,x=x_coors[1],label=name),family = "AvantGarde", colour = "white") +
  geom_text(data=compare2,aes(y=coor,x=x_coors[2],label=pos_before_esl),family = "AvantGarde", colour = "white") +  
  geom_text(data=compare2,aes(y=coor,x=x_coors[3],label=pos_after_esl),family = "AvantGarde", colour = "white") +  
  geom_text(data=compare2,aes(y=coor,x=x_coors[4],label=pos_diff,colour=diff_dir),family = "AvantGarde")+
  geom_text(data=compare2,aes(y=coor,x=x_coors[5],label=elllo),family = "AvantGarde", colour = "white") +
  scale_colour_manual(values=change_scheme1$color) +
  annotate("text",x=x_coors,y=max(compare2$coor+1.5),label=x_nms,color="white",size=4.3) +
  scale_fill_manual(values=color_scheme) + xlim(min(x_coors)-0.2,max(x_coors)+0.1) + my_theme() +
  ggtitle(title_text,subtitle=subtitle_text) + guides(colour =FALSE, fill=guide_legend(nrow=2))

path = str_c("~/elo/parts/",i,".png")
ggsave(path,g3, height = 7, width = 7)

}

