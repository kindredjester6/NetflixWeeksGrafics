library(ggplot2)
library(ggpmisc)
library(gridExtra)
library(ggmosaic)
library(treemapify)
library(dplyr)
df <- read.csv("all-weeks-global.csv")
dfSort <- df %>% arrange(desc(weekly_hours_viewed))
dfHeadSort <-dfSort %>% head(n = 25) %>% distinct(show_title, .keep_all = TRUE)
dfTable <- dfHeadSort %>% select(season_title,
                                 weekly_hours_viewed) %>% mutate(weekly_hours_viewed = format(weekly_hours_viewed,
                                                                                              big.mark=","))

dfSelect <- df %>% select(weekly_hours_viewed, category)
dfPorc <- dfSelect %>% mutate(weekly_hours_viewed / sum(weekly_hours_viewed))
dfDef <- dfPorc %>% group_by(category) %>% summarise(suma = sum(`weekly_hours_viewed/sum(weekly_hours_viewed)`))
  
treeMapGF <- ggplot(dfDef, aes(area=suma,
                       fill=category,
                       subgroup=category,
                       label=paste(category,
                                   scales::percent(suma, accuracy = 0.01),
                                   sep = "\n"))) +
  labs(title = "Weekly Hour Viewed by Category(hours)") +
  geom_treemap() + 
  geom_treemap_text(colour = "white", place = "centre", size = 10) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set1")

tabla_grafica <- tableGrob(head(rename(dfTable, "Season Title" = season_title,
                                  "Hours" = weekly_hours_viewed),
                                n = 5))

p1 <- tableGrob(head(rename(dfTable, "Season Title" = season_title,
                            "Hours" = weekly_hours_viewed),
                     n = 5))

# Mostrar


#----------------------InicioMosaico-----------------------------
# Crear un gráfico de mosaico con datos de ejemplo
# dfP1 <- df %>% select(category, weekly_hours_viewed) %>% filter(category == 'TV (Non-English)' | category == 'TV (English)')
# dfP2 <- df %>% select(category, weekly_hours_viewed) %>% filter(category == 'TV (English)')
# mosaicFilms <-  ggplot(data = df) +
#   geom_mosaic(aes(x = product(weekly_hours_viewed),
#                   fill = category),
#               offset = .525, 
#               position = "identity")+
#   scale_alpha_manual(values = c(0.7, 0.9))
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
#   theme_mosaic()
#----------------------Inicio geomBar--------------------------------
#----------------------TOP1-----------------------------------------
dfTop1 <- dfSort %>% filter(show_title==head(dfHeadSort$show_title, n=1))

dfTest <- dfTop1%>%select(weekly_rank, weekly_hours_viewed, week, show_title)

dfTest <- dfTop1%>%mutate(weekly_hours_viewed = weekly_hours_viewed/1000000,
                          weekly_rank = (1 - as.double(paste0(".", as.character(dfTop1$weekly_rank-1))))*head(weekly_hours_viewed, n=1)
                          )
#(1 - dfTest$weekly_rank/head(dfTest$weekly_hours_viewed, n=1))+0.1  Esta instruccion vuelve a la normalidad el ranking
dfTest <- dfTest%>%mutate(year = substr(dfTop1$week, 1,4))

dfTest <- dfTest%>%arrange((year))

geomBarTop1 <- dfTest %>% ggplot(aes(x= as.numeric(1:nrow(dfTest)),
                 y = weekly_hours_viewed)) +
  geom_bar(stat = "identity",aes(fill=paste0(show_title, ", Weekly Hours Viewed")))+
  scale_fill_manual("",values = "#ffaf45")+
  facet_wrap(~dfTest$year, nrow=1,scales = "free_x")+
  labs(title = dfTest$show_title[1], subtitle = "Weekly hours TOP1",
       y="",
       x = "") + 
  theme(axis.text.y = element_text(size = 10,
                                   color = "gold",
                                   face = "bold"),
        axis.text.x = element_text(size = 9, 
                                   color = "blue",
                                   face = "italic", 
                                   angle = 320,
                                   vjust = 0.9,
                                   hjust = -0.009),
        title = element_text(size = 14, 
                             color = "Black",
                             face = "italic"),
        plot.title = element_text(vjust = -4.5),
        axis.title = element_text(size = 12, 
                                  color = "Black",
                                  face = "bold"),
        panel.background = element_rect(fill = "#707770"),
        plot.subtitle = element_text(color = "gray",
                                     hjust = 0.2 ))

geomBarTop1 <- geomBarTop1 + geom_point(size=1.3,color="#9e211a",aes(y = weekly_rank)) +
             geom_line(aes(y = weekly_rank, color=paste0(show_title, ", Weekly Rank")), size = 1)+
            scale_x_continuous(breaks = as.numeric(1:nrow(dfTest)), labels = substr(dfTop1$week, 6,10))+
            scale_colour_manual("",values="#9e2129") +
            theme(legend.position = "right")

#----------------------TOP2-----------------------------------------

dfTop5 <- dfSort %>% filter(show_title == head(slice(dfHeadSort, 3)$show_title))

dfTop5Mut <- dfTop5%>%select(weekly_rank, weekly_hours_viewed, week, show_title, season_title)

#(1 - dfTop5Mut$weekly_rank/head(dfTop5Mut$weekly_hours_viewed, n=1))+0.1  Esta instruccion vuelve a la normalidad el ranking
dfTop5Mut <- dfTop5Mut%>%mutate(year = substr(dfTop5$week, 1,4))

dfTop5Mut <- dfTop5Mut%>%mutate(weekly_hours_viewed = weekly_hours_viewed/1000000)

dfTop5Mut <- dfTop5Mut%>%mutate(
                          weekly_rank = (1 - as.double(paste0(".", as.character(dfTop5Mut$weekly_rank-1))))*
                            (
                              max((dfTop5Mut%>%group_by(week) %>% summarise(suma = sum(weekly_hours_viewed, na.rm = TRUE)))$suma)
 ))

#(1 - dfTop5Mut$weekly_rank/head(dfTop5Mut$weekly_hours_viewed, n=1))+0.1  Esta instruccion vuelve a la normalidad el ranking

dfTop5Mut <- dfTop5Mut%>%mutate(week = substr(dfTop5$week, 6,10))

dfTop5Mut <- dfTop5Mut%>%arrange((week))

dfTop5Mut <- dfTop5Mut %>% mutate(season_title = if_else(season_title == 'N/A',
                                                   'Stranger Things 1',
                                                   season_title))

geomBarTop2 <- dfTop5Mut %>% ggplot(aes(x= week,
                                     y = weekly_hours_viewed)) +
  geom_bar(stat = "identity",aes(fill=season_title))+
  facet_wrap(~dfTop5Mut$year, nrow=1,scales = 'free_x')+
  labs(title = dfTop5Mut$show_title[1], subtitle = "Weekly hours TOP 2",
       y="",
       x = "") +
  theme(axis.text.y = element_text(size = 10,
                                   color = "gold",
                                   face = "bold"),
        axis.text.x = element_text(size = 9, 
                                   color = "blue",
                                   face = "italic", 
                                   angle = 320,
                                   vjust = 0.9,
                                   hjust = -0.009),
        title = element_text(size = 14, 
                             color = "Black",
                             face = "italic"),
        plot.title = element_text(vjust = -4.5),
        axis.title = element_text(size = 12, 
                                  color = "Black",
                                  face = "bold"),
        panel.background = element_rect(fill = "#707770"),
        plot.subtitle = element_text(color = "gray",
                                     hjust = 0.2 ))

geomBarTop2 <- geomBarTop2 + geom_point(size=1.3,aes(y = weekly_rank, color = season_title)) +
  geom_line(aes(y = weekly_rank, color = season_title, group = season_title), size = 1)+
  theme(legend.position = "right")

#----------------------Recommended-----------------------------------------
dfReco <- dfSort %>% filter(show_title == 'RRR (Hindi)')

dfRecoMut <- dfReco%>%select(weekly_rank, weekly_hours_viewed, week, show_title)

dfRecoMut <- dfReco%>%mutate(weekly_hours_viewed = weekly_hours_viewed/1000000,
                          weekly_rank = (1 - as.double(paste0(".", as.character(dfReco$weekly_rank-1))))*head(weekly_hours_viewed, n=1)
)
#(1 - dfRecoMut$weekly_rank/head(dfRecoMut$weekly_hours_viewed, n=1))+0.1  Esta instruccion vuelve a la normalidad el ranking
dfRecoMut <- dfRecoMut%>%mutate(year = substr(dfReco$week, 1,4))

dfRecoMut <- dfRecoMut%>%arrange((week))

geomBarRec <- dfRecoMut %>% ggplot(aes(x= as.numeric(1:nrow(dfRecoMut)),
                                     y = weekly_hours_viewed)) +
  geom_bar(stat = "identity",aes(fill=paste0(show_title, ", Weekly Hours Viewed")))+
  scale_fill_manual("",values = "#ffaf45")+
  facet_wrap(~dfRecoMut$year, nrow=1,scales = 'free_x')+
  labs(title = dfRecoMut$show_title[1], subtitle = "Recomended Film",
       y="",
       x = "") + 
  theme(axis.text.y = element_text(size = 10,
                                   color = "gold",
                                   face = "bold"),
        axis.text.x = element_text(size = 9, 
                                   color = "blue",
                                   face = "italic", 
                                   angle = 320,
                                   vjust = 0.9,
                                   hjust = -0.009),
        title = element_text(size = 14, 
                             color = "Black",
                             face = "italic"),
        plot.title = element_text(vjust = -4.5),
        axis.title = element_text(size = 12, 
                                  color = "Black",
                                  face = "bold"),
        panel.background = element_rect(fill = "#707770"),
        plot.subtitle = element_text(color = "gray",
                                     hjust = 0.2 ))

geomBarRec <- geomBarRec + geom_point(size=1.3,color="#9e211a",aes(y = weekly_rank)) +
  geom_line(aes(y = weekly_rank, color=paste0(show_title, ", Weekly Rank")), size = 1)+
  scale_x_continuous(breaks = as.numeric(1:nrow(dfTest)), labels = substr(dfTest$week, 6,10))+
  scale_colour_manual("",values="#9e2129") +
  theme(legend.position = "right")

#---------------------Fin geomBar------------------------------------------

#---------------------Conjunto de graficos-----------------------------------------------------
((treeMapGF + tabla_grafica) / (geomBarTop1 / geomBarTop2 / geomBarRec)) +
  plot_annotation(title = 'Conjunto de graficos')

ggsave("mi_grafico.png", width = 20, height = 20)