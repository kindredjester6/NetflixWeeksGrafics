library(ggplot2)
library(gridExtra)
library(ggmosaic)
  
ggplot() +
  inset_element(tabla_grafica, 
                left = unit(0.5, "npc"),
                bottom = unit(0.5, "npc"),
                right = unit(0.5, "npc"), 
                top = unit(0.5, "npc"),
                align_to = "full")


df <- read.csv("all-weeks-global.csv")


tabla_grafica <- tableGrob(head(rename(dfTable, "Season Title" = season_title,
                                  "Hours" = weekly_hours_viewed),
                                n = 5))

p1 <- tableGrob(head(rename(dfTable, "Season Title" = season_title,
                            "Hours" = weekly_hours_viewed),
                     n = 5))


grid.arrange(tabla_grafica)

# Mostrar

max(df$weekly_views)
dfSort <- df %>% arrange(desc(weekly_hours_viewed))
dfHeadSort <-dfSort %>% head(n = 25) %>% distinct(show_title, .keep_all = TRUE)
dfTable <- dfHeadSort %>% select(season_title,
                                 weekly_hours_viewed) %>% mutate(weekly_hours_viewed = format(weekly_hours_viewed,
                                                                big.mark=","))

#----------------------InicioMosaico-----------------------------
# Crear un gráfico de mosaico con datos de ejemplo
dfP1 <- df %>% select(category, weekly_hours_viewed) %>% filter(category == 'TV (Non-English)' | category == 'TV (English)')
dfP2 <- df %>% select(category, weekly_hours_viewed) %>% filter(category == 'TV (English)')
mosaicFilms <-  ggplot(data = df) +
  geom_mosaic(aes(x = product(weekly_hours_viewed),
                  fill = category),
              offset = .525, 
              position = "dodge")+
  scale_alpha_manual(values = c(0.7, 0.9))
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  theme_mosaic()

mosaicFilms
#----------------------Inicio geomBar--------------------------------
dfTop1 <- dfSort %>% filter(show_title==head(dfHeadSort$show_title, n=1))


#----------------------TOP1-----------------------------------------

geomBarTop1 <- dfTop1 %>% ggplot(position = "dodge") +
  stat_summary(aes(x = week,
                   y = weekly_hours_viewed/1000000, fill = "red"), geom = "col", fun.data = mean_se)+
  stat_summary(aes(x = week,
                   y = weekly_rank), group = dfTop1$weekly_rank, geom = "smooth", fun.data = mean_se)
geomBarTop1
#----------------------TOP2-----------------------------------------
dfTop5 <- dfSort %>% filter(show_title == head(slice(dfHeadSort, 2)$show_title))


#----------------------Recommended-----------------------------------------
dfReco <- dfSort %>% filter(show_title == 'RRR (Hindi)')


#---------------------Fin geomBar------------------------------------------