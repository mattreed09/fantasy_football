


library(xml2)
library(rvest)

week=1

# GET DATA FROM THE WEBSITE


s_league = jsonlite::fromJSON("https://fantasy.premierleague.com/api/leagues-classic/273521/standings")$standings$results#/?page_new_entries=1&page_standings=1&phase=1

#https://fantasy.premierleague.com/api/entry/2545729/



live = jsonlite::fromJSON(paste0("https://fantasy.premierleague.com/api/event/",week,"/live/"))



team_ids = s_league$entry
team_names = s_league$entry_name
team_owner = s_league$player_name

bootstrap = jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")

name_ids = bootstrap$elements %>% select(first_name, second_name, web_name, id)


picks = map( paste0("https://fantasy.premierleague.com/api/entry/",team_ids,"/event/",week,"/picks/")  , jsonlite::fromJSON)

#MANIPULATE THE DATA INTO A FRIENDLY FORMAT

picks2 = map(picks, function(x)x$picks) %>%
  map( left_join, live$elements,  by = c("element"="id")) %>%
  map(left_join, name_ids, by = c("element" = "id") )


for(i in 1:length(team_names))
{
  picks2[[i]]$team_name = team_names[i]
  picks2[[i]]$team_id = team_ids[i]
  picks2[[i]]$team_owner = team_owner[i]
}

picks3 = map(picks2, function(x)x$stats )

without_stats = map(picks2, function(x)x %>% select(-stats) )

for (i in 1:length(without_stats))
{
  without_stats[[i]] = without_stats[[i]] %>% bind_cols(picks3[[i]])
}


all = bind_rows(without_stats)


temp = live$elements %>% select(-stats, -explain) %>% bind_cols(live$elements$stats) %>% left_join(name_ids)

write_csv(temp, paste0("C:/Users/matt.reed/OneDrive - Sainsbury's Supermarkets Ltd/Git/ada-an_training/Languages/R/visualisation/fantasy football data/live_week_",week,".csv"))
write_csv(all %>% select(-explain), paste0("C:/Users/matt.reed/OneDrive - Sainsbury's Supermarkets Ltd/Git/ada-an_training/Languages/R/visualisation/fantasy football data/s_table_week_",week,".csv"))

# DATA MANIPULIATION DONE, NOW LETS LOOK AT THE ACTUAL NUMBERS


player_points = all %>% group_by(web_name) %>%
  summarise(points = min(total_points),
            league_points = sum(total_points * multiplier ),
            owners = n_distinct(team_name),
            captains = sum(multiplier >= 2),
            triple_captains = sum(multiplier == 3),
            benched = sum(multiplier == 0),
            missed_points = sum(league_points*(multiplier == 0))
            ) %>%
  arrange(-league_points) %>%
  mutate(web_name = factor(web_name, levels = web_name))


ggplot(player_points %>% head(15), aes(reorder(web_name, league_points), league_points) ) + 
  geom_bar(stat="identity") +
  geom_text(aes(label = league_points, y = league_points - 10), color = "white", family = "Mary Ann") +
  coord_flip() + 
  sainstheme::s_theme2() +
  geom_hline(yintercept = 0, size =1)+
  labs(x = "", y = "", title = "Top Total League Points")+
  scale_y_continuous(breaks=NULL)+
  theme(panel.grid.major.y = element_blank(),
        plot.title= element_text(hjust  = 0.5),
        axis.text.y = element_text(margin = margin(r = -0.5,unit = "cm") ))  

ggplot(player_points %>% filter(captains != 0),
                         aes(reorder(web_name, captains), captains)) + 
  geom_bar(stat="identity")+
  geom_text(aes(label = captains, y = captains - 0.25), color = "white", family = "Mary Ann ExtraBold")+
  coord_flip()+
  sainstheme::s_theme2() +
  geom_hline(yintercept = 0, size = 1)+
  labs(x = "", y = "", title = "Total Captaincies")+
  scale_y_continuous(breaks=NULL)+
  theme(panel.grid.major.y  = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(margin = margin(r = -0.5,unit = "cm") ))



most_points_on_bench = all %>%
  group_by(team_owner, team_name) %>%
  summarise(points = sum(total_points*multiplier),
            left_on_bench = sum(total_points * (multiplier == 0)),
            players = paste( if_else(multiplier == 0, web_name, NULL) , collapse = ", " ) ) %>%
  mutate(players = str_remove_all(players, "NA, ") ) %>%
  arrange(-left_on_bench)

ggplot(most_points_on_bench, aes( reorder(team_owner, left_on_bench),left_on_bench ) ) + 
  geom_bar(stat="identity") +
  geom_text(aes(label = left_on_bench, y = left_on_bench - 1), color = "white", family = "Mary Ann ExtraBold")+
  coord_flip()+
  sainstheme::s_theme2() +
  geom_hline(yintercept = 0, size = 1)+
  labs(x = "", y = "", title = "Points left on bench") + 
  theme(panel.grid.major.y  = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 13, margin = margin(r = -0.5,unit = "cm") ))



ggplot(s_league, aes( reorder(player_name,total), total, color = player_name) ) + 
  geom_point(size = 2) +
  geom_segment(aes(x = player_name, xend = player_name, y = 0, yend = total), size = 1) + 
  geom_hline(yintercept = 0, size = 1)+
  coord_flip() +
  sainstheme::s_theme2() +
  labs(x = "",  y= "", title = "Total Weekly Points", subtitle = paste0("week ", week) )+
  theme(legend.position = "none") +
  theme(panel.grid.major.y  = element_blank(),
        panel.grid.minor.x = element_line(color  ="grey90"),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 13, margin = margin(r = -0.5,unit = "cm") ))


ggplot(player_points %>% arrange(-missed_points) %>% head(15),
       aes( reorder(web_name, missed_points), missed_points)) + 
  geom_bar(stat="identity")+
geom_text(aes(label = missed_points, y = missed_points - 10), color = "white", family = "Mary Ann") +
  coord_flip() + 
  sainstheme::s_theme2() +
  geom_hline(yintercept = 0, size =1)+
  labs(x = "", y = "", title = "Top Total Points Left on Bench")+
  scale_y_continuous(breaks=NULL)+
  theme(panel.grid.major.y = element_blank(),
        plot.title= element_text(hjust  = 0.5),
        axis.text.y = element_text(margin = margin(r = -0.5,unit = "cm") ))  


ggplot(s_league, aes(1, total) ) + 
  geom_point()+
  ggrepel::geom_text_repel(aes(label = player_name), 
                           direction = "x", 
                           segment.color = NA,
                           family = "Mary Ann ExtraBold") +
  sainstheme::s_theme2()+
  scale_x_continuous(labels = NULL)+
  labs(x = "", y = "", title = paste0("Week ", week, " points"))+
  scale_y_continuous(breaks = seq(0,ceiling(max(s_league$total)/10)*10, 5  ))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.minor.y = element_line(color = "grey85"),
        plot.title= element_text(hjust  = 0.5))



