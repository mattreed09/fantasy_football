#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(xml2)
library(rvest)
library(extrafont)
library(tidyverse)
#library(MattPack)
library(gganimate)
library(ggrepel)
font = "Gadugi"
theme_set(theme_minimal() + theme(rect = element_rect(fill = "#f0f3f0", 
                                                color = NA), text = element_text(size = 18, family = font), 
                            plot.title = element_text(size = 25, hjust = 0.5), plot.caption = element_text(size = 12), 
                            axis.title = element_text(size = 22, hjust = 0.5), axis.text = element_text(size = 18), 
                            axis.ticks = element_blank(), axis.line = element_blank(), 
                            plot.background = element_rect(fill = "#f0f3f0", color = NA), 
                            panel.background = element_rect(fill = "#f0f3f0", color = NA), 
                            panel.border = element_blank(), panel.grid.major.y = element_line(color = "#d0d7d0", 
                                                                                              size = 0.9), panel.grid.minor = element_blank(), 
                            legend.position = "top", legend.title = element_blank(), 
                            legend.text = element_text(size = 18), legend.box.background = element_rect(fill = "#f0f3f0", 
                                                                                                        color = NA), legend.key = element_rect(fill = "#f0f3f0", 
                                                                                                                                               color = NA), legend.direction = "horizontal", legend.justification = "center")
          )

league_num = 273521

folder = paste0("C:/Users/matt.reed/OneDrive - Sainsbury's Supermarkets Ltd/Git/ada-an_training/Languages/R/visualisation/fantasy football data/", league_num)


all_leagues = map(paste0(folder,"/",list.files(folder, pattern = "s_actual_table_week")),
                  read_csv) %>%
map(function(x)x %>% mutate(row_rank = as.numeric(rownames(x)) )) 

for (i in length(all_leagues):1){
    all_leagues[[i]]$week = i
}

all_leagues = all_leagues %>% map_df(I)

for (ent in unique(all_leagues$entry)){
    all_leagues[all_leagues$entry == ent,]$entry_name = all_leagues[all_leagues$entry == ent,]$entry_name[nrow(all_leagues[all_leagues$entry == ent,])]
    all_leagues[all_leagues$entry == ent,]$player_name = all_leagues[all_leagues$entry == ent,]$player_name[nrow(all_leagues[all_leagues$entry == ent,])]
}


#all_leagues = map(all_leagues, function(x)x %>% mutate(row_rank = as.numeric(rownames(x)) ) %>% select(player_name, row_rank, entry)) 

player_data <- map(paste0(folder, "/", list.files(folder, pattern = "live_week")), read_csv)


team_players = map(paste0(folder,"/", list.files(folder, pattern = "s_table_week")) %>% sort, read_csv )  

for (i in length(team_players):1){
    team_players[[i]]$week = i
}

team_players = team_players %>% map_df(I)
    
for (ent in unique(team_players$team_id) ){
    team_players[team_players$team_id == ent,]$team_owner = team_players[team_players$team_id == ent,]$team_owner[nrow(team_players[team_players$team_id == ent,])]
    team_players[team_players$team_id == ent,]$team_name = team_players[team_players$team_id == ent,]$team_name[nrow(team_players[team_players$team_id == ent,])]
}


goals <- (team_players %>% 
                 group_by(team_id, team_owner, week) %>% 
                 summarise(gf = sum(goals_scored*multiplier), 
                           ga = sum(goals_conceded*multiplier ),
                           assists = sum(assists*multiplier),
                           cs = sum(clean_sheets*multiplier),
                           bps = sum(bonus*multiplier)))





# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fantasy Football Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
         sidebarPanel(
             style = "position:fixed;width:inherit;",
             selectInput("team_owner", "Team Owner:",
                         all_leagues %>% map_df(I) %>% pull(player_name) %>% unique() %>% sort
             ),
             selectInput("stat", "Statistic:",
                         names(goals)[4:length(goals)]
             )
         ),
        
        

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("summary_plot"),
            br(),
           plotOutput("lineplot"),
           br(),
           plotOutput("highest_scorer"),
           br(),
           plotOutput("rivals")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$summary_plot <- renderPlot({
        ggplot(goals %>% filter(team_owner == input$team_owner), aes(week, !!as.name(input$stat) )) +
            geom_col() + 
            labs(title = paste0(input$team_owner, "\'s ", input$stat, " per week") ) + 
            scale_y_continuous(limits = c(0,1.1*max(goals %>% pull(!!as.name(input$stat)))))
            
    })
    
    output$lineplot <- renderPlot({
        ggplot(all_leagues %>% filter(player_name == input$team_owner), aes(x=week, y = event_total)) + 
            geom_col() +
            labs(title = paste0(input$team_owner, "\'s points per week")) +
            scale_y_continuous(limits = c(0, max(all_leagues$event_total ) )) + 
            scale_x_continuous(limits = c(0+0.5, max(all_leagues$week)+0.5))
    })
    output$highest_scorer <- renderPlot({
        ggplot(team_players %>% filter(team_owner == input$team_owner) %>% group_by(week) %>% arrange(-total_points*multiplier) %>% slice(1),
               aes(x=week, y = total_points*multiplier )) + 
            geom_col() +
            geom_text(aes(label = web_name, y = 0.95*total_points*multiplier-0.4), color = "white", size = 5) +
            labs(title = paste0(input$team_owner, "\'s best player by week")) +
            scale_y_continuous(limits = c(0, 1.1*max(team_players$total_points*team_players$multiplier ) ))  
    
        
    })


    output$rivals <- renderPlot({
        rivals <- all_leagues %>%
            mutate(is_rival = entry %in% head(all_leagues %>%
                                                  left_join(all_leagues %>%
                                                                filter(player_name == input$team_owner) %>%
                                                                select(week,matt_rank = row_rank ) ) %>%
                                                  mutate(diff = abs(matt_rank - row_rank) ) %>%
                                                  group_by(entry) %>%
                                                  summarise(diffs = sum(diff)) %>% arrange(diffs))$entry) %>%
            mutate(group = if_else(player_name == input$team_owner, "1",
                                   if_else(is_rival, "2", "3") ) )
        
    ggplot(rivals, aes(week, row_rank,  group = player_name, color = group) ) +
        geom_text_repel(data = filter(rivals, is_rival & week == max(rivals$week)),
                                 aes(x = max(rivals$week),
                                     label = str_extract(player_name, "^.*? \\w"),
                                     y = row_rank,
                                     group = player_name,
                                     alpha = player_name == input$team_owner),
                                 nudge_x = 0.05,
                                 direction = "y",
                                 color = "grey15", size = 6, hjust = 0, family = "Gadugi") +
                 scale_y_reverse(breaks = NULL)+
                 theme(legend.position = "none") +
                 labs(x= "Gameweek", y = "Rank", title= paste0(input$team_owner, "\'s Position over time"), caption = "@MattTheReed")+
                 geom_point(data = filter(rivals, is_rival & week == max(rivals$week)), size=3) +
                 scale_color_manual(values = c("#d010f0", "#a06060","grey80") )+
                 geom_line(size = 0.9) +
        geom_line(data = filter(rivals, is_rival), size = 0.9) +
                 scale_x_continuous(breaks = 1:max(rivals$week), limits = c(1, max(rivals$week)*1.1)) +
        scale_alpha_manual(values = c(0.5, 1))
    })

    }

# Run the application 
shinyApp(ui = ui, server = server)



# rsconnect::deployApp('C:\\Users\\matt.reed\\OneDrive - Sainsbury\'s Supermarkets Ltd\\Git\\fantasy_football\\Fantasy app')

