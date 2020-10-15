install.packages("tidyverse")
install.packages("dplyr")
install.packages("na.tools")
install.packages("ggimage")
install.packages("ggrepel")
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)

#Loading NFL ScrapR dataset

pbp <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))
first <- 2009 #first season to grab. min available=2009
last <- 2018 # most recent season

datalist = list()
for (yr in first:last) {
  pbp <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
  games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", yr, ".csv")))
  pbp <- pbp %>% inner_join(games %>% distinct(game_id, week, season)) %>% select(-fumble_recovery_2_yards, -blocked_player_id, -fumble_recovery_2_player_id)
  datalist[[yr]] <- pbp # add it to your list
}
pbp_all <- dplyr::bind_rows(datalist)

#Creating pbp_fg dataset

pbp_all %>% select(posteam, defteam, desc, play_type) %>% head
pbp_fg <- pbp_all %>% filter(play_type=="field_goal" | play_type=="extra_point") %>% select(play_type, posteam_type, desc, home_team, field_goal_result, kick_distance, fg_prob, field_goal_attempt, kicker_player_name, posteam, kicker_player_id, season, wpa, epa, extra_point_result, extra_point_attempt)
mutate(made_kick = ifelse(field_goal_result == "made" | extra_point, 1, 0))

#Creating made_kick variable and editing it to remove blocked kicks
pbp_fg <- pbp_fg %>% mutate(made_kick = ifelse(field_goal_result == "made" | extra_point_attempt == "good", 1, 0))
pbp_fg <- pbp_fg %>% mutate(made_kick_xp = ifelse(extra_point_result=="good", 1, 0))
pbp_fg <- pbp_fg %>% mutate(made_kick = replace(made_kick, made_kick_xp==1, 1))
pbp_fg <- pbp_fg %>% mutate(made_kick = replace(made_kick, made_kick_xp==0, 0))
pbp_fg <- pbp_fg %>% mutate(made_kick = replace(made_kick, made_kick_xp==0, 0))
pbp_fg <- pbp_fg %>% mutate(made_kick = replace(made_kick, field_goal_result=="blocked", NA))
pbp_fg <- pbp_fg %>% mutate(made_kick = replace(made_kick, extra_point_result=="blocked", NA))
pbp_fg <- pbp_fg %>% mutate(made_kick = replace(made_kick, extra_point_result=="aborted", NA))
pbp_fg <- pbp_fg %>% mutate(kick_distance = replace(kick_distance, kick_distance>56, 56))
pbp_fg <- pbp_fg %>% mutate(kick_distance = replace(kick_distance, kick_distance<20, 20))
                            
#Creating expected field goal variable
pbp_fg <- select (pbp_fg,-c(extra_point_result, extra_point_attempt, wpa, epa, field_goal_attempt, fg_prob, made_kick_xp, field_goal_result))
pbp_fg <- na.omit(pbp_fg)
pbp_fg <- pbp_fg %>% group_by(kick_distance) %>% mutate(total_made_kicks = sum(made_kick), total_kicks = n()) %>% mutate(xfg = total_made_kicks/total_kicks)

#Mapping expected field goal

chart1 <- ggplot(data = pbp_fg, mapping = aes(x = kick_distance, y = made_kick)) +
  geom_smooth()

#Mapping kabex per player

chart2 <- ggplot(data = pbp_fg, mapping = aes(x = total_kicks_player, y = kicks_above_expectation)) +
  geom_point()  +
  geom_text_repel(data = subset(pbp_fg, total_kicks_player>400 | kicks_above_expectation<10), mapping = aes(label = kicker_player_name))

#Creating player xg
pbp_fg <- pbp_fg %>% group_by(kicker_player_name) %>% mutate(total_made_kicks_player = sum(made_kick), xfg_player=sum(xfg), total_kicks_player=n()) %>% mutate (kicks_above_expectation = total_made_kicks_player - xfg_player) %>% view

pbp_fg %>% group_by(kicker_player_name, season) %>% summarize(kicks_above_expectation=sum(made_kick)-sum(xfg), attempts=n(), xfg_total=sum(xfg), kicks_made=sum(made_kick)) %>% view
pbp_fg %>% group_by(kicker_player_name, season) %>% summarize(kicks_above_expectation=sum(made_kick)-sum(xfg), attempts=n(), xfg_total=sum(xfg), kicks_made=sum(made_kick), kabexpk=mean(made_kick-xfg)) %>% view
pbp_fg %>% group_by(kicker_player_name) %>% summarize(kicks_above_expectation=sum(made_kick)-sum(xfg), attempts=n(), xfg_total=sum(xfg), kicks_made=sum(made_kick), kabexpk=mean(made_kick-xfg)) %>% view

#Looking at it by NFL stadium

pbp_fg %>% group_by(home_team) %>% filter(posteam_type=="away") %>% summarize(kicks_above_expectation=sum(made_kick)-sum(xfg), attempts=n(), xfg_total=sum(xfg), kicks_made=sum(made_kick), kabexpk=mean(made_kick-xfg)) %>% view


#Now looking at LA Rams games

pbp_rams <- pbp_all %>% filter(home_team=="field_goal" | away_team=="extra_point")
