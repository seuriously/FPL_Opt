library(rvest)
library(dplyr)
library(jsonlite)

setwd("D:/mydata/TSEL WORK/OTHERS/FPL")

n_top = 1000

topUsers_list = list()
pointHistory_list = list()
seasonHistory_list = list()

letMeSleep = function(i, j, x){
  if(i%%j == 0) {
    print(paste("sleeping for", x, "seconds"))
    Sys.sleep(x)
  }
}

for (i in 1:(n_top/50)){
  letMeSleep(i, 150, 15)
  url<-paste0("https://fantasy.premierleague.com/drf/leagues-classic-standings/313?phase=1&le-page=1&ls-page=",i)
  dat = read_json(url, simplifyVector = T)
  topUsers_list[[i]] = dat$standings$results
  print(paste("getting top", 50*i, "users"))
}
topUsers = bind_rows(topUsers_list)

for (i in 1:nrow(topUsers)){
  letMeSleep(i, 150, 15)
  url_history = paste0("https://fantasy.premierleague.com/drf/entry/",topUsers$entry[i],"/history")
  dat = read_json(url_history, simplifyVector = T)
  pointHistory_list[[i]] = dat$history
  seasonHistory_list[[i]] = dat$season
  print(paste("getting user", topUsers$entry_name[i], "rank:", topUsers$rank[i]))
}
pointHistory = bind_rows(pointHistory_list)
seasonHistory = bind_rows(seasonHistory_list)

pointHistory = left_join(pointHistory, topUsers[,c(2,11,7)], by=c("entry"="entry"))
summary_shifu = function (normalize=FALSE){
  temp = pointHistory %>% 
    group_by(entry) %>% 
    arrange(event) %>%
    mutate(index = (points - lag(points, default = first(points)))*log(points/lag(points, default = first(points)))) %>%
    ungroup() %>%
    group_by(entry, entry_name, rank.y) %>%
    summarise(avg = mean(points), PSI=mean(index), stddev = sd(points))
  
  if(normalize){
    temp$norm_avg = (temp$avg-min(temp$avg))/(max(temp$avg)-min(temp$avg))
    temp$norm_psi = 1-(temp$PSI-min(temp$PSI))/(max(temp$PSI)-min(temp$PSI))
    temp$sum_norm = temp$norm_avg+temp$norm_psi
  }
  temp
}
temp = summary_shifu(T)

plot_shifu = function(x,y,label){
  plot(x, y)
  abline(v = mean(x),col="purple")
  abline(h = mean(y),col="red")
  print("Double click the points to see the labels")
  identify(x, y, labels = label, cex = 0.7, col="blue")
  #text(x, y, temp$entry_name, cex=0.6, pos=4, col="red")
}
plot_shifu(temp$avg, temp$PSI,temp$entry_name)

getPlayers = function(){
  url_players = "https://fantasy.premierleague.com/drf/bootstrap-static"
  dat = read_json(url_players, simplifyVector = T)
  players = dat$elements
  clubs = dat$teams
  players = left_join(players, clubs[,c("code", "name")], by=c("team_code"="code"))
  colnames(players)[59] = "club_name"
  return(players)
}
players = getPlayers()

getPlayerHistory = function(){
  player_hist_list = list()
  for( i in 1:nrow(players)){
    letMeSleep(i, 150, 15)
    url_players_hist = paste0("https://fantasy.premierleague.com/drf/element-summary/", players$id[i])
    dat = read_json(url_players_hist, simplifyVector = T)
    player_hist_list[[i]] = dat$history
    player_hist_list[[i]] = player_hist_list[[i]][,c("round", "total_points", "element")]
    print(paste("getting player", players$web_name[i], "history -", i))
  }
  player_hist = bind_rows(player_hist_list)
  return(player_hist)
}

player_hist = getPlayerHistory()

gameweek = max(pointHistory$event)
getLatestPick = function(gameweek){
  latestpicks_list = list()
  for( i in 1:nrow(topUsers)){
    letMeSleep(i, 150, 15)
    url_pick = paste0("https://fantasy.premierleague.com/drf/entry/",topUsers$entry[i],"/event/", gameweek,"/picks")
    dat = read_json(url_pick, simplifyVector = T)
    latestpicks_list[[i]] = dat$picks
    print(paste("getting latest pick by user", topUsers$entry_name[i], "rank:", topUsers$rank[i], "gameweek", gameweek))
  }
  latestpicks = bind_rows(latestpicks_list)
  latestpicks$entry = rep(topUsers$entry, each=15)
  latestpicks$entry_name = rep(topUsers$entry_name, each=15)
  latestpicks = left_join(latestpicks, players[,c("id","web_name", "club_name", "element_type", "now_cost")], by=c("element"="id"))
  ph = player_hist %>% filter(round==gameweek)
  latestpicks = left_join(latestpicks, ph, by=c("element" = "element"))
  latestpicks$element_type = ifelse(latestpicks$element_type==1, "GK", 
                                    ifelse(latestpicks$element_type==2, "DF",
                                           ifelse(latestpicks$element_type==3, "MF", "ST")))
  if(nrow(latestpicks)!=15*n_top){
    latestpicks = latestpicks %>% 
      group_by_at(names(latestpicks)[-grep("total_", names(latestpicks))]) %>% 
      summarise(total_points = sum(total_points)) %>%
      ungroup()
    
  }
  return(latestpicks)
}
latestpicks = getLatestPick(gameweek)
latestpicks_h = getLatestPick(gameweek-1)
latestpicks_21 = getLatestPick(gameweek-2)
latestpicks_20 = getLatestPick(gameweek-3)
  

#------------------------------------------------------------------------
# 2. most picked by top users
#latestpicks = left_join(latestpicks, temp[,c("entry", "sum_norm")], by=c("entry"="entry"))


f_top_players = function(df){
  df %>% 
  #filter(sum_norm > 1) %>%
  group_by(element, web_name, club_name, element_type, now_cost, total_points) %>%
  summarise(n_chosen = n(), n_captain = sum(is_captain)) %>%
  arrange(desc(n_chosen)) %>%
  ungroup()
}

optimizer = function(latestpicks){
  top_players = f_top_players(latestpicks)
  
  library(lpSolve)
  #Create the constraints
  num_gk = 2
  num_def = 5
  num_mid = 5
  num_fwd = 3
  max_cost = 1021
  # Create vectors to constrain by position
  top_players$Goalkeeper = ifelse(top_players$element_type == "GK", 1, 0)
  top_players$Defender = ifelse(top_players$element_type == "DF", 1, 0)
  top_players$Midfielder = ifelse(top_players$element_type == "MF", 1, 0)
  top_players$Forward = ifelse(top_players$element_type == "ST", 1, 0)
  # Create vector to constrain by max number of players allowed per team
  team_constraint = unlist(lapply(unique(top_players$club_name), function(x, top_players){
    ifelse(top_players$club_name==x, 1, 0)
  }, top_players=top_players))
  # next we need the constraint directions
  const_dir <- c("=", "=", "=", "=", rep("<=", 21))
  # The vector to optimize against
  objective = top_players$n_chosen
  # Put the complete matrix together
  const_mat = matrix(c(top_players$Goalkeeper, top_players$Defender, top_players$Midfielder, top_players$Forward,
                       top_players$now_cost, team_constraint),
                     nrow=(5 + length(unique(top_players$club_name))),
                     byrow=TRUE)
  const_rhs = c(num_gk, num_def, num_mid, num_fwd, max_cost, rep(3, 20))
  # And solve the linear system
  x = lp ("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)
  final_team = print(arrange(top_players[which(x$solution==1),], desc(Goalkeeper), desc(Defender), desc(Midfielder), desc(Forward), desc(n_chosen)))
  final_team
}

gw_now = optimizer(latestpicks) 
gw_now %>% ungroup %>% summarise(now_cost = sum(now_cost), total_points=sum(total_points))
gw_prev = optimizer(latestpicks_h)
gw_prev %>% ungroup %>% summarise(now_cost = sum(now_cost), total_points=sum(total_points))
gw_21 = optimizer(latestpicks_21)
gw_20 = optimizer(latestpicks_20)

performance = function(tr_team, te){
  left_join(tr_team[,c("element", "web_name", "club_name")], f_top_players(te)[,c("element","total_points")], by=c("element" = "element"))

}
res = performance(gw_prev, latestpicks)
res_21 = performance(gw_21, latestpicks_h)
res_20 = performance(gw_20, latestpicks_21)
sum(res$total_points)
sum(res_21$total_points)
sum(res_20$total_points,na.rm = T)
latestpicks %>% group_by(web_name, club_name, element_type, total_points) %>% summarise(n = sum(is_captain)) %>% arrange(desc(n))
# idea:
#   - most captained week-in week-out per user, who and what positioned
#   - distribution of players selected by top 1000 on latest week

