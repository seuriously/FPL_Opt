# FPL_Opt
Comeback is real. This repo is an effort to optimize my FPL team by following top n user's team selection.
A fun exercise on data manipulation.

## Intro: What the hell is FPL?
FPL stands for [Fantasy Premier League](https://fantasy.premierleague.com). Basically we have to choose 15 EPL (English Premier League) players while gathering as much points as possible in a season. There are some constraint we need to comply.
- Total cost of players should not exceed the budget given. We are given £100 at the start of the season.
- Number of players selected from the same EPL team must not be more than 3.
- 15 players must be consists of 2 GK, 5 DF, 5 MF, and 3 ST.
- Out of 15, 11 players are selected as outfield players while the rest will be sitting on bench. Automatic subs will be applied if the outfield players does not play at all.
- A free transfer is given everyweek on 1 player. Any additional transfer will lead to -4 hits for every player.
- Players will be scored based on their real performance on the field. Further scoring system can be seen below:

Action | Fantasy Points
---|---
For playing in a match (less than 60 minutes)|1 point
|For playing at least 60 minutes in a match |2 points
|For each goal scored by a goalkeeper or defender |8 points
|For each goal scored by a midfielder |7 points
|For each goal scored by a forward |6 points
|For each goal assist |4 points
|For a clean sheet by a goalkeeper (the goalkeeper must also play at least 60 minutes) |4 points
|For a clean sheet by a defender (the defender must also play at least 60 minutes) |4 points
|For a clean sheet by a midfielder (the midfielder must also play at least 60 minutes) |1 point
|For every 4 shot saves by a goalkeeper |1 point
|For each penalty save |4 points
|For every three tackles in the game |2 points
|For every 60 successful passes by a midfielder |2 points
|For each penalty miss |-2 points
|For every 2 goals conceded by the goalkeeper's team or a defender's team |-1 point
|For each yellow card |-1 point
|For each own goal conceded |-2 points
|For each red card (includes any yellow card points) |-3 points

- Captaining a player will give us a double point on that particular player. If the player we captained is not playing, then the double point will be given to selected vice-captain.
Sounds like a proper optimization problem right? 

## The Way to Glory
So I've been playing this game for quite a long time now. I began to get really serious in season 2014/2015, the same time I decided to focus on analytics.
I used to follow my team up close week in and week out. But these days I lost my passion and let things go by pure luck.

![alt text](https://imgur.com/Y19FyT4.png "Point history through out the years")

However, this fellow college of mine started to boast his position in my local league as he's currently top of the league. Normally he stays in mid table for the past few seasons.
**We need to put him back to his place.**

So I identified at least 3 ways we can prepare our team better. Since this is a mid season, it is also a good time to use wildcard if our team is really shite.
1. _The Bill Mill_ : [Bill Mill](https://llimllib.github.io/fantasypl/) creates a really nice tutorial on how to select team based 3 variables: opponents, home or away, and average past scores.
2. _Follow the Shifu_ : We look for the greatest user this season, then copy paste his very own team. We define greatest as consistent in scoring high point. High average point, Low PSI.
3. _Ride the Wave_: We look for top n users, summarize their selection as our selection. Anything happen to top n users, will also happened to you. Just ride the wave and enjoy.

## The Bill Mill
I wont be explaining much about this method. You can check it out yourself in his [page](https://llimllib.github.io/fantasypl/). An R implementation can be found [here](http://pena.lt/y/2014/07/24/mathematically-optimising-fantasy-football-teams/) 


## Follow the Shifu
In this exercise, I will evaluate from top 1000 users who are the greatest user in current season. A user who is a global rank 1 this week does not mean he's a great player. He might be helped by luck and score some nice points on certain weeks.
We want to find user who scored high while also consistent on scoring those points.
```R
n_top = 1000
```
First we need to crawl the [leaderboard](https://fantasy.premierleague.com/a/leagues/standings/313/classic) page of global league. Lucky for us, EPL page has been using JSON format to retrieve the data.
Although there's no official API given by EPL, we can use the link responsible by adding Sys.sleep().
In order to find the link, on the page we want to get the data from, right click and choose inspect element. Click on network tab, then refresh the page to get the links used to open the current page.

![alt text](https://imgur.com/rdwqPCl.png "Finding the right links"). 

Once we get the link, get the list of top users.
```R
for (i in 1:(n_top/50)){
  letMeSleep(i, 150, 15)
  url<-paste0("https://fantasy.premierleague.com/drf/leagues-classic-standings/313?phase=1&le-page=1&ls-page=",i)
  dat = read_json(url, simplifyVector = T)
  topUsers_list[[i]] = dat$standings$results
  print(paste("getting top", 50*i, "users"))
}
topUsers = bind_rows(topUsers_list)
```

After getting the list of top users, let's get their past performance this season to determine whether they are consistent or just luck.

```R
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
```
Here, I prepared a dataframe **seasonHistory** to know top user's performance in previous years. However, in this exercise I won't be using them just yet.
In order for us to read and identify team easily in our pointHistory, we need variable team name from topUsers. Do left join. Next, create a function to summarize
our beloved top 1000 users performance. I simply used PSI and average as performance metric.
```R
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
```
By now, we have a dataframe **temp** that can help us finding the Shifu. Let's plot the data to grasp the distribution.
```R
plot_shifu = function(x,y,label){
  plot(x, y)
  abline(v = mean(x),col="purple")
  abline(h = mean(y),col="red")
  print("Double click the points to see the labels")
  identify(x, y, labels = label, cex = 0.7, col="blue")
  #text(x, y, temp$entry_name, cex=0.6, pos=4, col="red")
}
plot_shifu(temp$avg, temp$PSI, temp$entry_name)
```
This plot is an interactive one. Since too many points are overlapping each other, labeling these point will make the plot hard to read.
This interactive plot will only label the dots that we click. Once we are happy on the dots where we want to see the labels, press esc.

![alt text](https://imgur.com/rvSC6Zl.png "Shifu Plot")

So what can we conclude by now? Current top user, [Boom Xhaka Laca](https://fantasy.premierleague.com/a/team/870749) is indeed a top notch user. 
His average point banging on the windows from other top users. He's also doing it consistently with below average PSI. However, a true genius
would goes to [El-GaMal](https://fantasy.premierleague.com/a/team/1133682). Although his average points not as good, but his PSI score is the lowest 
thus placing him in 716 position.

## Ride the Wave
Idea:
- get current team from top n users.
- summarize players by **team selection** .
- apply linear programming to find optimum team selected by top n users.
- do performance testing by selecting players from week i, test it on week i+1.

Performance testing on 4 weeks (19-20, 20-21, 21-22, 22-23) shows this method will give us score >= average gameweek points.
Ride the wave vs average point on that week (90-62, 57-45, 45-45, 79-58).

## Selecting Captain
Idea: 
- Find the player who got most capped as captain on the latest week.

If wildcard is still an option and you need to change lots of players (by lots I mean > 5 infield players), probably its best to use it now. 
Otherwise, hold the wildcard until double week.

My target by the end of season: top 100,000 global. Lets hope luck is on our side now.
