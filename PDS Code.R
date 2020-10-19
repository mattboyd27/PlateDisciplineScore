# Load packages
library(tidyverse)
library(randomForest)

# Load all 2019 pitches from Statcast
load("~/Full year Statcast data/2019 Hitting data.RData")

#Strike zone
x=c(-0.95,0.95,0.95,-0.95,-0.95)
y=c(1.5,1.5,3.5,3.5,1.5)
sz=data.frame(x,y)
x=c(0,0.95,0.95,-0.95,-0.95,0)
y=c(0.5,0.2,0.1,0.1,0.2,0.5)
plate=data.frame(x,y)

# Preperation for called strike model
zone=subset(season2019,description=="called_strike"|description=="ball")
zone$release_spin_rate=as.numeric(zone$release_spin_rate)
zone$description1=ifelse(zone$description=="ball",0,1)
zone=subset(zone,!is.na(plate_z)|!is.na(plate_x))
zone=subset(zone,!is.na(description1))

# test/train. Train on 10% of data because dataset is so big
df=sort(sample(nrow(data), nrow(data)*.1))
train=data[df,]
test=data[-df,]

# model
model=randomForest(description1~plate_x+plate_z,data=train,ntree=201)
plot(model)

# Predict on test data
test$pred=predict(model,test,type="response")

# Remove NAs
season2019=subset(season2019,!is.na(plate_z)|!is.na(plate_x))

# Predict on entire dataset 
season2019$pred=predict(model,season2019,type="response")


season2019$swing_prob=predict(swing_model,season2019,type="response")
#visualize
ggplot()+
  geom_point(data=season2019,aes(x=-plate_x,y=plate_z,color=pred))+
  geom_path(data=sz,aes(x=x,y=y))+labs(color="Called Strike Probability")+
  scale_color_gradient2(high="goldenrod2",mid="white",low="blue",midpoint=0.5)+
  ggtitle("MLB Called Strike Probability",subtitle="2019 Season")+
  geom_path(data=plate,aes(x=x,y=y))+ylim(-4,7.5)+xlim(-4,4)
# discipline points
season2019=season2019[,c("player_name","description","plate_x","plate_z","strikes","launch_speed",
                         "pitch_type","p_throws","game_date","inning","balls","pred",
                         "pfx_x","pfx_z","release_pos_x","rlease_pos_z","release_speed")]

# Swing probability model preperation
season2019$swing=ifelse(season2019$description %in% c("foul","swinging_strike","hit_into_play_no_out",
                                                      "hit_into_play","foul_bunt","hit_into_play_score",
                                                      "foul_tip","swinging_strike_blocked",
                                                      "missed_bunt","bunt_foul_tip"),1,0)

season2019=subset(season2019,!is.na(swing))
season2019$strikes=as.factor(season2019$strikes)
#test/train. Train on 10% of data because dataset is so big
df=sort(sample(nrow(data), nrow(data)*.1))
train=data[df,]
test=data[-df,]
# swing probability model
swing_model=randomForest(swing~plate_x+plate_z+strikes+pfx_x+pfx_z+release_pos_x+
                           release_pos_z+release_speed,
                         data=train,ntree=401)
# predict on the entire dataset
season2019$swing_prob=predict(model,season2019_1,type="response")

# lay off gets points, swings subtract points
swings=subset(season2019,description %in% c("foul","swinging_strike","hit_into_play_no_out",
                                            "hit_into_play","foul_bunt","hit_into_play_score",
                                            "foul_tip","swinging_strike_blocked",
                                            "missed_bunt","bunt_foul_tip"))
take=subset(season2019,description %in% c("called_strike","ball","blocked_ball","hit_by_pitch",
                                          "pitchout"))

# Create plate discipline score per pitch
swings$points=(swings$pred-(1-swings$pred))-swings$swing_prob
take$points=((1-take$pred)-take$pred)+take$swing_prob

# group by player for the swings data frame
swings=data.frame(swings)
groupS=swings%>%
  group_by(player_name)%>%
  summarize(swing_points=round(sum(points),6),
            n=n())%>%
select(player_name,swing_points,n)
groupS=data.frame(groupS)

# group by player for the swings data frame
take=data.frame(take)
groupT=take%>%
  group_by(player_name)%>%
  summarize(take_points=round(sum(points),6),
            n=n())%>%
  select(player_name,take_points,n)%>%
  arrange(desc(take_points))
groupT=data.frame(groupT)


# join the two data frames
join=inner_join(groupS,groupT,by=c("player_name"="player_name"))


#total points
join$total=join$take_points+join$swing_points
join$pitches=join$n.x+join$n.y
join=subset(join,pitches>=200)
join=join[,c(1,6,7)]
join%>%arrange(desc(pitches))

# points per pitch
join$points_per_pitch=join$total/join$pitches
join%>%arrange(desc(points_per_pitch))

# join 2019 fangraphs
fangraphs <- read_csv("FanGraphs Leaderboard 2019.csv")
fangraphs=separate(data=fangraphs,col=BB.,into=c("BB","P"),sep="%")
fangraphs=separate(data=fangraphs,col=K.,into=c("K","L"),sep="%")
group=inner_join(join,fangraphs,by=c("player_name"="?..Name"))
group$BB=as.numeric(group$BB)
group$K=as.numeric(group$K)
group=data.frame(group)

group=group[,c(1,2,3,4,12,14,16,17,18,19,20,21,26)]
head(group%>%arrange(desc(points_per_pitch)),1)


#visualize PDS per pitch vs BB%
ggplot(data=group,aes(x=points_per_pitch,y=BB))+geom_point()+
  geom_smooth(fill=NA,se=FALSE,method=lm,color="dodgerblue")+ylab("Base on Balls Percent")+
  xlab("PDS Per Pitch")+ggtitle("Base on Balls Percent vs. PDS Per Pitch",subtitle="2019 Season")

