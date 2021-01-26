# Load libraries
library(tidyverse)
library(baseballr)
library(caret)
library(janitor)
library(splines)

# scrape 2020 hitting data from baseballr
data1=scrape_statcast_savant(start_date="2020-07-22",end_date="2020-07-30",player_type="hitter")
data2=scrape_statcast_savant(start_date="2020-08-01",end_date="2020-08-09",player_type="hitter")
data3=scrape_statcast_savant(start_date="2020-08-10",end_date="2020-08-17",player_type="hitter")
data4=scrape_statcast_savant(start_date="2020-08-18",end_date="2020-08-25",player_type="hitter")
data5=scrape_statcast_savant(start_date="2020-08-26",end_date="2020-09-02",player_type="hitter")
data6=scrape_statcast_savant(start_date="2020-09-03",end_date="2020-09-10",player_type="hitter")
data7=scrape_statcast_savant(start_date="2020-09-11",end_date="2020-09-19",player_type="hitter")
data8=scrape_statcast_savant(start_date="2020-09-20",end_date="2020-09-28",player_type="hitter")

data=rbind(data1,data2,data3,data4,data5,data6,data7,data8)
rm(data1,data2,data3,data4,data5,data6,data7,data8)

# clean data
data=data%>% 
  filter(balls!="4", !pitch_type %in% c("KN","null","FO","CS"))%>% # filter the noise
  mutate(called=case_when(
    description == "ball" ~ 0, # 0 for ball
    description == "called_strike" ~ 1, # 1 for a strike
    TRUE ~ NA_real_), # NA otherwise
    pitch_type=case_when(
      pitch_type %in% c("KC","CU") ~ "CU", # knuckle curveballs and curveballs as curveballs
      pitch_type %in% c("CH","FS") ~ "CH", # changeups and splitters as changeups
      TRUE ~ pitch_type # everything else stays the same
    ))%>%
  drop_na(plate_x, plate_z)%>% # drop NAs in plate location
  unite("count",balls, strikes,sep="-")%>% # create factors
  mutate(called=as.factor(called),
         count=as.factor(count),
         stand=as.factor(stand),
         p_throws=as.factor(p_throws),
         pitch_type=as.factor(pitch_type))


called_pitches=data%>% # filter just called pitches
  drop_na(called)


x=c(-0.95,0.95,0.95,-0.95,-0.95)
y=c(1.5,1.5,3.5,3.5,1.5)
sz=data.frame(x,y) # create strike zone

# split data up to create a smaller sample size
# split to train and test
vector = createDataPartition(called_pitches$called, p = 0.8, list= F, times = 1) # 20/80
train = called_pitches[vector,]
test = called_pitches[-vector,]

# fit logistic regression model to find called strike probability
model=glm(called~ns(plate_z,6)+ns(plate_x,6)+count+stand+p_throws+ns(sz_top,7)*ns(sz_bot,6)+
            release_speed,
          data=train,family="binomial")

test%>% # test accuracy on test data set
  mutate(pred=1*(predict(model,test,type="response")>=0.5),
         accurate=1*(pred==called))%>%
  drop_na(pred,accurate)%>%
  summarise(accuracy=mean(accurate))

# visualize probability
ggplot()+stat_summary_hex(data=test,aes(x=plate_x,y=plate_z,z=predict(model,test,type="response")),
                          bins=50)+
  geom_path(data=sz,aes(x=x,y=y),color="red",size=1) +
  scale_fill_gradient2(high="forestgreen",low="blue",mid="black",midpoint=0.5)

# predict model onto entire data set
data=data%>%
  mutate(strike_prob=predict(model,data,type="response"),
         swing=ifelse(description %in% c("foul","swinging_strike","hit_into_play_no_out", 
                                         "hit_into_play","foul_bunt","hit_into_play_score",
                                         "foul_tip","swinging_strike_blocked",
                                         "missed_bunt","bunt_foul_tip"),1,0)) # 1 if swing, 0 if not

# predict swing probability
vector3=createDataPartition(data$swing, p=0.5, list=F, times=1)
swing_data=data[vector3,] # split data in half because initial data set is very large
vector2=createDataPartition(swing_data$swing, p=0.8, list=F, times=1) # 20/80 split
train2=swing_data[vector2,]
test2=swing_data[-vector2,]

# logistic regression model
swing_model=glm(swing~ns(plate_z,5)+ns(plate_x,5)+ns(pfx_x,3)+ns(pfx_z,3)+count+pitch_type+ns(sz_top,6)+
                  ns(sz_bot,6),
                data=train2,family="binomial")

test2%>% # test accuracy of the model
  mutate(pred=1*predict(swing_model, test2, type="response")>=0.5,
         accurate=1*(pred==swing))%>%
  summarise(accuracy=mean(accurate))

# predict on entire data set
data=data%>%
  mutate(swing_prob=predict(swing_model,data,type="response"),
         points=case_when(
           swing == 1 ~ strike_prob-(1-strike_prob) - swing_prob, # create metric with probabilitys 
           swing == 0 ~ (1-strike_prob)-strike_prob + swing_prob))

# data set for only hitters who say at least 400 pitches
join_data=data%>%
  group_by(player_name)%>%
  summarise(avg_points=mean(points),
            n=n())%>%
  filter(n>=400)%>% # >= 400 pitches
  arrange(desc(avg_points))%>%
  select(player_name,avg_points,n)%>%
  ungroup()

# join with Fangraphs stats
fangraphs=read_csv("fangraphs.csv",
                  col_types =  cols(
                     'BB%'=col_number(),
                     "K%"=col_number()))

# inner join
new_data=inner_join(join_data,fangraphs,by=c("player_name"="Name"))
cor(new_data$`BB%`,new_data$avg_points)

# visualize the relationship
ggplot(data=new_data,aes(x=avg_points,y=`BB%`))+geom_point()+geom_smooth(method="lm")+
  labs(title="BB% vs. Avg Points",x="Avg Points",subtitle="2020 Season")+theme_bw()
