#rm(list=ls())
library(nflscrapR)
library(ggplot2)
library(dplyr)
library(tidyr)

# Collect data for 2018 NFL season using nflscrapR library (https://www.rdocumentation.org/packages/nflscrapR/versions/1.4.0)

#pbp_2018 <- season_play_by_play(2018)
pbp_2018 <- read.csv("C:/Users/micha/Dropbox/Files/Projects/nfl_pbp_2018.csv",stringsAsFactors = FALSE)[,-c(1,2)]

#long to wide
pbp_2018$long_pass=0
pbp_2018$short_pass=0
pbp_2018$run_end=0
pbp_2018$run_guard=0
pbp_2018$run_tackle=0
pbp_2018$run_left=0
pbp_2018$run_middle=0
pbp_2018$run_right=0

pbp_2018$run_yards_end=0
pbp_2018$run_yards_guard=0
pbp_2018$run_yards_tackle=0
pbp_2018$run_yards_left=0
pbp_2018$run_yards_middle=0
pbp_2018$run_yards_right=0
pbp_2018$postseason=0

pbp_2018$long_pass[pbp_2018$PassLength=="Deep"]=1
pbp_2018$short_pass[pbp_2018$PassLength=="Short"]=1
pbp_2018$GameID=as.Date(substr(pbp_2018$GameID,1,8),format="%Y%m%d", origin = "1960-10-01")
pbp_2018$postseason[pbp_2018$GameID>"2018-12-31"]=1

pbp_2018$run_end[pbp_2018$RunGap=="end"]=1
pbp_2018$run_guard[pbp_2018$RunGap=="guard"]=1
pbp_2018$run_tackle[pbp_2018$RunGap=="tackle"]=1
pbp_2018$run_left[pbp_2018$RunLocation=="left"]=1
pbp_2018$run_middle[pbp_2018$RunLocation=="middle"]=1
pbp_2018$run_right[pbp_2018$RunLocation=="right"]=1

pbp_2018$run_yards_end[pbp_2018$RunGap=="end" & !is.na(pbp_2018$RunGap)]=pbp_2018$Yards.Gained[pbp_2018$RunGap=="end" & !is.na(pbp_2018$RunGap)]
pbp_2018$run_yards_guard[pbp_2018$RunGap=="guard" & !is.na(pbp_2018$RunGap)]=pbp_2018$Yards.Gained[pbp_2018$RunGap=="guard" & !is.na(pbp_2018$RunGap)]
pbp_2018$run_yards_tackle[pbp_2018$RunGap=="tackle" & !is.na(pbp_2018$RunGap)]=pbp_2018$Yards.Gained[pbp_2018$RunGap=="tackle" & !is.na(pbp_2018$RunGap)]
pbp_2018$run_yards_left[pbp_2018$RunLocation=="left" & !is.na(pbp_2018$RunLocation)]=pbp_2018$Yards.Gained[pbp_2018$RunLocation=="left" & !is.na(pbp_2018$RunLocation)]
pbp_2018$run_yards_middle[pbp_2018$RunLocation=="middle" & !is.na(pbp_2018$RunLocation)]=pbp_2018$Yards.Gained[pbp_2018$RunLocation=="middle" & !is.na(pbp_2018$RunLocation)]
pbp_2018$run_yards_right[pbp_2018$RunLocation=="right" & !is.na(pbp_2018$RunLocation)]=pbp_2018$Yards.Gained[pbp_2018$RunLocation=="right" & !is.na(pbp_2018$RunLocation)]

pbp_2018$down=as.numeric(as.character(pbp_2018$down))


freq.rush=as.data.frame(pbp_2018) %>% dplyr::count(Rusher,Rusher_ID)
freq.rush=as.data.frame(freq.rush)
freq.rush=plyr::ddply(freq.rush,~Rusher_ID,mutate,main_ID=max(n))
freq.rush$Rusher_ID[freq.rush$n==freq.rush$Main_ID]
freq.rush.corrected=freq.rush[which(freq.rush$n==freq.rush$main_ID),]


pbp_2018_fix=merge(pbp_2018,freq.rush.corrected,by=c("Rusher_ID"),all=FALSE)
pbp_2018_fix=pbp_2018_fix[which(pbp_2018_fix$n==pbp_2018_fix$main_ID),]
pbp_2018_fix$Rusher=pbp_2018_fix$Rusher.y
pbp_2018_fix=pbp_2018_fix[which(pbp_2018_fix$n==pbp_2018_fix$main_ID),]
test=subset(pbp_2018_fix,Rusher.y=="S.Barkley" & GameID=="2018-12-16")
test1=subset(pbp_2018_fix,Rusher_ID=="00-0034844" & GameID=="2018-12-16")

freq.rec=as.data.frame(pbp_2018) %>% dplyr::count(Receiver,Receiver_ID)
freq.rec=as.data.frame(freq.rec)
freq.rec=plyr::ddply(freq.rec,~Receiver_ID,mutate,main_ID=max(n))
freq.rec$Receiver_ID[freq.rec$n==freq.rec$Main_ID]
freq.rec.corrected=freq.rec[which(freq.rec$n==freq.rec$main_ID),]
colnames(freq.rec.corrected)[c(3,4)]=paste0(colnames(freq.rec.corrected)[c(3,4)],".rec")

pbp_2018_fix=merge(pbp_2018_fix,freq.rec.corrected,by=c("Receiver_ID"),all=FALSE)
pbp_2018_fix=pbp_2018_fix[which(pbp_2018_fix$n.rec==pbp_2018_fix$main_ID.rec),]
pbp_2018_fix$Receiver=pbp_2018_fix$Receiver.y
test=subset(pbp_2018_fix,Receiver.y=="T.Hill" & GameID=="2018-10-01")
test1=subset(pbp_2018_fix,Receiver_ID=="00-0033040" & GameID=="2018-10-01")

pbp_2018_fix=subset(pbp_2018_fix,n>10 & n.rec>10)

rush_plyr_game <- pbp_2018_fix %>% filter(PlayType == 'Run') %>%
  group_by(Rusher,Rusher_ID,GameID,HomeTeam,posteam,DefensiveTeam) %>% 
  dplyr::summarise(carries = n(),
            total_yards = sum(Yards.Gained),
            touchdown=sum(Touchdown),
            fumble=sum(Fumble),
            first_downs=sum(FirstDown),
            mean_ydstogo=mean(ydstogo),
            mean_ScoreDiff=mean(ScoreDiff),
            mean_ydline=mean(yrdline100), #99 is your own 1 yard line, 1 is the opponents 1 yard line. 
            mean_qtr=mean(qtr,na.rm=T),
            mean_down=mean(down,na.rm=T),
            end_run=sum(run_end),
            guard_run=sum(run_guard),
            tackle_run=sum(run_tackle),
            left_run=sum(run_left),
            middle_run=sum(run_middle),
            right_run=sum(run_right),
            end_run_yards=sum(run_yards_end),
            guard_run_yards=sum(run_yards_guard),
            tackle_run_yards=sum(run_yards_tackle),
            left_run_yards=sum(run_yards_left),
            middle_run_yards=sum(run_yards_middle),
            right_run_yards=sum(run_yards_right),
            max_run=max(Yards.Gained),
            med_run=median(Yards.Gained)) %>% 
  filter(carries >= 1) %>%
  arrange(desc(total_yards)) %>%
  mutate(yards_per_carry=total_yards/carries,
         first_downs_share=first_downs/carries,
         left_run=round(left_run/carries,2),
         middle_run=round(middle_run/carries,2),
         right_run=round(right_run/carries,2),
         end_run=round(end_run/(end_run+guard_run+tackle_run),2),
         guard_run=round(guard_run/(end_run+guard_run+tackle_run),2),
         tackle_run=round(tackle_run/(end_run+guard_run+tackle_run),2),
         end_run_yards=round(end_run_yards/total_yards,2),
         tackle_run_yards=round(tackle_run_yards/total_yards,2),
         guard_run_yards=round(guard_run_yards/total_yards,2),
         left_run_yards=round(left_run_yards/total_yards,2),
         right_run_yards=round(right_run_yards/total_yards,2),
         middle_run_yards=round(middle_run_yards/total_yards,2)
        )

rush_plyr_game$avg_v_median_yards_carry=rush_plyr_game$yards_per_carry/rush_plyr_game$med_run

rush_plyr_game.df=as.data.frame(rush_plyr_game)
for (i in 7:ncol(rush_plyr_game.df)){
  rush_plyr_game.df[,i]=as.numeric(gsub(Inf,NA,rush_plyr_game.df[,i]))
  rush_plyr_game.df[,i]=as.numeric(gsub(-Inf,NA,rush_plyr_game.df[,i]))
  rush_plyr_game.df[,i]=as.numeric(gsub(NaN,NA,rush_plyr_game.df[,i]))
}

rush_plyr_game.df=arrange(rush_plyr_game.df,GameID)
rush_plyr_game.df=arrange(rush_plyr_game.df,Rusher_ID)

rec_plyr_game <- pbp_2018_fix %>% filter(PlayType == 'Pass') %>%
  group_by(Receiver,Receiver_ID,GameID,HomeTeam,posteam,DefensiveTeam,Passer) %>% 
  dplyr::summarise(receptions=sum(Reception),
            total_yards = sum(Yards.Gained),
            yac=sum(YardsAfterCatch),
            touchdown=sum(Touchdown),
            fumble=sum(Fumble),
            mean_ScoreDiff=mean(ScoreDiff),
            mean_qtr=mean(qtr,na.rm=T),
            mean_down=mean(down,na.rm=T),
            pass_attempts=sum(PassAttempt),
            long_pass=sum(long_pass),
            short_pass=sum(short_pass),
            first_downs=sum(FirstDown),
            mean_ydstogo=mean(ydstogo),
            max_rec=max(Yards.Gained),
            med_rec=median(Yards.Gained)) %>% 
  filter(receptions>=1) %>%
  arrange(desc(total_yards)) %>% 
  mutate(catch_rate=receptions/pass_attempts,yards_per_catch=total_yards/receptions,first_downs_share=first_downs/receptions,
         short_pass_share=short_pass/pass_attempts,
         long_pass_share=long_pass/pass_attempts)
  
rec_plyr_game$avg_v_median_yards_rec=rec_plyr_game$yards_per_catch/rec_plyr_game$med_rec

rec_plyr_game.df=as.data.frame(rec_plyr_game)

rec_plyr_game.df=as.data.frame(rec_plyr_game)
for (i in 7:ncol(rec_plyr_game.df)){
  rec_plyr_game.df[,i]=as.numeric(gsub(Inf,NA,rec_plyr_game.df[,i]))
  rec_plyr_game.df[,i]=as.numeric(gsub(-Inf,NA,rec_plyr_game.df[,i]))
  rec_plyr_game.df[,i]=as.numeric(gsub(NaN,NA,rec_plyr_game.df[,i]))
}

rec_plyr_game.df=arrange(rec_plyr_game.df,GameID)
rec_plyr_game.df=arrange(rec_plyr_game.df,Receiver_ID)

write.csv(rush_plyr_game.df,"C:/Users/micha/Dropbox/Files/Projects/rush_game_data_2018.csv",row.names = FALSE)
write.csv(rec_plyr_game.df,"C:/Users/micha/Dropbox/Files/Projects/rec_game_data_2018.csv",row.names = FALSE)

# rushing_stats <- pbp_2018_fix %>%
#   filter(PlayType == 'Run' & Rusher %in% rush_cnt$Rusher & Yards.Gained <=50) %>%
#   filter(down!=4 & !is.na(down)) %>%
#   filter(!is.na(RunLocation))