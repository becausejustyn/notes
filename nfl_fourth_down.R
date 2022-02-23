
#https://rpubs.com/afuecker/727520

library(ggplot2)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(scales)
library(dplyr)
library(cowplot)

pbp

fourth_down <- data %>%
  filter(
    down == 4, 
    season_type == "REG"
    ) %>%
  select(
    posteam, ydstogo, yardline_100, 
    fixed_drive_result, fixed_drive, 
    fourth_down_converted, play_type,
    season, special_teams_play, qtr,
    game_seconds_remaining, score_differential) %>%
  data.frame()

fourth_down %>%
  mutate(posteam = as_factor(posteam))

summary(fourth_down)

## How Many More Yards To Go?
## 

ydstogo_tot <- fourth_down %>%
  select(ydstogo) %>%
  group_by(ydstogo) %>%
  summarise(n=length(ydstogo), .groups='keep') %>%
  data.frame()

xbreaks <- seq(1, max(fourth_down$ydstogo, by=1))
all_hist <- ggplot(fourth_down, aes(x=ydstogo)) + 
  geom_histogram(bins=max(fourth_down$ydstogo), color="black", fill="steelblue2") +
  labs(title="Histogram of Yards to Go on 4th Down, 2020 Regular Season", x="Yards To Go", y="Number of Instances") +
  theme_light() +
  theme(plot.title = element_text(hjust=.5)) +
  scale_x_continuous(breaks=xbreaks, labels=xbreaks) + 
  geom_label_repel(inherit.aes=FALSE, 
                   data=ydstogo_tot,
                   aes(x=ydstogo, y=n, label= ifelse(n == max(n), n, "")), 
                   box.padding = 2, 
                   point.padding = 2, 
                   nudge_x=1.5,
                   size=4, 
                   color="Grey50", 
                   segment.color="black") +
  annotate("rect", xmin=40, xmax=43, ymin=-10, ymax=40, alpha=0.2) +
  annotate("text", x=38, y=120, label= paste(sum(ydstogo_tot[ydstogo_tot$ydstogo>39, "n"]), "plays at more\nthan 40 yards to go"))



conv_attempt <- fourth_down %>%
  filter(play_type %in% c("run", "pass")) %>%
  select(ydstogo) %>%
  data.frame()

conv_ydstogo_tot <- conv_attempt %>%
  select(ydstogo) %>%
  group_by(ydstogo) %>%
  summarise(n=length(ydstogo), .groups='keep') %>%
  data.frame()

conv_xbreaks <- seq(1, max(conv_attempt$ydstogo, by=1))
conv_hist <- ggplot(conv_attempt, aes(x=ydstogo)) + 
  geom_histogram(bins=max(conv_attempt$ydstogo), color="black", fill="steelblue2") +
  labs(title="Histogram of Yards to Go on 4th Down, Conversion Attempts", x="Yards To Go", y="Number of Instances") +
  theme_light() +
  theme(plot.title = element_text(hjust=.5)) +
  scale_x_continuous(breaks=conv_xbreaks, labels=conv_xbreaks)  +
  geom_label_repel(inherit.aes=FALSE, 
                   data=conv_ydstogo_tot,
                   aes(x=ydstogo, y=n, label= ifelse(n == max(n), n, "")), 
                   box.padding = 2, 
                   point.padding = 2, 
                   nudge_x=1,
                   size=4, 
                   color="Grey50", 
                   segment.color="black") 

plot_grid(all_hist, conv_hist, nrow=2)

## Team Play Results

play_selection <- fourth_down %>%
  filter(play_type != "no_play") %>%
  select(posteam, play_type, fourth_down_converted, fixed_drive_result) %>%
  mutate(my_play_type = ifelse(
    fourth_down_converted==1, "Convert", ifelse(
      fixed_drive_result=="Field goal", fixed_drive_result, ifelse(
        fixed_drive_result=="Punt", fixed_drive_result, "Fail"
      )
    )
  )
  ) %>%
  group_by(posteam, my_play_type) %>%
  summarize(n=length(fixed_drive_result), .groups='keep') %>%
  group_by(posteam)%>%
  mutate(percent_of_total=round(100*n/sum(n),1))%>%
  ungroup() %>%
  data.frame()

afc_teams <- c("BUF", "MIA", "NE", "NYJ", "BAL", "CIN", "CLE", "PIT", "HOU", "IND", "JAX", "TEN", "DEN", "KC", "LV", "LAC")
nfc_teams <- c("DAL", "NYG", "PHI", "WAS", "CHI", "DET", "GB", "MIN", "ATL", "CAR", "NO", "TB", "ARI", "LA", "SF", "SEA")

afc_pies <- ggplot(data=play_selection[play_selection$posteam %in% afc_teams,], aes(x="", y=n, fill=my_play_type)) +
  geom_bar(stat="identity", position="fill", width=1) +
  coord_polar(theta="y", start=0) +
  theme_light()+
  labs(fill="Play Type", x=NULL, y=NULL, title="4th Down Play Results by AFC Team") +
  theme(plot.title=element_text(hjust=0.5),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette="RdYlBu") +
  facet_wrap(~posteam, ncol=4, nrow=4)+
  geom_text(aes(x=1.75, label=paste0(percent_of_total, "%")),
            size=3,
            position=position_fill(vjust=.5))

nfc_pies <- ggplot(data=play_selection[play_selection$posteam %in% nfc_teams,], aes(x="", y=n, fill=my_play_type)) +
  geom_bar(stat="identity", position="fill") +
  coord_polar(theta="y", start=0) +
  theme_light()+
  labs(fill="Play Type", x=NULL, y=NULL,title="4th Down Play Results by NFC Team") +
  theme(plot.title=element_text(hjust=0.5),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())+
  scale_fill_brewer(palette="RdYlBu") +
  facet_wrap(~posteam, ncol=4, nrow=4)+
  geom_text(aes(x=1.75, label=paste0(percent_of_total, "%")),
            size=3,
            position=position_fill(vjust=.5))

pie_grid <- plot_grid(nfc_pies + theme(legend.position = "none"), afc_pies + theme(legend.position = "none"), nrow=1)
legend <- get_legend(afc_pies + theme(legend.box.margin=margin(0,0,0,12)))
plot_grid(pie_grid, legend, rel_widths=c(2,.18))

## Team Punting

punts <- fourth_down %>%
  filter(play_type=="punt") %>%
  select(fixed_drive, posteam, season, play_type) %>%
  group_by(posteam, season) %>%
  summarise(n=length(fixed_drive), .groups='keep') %>%
  data.frame()

team_punts <- merge(x=punts, y=teams_colors_logos, by.x="posteam", by.y="team_abbr")

ggplot(team_punts, aes(x=reorder(posteam, n), y=n)) +
  geom_bar(stat="identity", aes(fill=posteam)) +
  coord_flip() + 
  labs(title="Punts by Team in 2020 NFL Regular Season", 
       x="Team", y="Number of Punts", 
       fill="Team") +
  scale_color_manual(values=team_punts$team_color) +
  scale_fill_manual(values=team_punts$team_color) +
  theme_light() +
  theme(plot.title = element_text(hjust=.5)) +
  geom_text(data=team_punts, aes(x=posteam, y=n, label=n, fill=NULL),hjust=-0.1, size=4)

## Where Are Teams Going For It?

go_for_it <- fourth_down %>%
  filter(play_type == "run" | play_type == "pass") %>%
  select(yardline_100, ydstogo, play_type) %>%
  mutate(my_ydstogo = ifelse(ydstogo > 15, 15, ydstogo),
         my_yardline = cut(yardline_100, breaks=seq(0,100, by=5))
  ) %>%
  group_by(my_ydstogo, my_yardline) %>%
  summarize(n=length(play_type), .groups='keep')%>%
  data.frame()

all_for_it <- fourth_down %>%
  filter(play_type %in% c("run", "pass", "punt", "field_goal")) %>%
  select(yardline_100, ydstogo, play_type) %>%
  mutate(my_ydstogo = ifelse(ydstogo > 15, 15, ydstogo),
         my_yardline = cut(yardline_100, breaks=seq(0,100, by=5))
  ) %>%
  group_by(my_ydstogo, my_yardline) %>%
  summarize(all_n=length(play_type), .groups='keep')%>%
  data.frame()

total <- merge(go_for_it,all_for_it, by=c("my_ydstogo", "my_yardline"))
total$percent <- total$n/total$all_n

ylabs <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45",
           "45-50", "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80-85", "85-90",
           "90-95")

ggplot(total, aes(x=my_ydstogo, y=my_yardline, fill=percent)) +
  geom_tile(color="grey") +
  geom_text(aes(label=scales::percent(accuracy=0.1, percent)), size=3)+
  labs(title="4th Down Conversion Attempts Percentage\nby Goal Distance and Yards to Go\n2020 Regular Season",
       x="Yards to go",
       y="Yards from Opponent Endzone",
       fill="Percentage of Plays\nThat Are Conversion\nAttempts",
       caption="Any play of 15 or more yards to go labeled as 15+") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.5)) +
  scale_x_continuous(labels=append(seq(1,14,by=1), "15+"), breaks=seq(1,15,by=1)) +
  scale_y_discrete(labels=ylabs) + 
  scale_fill_continuous(low="blue", high="red", breaks=seq(0,1, by=.1)) 

## Play Type Probability

probs <- fourth_down %>%
  filter(play_type %in% c("run", "pass", "punt", "field_goal")  &qtr != 5) %>%
  select(play_type, game_seconds_remaining, score_differential) %>%
  mutate(minutes = ceiling(game_seconds_remaining/60),
         my_playtype = ifelse(play_type %in% c("run", "pass"), "attempt", play_type),
         leading = ifelse(score_differential > 0, "Leading", ifelse(score_differential <0, "Trailing","Tied"))) %>%
  group_by(my_playtype, minutes, leading) %>%
  summarise(n=length(game_seconds_remaining), .groups='keep') %>%
  group_by(minutes, leading)%>%
  mutate(percent_of_total=round(100*n/sum(n),1))%>%
  ungroup() %>%
  data.frame()

minute_levels <-seq(60, 1, by=-1)
minute_breaks <- seq(60, 0, by=-5)
probs$minutes <- factor(probs$minutes, levels=minute_levels)
probs$leading <- factor(probs$leading, levels=c("Leading", "Trailing", "Tied"))
ggplot(probs, aes(x=minutes, y=percent_of_total, group=my_playtype)) +
  geom_point(aes(color=my_playtype), size=2) +
  geom_line(aes(color=my_playtype), linetype="dotted") +
  labs(title = "Probability of Play Type by Time Remaining, 2020 Regular Season",
       x="Minutes Remaining",
       y="Probability",
       fill="Play Type",
       caption="Overtime is excluded")+
  theme_light() +
  theme(plot.title=element_text(hjust=0.5)) +
  geom_vline(xintercept=31, linetype="solid", size=1) +
  scale_color_brewer(palette="Set2", name="Play Type") +
  scale_x_discrete(labels=minute_breaks, breaks=minute_breaks ) +
  facet_wrap(~leading, ncol=1)