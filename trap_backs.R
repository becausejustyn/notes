

# Football: Visualizing TRAP Backs

library(tidyverse)
library(dplyr)
library(ggimage)
library(nflfastR)

pbp <- get_pbp_data(2021)



joined <- load_ngs(stat_type = "rushing", join_roster = TRUE, pbp_data = pbp) %>%
  mutate(
    ten_zone_rush = if_else(yardline_100 <= 10 & rush_attempt == 1, 1, 0),
    ten_zone_pass = if_else(yardline_100 <= 10 & pass_attempt == 1 & sack == 0, 1, 0),
    ten_zone_rec = if_else(yardline_100 <= 10 & complete_pass == 1, 1, 0),
    field_touch = case_when(
      yardline_100 <= 100 & yardline_100 >= 81 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_100_81",
      yardline_100 <= 80 & yardline_100 >= 61 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_80_61",
      yardline_100 <= 60 & yardline_100 >= 41 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_60_41",
      yardline_100 <= 40 & yardline_100 >= 21 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_40_21",
      yardline_100 <= 20 & yardline_100 >= 0 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_20_1",
      TRUE ~ "other"
    )
  )

#Visualizing TB Touch Percent Based on Distance from the End Zone

rb_touches <- joined %>%
  filter(
    player_position == "RB",
    !field_touch == "other"
    ) %>%
  group_by(
    name,
    rusher_player_id,
    field_touch
  ) %>%
  summarise(touches = n()) %>%
  group_by(name, rusher_player_id) %>%
  mutate(
    total_touches = sum(touches),
    pct_touches = touches / total_touches
  ) %>%
  filter(total_touches >= 100)



rb_touches_2 <- rb_touches %>%
  filter(field_touch == "touch_20_1") %>%
  select(name, rusher_player_id, pct_touches)

rb_touches <- left_join(
  rb_touches, rb_touches_2,
  by = c("name", "rusher_player_id")) %>%
  mutate(
    field_touch = factor(field_touch, levels = c("touch_20_1", "touch_40_21", "touch_60_41", "touch_80_61", "touch_100_81"))
  ) %>% ungroup()



library(RColorBrewer)


colors <- brewer.pal(name = "RdYlGn", n = rb_touches[['field_touch']] %>% nlevels())

names(colors) <- rb_touches[['field_touch']] %>% levels() %>% rev()

rb_touches %>% distinct(name, .keep_all = TRUE) %>% slice_max(pct_touches.y, n = 15)

rb_touches %>%
  ggplot(aes(
    x = pct_touches.x, 
    y = reorder(name, pct_touches.y), 
    fill = field_touch
  )) +
  geom_col(position = "fill") +
  scale_fill_manual(
    values = colors,
    limits = c("touch_100_81", "touch_80_61", "touch_60_41", "touch_40_21", "touch_20_1"), 
    labels = c("100 to 81 yds", "80 to 61 yds", "60 to 41 yds", "40 to 21 yds", "20 to 1 yds")
  ) +
  labs(
    x = "Percent of plays",
    fill = "Dist from end zone",
    title = "RB touch % based on how far away from the goal line the touch was (min. 100 touches)",
    caption = "Data: nflfastR"
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  )


rb_touches %>%
  ggplot() +
  geom_col(aes(
    x = pct_touches.x, 
    y = reorder(name, pct_touches.y), 
    fill = field_touch), position = "fill") +
  scale_fill_manual(
    values = colors,
    limits = c("touch_100_81", "touch_80_61", "touch_60_41", "touch_40_21", "touch_20_1"), 
    labels = c("100 to 81 yds", "80 to 61 yds", "60 to 41 yds", "40 to 21 yds", "20 to 1 yds")) +
  labs(
    x = "Percent of plays",
    fill = "Dist from end zone",
    title = "RB touch % based on how far away from the goal line the touch was (min. 100 touches)",
    caption = "Data: nflfastR"
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)#, expand = c(0, 0.01)
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  )
  

rb_touches %>%
  ungroup() %>%
  group_by(name) %>%
  slice_max(pct_touches.x, n = 15) %>%
  ggplot() +
  geom_col(aes(
    x = pct_touches.x, 
    y = reorder(name, pct_touches.y), 
    fill = field_touch), position = "fill") +
  scale_fill_manual(
    values = colors,
    limits = c("touch_100_81", "touch_80_61", "touch_60_41", "touch_40_21", "touch_20_1"), 
    labels = c("100 to 81 yds", "80 to 61 yds", "60 to 41 yds", "40 to 21 yds", "20 to 1 yds"))


# Visualizing High-Value Touches and the TRAP Model

#high-value touches (HVT)


rb_hvt <- joined %>%
  filter(player_position == "RB") %>%
  group_by(
    player_display_name,
    player_gsis_id,
    posteam
  ) %>%
  summarise(
    rush_attempts = sum(rush_attempt),
    ten_zone_rushes = sum(ten_zone_rush),
    receptions = sum(complete_pass),
    total_touches = rush_attempts + receptions,
    hvts = receptions + ten_zone_rushes,
    non_hvts = total_touches - hvts,
    hvt_pct = hvts / total_touches,
    non_hvt_pct = non_hvts / total_touches
  )

#correct format needed for the ggplot 

rb_hvt <- rb_hvt %>%
  pivot_longer(cols = c(hvt_pct, non_hvt_pct), names_to = "hvt_type", values_to = "touch_pct")

hvt_lookup <- rb_hvt %>%
  filter(hvt_type == "hvt_pct") %>%
  select(player_display_name, player_gsis_id, hvt_type, touch_pct)

rb_hvt <- left_join(rb_hvt,
  hvt_lookup,
  by = c(
    "player_display_name",
    "player_gsis_id"
  )
)

rb_hvt <- left_join(rb_hvt,
                    nflfastR::teams_colors_logos,
                    by = c("posteam" = "team_abbr")
) %>%
  filter(total_touches >= 100, hvt_type.x == "hvt_pct")


rb_hvt %>%

rb_hvt %>%
  ungroup() %>%
  distinct(player_display_name, .keep_all = TRUE) %>%
  ggplot(aes(
    x = touch_pct.x, 
    y = reorder(player_display_name, touch_pct.x),
    fill = team_color
    )) + 
  geom_col() +
  #geom_text() +
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  labs(
    x = "Percent of plays",
    fill = "Distance from goal line",
    title = "Visualization of TRAP backs, displaying RB high value touches (carries inside the 10\nand catches) as a % of total touches (min 100 touches)",
    caption = "Data: nflfastR, NFL NGS"
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank()
  )