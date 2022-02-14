# Libraries 

library(tidyverse)
library(understatr)
library(magick)
library(bstfun)
library(gt)
library(glue)
library(ggbeeswarm)
library(cowplot)
library(grid)
library(ggplotify)
library(ggrepel)
library(extrafont)

# Data Pulling 

data <- get_player_shots(2097)

# Data Wrangling 

data <- data %>%
  mutate(X = X * 120,
         Y = Y * 80) %>%
  mutate(season = case_when(year == 2014 ~ "2014/15",
                            year == 2015 ~ "2015/16",
                            year == 2016 ~ "2016/17",
                            year == 2017 ~ "2017/18",
                            year == 2018 ~ "2018/19",
                            year == 2019 ~ "2019/20",
                            year == 2020 ~ "2020/21",
                            year == 2021 ~ "2021/22")) %>%
  mutate(situation = case_when(situation == "OpenPlay"|
                                 situation == "FromCorner" ~ "Open Play",
                               situation == "SetPiece"|
                                 situation == "DirectFreekick" ~ "Free Kick",
                               situation == "Penalty" ~ "Penalty")) %>%
  mutate(result = case_when(result == "BlockedShot"|
                              result == "MissedShots"|
                              result == "ShotOnPost" ~ "Missed",
                            result == "SavedShot" ~ "Saved",
                            result == "Goal"|
                              result == "OwnGoal" ~ "Goal")) %>%
  mutate(isGoal = ifelse(result == "Goal", 1, 0))

# Create Table 

df <- data %>%
  group_by(situation, result) %>%
  summarise(shots = n()) %>%
  spread(result, shots) %>%
  ungroup() %>%
  replace(is.na(.), 0)

tabledata <- df %>%
  gt() %>%
  tab_header(title = glue("{unique(data$player)}")) %>%
  tab_options(table.background.color = "#17202A") %>%
  tab_style(style = list(cell_text(weight = "bold", color = "#17202A")),
            locations = cells_column_labels(situation)) %>%
  tab_style(style = list(cell_text(weight = "bold", color = "#41ab5d")),
            locations = cells_column_labels(Goal)) %>%
  tab_style(style = list(cell_text(weight = "bold", color = "#CB4335")),
            locations = cells_column_labels(Missed)) %>%
  tab_style(style = list(cell_text(weight = "bold", color = "#fec44f")),
            locations = cells_column_labels(Saved)) %>%
  bstfun::as_ggplot()

grid.newpage()
vp <- viewport(name = "rotate", angle = 19.5, width = 0.7, height = 0.7)
pushViewport(vp)
table <- print(tabledata, vp = "rotate", newpage = FALSE)
table <- grid.grab()
table <- as.ggplot(table)

ggsave("table.png", width = 2000, height = 1000, units = "px")

# Plotting Polar Map

main <- ggplot() +
  geom_quasirandom(data = data, aes(x = season, y = X, colour = result, shape = situation), method = "frowney", size = 2, show.legend = FALSE) +
  scale_colour_manual(values = c("#41ab5d", "#CB4335", "#fec44f")) +
  scale_shape_manual(values = c(4, 21, 2)) +
  geom_vline(xintercept = seq(1.5, 12), colour = "white", size = 1) +
  coord_polar(clip = "off", start = 1.15) +
  scale_x_discrete(limits = c("2024/25", "2023/24", "2022/23", "2021/22", "2020/21", "2019/20", "2018/19", "2017/18", "2016/17", "2015/16", "2014/15"), labels = c("2022/23" = "", "2023/24" = "", "2024/25" = "")) +
  scale_y_continuous(limits = c(65, 120)) +
  theme_minimal() +
  labs(title = "Player Shooting Profiles",
       subtitle = "Big 5 leagues | 2014-2022",
       caption = "Design inspired by @petermckeever\nCreated by @placeholder2004") +
  theme(plot.background = element_rect(colour = "#17202A", fill = "#17202A"),
        panel.background = element_rect(colour = "#17202A", fill = "#17202A"),
        plot.title = element_text(colour = "white", face = "bold", size = 30, hjust = 0, family = "Fried Chicken Bold"),
        plot.subtitle = element_text(colour = "white", size = 20, hjust = 0),
        plot.caption = element_text(colour = "white", size = 12, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "white", size = 12),
        axis.title = element_blank()) +
  geom_hline(yintercept = 90, colour = "grey", linetype = "longdash", size = 0.5) +
  geom_hline(yintercept = 100, colour = "grey", linetype = "longdash", size = 0.5) +
  geom_hline(yintercept = 110, colour = "grey", linetype = "longdash", size = 0.5) +
  geom_hline(yintercept = 120, colour = "grey", linetype = "solid", size = 0.5)

main <- ggdraw(main)

# Create Legend

leg_data <- data %>%
  group_by(situation) %>%
  summarise(shots = n()) %>%
  mutate(shots = 75)

leg <- ggplot(leg_data) +
  geom_point(aes(x = situation, y = shots, shape = situation), size = 3, show.legend = FALSE, colour = "white") +
  scale_shape_manual(values = c(4, 21, 2)) +
  coord_polar() +
  theme_minimal() +
  theme(plot.background = element_rect(colour = "#17202A", fill = "#17202A"),
        panel.background = element_rect(colour = "#17202A", fill = "#17202A"),
        panel.grid.major = element_line(colour = "#353535"),
        panel.grid.minor = element_line(colour = "#353535"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank()) +
  geom_text_repel(aes(x = situation, y = shots, label = situation),
                  colour = "white")

# Add Legend 

main + draw_plot(leg, .7, .8, .2, .2)
ggsave("polar.png", width = 3000, height = 2800, units = "px")

# Add in table and image using {magick}

plot <- image_read("polar.png") %>%
  image_resize("1000x1000")
table <- image_read("table.png") %>%
  image_resize("770x700")

fin_plot <- magick::image_composite(plot, table, offset = "+315+355")
image_write(fin_plot, "polarshots.png")

add_image_centre <- function(plot_path, image_path) {
  
  # Read in plot
  fig <- image_read(plot_path)
  fig <- image_resize(fig, "1000x1000")
  
  # Read in image
  img <- image_read(image_path)
  img <- image_scale(img, "75x85")
  
  # Overlay
  image_composite(fig, img, offset = "+471+432")
  
}

polar <- add_image_centre(plot_path = "polarshots.png", image_path = "messi.png")
image_write(polar, "polarshotmap.png")