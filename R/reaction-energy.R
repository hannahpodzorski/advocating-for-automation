# Activation Energy Plot

library(tidyverse)

gsi_colors <- c("blue" = "#1C6EAC", "dark-blue" = "#005782", orange = "#EB7C32")
# Parabolic functions
para_1 <- function(x){
  h = 500 # x-coordinate of vertex
  k = 500 # y-coordinate of vertex
  a = -0.0064 
  
  a*(x - h)^2 + k
} 

para_2 <- function(x) {
  h = 500 # x-coordinate of vertex
  k = 250 # y-coordinate of vertex
  a = -0.0024 
  
  a*(x - h)^2 + k}

para_3 <- function(x) {
  h = 500 # x-coordinate of vertex
  k = 250 # y-coordinate of vertex
  a = -0.003622222 
  
  a*(x - h)^2 + k}

# Creating data sets
x <- 1:1200

d1 <- data.frame(x = x,
                y = c(rep.int(para_1(250), 249), 
                      para_1(x[250:800]),
                      rep.int(para_1(800), 400)))

d2 <- data.frame(x = x,
                 y = c(rep.int(para_1(250), 249), 
                       para_2(x[250:500]),
                       para_3(x[501:800]),
                       rep.int(para_1(800), 400)))

p1 <- ggplot() + 
  # Add Parabolas
  geom_path(data = d1, aes(x,y), size = 3) +
  # Add Starting and Ending Points
  geom_point(aes(x = 125, y = 200), size = 25, shape = 21, stroke = 2,
             color = gsi_colors["dark-blue"], fill = gsi_colors["blue"], alpha = 0.8) +
  geom_point(aes(x = 1000, y = 24), size = 25, shape = 21, stroke = 2,
             color = gsi_colors["dark-blue"], fill = gsi_colors["blue"], alpha = 0.8) +
  annotate("text", x = 125, y = para_1(250), 
           label = "Current\nWorkflow", 
           vjust = 1.25, hjust = 0.5, size = 8) +
  annotate("text", x = 1000, y = para_1(800), 
           label = "Automated\nWorkflow", 
           vjust = 1.25, hjust = 0.5, size = 8) +
  # Activation Energy Label
  geom_segment(aes(x = 250, xend = 799, y = 500, yend = 500), linetype = "dotted", size = 1.5, color = "grey") +
  geom_segment(aes(x = 250, xend = 799, y = 100, yend = 100), linetype = "dotted", size = 1.5, color = "grey") +
  geom_segment(aes(x = 800, xend = 800, y = 100, yend = 500), 
               arrow = arrow(length = unit(0.02, "npc"), ends = "both", type = "closed"), 
               size = 1.5, color = "grey") +
  annotate("text", x = 800, y = 300, label = "Activation Energy", size = 6, hjust = -0.1) +
  # Axes
  labs(x = "Progress", y = "Total Energy") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limit = c(-250, 500)) +
  # Themeing
  theme_classic(base_family = "sans", base_size = 28) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(arrow = arrow(type='closed', length = unit(13,'pt')),
                                 size = 2),
        panel.background = element_rect(fill = "transparent", color = NA))


p2 <- ggplot() + 
  # Add Parabolas
  geom_path(data = d1, aes(x,y), size = 3, color = "grey") +
  geom_path(data = d2, aes(x,y), size = 2) + 
  # Add Starting and Ending Points
  geom_point(aes(x = 125, y = 200), size = 25, shape = 21, stroke = 2,
             color = gsi_colors["dark-blue"], fill = gsi_colors["blue"], alpha = 0.8) +
  geom_point(aes(x = 1000, y = 24), size = 25, shape = 21, stroke = 2,
             color = gsi_colors["dark-blue"], fill = gsi_colors["blue"], alpha = 0.8) +
  annotate("text", x = 125, y = para_1(250), 
           label = "Current\nWorkflow", 
           vjust = 1.25, hjust = 0.5, size = 8) +
  annotate("text", x = 1000, y = para_1(800), 
           label = "Automated\nWorkflow", 
           vjust = 1.25, hjust = 0.5, size = 8) +
  # Activation Energy Label
  geom_segment(aes(x = 250, xend = 799, y = 250, yend = 250), linetype = "dotted", size = 1.5, color = "grey") +
  geom_segment(aes(x = 250, xend = 799, y = 100, yend = 100), linetype = "dotted", size = 1.5, color = "grey") +
  geom_segment(aes(x = 800, xend = 800, y = 100, yend = 250), 
               arrow = arrow(length = unit(0.02, "npc"), ends = "both", type = "closed"), 
               size = 1.5, color = "grey") +
  annotate("text", x = 800, y = 175, label = "Activation Energy", size = 6, hjust = -0.1) +
  # Axes
  labs(x = "Progress", y = "Total Energy") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limit = c(-250, 500)) +
  # Themeing
  theme_classic(base_family = "sans", base_size = 28) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(arrow = arrow(type='closed', length = unit(13,'pt')),
                                 size = 2),
        panel.background = element_rect(fill = "transparent", color = NA))

