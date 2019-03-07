library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
# library(sp)
# library(rgeos)
# library(raster)
# library(rgdal)
library(scales)
library(units)
library(viridis)
library(extrafont)

########################################
## load  and manipulate data
########################################

dat <- read.csv("raw_data/r9_c1.csv", stringsAsFactors = FALSE) %>% 
  filter(SiteCode != "LYBR1" | Year != 2011) %>% 
  mutate(site = rep(NA, n()))

dat$SiteCode[dat$SiteCode == "LYBR_RHTS"] <- "LYBR1"
dat$site[dat$SiteCode == "GRGU1"] <- "Great Gulf"
dat$site[dat$SiteCode == "HEGL1"] <- "Hercules-Glades"
dat$site[dat$SiteCode == "DOSO1"] <- "Dolly Sods"
dat$site[dat$SiteCode == "BOWA1"] <- "Boundary Waters"
dat$site[dat$SiteCode == "LYBR1"] <- "Lye Brook"

dat$site <- factor(dat$site,
                   levels = c("Great Gulf",
                              "Boundary Waters",
                              "Lye Brook",
                              "Dolly Sods",
                              "Hercules-Glades"))


dat <- dat %>% 
  group_by(site, Year) %>% 
  summarise(vis_Mm = sum(Value)) %>% 
  mutate(vis_km = 3910/vis_Mm) %>% 
  ungroup()



theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu", color = "#22211d"),
      plot.title = element_text(face = "bold"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.4),
      # panel.grid.major = element_line(color = "#ebebe5", size = 0.8),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      legend.position = "bottom",
      ...
    )
}


# For Windows - in each session
# Adjust the path to match your installation of Ghostscript
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.23/bin/gswin64.exe")

# load font for plotting
windowsFonts(Times=windowsFont("Ubuntu"))

plot_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = 'white'
  # color.grid.major = palette[3]
  color.axis.text = "black"
  color.axis.title = "black"
  color.title = "black"
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="bottom") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=10,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=10,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=11,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=11,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}    




#----------------------------------------------------------------------------


########################################
## plot
########################################


ggplot( plot_dat, aes(x = Year, y = vis_km)) +
  geom_line(color = "skyblue3", size = 1.2) +
  geom_point(color = "skyblue3", fill = "white", shape = 21, size = 4) +
  scale_y_continuous(breaks=seq(20,120, by=20), limits=c(0,120)) +
  scale_x_continuous(breaks=seq(2001,2015, by=2), limits=c(2001,2015)) +
  labs(y = "Visibility (km)") +
  theme_minimal() +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.line = element_line(colour = "black", size=0.7), axis.ticks = element_line(color="black",size=0.7)) +
  theme(axis.text.x=element_text(size=13,color="black")) +
  theme(axis.text.y=element_text(size=13,color="black")) +
  theme(axis.title.x=element_text(size=14,color="black", vjust=0)) +
  theme(axis.title.y=element_text(size=14,color="black", vjust=1.25)) +
  annotate("text", x=2011, y=3, label="Data taken from:", size=3.0) +
  annotate("text", x=2011, y=0, label="http://views.cira.colostate.edu/fed/SiteBrowser/Default.aspx", size=3.0) +
  annotate("text", x = 2006, y = 115, label = 'bold("Visibility at the Great Gulf Wilderness, \nWhite Mountain National Forest")', parse = TRUE, size = 6)

ggsave("figures/mt_wash_visibility.pdf", height = 7, width = 7, units = "in")



ggplot(dat, aes(x = Year, y = vis_km, group = site)) +
  geom_line(aes(color = site), size = 1.2) +
  scale_color_viridis(discrete = TRUE, name = "") +
  theme_minimal() +
  labs(y = "Visibility (km)") +
  scale_y_continuous(breaks=seq(0,160, by=20), limits=c(0,160)) +
  scale_x_continuous(breaks=seq(1992,2016, by=2), limits=c(1992,2016)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.line = element_line(colour = "black", size=0.7), axis.ticks = element_line(color="black",size=0.7)) +
  theme(axis.text.x=element_text(size=13,color="black", angle = 45, vjust = 0.95, hjust = 0.95)) +
  theme(axis.text.y=element_text(size=13,color="black")) +
  theme(axis.title.x=element_text(size=14,color="black", vjust=0)) +
  theme(axis.title.y=element_text(size=14,color="black", vjust=1.25)) +
  annotate("text", x=2010, y=3, label="Data taken from:", size=3.0) +
  annotate("text", x=2010, y=0, label="http://views.cira.colostate.edu/fed/SiteBrowser/Default.aspx", size=3.0) +
  annotate("text", x = 1999, y = 140, label = 'bold("Visibility at Class I Areas\n in the Eastern Region")', parse = TRUE, size = 6) 


ggsave("figures/r9_class1.pdf", height = 7, width = 7, units = "in")







ggplot(dat, aes(x = Year, y = vis_km, group = site)) +
  geom_line(aes(color = site), size = 2) +
  scale_color_viridis(discrete = TRUE, name = NULL,
                      guide = guide_legend(
                        keywidth = 3,
                        keyheight = 5,
                        units = "cm")) +
  theme_map() +
  labs(y = "Visibility (km)", title = "Visibility at Class I Areas") +
  scale_y_continuous(breaks=seq(0,160, by=20), limits=c(0,160)) +
  scale_x_continuous(breaks=seq(1992,2016, by=2), limits=c(1992,2016)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.line = element_line(colour = "black", size=0.7), axis.ticks = element_line(color="black",size=0.7)) +
  theme(axis.text.x=element_text(size=16,color="black", angle = 45, vjust = 0.95, hjust = 0.95)) +
  theme(axis.text.y=element_text(size=16,color="black")) +
  theme(axis.title.x=element_text(size=18,color="black", vjust=0)) +
  theme(axis.title.y=element_text(size=18,color="black", angle = 90, vjust = 1.25)) +
  theme(legend.text = element_text(size = 16, margin = margin(r = 0, l = -0.5, unit = "cm"))) +
  annotate("text", x=2010, y=3, label="Data taken from:", size=3.0) +
  annotate("text", x=2010, y=0, label="http://views.cira.colostate.edu/fed/SiteBrowser/Default.aspx", size=3.0) +
  # annotate("text", x = 1999, y = 150, label = 'bold("Visibility at \nClass I Areas")', parse = TRUE, size = 14) +
  theme(title = element_text(size = 36)) +
  guides(colour = guide_legend(override.aes = list(size=6))) +
  theme(text = element_text(family = "Ubuntu")) +
  theme(plot.margin = unit(c(1,1,0,1), "cm"))



ggsave("figures/r9_class1.jpg", height = 8.5, width = 11, units = "in")











