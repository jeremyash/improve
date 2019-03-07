library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(scales)
library(units)
library(viridis)
library(extrafont)
library(grid)
#----------------------------------------------------------------------------

#############################################################################
## load data
#############################################################################

# national data obtained here: http://views.cira.colostate.edu/fed/SvcUi/AqSummaryData.aspx
dat <- read_csv("raw_data/improve_2017.csv") %>% 
  mutate(site = rep(NA, n()))


# r9 class I areas
r9_sites <- c("GRGU1", "HEGL1", "DOSO1", "BOWA1", "LYBR_RHTS")


# create full name column
dat$site[dat$SiteCode == "GRGU1"] <- "Great Gulf Wilderness"
dat$site[dat$SiteCode == "HEGL1"] <- "Hercules-Glades Wilderness"
dat$site[dat$SiteCode == "DOSO1"] <- "Dolly Sods Wilderness"
dat$site[dat$SiteCode == "BOWA1"] <- "Boundary Waters Canoe Area Wilderness"
dat$site[dat$SiteCode == "LYBR_RHTS"] <- "Lye Brook Wilderness"


# region 9 data for srv
r9_dat <- dat %>% 
  filter(SiteCode %in% r9_sites) %>% 
  filter(GroupKey != "G50") 
r9_dat$GroupKey <- factor(r9_dat$GroupKey, levels = c("G10", "G90"))


# region 9 data for extinction
ext_dat <- r9_dat %>% 
  filter(GroupKey == "G90") %>% 
  dplyr::select(SiteCode, YearNum, EAmm_SO4:ESea_Salt, site) %>% 
  gather(component, ext_value, EAmm_SO4:ESea_Salt) %>% 
  mutate(comp_name = rep(NA, n())) %>% 
  mutate(YearNum = as.character(YearNum)) %>% 
  mutate(type = rep("measured", n()))

ext_dat$comp_name[ext_dat$component == "EAmm_SO4"] <- "Ammonium Sulfate"
ext_dat$comp_name[ext_dat$component == "EAmm_NO3"] <- "Ammonium Nitrate"
ext_dat$comp_name[ext_dat$component == "EOMC"] <- "Organic Mass"
ext_dat$comp_name[ext_dat$component == "ELAC"] <- "Elemental Carbon"
ext_dat$comp_name[ext_dat$component == "ESoil"] <- "Soil"
ext_dat$comp_name[ext_dat$component == "ECM"] <- "Coarse Mass"
ext_dat$comp_name[ext_dat$component == "ESea_Salt"] <- "Sea Salt"

ext_dat$comp_name <- factor(ext_dat$comp_name, levels = c("Sea Salt",
                                                          "Coarse Mass",
                                                          "Soil",
                                                          "Elemental Carbon",
                                                          "Organic Mass",
                                                          "Ammonium Nitrate",
                                                          "Ammonium Sulfate"))

# region 9 2018 RPGs and 2064 natural conditions
 # rayleighs number for cnverting between Mm-1 and dV
# rayleigh_site <- r9_dat %>% 
#   mutate(ray_no = RBext - TBext) %>% 
#   dplyr::select(SiteCode, ray_no) %>% 
#   distinct()
# 
# # rpgs from SIPs
# rpgs_2018 <- data_frame(SiteCode = c("BOWA1", "DOSO1", "GRGU1", "HEGL1", "LYBR_RHTS"),
#                         rpg_18_dv = c(18.6, , , 23.1))

# 2064 natural conditions
ext_nc <- read_csv("raw_data/nat_conditions.csv") %>% 
  rename(comp_name = Parameter, ext_value = Value) %>% 
  mutate(YearNum = rep("Natural \nconditions", n())) %>% 
  dplyr::select(SiteCode, YearNum, comp_name, ext_value) %>% 
  mutate(site = rep(NA, n()),
         type = rep("natural", n()))


ext_nc$comp_name <- factor(ext_nc$comp_name, levels = c("Sea Salt",
                                                          "Coarse Mass",
                                                          "Soil",
                                                          "Elemental Carbon",
                                                          "Organic Mass",
                                                          "Ammonium Nitrate",
                                                          "Ammonium Sulfate"))

# create full name column
ext_nc$site[ext_nc$SiteCode == "GRGU1"] <- "Great Gulf Wilderness"
ext_nc$site[ext_nc$SiteCode == "HEGL1"] <- "Hercules-Glades Wilderness"
ext_nc$site[ext_nc$SiteCode == "DOSO1"] <- "Dolly Sods Wilderness"
ext_nc$site[ext_nc$SiteCode == "BOWA1"] <- "Boundary Waters Canoe Area Wilderness"
ext_nc$site[ext_nc$SiteCode == "LYBR_RHTS"] <- "Lye Brook Wilderness"

# bind natural conditions to observed data
ext_dat <- bind_rows(ext_dat, ext_nc) %>% 
  mutate(YearNum = as.character(YearNum))



#############################################################################
## plots
#############################################################################

##-------------
## decomposition
##-------------

decomp_plot <- function(REGION_DAT, SITE_CODE) {
  
  # get site data
  dat <- REGION_DAT %>% 
    filter(SiteCode == SITE_CODE)
  
  
  # plot
  dat_plot <- ggplot(aes(x = YearNum, y = ext_value, fill = comp_name), data = dat) +
    geom_bar(stat = "identity") +
    scale_fill_viridis(discrete = TRUE, name = NULL,
                        guide = guide_legend(
                          # keywidth = 3,
                          # keyheight = 5,
                          units = "cm")) +
    labs(x = "Year", y = "Light Extinction, 1/Mm", title = unique(dat$site)) +
    theme_bw() +
    # scale_x_discrete(expand = c(0, 0)) +
    # scale_y_continuous(limits = c(0, max(dat$ext_value + 0.5*dat$ext_value)), expand = c(0, 0)) +
    scale_y_continuous(breaks = pretty_breaks(n=10), expand = expand_scale(0.01)) +
    # scale_x_continuous(limits = c(min(dat$YearNum) - 1, max(dat$YearNum)),
    #                    breaks = c(seq(min(dat$YearNum) - 1, 2017, 1), 2064)) +
    theme(axis.title = element_text(size = 20),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14, angle = 45,  vjust = 0.95, hjust = 0.95),
          plot.title = element_text(size = 24),
          legend.text = element_text(size = 16),
          legend.position = "right",
          panel.grid.major = element_blank()) +
    facet_grid(~type, scales = "free_x", space = "free_x") +
    theme(strip.background = element_blank(), 
          strip.text = element_blank(),
          strip.placement = "outside") 
  
  # save to file
  file_path <- paste("figures/", SITE_CODE, "_ext.pdf", sep = "")
  ggsave(filename = file_path,
         plot = dat_plot,
         height = 8.5,
         width = 11,
         units = "in")
}

# apply to sites and save to file
lapply(r9_sites, function(x) decomp_plot(ext_dat, x))



##-------------
## standard visual range
##-------------

svr_plot <- function(REGION_DAT, SITE_CODE) {

  # get site data
  dat <- REGION_DAT %>% 
    filter(SiteCode == SITE_CODE)
  
  # plot
  dat_plot <- ggplot(aes(YearNum, SVR, color = GroupKey, group = GroupKey), data = dat) +
    geom_point(size = 5) +
    geom_line(size = 2, show.legend = FALSE) +
    theme_minimal() +
    labs(x = "Year", y = "Visibility, km", title = unique(dat$site)) +
    scale_color_manual(values = c("skyblue4", "darkred"),
                       name = NULL,
                       labels = c("Clearest Days", "Haziest Days")) +
    scale_x_continuous(limits = c(min(dat$YearNum) - 1, max(dat$YearNum) + 1), 
                       breaks = seq(min(dat$YearNum) -1 , max(dat$YearNum) + 1, 5)) +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 24), 
          legend.text = element_text(size = 16),
          legend.position = "bottom") +
    guides(colour = guide_legend(override.aes = list(size=7))) 
  

  # save to file
  file_path <- paste("figures/", SITE_CODE, "_svr.pdf", sep = "")
  ggsave(filename = file_path,
         plot = dat_plot,
         height = 8.5,
         width = 11,
         units = "in")

}


lapply(r9_sites, function(x) svr_plot(r9_dat, x))


##-------------
## deciview plots
##-------------

dv_plot <- function(REGION_DAT, SITE_CODE) {

  # get site data
  dat <- REGION_DAT %>% 
    filter(SiteCode == SITE_CODE)
  
  # plot
  dat_plot <- ggplot(aes(YearNum, DV, color = GroupKey, group = GroupKey), data = dat) +
    geom_point(size = 5) +
    geom_line(size = 2, show.legend = FALSE) +
    theme_minimal() +
    labs(x = "Year", y = "Haze Index, dV", title = unique(dat$site)) +
    scale_color_manual(values = c("darkred", "skyblue4"),
                       name = NULL,
                       labels = c("Haziest Days", "Clearest Days")) +
    scale_x_continuous(limits = c(min(dat$YearNum) - 1, max(dat$YearNum) + 1), 
                       breaks = seq(min(dat$YearNum) -1 , max(dat$YearNum) + 1, 5)) +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 24),
          legend.text = element_text(size = 16),
          legend.position = "bottom") +
    guides(colour = guide_legend(override.aes = list(size=7))) 

  # save to file
  file_path <- paste("figures/", SITE_CODE, "_dv.pdf", sep = "")
  ggsave(filename = file_path,
         plot = dat_plot,
         height = 8.5,
         width = 11,
         units = "in")

}

lapply(r9_sites, function(x) dv_plot(r9_dat, x))


#----------------------------------------------------------------------------




########################################
## note from scott copeland for individual sites
########################################

# That kind of data about much visibility has changed is pretty easy to get.  For example, you can go to FED AQRV summary tool:
#   
#   http://views.cira.colostate.edu/fed/SiteBrowser/Default.aspx
# 
# Click on the site you’re interest in.  The deciview trends are shown.  If you click on the haze budgets, hazy days tab, that shows the changes in visibility on the worst days in each.  You can convert that light extinction directly to standard visual range in kilometers by this simple equation;  SVR = 3910/ Light Extinction     When light extinction is in 1/Mm   (“inverse megameters”).
# 
# 


#----------------------------------------------------------------------------


########################################
## archive
########################################


# dV trends
# 
# ggplot(aes(YearNum, DV, color = GroupKey, group = GroupKey), data = test_dat) +
#   geom_point(size = 3) +
#   geom_line(size = 2) +
#   theme_minimal() +
#   labs(x = "Year", y = "Haze Index, dV", title = unique(test_dat$site)) +
#   scale_color_manual(values = c("darkred", "skyblue4"),
#                      name = NULL,
#                      labels = c("Haziest Days", "Clearest Days")) +
#   scale_x_continuous(limits = c(min(test_dat$YearNum), max(test_dat$YearNum)), 
#                      breaks = seq(min(test_dat$YearNum), max(test_dat$YearNum), 5)) +
#   theme(axis.title = element_text(size = 20),
#         axis.text = element_text(size = 14),
#         plot.title = element_text(size = 24), 
#         legend.text = element_text(size = 16),
#         legend.position = "bottom")
# 
# ggsave("figures/test_dat_dv.pdf",
#        height = 8.5,
#        width = 11, 
#        units = "in")





#----------------------------------------------------------------------------


