## SPATIAL
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(maptools)

## DATA MANAGEMENT
library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
# library(zoo)

## PLOTTING
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

# most impaired data??....originally from Scott Copeland
dat <- read_csv("raw_data/sia_impairment_group_90_means_10_18.csv") %>%
  filter(site != "LYBR1" | year != 2011) %>%
  mutate(site_name = rep(NA, n()))



dat$site[dat$site == "LYBR_RHTS"] <- "LYBR1"
dat$site_name[dat$site == "GRGU1"] <- "Great Gulf"
dat$site_name[dat$site == "HEGL1"] <- "Hercules-Glades"
dat$site_name[dat$site == "DOSO1"] <- "Dolly Sods"
dat$site_name[dat$site == "BOWA1"] <- "Boundary Waters"
dat$site_name[dat$site == "LYBR1"] <- "Lye Brook"

dat$site_name <- factor(dat$site_name,
                   levels = c("Great Gulf",
                              "Boundary Waters",
                              "Lye Brook",
                              "Dolly Sods",
                              "Hercules-Glades"))


# calculate SVR in km
svr_dat <- dat %>% 
  mutate(SVR = 3910/(TBext)) %>% 
  filter(!(is.na(site_name))) 


# pull the Rayleighs number
ray_dat <- dat %>% 
  select(site, ss_rayleigh) %>% 
  distinct() %>% 
  rename(SiteCode = site)


# natural conditions
nc_dat <- read_csv("raw_data/nat_conditions.csv") %>% 
  group_by(SiteCode) %>% 
  summarise(sum_param = sum(Value)) %>% 
  ungroup() %>% 
  left_join(., ray_dat, by = "SiteCode") %>% 
  mutate(nc_svr = 3910/(sum_param + ss_rayleigh))

# baseline condition
base_dat <- svr_dat %>% 
  group_by(site) %>% 
  filter(year <= 2004 & year >= 2000) %>% 
  summarise(mean_svr = mean(SVR)) %>% 
  ungroup()
  
# 
# 
# doso_baseline <- svr_dat %>% 
#   filter(year >= 2000 & year <= 2004) %>% 
#   filter(site == "DOSO1") %>% 
#   summarise(mean_tbext  = mean(TBext))
# 
# doso_current <- svr_dat %>% 
#   filter(year == 2017) %>% 
#   filter(site == "DOSO1") %>% 
#   select(TBext)


#############################################################################
## plots
#############################################################################

plot_2017_trend <- function(SITE) {

  # filter to specific wilderness
  c1_dat <- svr_dat %>% 
    filter(site == SITE)
  
  # Get baselines SVR
  c1_2004_svr <- c1_dat %>% 
    filter(year == 2004) %>% 
    pull(SVR)
  
  c1_2004_svr <- c1_dat %>% 
    filter(year <= 2004 & year >= 2000) %>% 
    summarise(mean_svr = mean(SVR)) %>% 
    pull(mean_svr)
  
  # get slope of UPR
  nc_svr <- nc_dat %>% 
    filter(SiteCode == SITE) %>% 
    pull(nc_svr)
  
  c1_upr_slope <- (nc_svr - c1_2004_svr)/60
  
  # creae dataframe of UPR
  c1_upr <- data_frame(year = seq(2004, 2064, 1),
                         year_ind = seq(0, 60, 1)) %>% 
    mutate(svr_upr = c1_2004_svr + year_ind*c1_upr_slope) 
  
  
  # create dataframe for NC line
  c1_nc_df <- data_frame(year = c(-Inf, Inf),
                           SVR = c(nc_svr, nc_svr)) # (nc_val.nc_val)
  doso_nc_val <- 210.2151
  
  
  
  
  # 2 lines = geom_hline
  ggplot() +
    
    # Uniform rate of progress
    geom_line(aes(x = year,
                  y = svr_upr,
                  color = "Uniform rate of progress \nto 2064 goal"), 
              size = 1.2,
              data = c1_upr) +
    
    # Measured visibility
    geom_point(aes(year, SVR),
               size = 3, 
               color = "darkorange",
               data = c1_dat) +
    geom_line(aes(year, SVR, color = "Measured visibility"), 
              size = 1.5,
              data = c1_dat) +
    
    # Natural Background
    geom_hline(aes(yintercept = nc_svr), 
               size = 1.2, 
               color = "black",
               linetype = "dashed") +
    
    geom_text(aes(x = 2014, 
                  y = nc_svr + 11, 
                  label = "Natural background (2064 goal)",
                  fontface = "italic"),
              size = 2.5) +
    
    theme_minimal() +
    labs(x = "Year", y = "Visibility, km", title = "Trends in Hazy Day Visual Range") +
    # scale_color_viridis(discrete = TRUE, name = NULL,
    #                     guide = guide_legend(
    #                       # keywidth = 3,
    #                       # keyheight = 5,
    #                       # units = "cm",
    #                       nrow = 2,
    #                       ncol = 2)) +
    scale_color_manual(values = c("darkorange", "steelblue4"),
                       name = NULL,
                       guide = guide_legend(
                         keywidth = 1.5,
                         keyheight = 0.5,
                         units = "in",
                         nrow = 1,
                         ncol = 2)) +
    # to compare against old versions
    # scale_x_continuous(limits = c(2000, 2011),
    #                    breaks = seq(2000, 2010, 1),
    #                    minor_breaks = seq(2000, 2010, 1)) +
    scale_x_continuous(limits = c(2000, 2018),
                       breaks = seq(2000, 2017, 1),
                       minor_breaks = seq(2000, 2017, 1)) +
    scale_y_continuous(lim = c(0, 160), breaks = seq(0,150,30)) + #replace with round(NC+
    theme(axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10, margin = margin(5,0,-10,0)),
          axis.text.y = element_text(size = 8),
          axis.text.x = element_text(size = 8, angle = 45, hjust = 0.95, vjust = 0.95),
          plot.title = element_text(size = 11), 
          legend.text = element_text(size = 8, margin = margin(0,0,0,0)),
          legend.position = "bottom",
          legend.box.margin = margin(0,0,0,0),
          legend.margin = margin(0,15,2,0),
          plot.margin = margin(0,0,0,0),
          panel.spacing = margin(0,0,0,0))
  
  ggsave(paste("figures/", SITE, "_2017.jpg", sep = ""),
         height = 2.8,
         width = 3.6)
}

plot_2017_trend("DOSO1")
plot_2017_trend("HEGL1")

##-------------
## speciated plots
##-------------

plot_spec_emissions <- function(SITE) {
 
   # filter to specific wilderness
  c1_dat <- svr_dat %>% 
    filter(site == SITE)
  
  ext_dat <- c1_dat %>% 
    select(year, EAmm_SO4:ESea_Salt) %>% 
    filter(year >= 2000 & year <= 2004) %>% 
    select(-year) %>% 
    summarise_all( mean) %>% 
    gather(component, ext_value) %>% 
    mutate(comp_name = rep(NA, n()),
           site = rep("site", n()))
  
  
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
  
  # colors
  cols <- viridis_pal()(7)
  
  # upper plotting limit
  y_lim <- ext_dat %>% 
    summarise(total_ext = sum(ext_value)) %>% 
    pull(total_ext)
  
  # plot
  ggplot(aes(x = site, y = ext_value, fill = comp_name), data = ext_dat) +
    geom_bar(stat = "identity") +
    scale_fill_manual(name = NULL,
                      values = cols,
                      guide = guide_legend(
                        # keywidth = 3,
                        # keyheight = 5,
                        units = "cm")) +
    labs(x = "2000-2004 Baseline Average", y = "Light Extinction, 1/Mm", title = "Worst 20% Visibility Days \nSpeciated Particulate Matter") +
    theme_minimal() +
    # scale_y_continuous(limits = c(0,180), breaks = c(0,180, 20), labels = c(0,180,20), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, y_lim + 10), breaks = pretty_breaks(n=10), expand = expand_scale(0.01)) +
    theme(axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10, margin = margin(4,0,0,0)),
          axis.text.y = element_text(size = 8),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 11),
          legend.position = "right",
          panel.grid.major.x = element_blank()) 
  
  # save to file
  ggsave(paste("figures/", SITE, "_speciation_baseline.jpg", sep = ""),
         height = 2.8,
         width = 3.6,
         units = "in")

}

plot_spec_emissions("DOSO1")

plot_spec_emissions("HEGL1")

#############################################################################
## testing
#############################################################################


  
  # filter to specific wilderness
  c1_dat <- svr_dat %>% 
    filter(site == "HEGL1")


ext_dat <- c1_dat %>% 
  select(year, EAmm_SO4:ESea_Salt) %>% 
  filter(year >= 2000 & year <= 2004) %>% 
  select(-year) %>% 
  summarise_all( mean) %>% 
  gather(component, ext_value) %>% 
  mutate(comp_name = rep(NA, n()),
         site = rep("site", n()))

y_lim <- ext_dat %>% 
  summarise(total_ext = sum(ext_value)) %>% 
  pull(total_ext)
  