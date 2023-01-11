# Basic Plots of State/RAL leases
# by Yixin Sun in 2019.  Updated by Thom Covert in Feb 2021.
library(tidyverse)
library(sf)
library(lubridate)
library(gtable)
library(here)

root <- here()

source(file.path(root, "code/paths.R"))
source(file.path(root, "code/texas_constants.R"))

# we don't want spherical geometry
sf_use_s2(FALSE)

# =============================================================================
# modify theme_nothing to get rid of grid lines
# =============================================================================
theme_nothing <- function(base_size = 12, title_size = 15){
  return(
    theme(panel.grid.major = element_line(colour = "transparent"), 
          panel.grid.minor = element_line(colour = "transparent"),
          panel.background = element_blank(), axis.line = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank(),
          plot.title = element_text(size = title_size, hjust = 0.5), 
          plot.caption = element_text(size = 6)))
}

# =============================================================================
# set up leases and counties shapefile
# =============================================================================
load(file.path(gen, "final_leases.Rda"))
load(file.path(gen, "clean_leases_shapes.Rda"))

leases <-
  final_leases %>%
  filter(InSample) %>%
  mutate(Auction = if_else(Auction == 1, "Auction", "Negotiation"), 
         Auction = factor(Auction, levels = c("Negotiation", "Auction"))) %>%
  select(Lease_Number, Auction)

leases <-
  clean_leases_shapes %>%
  inner_join(leases)

counties <-
  file.path(raw_shape, "us_county/cb_2015_us_county_500k.shp") %>%
  st_read(stringsAsFactors = F) %>%
  filter(STATEFP == 48) %>%
  st_transform(main_crs)

# make texas outline
texas_outline <-
  st_union(counties) %>%
  st_sf() %>%
  st_cast("MULTILINESTRING")


# =============================================================================
# Map of all State/RAL leases in Texas
# =============================================================================
# read in basin layer
shale_names <- c("Barnett", "Haynesville-Bossier", "Spraberry", "Delaware", 
  "Eagle Ford", "Woordford", "Bend", "Tuscaloosa", "Abo-Yeso", "Bone Spring")

shale <-
  file.path(raw_shape,
            "TightOil_ShalePlays_US_EIA_Jan2019",
            "TightOil_ShalePlays_US_EIA_Jan2019.shp") %>%
  st_read(stringsAsFactors = F) %>%
  select(Basin, Shale_play) %>%
  st_transform(main_crs) %>%
  filter(Shale_play %in% shale_names) %>%
  st_intersection(st_union(counties))

lease_texas <-
  ggplot() +
  geom_sf(data = shale, fill = "sandybrown", color = "sandybrown") +
  geom_sf(data = texas_outline, color = "black", fill = NA) +
  geom_sf(data = leases, aes(color = Auction)) +
  scale_color_manual(values = c("#e31a1c", "#006837"),
                     name = "Land Type") +
  scale_shape_manual(values = c(15, 15, 15)) +
  theme_nothing() +
  theme(legend.position = c(0.25, 0.15))

ggsave(lease_texas, filename = file.path(fdir, "glo_leases_in_texas.png"),
       width = 6.5, height = 5)

ggsave(lease_texas, filename = file.path(fdir, "glo_leases_in_texas.pdf"),
       width = 6.5, height = 5)

# make a presentation version
lease_texas_prez <-
  ggplot() +
  geom_sf(data = shale, fill = "sandybrown", color = "sandybrown") +
  geom_sf(data = texas_outline, color = "black", fill = NA) +
  geom_sf(data = leases, aes(color = Auction)) +
  scale_color_manual(values = c("red", "blue"),
                     name = "Land Type") +
  scale_shape_manual(values = c(15, 15, 15)) +
  theme_nothing() +
  theme(legend.position = c(0.25, 0.15),
        legend.box.background = element_rect(colour = "black"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"))

ggsave(lease_texas_prez,
       filename = file.path(fdir, "glo_leases_in_texas_prez.png"),
       width = 7.5, height = 5,
       bg = "transparent")

# =============================================================================
# Zoomed in map showing an area with sample GLO and RAL leases
# =============================================================================
# set the plot area that we want to zoom in on
plot_area <- 
  c(xmin = -106, ymin = 31, xmax = -103, ymax = 32) %>%
  st_bbox(crs = st_crs(leases))

load(file.path(shape, "texas_grid10.Rda"))
texas_grid10 <-
  st_cast(texas_grid10 , "LINESTRING") %>%
  st_crop(plot_area)

# we need to extract the two different legends (Land Type and grid overlay)
# so that we can arrange them in different locations
grid_plot <-
  texas_grid10 %>%
  ggplot() +
  geom_sf(linetype = "dashed",
          aes(colour = str_wrap("10 mile by 10 mile Grid", width = 12)),
          size = .1) +
  scale_colour_manual(values = "gray26", name = "") +
  theme(legend.text = element_text(size = 10))

grid_legend <-
  gtable_filter(ggplot_gtable(ggplot_build(grid_plot)),
                "guide-box")

sample_leases <-
  leases %>%
  st_crop(plot_area)

sample_glo_plot <-
  sample_leases %>%
  ggplot() +
  geom_sf(aes(fill = Auction), size = .1) +
  geom_sf(data = texas_grid10, linetype = "dashed", color = "gray50",
          size = .1) +
  geom_sf(data = counties, color = "black", size= .3, fill = NA) +
  theme_nothing(title_size = 18) +
  scale_fill_manual(values = c("#253494", "#41b6c4"),
                    name = "Land Type") +
  xlim(-104.25, -103.4) +
  ylim(31.3, 31.9) +
  annotation_custom(grob = grid_legend,
                    xmin = -103, xmax = -103.15, ymin = 31.2, ymax = 31.3) +
  annotate("text", x = -103.6, y = 31.4, label = "Reeves",
           size = 4.5,  fontface = 2) +
  annotate("text", x = -103.42, y = 31.505, label = "Ward",
           size = 4.5,  fontface = 2) +
  annotate("text", x = -103.5, y = 31.7984, label = "Loving",
           size = 4.5,  fontface = 2) +
  annotate("text", x = -104.12, y = 31.85, label = "Culberson",
           size = 4.5,  fontface = 2)

ggsave(sample_glo_plot, filename = file.path(fdir, "sample_glo_leases.png"),
       width = 7.5, height = 5)

ggsave(sample_glo_plot, filename = file.path(fdir, "sample_glo_leases.pdf"),
       width = 7.5, height = 5)

# make a presentation version
sample_glo_plot_prez <-
  sample_leases %>%
  ggplot() +
  geom_sf(data = texas_grid10, linetype = "dashed", color = "gray50",
          size = .1, show.legend = FALSE) +
  geom_sf(data = counties, color = "black", size= .3, fill = NA,
          show.legend = FALSE) +
  geom_sf(aes(fill = Auction), size = .1) +  
  theme_nothing(title_size = 18) +
  scale_fill_manual(values = c("red", "blue"),
                    name = "Land Type") +
  scale_linetype(guide = FALSE) +
  theme(legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent")) +
  xlim(-104.25, -103.4) +
  ylim(31.3, 31.9) +
  annotate("text", x = -103.6, y = 31.4, label = "Reeves",
           size = 4.5,  fontface = 2) +
  annotate("text", x = -103.42, y = 31.505, label = "Ward",
           size = 4.5,  fontface = 2) +
  annotate("text", x = -103.5, y = 31.7984, label = "Loving",
           size = 4.5,  fontface = 2) +
  annotate("text", x = -104.12, y = 31.85, label = "Culberson",
           size = 4.5,  fontface = 2)

ggsave(sample_glo_plot_prez,
       filename = file.path(fdir, "sample_glo_leases_prez.png"),
       width = 7.5, height = 5,
       bg = "transparent")


