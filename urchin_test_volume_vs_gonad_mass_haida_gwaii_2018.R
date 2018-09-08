# Script to run a linear regression on urchin test volume versus gonad mass. 
# Author: Nathan Spindel
# Date: 2018-09-06

# Set working directory
wd <- "/Users/nathanspindel/Documents/coding_work/urchin_biometrics_haida_gwaii_2018/"
setwd(wd)

# Load functions
functions <- "urchin_biometrics_functions.R"
source(paste0(wd, functions))

# Load packages
packages <- c("tidyverse", "ggrepel", "viridis", "gridExtra")
ipak(packages)

# Store filename
filename <- "urchin_biometric_data_2018.csv"

# Import raw data 
raw.urchin.data <- read.csv(paste0(wd, filename), header = TRUE)

# Transform data
selected.urchin.data <- raw.urchin.data %>% 
  select(
    species,
    field_depth_ft, 
    collection_site,
    width_mm, 
    height_mm, 
    gonad_mass_g) 

# Coerce depth to factor for grouping.
# Calculate oblate spheroid volume (i.e. volume of an urchin test).
selected.urchin.data <- 
  mutate(selected.urchin.data, 
         field_depth_ft = factor(field_depth_ft),
         semi_minor_axis_mm = 0.5 * as.numeric(height_mm), 
         semi_major_axis_mm = 0.5 * as.numeric(width_mm),
         oblate_spheroid_volume_mm3 = 4/3 * pi * semi_major_axis_mm ^2 * semi_minor_axis_mm ) # Equation for volume of oblate spheroid = 4/3pia^2c

# Separate data by species.
red.urchin.data <- filter(selected.urchin.data, species == "Strongylocentrotus franciscanus")

green.urchin.data <- filter(selected.urchin.data, species == "Strongylocentrotus droebachiensis")
# Re-factor green urchin data because it was only collected at 2 depths, rather than 3.
green.urchin.data <- mutate(green.urchin.data,
                            field_depth_ft = factor(field_depth_ft))

# Store linear regression model attributes in a variable.
red.eqns <- red.urchin.data %>% split(.$field_depth_ft) %>%
  map(~ lm(gonad_mass_g ~ oblate_spheroid_volume_mm3, data = .)) %>%
  map(lm_eqn_coeffs) %>% 
  do.call(rbind, .) %>%
  as.data.frame() %>%
  set_names("equation") %>%
  mutate(field_depth_ft = rownames(.))

green.eqns <- green.urchin.data %>% split(.$field_depth_ft) %>%
  map(~ lm(gonad_mass_g ~ oblate_spheroid_volume_mm3, data = .)) %>%
  map(lm_eqn_coeffs) %>% 
  do.call(rbind, .) %>%
  as.data.frame() %>%
  set_names("equation") %>%
  mutate(field_depth_ft = rownames(.))

# Visualize data.
red.volume.vs.gonad.plot <- ggplot(red.urchin.data, aes(x = oblate_spheroid_volume_mm3, y = gonad_mass_g, color = as.factor(field_depth_ft))) +
  scale_color_viridis(direction = -1, discrete = TRUE) +
  ggtitle(expression(paste(italic("S. franciscanus")))) +
  labs(color = "Depth (ft)") +
  xlab(expression(paste("Test Volume (mm"^"3", ")"))) + 
  ylab("Gonad Mass (g)") +
  geom_smooth(method='lm') +
  geom_label_repel(data = red.eqns,aes(x = -Inf, y = Inf, label = equation), nudge_x = 0.1, parse = TRUE, segment.size = 0, show.legend = FALSE, direction = "y") +
  geom_point() +
  theme_classic()
  
quartz()
red.volume.vs.gonad.plot

green.volume.vs.gonad.plot <- ggplot(green.urchin.data, aes(x = oblate_spheroid_volume_mm3, y = gonad_mass_g, color = as.factor(field_depth_ft))) +
  scale_color_viridis(direction = -1, discrete = TRUE) +
  ggtitle(expression(paste(italic("S. droebachiensis")))) +
  labs(color = "Depth (ft)") +
  xlab(expression(paste("Test Volume (mm"^"3", ")"))) + 
  ylab("Gonad Mass (g)") +
  geom_smooth(method='lm') +
  geom_label_repel(data = green.eqns,aes(x = -Inf, y = Inf, label = equation), nudge_x = 0.1, parse = TRUE, segment.size = 0, show.legend = FALSE, direction = "y") +
  geom_point() +
  theme_classic()

quartz()
green.volume.vs.gonad.plot

# Combine red and green urchin plots into single plot as grobs.
volume.vs.gonad.x.species.plot <- grid.arrange(red.volume.vs.gonad.plot, green.volume.vs.gonad.plot, top = "Test Volume Versus Gonad Mass Linear Regression", nrow = 1, ncol = 2)

# Export volume x gonad mass x species x depth plot as pdf.
ggsave("urchin_test_volume_x_gonad_mass_x_species_x_depth.pdf", volume.vs.gonad.x.species.plot, width = 10, height = 5)

# Close all open graphical devices.
graphics.off()
