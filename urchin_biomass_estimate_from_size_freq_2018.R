# Estimate of urchin biomass based on size frequency.
# 
# Author: Nathan Spindel
# Date: 2018-09-10
# 
##################################################################################################################
# THE FOLLOWING NOTES WERE PROVIDED BY LYNN LEE VIA EMAIL ON 2018-09-05:
##################################################################################################################
# Cole’s notes for coding as discussed is that we want to be able to do the following:
#   
# - extract specified species (in this case red.urchin)
# 
# - use existing size histogram data to populate counted but not measured red urchins with randomly selected sizes from the histogram in a few different ways:
#   1. by transect (e.g. shallow, mid and deep level transects at each plot would each have their own histograms to populate from)
#   2. by site by transect depth level (e.g. combined histogram for both shallow transects at from each of 2 plots and draw random sizes from there)
#   3. by island by transect depth level (e.g. combined histogram for all shallow transects at both Murchison sites - total of 4 shallow transect - and draw random sizes from there)
# 
# - calculate the estimated red urchin biomass in grams per transect using this length-biomass conversion from unpublished DFO data from 10,000 urchins throughout the coast (unless you have another you think we should be using):
#   0.000968952467911056*(length.mm^2.791329)
# 
# - calculate the estimated biomass of urchins per m2 for shallow, mid and deep transect levels
# 
# We may want to estimate the biomass of urchins in other ways too but for now, let’s see what this gives us.
# And the reason for the 3 different methods of random size selection is simply to see if there is a large difference between the results using each method. 
# Once we send you more cleaned-up data, if you could send us a plot of what the histograms look like for each of the transects, that will give us a better idea of how similar or different they are.
##################################################################################################################


# Set working directory.
wd <- "/Users/nathanspindel/Kelp Core Urchin Biomass/"
setwd(wd)

# Load functions.
functions <- "urchin_biometrics_functions.R"
source(paste0(wd, functions))

# Load packages.
packages <- c("tidyverse", "ggrepel", "viridis", "gridExtra", "naniar")
ipak(packages)

# Store filename.
filename <- "KelpForestSurvey_PermanentTransects_20171124_transects.csv"

# Import raw data. 
raw.kelp.forest.survey.data <- read.csv(paste0(wd, filename), header = TRUE)

# Transform data:
# 1) Select only data for red urchins.
# 2) Coerce columns of interest to numeric.
# 3) Apply adjustment to cases where there is likely an error in the data due to the discrepancy
#     between mm and cm measurements.
# 4) Accomodate inconsistency in site naming.
# 5) Add column for area sampled.
# 6) Add column for estimated per-capita biomass (g).
selected.urchin.data <- raw.kelp.forest.survey.data %>% 
  select(
    Year,
    Month,
    Day,
    Site,
    Plot,
    SurveyType,
    TransectLevel,
    TransectLength.m,
    TransectWidth.m,
    Species,
    Size.mm,
    Size.cm,
    Count) %>%
  filter(., Species == "red.urchin") %>%
  mutate(.,
    # coerce to numeric.
    Size.mm = as.numeric(as.character(Size.mm)), 
    Size.cm = as.numeric(as.character(Size.cm)), 
    Count = as.numeric(as.character(Count)), 
    # Fix text case inconsistency.
    TransectLevel = as.factor(tolower(TransectLevel)),
    # Coerce to factor.
    Plot = as.factor(Plot),
    # Apply adjustment to account for instances where a measurement made in cm was entered in mm.
    Size.adjusted.mm = if_else(Size.mm < 20, Size.mm * 10, Size.mm),
    # Transfer data from cm column to mm column. Consolidate data in "Size.adjusted.mm" column.
    Size.adjusted.mm = if_else(is.na(Size.mm), Size.cm * 10, Size.adjusted.mm),
    # Accomodate inconsistency in site naming.
    Site.adjusted = as.factor(str_replace(Site, "Faraday South", "Faraday S")),
    # Add column for area sampled. Area = 30 m Long x 2 m wide = 60 m.
    TransectArea.m = 60,
    # Add column for estimated per capita biomass.
    # Formula for length-biomass conversion per LL 2018-09-05:  
    # 0.000968952467911056*(length.mm^2.791329)
    Biomass.estimate.g = 0.000968952467911056*Size.adjusted.mm^2.791329
    )   

# Store urchin data grouped by site and plot. 
site.x.plot.urchin.data <- selected.urchin.data %>% 
  group_by(Site.adjusted, Plot) 

# Visualize size frequency distribution by site and plot.
site.x.plot.size.freq.poly <- ggplot(data = site.x.plot.urchin.data, aes(Size.adjusted.mm, color = Plot)) +
  geom_freqpoly(binwidth = 10) +
  facet_wrap(vars(Site.adjusted)) +
  ggtitle(expression(paste("Size Frequency Distribution of ", italic("S. franciscanus")))) +
  xlab("Test Length (mm)") +
  ylab("Count") +
  scale_color_viridis(discrete = TRUE) +
  theme_classic()

site.x.plot.size.freq.poly  

# Visualize biomass estimate by site and plot.
site.x.plot.biomass.freq.poly <- ggplot(data = site.x.plot.urchin.data, aes(Biomass.estimate.g, color = Plot)) +
  geom_freqpoly(binwidth = 10) +
  facet_wrap(vars(Site.adjusted)) +
  ggtitle(expression(paste("Estimated Biomass Frequency Distribution of ", italic("S. franciscanus")))) +
  xlab("Estimated Biomass (g)") +
  ylab("Count") +
  scale_color_viridis(discrete = TRUE) +
  theme_classic()

site.x.plot.biomass.freq.poly

# Summarize data by site and plot.
site.x.plot.summary <- summarise(site.x.plot.urchin.data, 
   Mean.size.mm = mean(Size.adjusted.mm, na.rm = TRUE),
   Sd.size.mm = sd(Size.adjusted.mm, na.rm = TRUE),
   Count.urchins = n(),
   Biomass.estimate.g = ((0.000968952467911056*Mean.size.mm^2.791329) * Count.urchins))