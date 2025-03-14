# Model code to accompany "Proof"
# Author: AJ Kucharski (2022-)

# Set up plotting ----------------------------------------------------------

# Load packages
library(tidyverse); library(geostats)

# Set local directory
setwd("~/Documents/GitHub/proof/")

# Load functions
source("R/plotting_functions.R")
source("R/set_plot.R")

# Set colours and figure settings
col.list = list(rgb(0,0,0),rgb(0.35,0.35,0.35),rgb(0.7,0.7,0.7),rgb(0.5,0.5,0.5))
network.color= rgb(0.2,0.2,0.2)
width.main=6
width.narrow=4
height.main=3
height.wide=2
height.square=5

# Generate plots ----------------------------------------------------------

# Plot Koch snowflake 
C2_Koch_snowflake()

# Plot Weierstrass function and close up
C2_Weierstrass()

# Plot Condorcet calculation on jury decisions
C3_Condorcet_jury()

# Plot Delta variant in UK
C5_Delta()

# Estimative probability plot
C5_estimative()

# Probability of p-value < 0.05 when multiple hypotheses tested
C6_multiple_tests()
