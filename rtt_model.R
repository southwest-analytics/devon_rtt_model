## ************************************************************************* ##
##                                                                           ##
## Script name: rtt_model.R                                                  ##
##                                                                           ##
## Purpose of script: Referral To Treatment (RTT) simulation for a single    ##
##                    specialty for both admitted and non-admitted pathways  ##
##                    over a given period for a given number of trials.      ##
##                                                                           ##
## Author: Richard Blackwell                                                 ##
##                                                                           ##
## Date Created: 2023-08-02                                                  ##
##                                                                           ##
## Email: richard.blackwell@swahsn.com                                       ##
##                                                                           ##
## Notes: The README.md in the git repo gives a more detailed explanation    ##
##        of the simulation model and parameters                             ##
##                                                                           ##
## ************************************************************************* ##

# 0. Load Libraries and Define Functions ----
# *******************************************

# * 0.1. Libraries ----
# `````````````````````
library(tidyverse)
library(VGAM)
library(readxl)

# * 0.2. Functions ----
# `````````````````````

# Function to create a profile array from the supplied actual distribution 
# for one or more periods
fnCreateProfileFromActualDistribution <- function(profile_type_var){
  # The profile_type_var needs to be changed to represent the profile var
  # in the df_param_actual data frame
  profile_var <- gsub('_type_', '_', profile_type_var)
  # Get the entries for the profile variable from the df_param_actual data frame
  df <- df_param_actual[df_param_actual$variable == profile_var,]
  # Read in from an actual distribution
  # Get the 'from' and 'to' periods
  df_periods <- data.frame(from = df$period) %>% 
    mutate(to = lead(from, default = sim_periods+1)-1)
  # Using apply and rbind create an array of dimensions [periods, bins+1] containing
  # the supplied actual distributions
  arr <- do.call('rbind', 
                 # Cast as a list
                 list(
                   # Apply across each row of the df_periods dataframe
                   apply(
                     X = df_periods, 
                     MARGIN = 1, 
                     function(X){
                       # Get the row of data for the profile for the indicated period and
                       # ignoring the first two columns (variable and period) create a vector of
                       # values
                       v <- df[df$period == unname(X['from']), 3:(sim_bins+3)] %>% 
                         unname() %>% t() %>% c()
                       # Calculate the number of periods to fill with this vector
                       periods <- as.integer(X['to'] - X['from'] + 1)
                       # Duplicate the vector for each of the periods
                       v <- rep(v, periods)
                       return(v)
                     })))
  dim(arr) <- c(sim_bins+1, sim_periods)
  arr <- t(arr)
  return(arr)
}

# Function to create a profile array from the synthetic distribution 
# for one or more periods
fnCreateProfileFromSyntheticDistribution <- function(profile_type_var){
  # The profile_type_var needs to be changed to represent the shape1 and shape2
  # entries in the df_param_synthetic data frame
  profile_shape1_var <- gsub('_type_', '_shape1_', profile_type_var)
  profile_shape2_var <- gsub('_type_', '_shape2_', profile_type_var)
  # Create a data frame consisting of 'from' period, 'to' period, 
  # 'shape1' parameter and 'shape2' parameter which will be used to create the
  # synthesised distribution. The assumption is that both shape1 and shape2 
  # have the same period entries.
  # [DEVELOPMENT: This ought to be validated]
  df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable==profile_shape1_var],
                           shape1 = df_param_synthetic$value[df_param_synthetic$variable==profile_shape1_var],
                           shape2 = df_param_synthetic$value[df_param_synthetic$variable==profile_shape2_var]) %>% 
    mutate(to = lead(from, default = sim_periods+1)-1, .after = 'from')
  # Using apply and rbind create an array of dimensions [periods, bins+1] containing
  # the synthesised distributions
  arr <- do.call('rbind', 
                 # Cast as a list
                 list(
                   # Apply across each row of the df_periods dataframe
                   apply(
                     X = df_periods, 
                     MARGIN = 1, 
                     function(X){
                       # Get the row of data for the profile for the indicated period and
                       # ignoring the first two columns (variable and period) create a vector of
                       # values
                       v <- dbetabinom.ab(x = 0:sim_bins,
                                          size = sim_bins,
                                          shape1 = unname(X['shape1']),
                                          shape2 = unname(X['shape2']))
                       # Calculate the number of periods to fill with this vector
                       periods <- as.integer(X['to'] - X['from'] + 1)
                       # Duplicate the vector for each of the periods
                       v <- rep(v, periods)
                       return(v)
                     })))
  dim(arr) <- c(sim_bins+1, sim_periods)
  arr <- t(arr)
  return(arr)
}


# 1. Initialise Variables ----
# ****************************

# [DEVELOPMENT: This ought to be changed to command line argument input to aid automation for trusts]
inputfile <- './input/example_simulation.xlsx'

# Initiate timer
dtStart <- Sys.time()

# * 1.1. Simulation Variables ----
# ````````````````````````````````
df_param <- read_excel(path = inputfile, sheet = 'param')
df_param_synthetic <- read_excel(path = inputfile, sheet = 'param_synthetic')
df_param_actual <- read_excel(path = inputfile, sheet = 'param_actual')

sim_name <- df_param$value[df_param$variable=='name']
sim_trials <- as.integer(df_param$value[df_param$variable=='trials'])
sim_periods <- as.integer(df_param$value[df_param$variable=='periods'])
sim_bins <- as.integer(df_param$value[df_param$variable=='bins'])

# Create output directory
outputdir <- paste0(df_param$value[df_param$variable=='base_outdir'], '\\',
                    sim_name)
dir.create(path = outputdir, showWarnings = FALSE, recursive = TRUE)

# * 1.2. Profile Variables ----
# `````````````````````````````
# * * 1.2.1. Clock Stops ----
# Create the 2d arrays with dimensions [1:periods, 0:bins] for the profiles
csprof_nonadm <- array(data = rep(0, sim_periods * (sim_bins+1)),
                       dim = c(sim_periods, (sim_bins+1)))
csprof_adm <- csprof_nonadm

# * * 1.2.2. Demand ----
# Create the 2d arrays with dimensions [1:period, 0:bins] for the profiles
demprof_nonadm <- array(data = rep(0, sim_periods * (sim_bins+1)),
                        dim = c(sim_periods, (sim_bins+1)))
demprof_adm <- demprof_nonadm

# * * 1.2.3. ROTT ----
# Create the 2d arrays with dimensions [1:period, 0:bins] for the profiles
rottprof_nonadm <- array(data = rep(0, sim_periods * (sim_bins+1)),
                         dim = c(sim_periods, (sim_bins+1)))
rottprof_adm <- rottprof_nonadm

# * * 1.2.4. Conversion ----
# Create the 2d arrays with dimensions [1:period, 0:bins] for the profiles
convprof_nonadm <- array(data = rep(0, sim_periods * (sim_bins+1)),
                         dim = c(sim_periods, (sim_bins+1)))
convprof_adm <- convprof_nonadm

# * 1.3. Process Variables ----
# `````````````````````````````
# * * 1.3.1. Waiting List ----
# Create the 3d arrays with dimensions [0:periods, 0:bins, 1:trials] for the results
wl_nonadm <- array(data = rep(0, (sim_periods+1) * (sim_bins+1) * sim_trials),
                   dim = c((sim_periods+1), (sim_bins+1), sim_trials))
wl_adm <- wl_nonadm

# * * 1.3.2. Clock Stops ----
# Create the 3d arrays with dimensions [1:periods, 0:bins, 1:trials] for the results
cs_nonadm <- array(data = rep(0, sim_periods * (sim_bins+1) * sim_trials),
                   dim = c(sim_periods, (sim_bins+1), sim_trials))
cs_adm <- cs_nonadm

# * * 1.3.3. Demand ----
# Create the 2d arrays with dimensions [1:periods, 2] for the parameters
dem_param_nonadm <- array(data = rep(0, sim_periods * 2),
                          dim = c(sim_periods, 2))
dem_param_adm <- dem_param_nonadm
# Create the 2d arrays with dimensions [1:periods, 1:trails] for the volumes 
dem_vol_nonadm <- array(data = rep(0, sim_periods * sim_trials),
                        dim = c(sim_periods, sim_trials))
dem_vol_adm <- dem_vol_nonadm
# Create the 3d arrays with dimensions [1:periods, 0:bins, 1:trials] for the results
dem_nonadm <- array(data = rep(0, sim_periods * (sim_bins+1) * sim_trials),
                    dim = c(sim_periods, (sim_bins+1), sim_trials))
dem_adm <- dem_nonadm

# * * 1.3.4. Capacity ----
# Create the 2d arrays with dimensions [1:periods, 2] for the parameters
cap_param_nonadm <- array(data = rep(0, sim_periods * 2),
                          dim = c(sim_periods, 2))
cap_param_adm <- cap_param_nonadm
# Create the 2d arrays with dimensions [1:periods, 1:trails] for the volumes 
cap_vol_nonadm <- array(data = rep(0, sim_periods * sim_trials),
                        dim = c(sim_periods, sim_trials))
cap_vol_adm <- cap_vol_nonadm

# * * 1.3.5. Non-RTT ----
# Create the 1d arrays with dimensions [1:periods] for the parameters
nonrtt_param_nonadm <- array(data = rep(0, sim_periods),
                             dim = c(sim_periods))
nonrtt_param_adm <- nonrtt_param_nonadm
# Create the 2d arrays with dimensions [1:periods, 1:trails] for the volumes 
nonrtt_vol_nonadm <- array(data = rep(0, sim_periods * sim_trials),
                           dim = c(sim_periods, sim_trials))
nonrtt_vol_adm <- nonrtt_vol_nonadm

# * * 1.3.6. ROTT ----
# Create the 1d arrays with dimensions [1:periods] for the parameters
rott_param_nonadm <- array(data = rep(0, sim_periods),
                           dim = c(sim_periods))
rott_param_adm <- rott_param_nonadm
# Create the 2d arrays with dimensions [1:periods, 1:trails] for the volumes 
rott_vol_nonadm <- array(data = rep(0, sim_periods * sim_trials),
                         dim = c(sim_periods, sim_trials))
rott_vol_adm <- rott_vol_nonadm
# Create the 3d arrays with dimensions [1:periods, 0:bins, 1:trials] for the results
rott_nonadm <- array(data = rep(0, sim_periods * (sim_bins+1) * sim_trials),
                     dim = c(sim_periods, (sim_bins+1), sim_trials))
rott_adm <- rott_nonadm

# * * 1.3.7. Conversions ----
# Create the 1d arrays with dimensions [1:periods] for the parameters
conv_param_nonadm <- array(data = rep(0, sim_periods),
                           dim = c(sim_periods))
conv_param_adm <- conv_param_nonadm
# Create the 2d arrays with dimensions [1:periods, 1:trails] for the volumes 
conv_vol_nonadm <- array(data = rep(0, sim_periods * sim_trials),
                         dim = c(sim_periods, sim_trials))
conv_vol_adm <- conv_vol_nonadm
# Create the 3d arrays with dimensions [1:periods, 0:bins, 1:trials] for the results
conv_nonadm <- array(data = rep(0, sim_periods * (sim_bins+1) * sim_trials),
                     dim = c(sim_periods, (sim_bins+1), sim_trials))
conv_adm <- conv_nonadm

# * 2.1. Populate Time Independent Variables ----
# ```````````````````````````````````````````````
# * 2.2. Profile Variables ----
# `````````````````````````````

# * * 2.2.1. Clock Stops ----
# ```````````````````````````
# Create the clock stop profiles from either actual or synthesised distributions

# * * * 2.2.1.1. Non-Admitted ----
if(df_param$value[df_param$variable=='csprof_type_nonadm']=='actual'){
  # Read in from an actual distribution
  csprof_nonadm <- fnCreateProfileFromActualDistribution(profile_type_var = 'csprof_type_nonadm')
} else if(df_param$value[df_param$variable=='csprof_type_nonadm']=='synthetic'){
  # Create from a synthetic distribution
  csprof_nonadm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'csprof_type_nonadm')
}

# * * * 2.2.1.2. Admitted ----
# Create the clock stop profile from either an actual or synthesised distribution or a synthetic
if(df_param$value[df_param$variable=='csprof_type_adm']=='actual'){
  # Read in from an actual distribution
  csprof_adm <- fnCreateProfileFromActualDistribution(profile_type_var = 'csprof_type_adm')
} else if(df_param$value[df_param$variable=='csprof_type_adm']=='synthetic'){
  # Create from a synthetic distribution
  csprof_adm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'csprof_type_adm')
}

# * * 2.2.2. Demand ----
# ``````````````````````
# Create the demand profiles from either actual or synthesised distributions

# * * * 2.2.2.1. Non-Admitted ----
if(df_param$value[df_param$variable=='demprof_type_nonadm']=='actual'){
  # Read in from an actual distribution
  demprof_nonadm <- fnCreateProfileFromActualDistribution(profile_type_var = 'demprof_type_nonadm')
} else if(df_param$value[df_param$variable=='demprof_type_nonadm']=='synthetic'){
  # Create from a synthetic distribution
  demprof_nonadm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'demprof_type_nonadm')
}

# * * * 2.2.2.2. Admitted ----
if(df_param$value[df_param$variable=='demprof_type_adm']=='actual'){
  # Read in from an actual distribution
  demprof_adm <- fnCreateProfileFromActualDistribution(profile_type_var = 'demprof_type_adm')
} else if(df_param$value[df_param$variable=='demprof_type_adm']=='synthetic'){
  # Create from a synthetic distribution
  demprof_adm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'demprof_type_adm')
}

# * * 2.2.3. ROTT ----
# ``````````````````````
# Create the ROTT profiles from either actual or synthesised distributions

# * * * 2.2.3.1. Non-Admitted ----
if(df_param$value[df_param$variable=='rottprof_type_nonadm']=='actual'){
  # Read in from an actual distribution
  rottprof_nonadm <- fnCreateProfileFromActualDistribution(profile_type_var = 'rottprof_type_nonadm')
} else if(df_param$value[df_param$variable=='rottprof_type_nonadm']=='synthetic'){
  # Create from a synthetic distribution
  rottprof_nonadm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'rottprof_type_nonadm')
}

# * * * 2.2.3.2. Admitted ----
if(df_param$value[df_param$variable=='rottprof_type_adm']=='actual'){
  # Read in from an actual distribution
  rottprof_adm <- fnCreateProfileFromActualDistribution(profile_type_var = 'rottprof_type_adm')
} else if(df_param$value[df_param$variable=='rottprof_type_adm']=='synthetic'){
  # Create from a synthetic distribution
  rottprof_adm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'rottprof_type_adm')
}

# * * 2.2.4. Conversion ----
# ``````````````````````````
# Create the conversion profiles from either actual or synthesised distributions

# * * * 2.2.4.1. Non-Admitted ----
if(df_param$value[df_param$variable=='convprof_type_nonadm']=='actual'){
  # Read in from an actual distribution
  convprof_nonadm <- fnCreateProfileFromActualDistribution(profile_type_var = 'convprof_type_nonadm')
} else if(df_param$value[df_param$variable=='convprof_type_nonadm']=='synthetic'){
  # Create from a synthetic distribution
  convprof_nonadm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'convprof_type_nonadm')
}

# * * * 2.2.4.2. Admitted ----
if(df_param$value[df_param$variable=='convprof_type_adm']=='actual'){
  # Read in from an actual distribution
  convprof_adm <- fnCreateProfileFromActualDistribution(profile_type_var = 'convprof_type_adm')
} else if(df_param$value[df_param$variable=='convprof_type_adm']=='synthetic'){
  # Create from a synthetic distribution
  convprof_adm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'convprof_type_adm')
}

# * 2.3. Process Variables ----
# `````````````````````````````

# * * 2.3.1. Waiting List ----
# ````````````````````````````
# Create the initial (period=0) waiting list profiles from either actual or synthesised distributions
# the rest of the waiting list is time dependent and will calculated in the period iteration

# * * * 2.3.1.1. Non-Admitted ----
if(df_param$value[df_param$variable=='wl_type_nonadm']=='actual'){
  # Read the actual distribution into each trial at period 0 (row 1)
  v <- df_param_actual[df_param_actual$variable=='wl_nonadm', 3:(sim_bins+3)] %>% 
    unname() %>% t() %>% c()
  wl_nonadm[1,,] <- v 
} else if(df_param$value[df_param$variable=='wl_type_nonadm']=='synthetic'){
  # Create a waiting list profile from a synthetic distribution
  wl_size <- df_param_synthetic$value[df_param_synthetic$variable=='wl_size_nonadm']
  wl_shape1 <- df_param_synthetic$value[df_param_synthetic$variable=='wl_shape1_nonadm']
  wl_shape2 <- df_param_synthetic$value[df_param_synthetic$variable=='wl_shape2_nonadm']
  v <- tabulate(bin = rbetabinom.ab(n = wl_size, 
                                    size = sim_bins, 
                                    shape1 = wl_shape1, 
                                    shape2 = wl_shape2)+1,
                nbins = sim_bins+1)
  # Copy the synthesised distribution into each trial at period 0 (row 1)
  wl_nonadm[1,,] <- v 
}

# * * * 2.3.1.2. Admitted ----
if(df_param$value[df_param$variable=='wl_type_adm']=='actual'){
  # Read the actual distribution into each trial at period 0 (row 1)
  v <- df_param_actual[df_param_actual$variable=='wl_adm', 3:(sim_bins+3)] %>% 
    unname() %>% t() %>% c()
  wl_adm[1,,] <- v 
} else if(df_param$value[df_param$variable=='wl_type_adm']=='synthetic'){
  # Create a waiting list profile from a synthetic distribution
  wl_size <- df_param_synthetic$value[df_param_synthetic$variable=='wl_size_adm']
  wl_shape1 <- df_param_synthetic$value[df_param_synthetic$variable=='wl_shape1_adm']
  wl_shape2 <- df_param_synthetic$value[df_param_synthetic$variable=='wl_shape2_adm']
  v <- tabulate(bin = rbetabinom.ab(n = wl_size, 
                                    size = sim_bins, 
                                    shape1 = wl_shape1, 
                                    shape2 = wl_shape2)+1,
                nbins = sim_bins+1)
  # Copy the synthesised distribution into each trial at period 0 (row 1)
  wl_adm[1,,] <- v 
}

# * * 2.3.2. Clock Stops ----
# ```````````````````````````
# Clock stops are time dependent so nothing to calculate here

# * * 2.3.3. Demand ----
# ``````````````````````
# Populate the demand parameter and volume arrays from the input

# * * * 2.3.3.1. Non-Admitted ----
# Create a data frame of period from, period to, mean and sd
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='dem_mean_nonadm'],
                         mean = df_param_synthetic$value[df_param_synthetic$variable=='dem_mean_nonadm'],
                         sd = df_param_synthetic$value[df_param_synthetic$variable=='dem_sd_nonadm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_mean <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['mean'], X['to']-X['from']+1)})))
v_sd <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['sd'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
dem_param_nonadm[,1] <- v_mean
dem_param_nonadm[,2] <- v_sd

# Create the volume array for all the trials and periods
dem_vol_nonadm[,] <- round(rnorm(n = sim_periods * sim_trials,
                           mean = dem_param_nonadm[,1], 
                           sd = dem_param_nonadm[,2]))

# * * * 2.3.3.2. Admitted ----
# Create a data frame of period from, period to, mean and sd
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='dem_mean_adm'],
                         mean = df_param_synthetic$value[df_param_synthetic$variable=='dem_mean_adm'],
                         sd = df_param_synthetic$value[df_param_synthetic$variable=='dem_sd_adm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_mean <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['mean'], X['to']-X['from']+1)})))
v_sd <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['sd'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
dem_param_adm[,1] <- v_mean
dem_param_adm[,2] <- v_sd

# Create the volume array for all the trials and periods
dem_vol_adm[,] <- round(rnorm(n = sim_periods * sim_trials,
                              mean = dem_param_adm[,1], 
                              sd = dem_param_adm[,2]))

# * * 2.3.4. Capacity ----
# ````````````````````````
# Populate the capacity parameter and volume arrays from the input

# * * * 2.3.4.1. Non-Admitted ----
# Create a data frame of period from, period to, mean and sd
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='cap_mean_nonadm'],
                         mean = df_param_synthetic$value[df_param_synthetic$variable=='cap_mean_nonadm'],
                         sd = df_param_synthetic$value[df_param_synthetic$variable=='cap_sd_nonadm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_mean <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['mean'], X['to']-X['from']+1)})))
v_sd <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['sd'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
cap_param_nonadm[,1] <- v_mean
cap_param_nonadm[,2] <- v_sd

# Create the volume array for all the trials and periods
cap_vol_nonadm[,] <- round(rnorm(n = sim_periods * sim_trials,
                                 mean = cap_param_nonadm[,1], 
                                 sd = cap_param_nonadm[,2]))

# * * * 2.3.4.1. Admitted ----
# Create a data frame of period from, period to, mean and sd
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='cap_mean_adm'],
                         mean = df_param_synthetic$value[df_param_synthetic$variable=='cap_mean_adm'],
                         sd = df_param_synthetic$value[df_param_synthetic$variable=='cap_sd_adm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_mean <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['mean'], X['to']-X['from']+1)})))
v_sd <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['sd'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
cap_param_adm[,1] <- v_mean
cap_param_adm[,2] <- v_sd

# Create the volume array for all the trials and periods
cap_vol_adm[,] <- round(rnorm(n = sim_periods * sim_trials,
                              mean = cap_param_adm[,1], 
                              sd = cap_param_adm[,2]))

# * * 2.3.5. Non-RTT ----
# ```````````````````````
# Populate the non-RTT parameter and volume arrays from the input

# * * * 2.3.5.1. Non-Admitted ----
# Create a data frame of period from, period to, and probability
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='nonrtt_prob_nonadm'],
                         prob = df_param_synthetic$value[df_param_synthetic$variable=='nonrtt_prob_nonadm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_prob <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['prob'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
nonrtt_param_nonadm <- v_prob

# Create the volume array for all the trials and periods
nonrtt_vol_nonadm[,] <- rbinom(n = sim_periods*sim_trials, 
                            size = cap_vol_nonadm, 
                            prob = nonrtt_param_nonadm)

# * * * 2.3.5.2. Admitted ----
# Create a data frame of period from, period to, and probability
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='nonrtt_prob_adm'],
                         prob = df_param_synthetic$value[df_param_synthetic$variable=='nonrtt_prob_adm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_prob <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['prob'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
nonrtt_param_adm <- v_prob

# Create the volume array for all the trials and periods
nonrtt_vol_adm[,] <- rbinom(n = sim_periods*sim_trials, 
                            size = cap_vol_adm, 
                            prob = nonrtt_param_adm)

# * * 2.3.6. ROTT ----
# Populate the ROTT parameter and volume arrays from the input, 
# the ROTT results are time dependent so nothing to calculate here

# * * * 2.3.6.1. Non-Admitted ----
# Create a data frame of period from, period to, and probability
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='rott_prob_nonadm'],
                         prob = df_param_synthetic$value[df_param_synthetic$variable=='rott_prob_nonadm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_prob <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['prob'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
rott_param_nonadm <- v_prob

# Create the volume array for all the trials and periods
rott_vol_nonadm[,] <- rbinom(n = sim_periods*sim_trials, 
                             size = cap_vol_nonadm, 
                             prob = rott_param_nonadm)

# * * * 2.3.6.2. Admitted ----
# Create a data frame of period from, period to, and probability
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='rott_prob_adm'],
                         prob = df_param_synthetic$value[df_param_synthetic$variable=='rott_prob_adm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_prob <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['prob'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
rott_param_adm <- v_prob

# Create the volume array for all the trials and periods
rott_vol_adm[,] <- rbinom(n = sim_periods*sim_trials, 
                          size = cap_vol_adm, 
                          prob = rott_param_adm)

# * * 2.3.7. Conversions ----
# Populate the conversion parameter and volume arrays from the input, 
# the ROTT results are time dependent so nothing to calculate here

# * * * 2.3.7.1. Non-Admitted ----
# Create a data frame of period from, period to, and probability
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='conv_prob_nonadm'],
                         prob = df_param_synthetic$value[df_param_synthetic$variable=='conv_prob_nonadm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_prob <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['prob'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
conv_param_nonadm <- v_prob

# Create the volume array for all the trials and periods
conv_vol_nonadm[,] <- rbinom(n = sim_periods*sim_trials, 
                             size = cap_vol_nonadm, 
                             prob = conv_param_nonadm)

# * * * 2.3.7.2. Admitted ----
# Create a data frame of period from, period to, and probability
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='conv_prob_adm'],
                         prob = df_param_synthetic$value[df_param_synthetic$variable=='conv_prob_adm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_prob <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['prob'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
conv_param_adm <- v_prob

# Create the volume array for all the trials and periods
conv_vol_adm[,] <- rbinom(n = sim_periods*sim_trials, 
                          size = cap_vol_adm, 
                          prob = conv_param_adm)










# * 2.1. Populate Time Independent Variables ----
# ```````````````````````````````````````````````
# * 2.2. Profile Variables ----
# `````````````````````````````
# * * 2.2.1. Clock Stops ----
# * * 2.2.2. Demand ----
# * * 2.2.3. ROTT ----
# * * 2.2.4. Conversion ----
#
# * 2.3. Process Variables ----
# `````````````````````````````
# * * 2.3.1. Waiting List ----
# * * 2.3.2. Clock Stops ----
# * * 2.3.3. Demand ----
# * * 2.3.4. Capacity ----
# * * 2.3.5. Non-RTT ----
# * * 2.3.6. ROTT ----
# * * 2.3.7. Conversions ----


# ----------------------------


# Simulation outputs
# ******************
# Waiting Lists:
# wl_nonadm - 2d matrix (period, bin) showing the number of people waiting on 
#             the non-admitted waiting list by period and bin
# wl_adm - 2d matrix (period, bin) showing the number of people waiting on the 
#          admitted waiting list by period and bin

# Take-off Profiles:
# top_nonadm - 2d matrix (period, bin) showing the non-admitted 
#              take-off weighting by period and bin
# top_adm - 2d matrix (period, bin) showing the admitted 
#           take-off weighting by period and bin

# Clock Stops:
# cs_nonadm - 2d matrix (period, bin) showing the non-admitted clock stops by 
#             period and bin
# cs_adm - 2d matrix (period, bin) showing the admitted clock stops by 
#          period and bin

# Demand:
# dem_nonadm - 1d matrix (period) showing the new non-admitted demand by period
# dem_adm - 1d matrix (period) showing the new admitted demand by period

# Capacity:
# cap_nonadm - 1d matrix (period) showing the total non-admitted capacity by period
# cap_adm - 1d matrix (period) showing the total admitted capacity by period

# Non-RTT:
# nonrtt_nonadm - 1d matrix (period) showing the non-admitted non-RTT activity 
#                 by period
# nonrtt_adm - 1d matrix (period) showing the admitted non-RTT activity 
#                 by period

# ROTT:
# rott_nonadm - 1d matrix (period) showing the non-admitted removals other than 
#               treatment (ROTT) activity by period
# rott_adm - 1d matrix (period) showing the admitted removals other than 
#               treatment (ROTT) activity by period

# Conversions:
# conv_nonadm - 2d matrix (period, bin) showing the non-admitted pathways converted 
#               to admitted pathways by period and bin
# conv_adm - NOT REQUIRED AS MODELLING ASSUMPTION IS THAT NO CONVERSION EXISTS
#            FROM ADM TO NONADM PATHWAYS


# * 1.1. Waiting List [SINGLE DIST|MATRIX] ----
# *********************************************
# Get the size of the array that we will populate with the results of one simulation run
array_size <- (sim_periods+1)*(sim_bins+1)*(sim_trials)
# Create an array and fill with zeros for the admitted waiting list
wl_adm <- array(data = rep(0, array_size),
                dim = c(sim_periods+1, sim_bins+1, sim_trials))
# Create a copy of the admitted waiting list for the non-admitted waiting list
wl_nonadm <- wl_adm

# * * 1.1.1. Waiting List - NonAdm ----
# *************************************
if(df_param %>% filter(variable == 'wl_type_nonadm') %>% .$value == 'matrix'){
  # Read the waiting list from the param matrix into the first line of the waiting list array (period 0) for each trial
  wl_nonadm[1,,] <- df_param_matrix %>% 
    filter(variable == 'wl_nonadm' & period == 0) %>%
    select(-c(sim_code, variable, period)) %>% 
    t() %>% 
    as.vector()
} else if(df_param %>% filter(variable == 'wl_type_nonadm') %>% .$value == 'dist'){
  # Create and tabulate the waiting list from the param dist variables and write to the first line of the array for each trial
  waiting_list_size <- df_param_dist$value[df_param_dist$variable=='wl_size_nonadm']
  shape_param_1 <- df_param_dist$value[df_param_dist$variable=='wl_shape1_nonadm']
  shape_param_2 <- df_param_dist$value[df_param_dist$variable=='wl_shape2_nonadm']
  wl_nonadm[1,,] <- tabulate(bin = rbetabinom.ab(n = waiting_list_size, 
                                            size = bins, 
                                            shape1 = shape_param_1, 
                                            shape2 = shape_param_2),
                          nbins = sim_bins+1)
}

# * * 1.1.2. Waiting List - Adm ----
# **********************************
if(df_param %>% filter(variable == 'wl_type_adm') %>% .$value == 'matrix'){
  # Read the waiting list from the param matrix into the first line of the waiting list array (period 0) for each trial
  wl_adm[1,,] <- df_param_matrix %>% 
    filter(variable == 'wl_adm' & period == 0) %>%
    select(-c(sim_code, variable, period)) %>% 
    t() %>% 
    as.vector()
} else if(df_param %>% filter(variable == 'wl_type_adm') %>% .$value == 'dist'){
  # Create and tabulate the waiting list from the param dist variables and write to the first line of the array for each trial
  waiting_list_size <- df_param_dist$value[df_param_dist$variable=='wl_size_adm']
  shape_param_1 <- df_param_dist$value[df_param_dist$variable=='wl_shape1_adm']
  shape_param_2 <- df_param_dist$value[df_param_dist$variable=='wl_shape2_adm']
  wl_adm[0, ] <- tabulate(bin = rbetabinom.ab(n = waiting_list_size, 
                                              size = bins, 
                                              shape1 = shape_param_1, 
                                              shape2 = shape_param_2),
                          nbins = sim_bins+1)
}

# * 1.2. Take Off Profile [MULTIPLE MATRIX] ----
# **********************************************
# Resize the array as we only need period zero for the waiting list, note the lack of trial dimension as
# there is no variability in the take-off profile by trial
array_size <- (sim_periods)*(sim_bins+1)
# Create an array and fill with zeros for the admitted take-off profile
top_adm <- array(data = rep(0, array_size),
                dim = c(sim_periods, sim_bins+1))
# Create a copy of the admitted take-off profile for the non-admitted take-off profile
top_nonadm <- top_adm

# * * 1.2.1. Take Off Profile - NonAdm ----
# *****************************************
if(df_param %>% filter(variable == 'top_type_nonadm') %>% .$value == 'matrix'){
  # Read the take-off profile from the param matrix into the relevant line of the array (as defined by period)
  period_list <- df_param_matrix %>% 
    filter(variable == 'top_nonadm') %>% 
    arrange(period) %>% 
    distinct(period) %>% 
    .$period
  for(p in 1:length(period_list)){
    # Calculate the from and to period that we will block fill
    from_period <- period_list[p]
    to_period <- period_list[p+1]-1
    if(is.na(to_period)){
      to_period <- sim_periods
    }
    vec_values <- df_param_matrix %>% filter(variable == 'top_nonadm' & period == from_period) %>% 
      select(-c(sim_code, variable, period)) %>% 
      t() %>% 
      as.vector()
    # Add row by row to array NB: no need to add one to the period row one refers to period 1
    for(r in from_period:to_period){
      top_nonadm[r,] <- vec_values  
    }
  }
} else if(df_param %>% filter(variable == 'top_type_nonadm') %>% .$value == 'dist'){
  # Create and tabulate the take-off profile from the param dist variables and write to the relevant line of the array (as defined by period)
  period_list <- df_param_dist %>% 
    filter(variable == 'top_shape1_nonadm') %>% 
    arrange(period) %>% 
    distinct(period) %>% 
    .$period
  for(p in 1:length(period_list)){
    # Get the shape parameters
    shape_param_1 <- df_param_dist %>% filter(variable == 'top_shape1_nonadm' & period == period_list[p]) %>% .$value
    shape_param_2 <- df_param_dist %>% filter(variable == 'top_shape2_nonadm' & period == period_list[p]) %>% .$value
    
    # Calculate the from and to period that we will block fill
    from_period <- period_list[p]
    to_period <- period_list[p+1]-1
    if(is.na(to_period)){
      to_period <- sim_periods
    }
    # Create the take-off profile
    vec_values <- dbetabinom.ab(x = 0:sim_bins, 
                                size = sim_bins, 
                                shape1 = shape_param_1, 
                                shape2 = shape_param_2)
    # Add row by row to array NB: no need to add one to the period row one refers to period 1
    for(r in from_period:to_period){
      top_nonadm[r,] <- vec_values  
    }
  }
}

# * * 1.2.2. Take Off Profile - Adm ----
# **************************************
if(df_param %>% filter(variable == 'top_type_adm') %>% .$value == 'matrix'){
  # Read the take-off profile from the param matrix into the relevant line of the array (as defined by period)
  period_list <- df_param_matrix %>% 
    filter(variable == 'top_adm') %>% 
    arrange(period) %>% 
    distinct(period) %>% 
    .$period
  for(p in 1:length(period_list)){
    # Calculate the from and to period that we will block fill
    from_period <- period_list[p]
    to_period <- period_list[p+1]-1
    if(is.na(to_period)){
      to_period <- sim_periods
    }
    vec_values <- df_param_matrix %>% filter(variable == 'top_adm' & period == from_period) %>% 
      select(-c(sim_code, variable, period)) %>% 
      t() %>% 
      as.vector()
    # Add row by row to array NB: no need to add one to the period row one refers to period 1
    for(r in from_period:to_period){
      top_adm[r,] <- vec_values  
    }
  }
} else if(df_param %>% filter(variable == 'top_type_adm') %>% .$value == 'dist'){
  # Create and tabulate the take-off profile from the param dist variables and write to the relevant line of the array (as defined by period)
  period_list <- df_param_dist %>% 
    filter(variable == 'top_shape1_adm') %>% 
    arrange(period) %>% 
    distinct(period) %>% 
    .$period
  for(p in 1:length(period_list)){
    # Get the shape parameters
    shape_param_1 <- df_param_dist %>% filter(variable == 'top_shape1_adm' & period == period_list[p]) %>% .$value
    shape_param_2 <- df_param_dist %>% filter(variable == 'top_shape2_adm' & period == period_list[p]) %>% .$value
    
    # Calculate the from and to period that we will block fill
    from_period <- period_list[p]
    to_period <- period_list[p+1]-1
    if(is.na(to_period)){
      to_period <- sim_periods
    }
    # Create the take-off profile
    vec_values <- dbetabinom.ab(x = 0:sim_bins, 
                                size = sim_bins, 
                                shape1 = shape_param_1, 
                                shape2 = shape_param_2)
    # Add row by row to array NB: no need to add one to the period row one refers to period 1
    for(r in from_period:to_period){
      top_adm[r,] <- vec_values  
    }
  }
}

# * 1.3. Clock Stops [MULTIPLE] ---- 
# **********************************
# Create an array and fill with zeros for the admitted clock stops, again not needing
# a period zero for the clock stops
array_size <- (sim_periods+1)*(sim_bins+1)*(sim_trials)
cs_adm <- array(data = rep(0, array_size),
                dim = c(sim_periods, sim_bins+1, sim_trials))
# Create a copy of the admitted clock stops for the non-admitted clock stops
cs_nonadm <- cs_adm

# * * 1.3.1. Clock Stops - NonAdm ----
# ************************************
# No additional processing needed for the clock stops but for completeness we will keep the section header

# * * 1.3.2. Clock Stops - Adm ----
# *********************************
# No additional processing needed for the clock stops but for completeness we will keep the section header

# * 1.4. Demand [MULTIPLE DIST] ----
# **********************************
# Create a 2d array for the demand parameters (mean and sd), note lack of trials dimension as 
# no variability in the parameters by trial
dem_param_adm <- array(data = rep(0, sim_periods*2),
                 dim = c(sim_periods, 2))
# Create a copy of the new admitted demand parameters for the new non-admitted demand parameters
dem_param_nonadm <- dem_param_adm

# * * 1.4.1. Demand - NonAdm ----
# *******************************
# Get the parameter entries for the non-admitted demand
period_list <- df_param_dist %>% 
  filter(variable == 'dem_mean_nonadm') %>% 
  arrange(period) %>% 
  distinct(period) %>% 
  .$period
for(p in 1:length(period_list)){
  # Get the demand parameters
  dem_mean_nonadm <- df_param_dist %>% filter(variable == 'dem_mean_nonadm' & period == period_list[p]) %>% .$value
  dem_sd_nonadm <- df_param_dist %>% filter(variable == 'dem_sd_nonadm' & period == period_list[p]) %>% .$value
  # Calculate the from and to period that we will block fill
  from_period <- period_list[p]
  to_period <- period_list[p+1]-1
  if(is.na(to_period)){
    to_period <- sim_periods
  }
  # Add columns of means and sd to the array
  dem_param_nonadm[from_period:to_period, 1] <- dem_mean_nonadm
  dem_param_nonadm[from_period:to_period, 2] <- dem_sd_nonadm
}

# * * 1.4.2. Demand - Adm ----
# ****************************
period_list <- df_param_dist %>% 
  filter(variable == 'dem_mean_adm') %>% 
  arrange(period) %>% 
  distinct(period) %>% 
  .$period
for(p in 1:length(period_list)){
  # Get the demand parameters
  dem_mean_adm <- df_param_dist %>% filter(variable == 'dem_mean_adm' & period == period_list[p]) %>% .$value
  dem_sd_adm <- df_param_dist %>% filter(variable == 'dem_sd_adm' & period == period_list[p]) %>% .$value
  # Calculate the from and to period that we will block fill
  from_period <- period_list[p]
  to_period <- period_list[p+1]-1
  if(is.na(to_period)){
    to_period <- sim_periods
  }
  # Add columns of means and sd to the array
  dem_param_adm[from_period:to_period, 1] <- dem_mean_adm
  dem_param_adm[from_period:to_period, 2] <- dem_sd_adm
}

# * 1.5. Capacity [MULTIPLE DIST] ----
# ************************************
# Create a 2d array for the capacity parameters (mean and sd)
cap_param_adm <- array(data = rep(0, sim_periods*2),
                       dim = c(sim_periods, 2))
# Create a copy of the total admitted capacity parameters for the total 
# non-admitted capacity parameters
cap_param_nonadm <- cap_param_adm

# * * 1.5.1. Capacity - NonAdm ----
# *********************************
# Get the parameter entries for the non-admitted capacity
period_list <- df_param_dist %>% 
  filter(variable == 'cap_mean_nonadm') %>% 
  arrange(period) %>% 
  distinct(period) %>% 
  .$period
for(p in 1:length(period_list)){
  # Get the cacapity parameters
  cap_mean_nonadm <- df_param_dist %>% filter(variable == 'cap_mean_nonadm' & period == period_list[p]) %>% .$value
  cap_sd_nonadm <- df_param_dist %>% filter(variable == 'cap_sd_nonadm' & period == period_list[p]) %>% .$value
  # Calculate the from and to period that we will block fill
  from_period <- period_list[p]
  to_period <- period_list[p+1]-1
  if(is.na(to_period)){
    to_period <- sim_periods
  }
  # Add columns of means and sd to the array
  cap_param_nonadm[from_period:to_period, 1] <- cap_mean_nonadm
  cap_param_nonadm[from_period:to_period, 2] <- cap_sd_nonadm
}

# * * 1.5.2. Capacity - Adm ----
# ******************************
period_list <- df_param_dist %>% 
  filter(variable == 'cap_mean_adm') %>% 
  arrange(period) %>% 
  distinct(period) %>% 
  .$period
for(p in 1:length(period_list)){
  # Get the capacity parameters
  cap_mean_adm <- df_param_dist %>% filter(variable == 'cap_mean_adm' & period == period_list[p]) %>% .$value
  cap_sd_adm <- df_param_dist %>% filter(variable == 'cap_sd_adm' & period == period_list[p]) %>% .$value
  # Calculate the from and to period that we will block fill
  from_period <- period_list[p]
  to_period <- period_list[p+1]-1
  if(is.na(to_period)){
    to_period <- sim_periods
  }
  # Add columns of means and sd to the array
  cap_param_adm[from_period:to_period, 1] <- cap_mean_adm
  cap_param_adm[from_period:to_period, 2] <- cap_sd_adm
}

# * 1.6. Non-RTT Activity [MULTIPLE DIST] ----
# ********************************************
# Create a 1d array for the non-RTT admitted parameter (probability)
nonrtt_param_adm <- array(data = rep(0, sim_periods),
                       dim = c(sim_periods))
# Create a copy of the non-RTT admitted parameter for the non-RTT non-admitted parameter
nonrtt_param_nonadm <- nonrtt_param_adm

# * * 1.6.1. Non-RTT Activity - NonAdm ----
# *****************************************
period_list <- df_param_dist %>% 
  filter(variable == 'nonrtt_prob_nonadm') %>% 
  arrange(period) %>% 
  distinct(period) %>% 
  .$period
for(p in 1:length(period_list)){
  # Get the non-RTT parameter
  nonrtt_prob_nonadm <- df_param_dist %>% filter(variable == 'nonrtt_prob_nonadm' & period == period_list[p]) %>% .$value
  # Calculate the from and to period that we will block fill
  from_period <- period_list[p]
  to_period <- period_list[p+1]-1
  if(is.na(to_period)){
    to_period <- sim_periods
  }
  # Add column of non-RTT probability to the array
  nonrtt_param_nonadm[from_period:to_period] <- nonrtt_prob_nonadm  
}

# * * 1.6.2. Non-RTT Activity - Adm ----
# **************************************
period_list <- df_param_dist %>% 
  filter(variable == 'nonrtt_prob_adm') %>% 
  arrange(period) %>% 
  distinct(period) %>% 
  .$period
for(p in 1:length(period_list)){
  # Get the non-RTT parameter
  nonrtt_prob_adm <- df_param_dist %>% filter(variable == 'nonrtt_prob_adm' & period == period_list[p]) %>% .$value
  # Calculate the from and to period that we will block fill
  from_period <- period_list[p]
  to_period <- period_list[p+1]-1
  if(is.na(to_period)){
    to_period <- sim_periods
  }
  # Add column of non-RTT probability to the array
  nonrtt_param_adm[from_period:to_period] <- nonrtt_prob_adm  
}

# * 1.7. ROTT Activity [MULTIPLE DIST] ----
# *****************************************
# Create a 1d array for the ROTT admitted parameter (probability)
rott_param_adm <- array(data = rep(0, sim_periods),
                          dim = c(sim_periods))
# Create a copy of the ROTT admitted parameter for the ROTT non-admitted parameter
rott_param_nonadm <- rott_param_adm

# * * 1.7.1. ROTT Activity - NonAdm ----
# **************************************
period_list <- df_param_dist %>% 
  filter(variable == 'rott_prob_nonadm') %>% 
  arrange(period) %>% 
  distinct(period) %>% 
  .$period
for(p in 1:length(period_list)){
  # Get the ROTT parameter
  rott_prob_nonadm <- df_param_dist %>% filter(variable == 'rott_prob_nonadm' & period == period_list[p]) %>% .$value
  # Calculate the from and to period that we will block fill
  from_period <- period_list[p]
  to_period <- period_list[p+1]-1
  if(is.na(to_period)){
    to_period <- sim_periods
  }
  # Add column of ROTT probability to the array
  rott_param_nonadm[from_period:to_period] <- rott_prob_nonadm  
}

# * * 1.7.2. ROTT Activity - Adm ----
# ***********************************

period_list <- df_param_dist %>% 
  filter(variable == 'rott_prob_adm') %>% 
  arrange(period) %>% 
  distinct(period) %>% 
  .$period
for(p in 1:length(period_list)){
  # Get the ROTT parameter
  rott_prob_adm <- df_param_dist %>% filter(variable == 'rott_prob_adm' & period == period_list[p]) %>% .$value
  # Calculate the from and to period that we will block fill
  from_period <- period_list[p]
  to_period <- period_list[p+1]-1
  if(is.na(to_period)){
    to_period <- sim_periods
  }
  # Add column of ROTT probability to the array
  rott_param_adm[from_period:to_period] <- rott_prob_adm
}

# * 1.8. Conversion Activity [MULTIPLE DIST] ----
# ***********************************************
# Create an array and fill with zeros for the admitted clock stops, again not needing
# a period zero for the clock stops
conv_nonadm <- array(data = rep(0, array_size),
                dim = c(sim_periods, sim_bins+1, sim_trials))
# Create a 1d array for the conversion parameter (probability)
conv_param_nonadm <- array(data = rep(0, sim_periods),
                        dim = c(sim_periods))

# * * 1.8.1. Conversion Activity - NonAdm to Adm ----
# ***************************************************
period_list <- df_param_dist %>% 
  filter(variable == 'conv_prob_nonadm') %>% 
  arrange(period) %>% 
  distinct(period) %>% 
  .$period
for(p in 1:length(period_list)){
  # Get the conversion parameter
  conv_prob_nonadm <- df_param_dist %>% filter(variable == 'conv_prob_nonadm' & period == period_list[p]) %>% .$value
  # Calculate the from and to period that we will block fill
  from_period <- period_list[p]
  to_period <- period_list[p+1]-1
  if(is.na(to_period)){
    to_period <- sim_periods
  }
  # Add column of conversion probability to the array
  conv_param_nonadm[from_period:to_period] <- conv_prob_nonadm  
}

# * * 1.8.2. No conversion step necessary for Adm to NonAdm ----
# **************************************************************
# The model assumes no conversion from admitted to non-admitted pathways

# 2. Start of Simulation Iteration ----
# *************************************
# Note: this is the section that we will convert into a function
for(t in 1:sim_trials){
  # * 2.1. Create demand ----
  # *************************
  # Create a 2d array and fill with zeros for the new admitted demand
  dem_adm <- array(data = rep(0, sim_periods*sim_trials),
                   dim = c(sim_periods, sim_trials))
  # Create a copy of the new admitted demand for the new non-admitted demand
  dem_nonadm <- dem_adm
  
  # * * 2.1.1. Create demand - Non-Admitted ----
  # ********************************************
  # Pre-populate the dem_nonadm by running the random number generator
  dem_nonadm <- round(rnorm(n = sim_periods*sim_trials, mean = dem_param_nonadm[,1], sd = dem_param_nonadm[,2]))
  dim(dem_nonadm) <- c(sim_periods, sim_trials)
  # * * 2.1.2. Create demand - Admitted ----
  # ****************************************
  # Pre-populate the dem_adm by running the random number generator
  dem_adm <- round(rnorm(n = sim_periods*sim_trials, mean = dem_param_adm[,1], sd = dem_param_adm[,2]))
  dim(dem_adm) <- c(sim_periods, sim_trials)
  
  # * 2.2. Create capacity ----
  # ***************************
  # Create a 1d array and fill with zeros for the total admitted capacity
  cap_adm <- array(data = rep(0, sim_periods*sim_trials),
                   dim = c(sim_periods, sim_trials))
  # Create a copy of the total admitted capacity for the total non-admitted capacity
  cap_nonadm <- cap_adm
  
  # * * 2.2.1. Create capacity - Non-Admitted ----
  # **********************************************
  # Pre-populate the cap_nonadm by running the random number generator
  cap_nonadm <- round(rnorm(n = sim_periods*sim_trials, mean = cap_param_nonadm[,1], sd = cap_param_nonadm[,2]))
  dim(cap_nonadm) <- c(sim_periods, sim_trials)
  
  # * * 2.2.2. Create capacity - Admitted ----
  # ******************************************
  # Pre-populate the cap_adm by running the random number generator
  cap_adm <- round(rnorm(n = sim_periods*sim_trials, mean = cap_param_adm[,1], sd = cap_param_adm[,2]))
  dim(cap_adm) <- c(sim_periods, sim_trials)
  
  # * 2.3. Create non-RTT activity ----
  # ***********************************
  # Create a 2d array and fill with zeros for the non-RTT admitted activity
  nonrtt_adm <- array(data = rep(0, sim_periods*sim_trials),
                      dim = c(sim_periods, sim_trials))
  # Create a copy of the non-RTT admitted activity for the non-RTT non-admitted activity
  nonrtt_nonadm <- nonrtt_adm
  
  # * * 2.3.1. Create non-RTT activity - Non-Admitted ----
  # ******************************************************
  # Pre-populate the nonrtt_nonadm by running the random number generator and using the pre-populated capacity
  nonrtt_nonadm <- rbinom(n = sim_periods*sim_trials, size = cap_nonadm, prob = nonrtt_param_nonadm)
  dim(nonrtt_nonadm) <- c(sim_periods, sim_trials)
  
  # * * 2.3.2. Create non-RTT activity - Admitted ----
  # **************************************************
  # Pre-populate the nonrtt_adm by running the random number generator and using the pre-populated capacity
  nonrtt_adm <- rbinom(n = sim_periods*sim_trials, size = cap_adm, prob = nonrtt_param_adm)
  dim(nonrtt_adm) <- c(sim_periods, sim_trials)
  
  # * 2.4. Create non-RTT activity ----
  # ***********************************
  # Create a 1d array and fill with zeros for the ROTT admitted activity
  rott_adm <- array(data = rep(0, sim_periods*sim_trials),
                    dim = c(sim_periods,sim_trials))
  # Create a copy of the ROTT admitted activity for the ROTT non-admitted activity
  rott_nonadm <- rott_adm
  # Note: We are unsure of the driver of ROTT so for ease of caclulation of the probablity the
  # model assumes ROTT is a function of total capacity, removal is also assumed to be unweighted
  # unlike take-off profiles but this could be changed should the coder wish to do so
  
  # * * 2.4.1. Create non-RTT activity - Non-Admitted ----
  # ******************************************************
  # Pre-populate the rott_nonadm by running the random number generator and using the pre-populated capacity
  rott_nonadm <- rbinom(n = sim_periods*sim_trials, size = cap_nonadm, prob = rott_param_nonadm)
  dim(rott_nonadm) <- c(sim_periods, sim_trials)
  
  # * * 2.4.2. Create non-RTT activity - Admitted ----
  # **************************************************
  # Pre-populate the rott_adm by running the random number generator and using the pre-populated capacity
  rott_adm <- rbinom(n = sim_periods*sim_trials, size = cap_adm, prob = rott_param_adm)
  dim(rott_adm) <- c(sim_periods, sim_trials)
  
  # * 2.5. Create conversion activity ---- 
  # **************************************
  # Create a 2d array and fill with zeros for the conversion from non-admitted to 
  # admitted activity. NB: There is assumed to be no conversion in the opposite direction
  conv_vol_nonadm <- array(data = rep(0, sim_periods*sim_trials),
                           dim = c(sim_periods, sim_trials))
  
  # * * 2.5.1. Create conversion activity - Non-Admitted ----
  # *********************************************************
  # Pre-populate the conv_nonadm by running the random number generator and using 
  # the pre-populated capacity and removing the pre-populated non-RTT activity
  conv_vol_nonadm <- rbinom(n = sim_periods*sim_trials, size = (cap_nonadm - nonrtt_nonadm), prob = conv_param_nonadm)
  dim(conv_vol_nonadm) <- c(sim_periods, sim_trials)
  
  # * * 2.5.2. Create conversion activity - Admitted ----
  # *****************************************************
  # Model assumes no conversion from admitted to non-admitted pathways
  
  # Run the period loop
  for(p in 1:sim_periods){
    # 3. Start of Period Iteration ----
    # *********************************
    
    # * 3.1. Process Non-Admitted Activity ----
    # *****************************************
    
    # * * 3.1.1. Initialise waiting list - Non-Admitted ----
    # ******************************************************
    # Initialise the current period waiting list with that of the previous period
    # NB: As this is zero based p+1 is the current period
    wl_nonadm[p+1,,t] <- wl_nonadm[p,,t]
    
    # * * 3.1.2. Process ROTT - Non-Admitted ----
    # *******************************************
    # Create ROTT sample from waiting list, and check that we don't sample more than is in the waiting list
    rott_sample_nonadm <- sample(x = rep(0:sim_bins, wl_nonadm[p+1,,t]),
                                 size = min(sum(wl_nonadm[p+1,,t]), 
                                            rott_nonadm[p,t]),
                                 replace = FALSE,
                                 prob = NULL) # Replace prob with weightings if required
    # Remove the sample from the current waiting list
    # NB: We tabulate the sample with elements incremented by one as tabulate only 
    # deals with positive integers
    wl_nonadm[p+1,,t] <- wl_nonadm[p+1,,t] - tabulate(bin = rott_sample_nonadm+1,
                                                    nbins = sim_bins+1)
    
    # * * 3.1.3. Process conversions - Non-Admitted ----
    # **************************************************
    # Create conversion sample, and check that we don't sample more than is in the waiting list
    conv_sample_nonadm <- sample(x = rep(0:sim_bins, wl_nonadm[p+1,,t]),
                                 size = min(sum(wl_nonadm[p+1,,t]), 
                                            conv_vol_nonadm[p,t]),
                                 replace = FALSE,
                                 prob = NULL) # Replace prob with weightings if required
    # Remove the sample from the waiting list
    # NB: We tabulate the sample with elements incremented by one as tabulate only 
    # deals with positive integers
    wl_nonadm[p+1,,t] <- wl_nonadm[p+1,,t] - tabulate(bin = conv_sample_nonadm+1,
                                                    nbins = sim_bins+1)
    # Record the sample in the conv_nonadm array for this period
    conv_nonadm[p,,t] <- tabulate(bin = conv_sample_nonadm+1,
                                 nbins = sim_bins+1)
    
    # * * 3.1.4. Process clock stops - Non-Admitted ----
    # **************************************************
    # Create clock stop sample from waiting list based on current period take-off profile
    # The rep used in the x converts from tabulated form into list form
    # The rep used in the prob creates a list of probabilites in the take-off profile 
    # based on the bin represented by x (adding one as the bins are zero based), 
    # and check that we don't sample more than is in the waiting list
    cs_sample_nonadm <- sample(x = rep(0:sim_bins, wl_nonadm[p+1,,t]),
                               size = min(sum(wl_nonadm[p+1,,t]),
                                          cap_nonadm[p,t] - nonrtt_nonadm[p,t] - conv_vol_nonadm[p,t]),
                               replace = FALSE,
                               prob = top_nonadm[p, rep(0:sim_bins, wl_nonadm[p+1,,t])+1])
    # Remove the sample from the waiting list
    wl_nonadm[p+1,,t] <- wl_nonadm[p+1,,t] - tabulate(bin = cs_sample_nonadm+1,
                                                    nbins = sim_bins+1)
    
    # Record the sample in the cs_nonadm array for this period
    cs_nonadm[p,,t] <- tabulate(bin = cs_sample_nonadm+1,
                                nbins = sim_bins+1)    
    
    # * * 3.1.5. Process demand and process wait times - Non-Admitted ----
    # ********************************************************************
    # Right shift the current waiting list by right shifting the bins and summing
    # the last two bins together and add in the new demand to the first bin
    wl_nonadm[p+1,,t] <- c(dem_nonadm[p,t], 
                           wl_nonadm[p+1, 1:sim_bins-1, t],
                           sum(wl_nonadm[p+1, sim_bins:(sim_bins+1), t]))

    # * 3.2. Process Admitted Activity ----
    # *************************************
    
    # * * 3.2.1. Initialise waiting list - Admitted ----
    # **************************************************
    # Initialise the current period waiting list with that of the previous period
    # NB: As this is zero based p+1 is the current period
    wl_adm[p+1,,t] <- wl_adm[p,,t]
    
    # * * 3.2.2. Process ROTT - Admitted ----
    # *******************************************
    # Create ROTT sample from waiting list, and check that we don't sample more than is in the waiting list
    rott_sample_adm <- sample(x = rep(0:sim_bins, wl_adm[p+1,,t]),
                              size = min(sum(wl_adm[p+1,,t]), 
                                         rott_adm[p,t]),
                              replace = FALSE,
                              prob = NULL) # Replace prob with weightings if required
    # Remove the sample from the current waiting list
    # NB: We tabulate the sample with elements incremented by one as tabulate only 
    # deals with postive integers
    wl_adm[p+1,,t] <- wl_adm[p+1,,t] - tabulate(bin = rott_sample_adm+1,
                                              nbins = sim_bins+1)
    
    # * * 3.2.3. Process conversions - Admitted ----
    # **********************************************
    # Model assumes no conversion between admitted and non-admitted pathways
    
    # * * 3.2.4. Process clock stops - Admitted ----
    # **********************************************
    # Create clock stop sample from waiting list based on current period take-off profile
    # The rep used in the x converts from tabulated form into list form
    # The rep used in the prob creates a list of probabilites in the take-off profile 
    # based on the bin represented by x (adding one as the bins are zero based)
    # and check that we don't sample more than is in the waiting list
    cs_sample_adm <- sample(x = rep(0:sim_bins, wl_adm[p+1,,t]),
                            size = min(sum(wl_adm[p+1,,t]),
                                       cap_adm[p,t] - nonrtt_adm[p,t]),
                            replace = FALSE,
                            prob = top_adm[p, rep(0:sim_bins, wl_adm[p+1,,t])+1])
    # Remove the sample from the waiting list
    wl_adm[p+1,,t] <- wl_adm[p+1,,t] - tabulate(bin = cs_sample_adm+1,
                                              nbins = sim_bins+1)
    
    # Record the sample in the cs_adm array for this period
    cs_adm[p,,t] <- tabulate(bin = cs_sample_adm+1,
                            nbins = sim_bins+1)
    
    # * * 3.2.5. Process demand and process wait times - Admitted ----
    # ****************************************************************
    
    # Before processing the wait times add in conversions from non-admitted 
    # to admitted pathways to the current waiting list
    wl_adm[p+1,,t] <- wl_adm[p+1,,t] + conv_nonadm[p,,t]
    
    # Right shift the current waiting list by right shifting the bins and summing
    # the last two bins together and add in the new demand to the first bin
    wl_adm[p+1,,t] <- c(dem_adm[p,t], 
                        wl_adm[p+1, 1:sim_bins-1, t],
                        sum(wl_adm[p+1, sim_bins:(sim_bins+1), t]))
    
    
    
    
  }
}
dtEnd <- Sys.time()
dtEnd - dtStart

# Save the results for use in charting and reporting
save(list = c('sim_name', 
              'sim_trials', 'sim_periods', 'sim_bins',
              'wl_nonadm', 'wl_adm',
              'cs_nonadm', 'cs_adm',
              'dem_nonadm', 'dem_adm',
              'cap_nonadm', 'cap_adm',
              'nonrtt_nonadm', 'nonrtt_adm',
              'rott_nonadm', 'rott_adm',
              'conv_vol_nonadm', 'conv_nonadm'),
     file = paste0(outputdir, '\\', sim_name, '.RObj'))

# Save human readable format of results
write.csv(data.frame(sim_name, sim_trials, sim_periods, sim_bins), paste0(outputdir, '/sim_param.csv'))
write.csv(melt(wl_adm), paste0(outputdir, '/wl_adm.csv'))
write.csv(melt(wl_nonadm), paste0(outputdir, '/wl_nonadm.csv'))
write.csv(melt(cs_adm), paste0(outputdir, '/cs_adm.csv'))
write.csv(melt(cs_nonadm), paste0(outputdir, '/cs_nonadm.csv'))
write.csv(melt(dem_adm), paste0(outputdir, '/dem_adm.csv'))
write.csv(melt(dem_nonadm), paste0(outputdir, '/dem_nonadm.csv'))
write.csv(melt(cap_adm), paste0(outputdir, '//cap_adm.csv'))
write.csv(melt(cap_nonadm), paste0(outputdir, '/cap_nonadm.csv'))
write.csv(melt(nonrtt_adm), paste0(outputdir, '/nonrtt_adm.csv'))
write.csv(melt(nonrtt_nonadm), paste0(outputdir, '/nonrtt_nonadm.csv'))
write.csv(melt(rott_adm), paste0(outputdir, '/rott_adm.csv'))
write.csv(melt(rott_nonadm), paste0(outputdir, '/rott_nonadm.csv'))
write.csv(melt(conv_vol_nonadm), paste0(outputdir, '/conv_vol_nonadm.csv'))
write.csv(melt(conv_nonadm), paste0(outputdir, '/conv_nonadm.csv'))

# Zip and clear temporary files
zip(zipfile = paste0(outputdir, '/', sim_name, '.zip'), 
    files = c(paste0(outputdir, '/sim_param.csv'), 
              paste0(outputdir, '/wl_adm.csv'), paste0(outputdir, '/wl_nonadm.csv'),
              paste0(outputdir, '/cs_adm.csv'), paste0(outputdir, '/cs_nonadm.csv'),
              paste0(outputdir, '/dem_adm.csv'), paste0(outputdir, '/dem_nonadm.csv'),
              paste0(outputdir, '/cap_adm.csv'), paste0(outputdir, '/cap_nonadm.csv'),
              paste0(outputdir, '/nonrtt_adm.csv'), paste0(outputdir, '/nonrtt_nonadm.csv'),
              paste0(outputdir, '/rott_adm.csv'), paste0(outputdir, '/rott_nonadm.csv'),
              paste0(outputdir, '/conv_vol_nonadm.csv'), paste0(outputdir, '/conv_nonadm.csv')),
    extras = '-m')

# Clear all data apart from outputdir and sim_name
# rm(list=ls()[!grepl('sim_name|outputdir', ls())])
