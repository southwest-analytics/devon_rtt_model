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
library(reshape2)

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
  arr <- arr / apply(arr, 1, sum)
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

# Function to write a 2d (volume) array as a csv file
fnWrite2dArray <- function(array, filename){
  df <- melt(array) %>% 
    rename_with(.fn = ~c('period','trial','volume'))
  write.csv(df, filename, row.names = FALSE)
}

# Function to write a 3d (profile) array as a csv file
fnWrite3dArray <- function(array, filename, bool_wl_special_case = FALSE){
  df <- melt(array) %>% 
    rename_with(.fn = ~c('period','bin','trial','volume')) %>% 
    mutate(bin = bin - 1)
  if(bool_wl_special_case){
    df <- df %>% mutate(period = period - 1)
  }
  write.csv(df, filename, row.names = FALSE)
}


# 1. Initialise Variables ----
# ****************************

# [DEVELOPMENT: This ought to be changed to command line argument input to aid automation for trusts]
inputfile <- './input/RK9_C110.xlsx'

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

# 2. Populate Time Independent Variables ----
# *******************************************
# * 2.1. Profile Variables ----
# `````````````````````````````

# * * 2.1.1. Clock Stops ----
# ```````````````````````````
# Create the clock stop profiles from either actual or synthesised distributions

# * * * 2.1.1.1. Non-Admitted ----
if(df_param$value[df_param$variable=='csprof_type_nonadm']=='actual'){
  # Read in from an actual distribution
  csprof_nonadm <- fnCreateProfileFromActualDistribution(profile_type_var = 'csprof_type_nonadm')
} else if(df_param$value[df_param$variable=='csprof_type_nonadm']=='synthetic'){
  # Create from a synthetic distribution
  csprof_nonadm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'csprof_type_nonadm')
}

# * * * 2.1.1.2. Admitted ----
# Create the clock stop profile from either an actual or synthesised distribution or a synthetic
if(df_param$value[df_param$variable=='csprof_type_adm']=='actual'){
  # Read in from an actual distribution
  csprof_adm <- fnCreateProfileFromActualDistribution(profile_type_var = 'csprof_type_adm')
} else if(df_param$value[df_param$variable=='csprof_type_adm']=='synthetic'){
  # Create from a synthetic distribution
  csprof_adm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'csprof_type_adm')
}

# * * 2.1.2. Demand ----
# ``````````````````````
# Create the demand profiles from either actual or synthesised distributions

# * * * 2.1.2.1. Non-Admitted ----
if(df_param$value[df_param$variable=='demprof_type_nonadm']=='actual'){
  # Read in from an actual distribution
  demprof_nonadm <- fnCreateProfileFromActualDistribution(profile_type_var = 'demprof_type_nonadm')
} else if(df_param$value[df_param$variable=='demprof_type_nonadm']=='synthetic'){
  # Create from a synthetic distribution
  demprof_nonadm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'demprof_type_nonadm')
}

# * * * 2.1.2.2. Admitted ----
if(df_param$value[df_param$variable=='demprof_type_adm']=='actual'){
  # Read in from an actual distribution
  demprof_adm <- fnCreateProfileFromActualDistribution(profile_type_var = 'demprof_type_adm')
} else if(df_param$value[df_param$variable=='demprof_type_adm']=='synthetic'){
  # Create from a synthetic distribution
  demprof_adm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'demprof_type_adm')
}

# * * 2.1.3. ROTT ----
# ``````````````````````
# Create the ROTT profiles from either actual or synthesised distributions

# * * * 2.1.3.1. Non-Admitted ----
if(df_param$value[df_param$variable=='rottprof_type_nonadm']=='actual'){
  # Read in from an actual distribution
  rottprof_nonadm <- fnCreateProfileFromActualDistribution(profile_type_var = 'rottprof_type_nonadm')
} else if(df_param$value[df_param$variable=='rottprof_type_nonadm']=='synthetic'){
  # Create from a synthetic distribution
  rottprof_nonadm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'rottprof_type_nonadm')
}

# * * * 2.1.3.2. Admitted ----
if(df_param$value[df_param$variable=='rottprof_type_adm']=='actual'){
  # Read in from an actual distribution
  rottprof_adm <- fnCreateProfileFromActualDistribution(profile_type_var = 'rottprof_type_adm')
} else if(df_param$value[df_param$variable=='rottprof_type_adm']=='synthetic'){
  # Create from a synthetic distribution
  rottprof_adm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'rottprof_type_adm')
}

# * * 2.1.4. Conversion ----
# ``````````````````````````
# Create the conversion profiles from either actual or synthesised distributions

# * * * 2.1.4.1. Non-Admitted ----
if(df_param$value[df_param$variable=='convprof_type_nonadm']=='actual'){
  # Read in from an actual distribution
  convprof_nonadm <- fnCreateProfileFromActualDistribution(profile_type_var = 'convprof_type_nonadm')
} else if(df_param$value[df_param$variable=='convprof_type_nonadm']=='synthetic'){
  # Create from a synthetic distribution
  convprof_nonadm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'convprof_type_nonadm')
}

# * * * 2.1.4.2. Admitted ----
if(df_param$value[df_param$variable=='convprof_type_adm']=='actual'){
  # Read in from an actual distribution
  convprof_adm <- fnCreateProfileFromActualDistribution(profile_type_var = 'convprof_type_adm')
} else if(df_param$value[df_param$variable=='convprof_type_adm']=='synthetic'){
  # Create from a synthetic distribution
  convprof_adm <- fnCreateProfileFromSyntheticDistribution(profile_type_var = 'convprof_type_adm')
}

# * 2.2. Process Variables ----
# `````````````````````````````

# * * 2.2.1. Waiting List ----
# ````````````````````````````
# Create the initial (period=0) waiting list profiles from either actual or synthesised distributions
# the rest of the waiting list is time dependent and will calculated in the period iteration

# * * * 2.2.1.1. Non-Admitted ----
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

# * * 2.2.2. Clock Stops ----
# ```````````````````````````
# Clock stops are time dependent so nothing to calculate here

# * * 2.2.3. Demand ----
# ``````````````````````
# Populate the demand parameter and volume arrays from the input

# * * * 2.2.3.1. Non-Admitted ----
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

# Ensure that there are no negative demand entries and if there are set to zero
dem_vol_nonadm[dem_vol_nonadm<0] <- 0

# Create the actual demand by bin, period and trial
res <- lapply(X = 1:sim_trials,
       FUN = function(t){
         lapply(X = 1:sim_periods,
                FUN = function(p, t){
                  tabulate(
                    bin = sample(x = 1:(sim_bins+1),
                                 size = dem_vol_nonadm[p, t],
                                 replace = TRUE,
                                 prob = demprof_nonadm[p, ]),
                    nbins = sim_bins+1)
                },
                t
          )
       })

# Unlist the results from the lapply
res <- unlist(res)
# Note dimensions are deliberately in the incorrect order (bins, periods, trials) rather than
# (periods, bins, trials) due to the way we have to unlist then reshape
dim(res) <- c(sim_bins+1, sim_periods, sim_trials)
# Reshape to the correct dimension order (periods, bins, trials)
dem_nonadm[,,] <- aperm(res, c(2, 1, 3))

# * * * 2.2.3.2. Admitted ----
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

# Ensure that there are no negative demand entries and if there are set to zero
dem_vol_adm[dem_vol_adm<0] <- 0

# Create the actual demand by bin, period and trial
res <- lapply(X = 1:sim_trials,
              FUN = function(t){
                lapply(X = 1:sim_periods,
                       FUN = function(p, t){
                         tabulate(
                           bin = sample(x = 1:(sim_bins+1),
                                        size = dem_vol_adm[p, t],
                                        replace = TRUE,
                                        prob = demprof_adm[p, ]),
                           nbins = sim_bins+1)
                       },
                       t
                )
              })

# Unlist the results from the lapply
res <- unlist(res)
# Note dimensions are deliberately in the incorrect order (bins, periods, trials) rather than
# (periods, bins, trials) due to the way we have to unlist then reshape
dim(res) <- c(sim_bins+1, sim_periods, sim_trials)
# Reshape to the correct dimension order (periods, bins, trials)
dem_adm[,,] <- aperm(res, c(2, 1, 3))


# * * 2.2.4. Capacity ----
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

# Ensure that there are no negative capacity entries and if there are set to zero
cap_vol_nonadm[cap_vol_nonadm<0] <- 0

# * * * 2.2.4.1. Admitted ----
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

# Ensure that there are no negative capacity entries and if there are set to zero
cap_vol_adm[cap_vol_adm<0] <- 0

# * * 2.2.5. Non-RTT ----
# ```````````````````````
# Populate the non-RTT parameter and volume arrays from the input

# * * * 2.2.5.1. Non-Admitted ----
# Create a data frame of period from, period to, and probability
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='nonrtt_prob_nonadm'],
                         prob = df_param_synthetic$value[df_param_synthetic$variable=='nonrtt_prob_nonadm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_prob <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['prob'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
nonrtt_param_nonadm <- v_prob

# Create the volume array for all the trials and periods
# Shouldn't need any error checking here as we have already ensured that cap_vol_nonadm is not negative
nonrtt_vol_nonadm[,] <- rbinom(n = sim_periods*sim_trials, 
                            size = cap_vol_nonadm, 
                            prob = nonrtt_param_nonadm)

# * * * 2.2.5.2. Admitted ----
# Create a data frame of period from, period to, and probability
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='nonrtt_prob_adm'],
                         prob = df_param_synthetic$value[df_param_synthetic$variable=='nonrtt_prob_adm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_prob <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['prob'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
nonrtt_param_adm <- v_prob

# Create the volume array for all the trials and periods
# Shouldn't need any error checking here as we have already ensured that cap_vol_nonadm is not negative
nonrtt_vol_adm[,] <- rbinom(n = sim_periods*sim_trials, 
                            size = cap_vol_adm, 
                            prob = nonrtt_param_adm)

# * * 2.2.6. ROTT ----
# Populate the ROTT parameter and volume arrays from the input, 
# the ROTT results are time dependent so nothing to calculate here

# * * * 2.2.6.1. Non-Admitted ----
# Create a data frame of period from, period to, and probability
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='rott_prob_nonadm'],
                         prob = df_param_synthetic$value[df_param_synthetic$variable=='rott_prob_nonadm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_prob <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['prob'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
rott_param_nonadm <- v_prob

# Create the volume array for all the trials and periods
# Shouldn't need any error checking here as we have already ensured that cap_vol_nonadm is not negative
rott_vol_nonadm[,] <- rbinom(n = sim_periods*sim_trials, 
                             size = cap_vol_nonadm, 
                             prob = rott_param_nonadm)

# * * * 2.2.6.2. Admitted ----
# Create a data frame of period from, period to, and probability
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='rott_prob_adm'],
                         prob = df_param_synthetic$value[df_param_synthetic$variable=='rott_prob_adm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_prob <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['prob'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
rott_param_adm <- v_prob

# Create the volume array for all the trials and periods
# Shouldn't need any error checking here as we have already ensured that cap_vol_nonadm is not negative
rott_vol_adm[,] <- rbinom(n = sim_periods*sim_trials, 
                          size = cap_vol_adm, 
                          prob = rott_param_adm)

# * * 2.2.7. Conversions ----
# Populate the conversion parameter and volume arrays from the input, 
# the ROTT results are time dependent so nothing to calculate here

# * * * 2.2.7.1. Non-Admitted ----
# Create a data frame of period from, period to, and probability
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='conv_prob_nonadm'],
                         prob = df_param_synthetic$value[df_param_synthetic$variable=='conv_prob_nonadm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_prob <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['prob'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
conv_param_nonadm <- v_prob

# Create the volume array for all the trials and periods
# Shouldn't need any error checking here as we have already ensured that cap_vol_nonadm is not negative
conv_vol_nonadm[,] <- rbinom(n = sim_periods*sim_trials, 
                             size = cap_vol_nonadm, 
                             prob = conv_param_nonadm)

# * * * 2.2.7.2. Admitted ----
# Create a data frame of period from, period to, and probability
df_periods <- data.frame(from = df_param_synthetic$period[df_param_synthetic$variable=='conv_prob_adm'],
                         prob = df_param_synthetic$value[df_param_synthetic$variable=='conv_prob_adm']) %>%
  mutate(to = lead(from, default = (sim_periods+1))-1, .after = 'from')

# Replicate the entries to create the parameter array columns
v_prob <- do.call('c',list(apply(X = df_periods, MARGIN = 1, function(X){rep(X['prob'], X['to']-X['from']+1)})))
# Insert the columns into the blank parameter array
conv_param_adm <- v_prob

# Create the volume array for all the trials and periods
# Shouldn't need any error checking here as we have already ensured that cap_vol_nonadm is not negative
conv_vol_adm[,] <- rbinom(n = sim_periods*sim_trials, 
                          size = cap_vol_adm, 
                          prob = conv_param_adm)

# 3. Process simulation run (trial) ----
# **************************************
for(t in 1:sim_trials){

  
  # 4. Process simulation periods ----
  # **********************************
  for(p in 1:sim_periods)
  {
    # NB: When referring to the waiting list arrays we must remember that the 
    # one based array includes an entry for period zero and as such we need to 
    # refer to p+1 instead of p but only for the waiting list arrays. For all 
    # other we will refer to p as p.
    # * 4.1. Initialise Waiting List ----
    # ```````````````````````````````````
    # Set the current period waiting list to be that of the previous period
    # * * 4.1.1. Non-Admitted ----
    wl_nonadm[p+1,,t] <- wl_nonadm[p,,t]
    # * * 4.1.2. Admitted ----
    wl_adm[p+1,,t] <- wl_adm[p,,t]
    
    # * 4.2. Calculate Clock Stopping Capacity ----
    # `````````````````````````````````````````````
    # Remove the non-RTT activity from the capacity to leave the clock stopping activity
    # * * 4.2.1. Non-Admitted ----
    cs_vol_nonadm <- cap_vol_nonadm[p,t] - nonrtt_vol_nonadm[p,t]
    # Check for negative activity and reset to zero if negative
    cs_vol_nonadm[cs_vol_nonadm<0] <- 0
    
    # * * 4.2.2. Admitted ----
    cs_vol_adm <- cap_vol_adm[p,t] - nonrtt_vol_adm[p,t]
    # Check for negative activity and reset to zero if negative
    cs_vol_adm[cs_vol_adm<0] <- 0
    
    # * 4.3. Process ROTT ----
    # ````````````````````````

    # * * 4.3.1. Non-Admitted ----

    # Get the ROTT and waiting list volumes for this period and trial (NB: p+1 for wl)
    # If the ROTT volume is greater than the waiting list (shouldn't really happen)
    # then set the ROTT volume to be the size of the waiting list
    rott_vol_nonadm[p,t] <- min(rott_vol_nonadm[p,t], sum(wl_nonadm[p+1,,t]))
    # Check to see that the waiting list is not empty or not of size 1 due 
    # following error if waiting list size is one.
    # (Error in sample.int(x, size, replace, prob) : incorrect number of probabilities
    if(sum(wl_nonadm[p+1,,t])>1){
      # Sample the waiting list using the ROTT profile
      rott_nonadm[p,,t] <- tabulate(bin = sample(x = rep(0:sim_bins, wl_nonadm[p+1,,t]),
                                                 size = rott_vol_nonadm[p,t],
                                                 replace = FALSE,
                                                 prob = rep(rottprof_nonadm[p,], wl_nonadm[p+1,,t]))+1,
                                    nbins = sim_bins+1)
    } else {
      # Otherwise the sample can only be the current waiting list
      rott_nonadm[p,,t] <- wl_nonadm[p+1,,t]
    }
      
    # Remove the ROTT sample from the waiting list
    wl_nonadm[p+1,,t] <- wl_nonadm[p+1,,t] - rott_nonadm[p,,t]
    
    # * * 4.3.2. Admitted ----
    
    # Get the ROTT and waiting list volumes for this period and trial (NB: p+1 for wl)
    # If the ROTT volume is greater than the waiting list (shouldn't really happen)
    # then set the ROTT volume to be the size of the waiting list
    rott_vol_adm[p,t] <- min(rott_vol_adm[p,t], sum(wl_adm[p+1,,t]))
    # Check to see that the waiting list is not empty or not of size 1 due 
    # following error if waiting list size is one.
    # (Error in sample.int(x, size, replace, prob) : incorrect number of probabilities
    if(sum(wl_adm[p+1,,t])>1){
      # Sample the waiting list using the ROTT profile
      rott_adm[p,,t] <- tabulate(bin = sample(x = rep(0:sim_bins, wl_adm[p+1,,t]),
                                              size = rott_vol_adm[p,t],
                                              replace = FALSE,
                                              prob = rep(rottprof_adm[p,], wl_adm[p+1,,t]))+1,
                                 nbins = sim_bins+1)
    } else {
      # Otherwise the sample can only be the current waiting list
      rott_adm[p,,t] <- wl_adm[p+1,,t]
    }
    # Remove the ROTT sample from the waiting list
    wl_adm[p+1,,t] <- wl_adm[p+1,,t] - rott_adm[p,,t]
    
    # * 4.4. Remove Conversions ----
    # ``````````````````````````````
    # * * 4.4.1. Non-Admitted ----

    # Get the conversion and waiting list volumes for this period and trial (NB: p+1 for wl)
    # If the conversion volume is greater than the waiting list (shouldn't really happen)
    # then set the conversion volume to be the size of the waiting list
    conv_vol_nonadm[p,t] <- min(conv_vol_nonadm[p,t], sum(wl_nonadm[p+1,,t]))
    # Check to see that the waiting list is not empty or not of size 1 due 
    # following error if waiting list size is one.
    # (Error in sample.int(x, size, replace, prob) : incorrect number of probabilities
    if(sum(wl_nonadm[p+1,,t])>1){
      # Sample the waiting list using the conversion profile
      conv_nonadm[p,,t] <- tabulate(bin = sample(x = rep(0:sim_bins, wl_nonadm[p+1,,t]),
                                              size = conv_vol_nonadm[p,t],
                                              replace = FALSE,
                                              prob = rep(convprof_nonadm[p,], wl_nonadm[p+1,,t]))+1,
                                 nbins = sim_bins+1)
    } else {
      # Otherwise the sample can only be the current waiting list
      conv_nonadm[p,,t] <- wl_nonadm[p+1,,t]
    }
    # Remove the conversion sample from the waiting list
    wl_nonadm[p+1,,t] <- wl_nonadm[p+1,,t] - conv_nonadm[p,,t]

    # * * 4.4.2. Admitted ----

    # Get the conversion and waiting list volumes for this period and trial (NB: p+1 for wl)
    # If the conversion volume is greater than the waiting list (shouldn't really happen)
    # then set the conversion volume to be the size of the waiting list
    conv_vol_adm[p,t] <- min(conv_vol_adm[p,t], sum(wl_adm[p+1,,t]))
    # Check to see that the waiting list is not empty or not of size 1 due 
    # following error if waiting list size is one.
    # (Error in sample.int(x, size, replace, prob) : incorrect number of probabilities
    if(sum(wl_adm[p+1,,t])>1){
      # Sample the waiting list using the conversion profile
      conv_adm[p,,t] <- tabulate(bin = sample(x = rep(0:sim_bins, wl_adm[p+1,,t]),
                                                 size = conv_vol_adm[p,t],
                                                 replace = FALSE,
                                                 prob = rep(convprof_adm[p,], wl_adm[p+1,,t]))+1,
                                    nbins = sim_bins+1)
    } else {
      # Otherwise the sample can only be the current waiting list
      conv_adm[p,,t] <- wl_adm[p+1,,t]
    }
    # Remove the conversion sample from the waiting list
    wl_adm[p+1,,t] <- wl_adm[p+1,,t] - conv_adm[p,,t]

    # * 4.5. Remove Clock Stops ----
    # ``````````````````````````````
    # * * 4.5.1. Non-Admitted ----
    # Get the clock stop and waiting list volumes for this period and trial (NB: p+1 for wl)
    # If the clock stop volume is greater than the waiting list (shouldn't really happen)
    # then set the clock stop volume to be the size of the waiting list
    cs_vol_nonadm <- min(cs_vol_nonadm, sum(wl_nonadm[p+1,,t]))
    # Check to see that the waiting list is not empty or not of size 1 due 
    # following error if waiting list size is one.
    # (Error in sample.int(x, size, replace, prob) : incorrect number of probabilities
    if(sum(wl_nonadm[p+1,,t])>1){
      # Sample the waiting list using the clock stop profile
      cs_nonadm[p,,t] <- tabulate(bin = sample(x = rep(0:sim_bins, wl_nonadm[p+1,,t]),
                                               size = cs_vol_nonadm,
                                               replace = FALSE,
                                               prob = rep(csprof_nonadm[p,], wl_nonadm[p+1,,t]))+1,
                                  nbins = sim_bins+1)
    } else {
      # Otherwise the sample can only be the current waiting list
      cs_nonadm[p,,t] <- wl_nonadm[p+1,,t]
    }
    # Remove the conversion sample from the waiting list
    wl_nonadm[p+1,,t] <- wl_nonadm[p+1,,t] - cs_nonadm[p,,t]
    
    # * * 4.5.2. Admitted ----
    # Get the clock stop and waiting list volumes for this period and trial (NB: p+1 for wl)
    # If the clock stop volume is greater than the waiting list (shouldn't really happen)
    # then set the clock stop volume to be the size of the waiting list
    cs_vol_adm <- min(cs_vol_adm, sum(wl_adm[p+1,,t]))
    # Check to see that the waiting list is not empty or not of size 1 due 
    # following error if waiting list size is one.
    # (Error in sample.int(x, size, replace, prob) : incorrect number of probabilities
    if(sum(wl_adm[p+1,,t])>1){
      # Sample the waiting list using the clock stop profile
      cs_adm[p,,t] <- tabulate(bin = sample(x = rep(0:sim_bins, wl_adm[p+1,,t]),
                                            size = cs_vol_adm,
                                            replace = FALSE,
                                            prob = rep(csprof_adm[p,], wl_adm[p+1,,t]))+1,
                               nbins = sim_bins+1)
    } else {
      # Otherwise the sample can only be the current waiting list
      cs_adm[p,,t] <- wl_adm[p+1,,t]
    }
    # Remove the conversion sample from the waiting list
    wl_adm[p+1,,t] <- wl_adm[p+1,,t] - cs_adm[p,,t]
    
    # * 4.6. Add in Conversions ----
    # ``````````````````````````````

    # * * 4.6.1. Non-Admitted ----
    # Add admitted pathways converting into non-admitted pathways
    wl_nonadm[p+1,,t] <- wl_nonadm[p+1,,t] + conv_adm[p,,t]

    # * * 4.6.2. Admitted ----
    # Add non-admitted pathways converting into admitted pathways
    wl_adm[p+1,,t] <- wl_adm[p+1,,t] + conv_nonadm[p,,t]
    
    # * 4.7. Increment Period ----
    # ````````````````````````````
    # Move both waiting lists forward by one period (i.e. bin 0 becomes bin 1)
    
    # * * 4.7.1. Non-Admitted ----
    # Calculate the penultimate bin as the sum of the final and penultimate bin
    wl_nonadm[p+1,sim_bins,t] <- sum(wl_nonadm[p+1,sim_bins:(sim_bins+1),t])
    # Shift everthing right by one period
    wl_nonadm[p+1,2:(sim_bins+1),t] <- wl_nonadm[p+1,1:sim_bins,t]
    # Set the first bin as zero
    wl_nonadm[p+1,1,t] <- 0

    # * * 4.7.2. Admitted ----
    # Calculate the penultimate bin as the sum of the final and penultimate bin
    wl_adm[p+1,sim_bins,t] <- sum(wl_adm[p+1,sim_bins:(sim_bins+1),t])
    # Shift everthing right by one period
    wl_adm[p+1,2:(sim_bins+1),t] <- wl_adm[p+1,1:sim_bins,t]
    # Set the first bin as zero
    wl_adm[p+1,1,t] <- 0
    
    # * 4.8. Add in Demand ----
    # `````````````````````````
    # * * 4.8.1. Non-Admitted ----
    # Check to see that the waiting list is not empty
    if(dem_vol_nonadm[p,t]>0){
      dem_nonadm[p,,t] <- tabulate(bin = rbetabinom(n = dem_vol_nonadm[p,t],
                                                    size = sim_bins+1,
                                                    prob = demprof_nonadm[p,])+1,
                                   nbins = sim_bins+1)
    }
    
    # Add the demand sample into the waiting list
    wl_nonadm[p+1,,t] <- wl_nonadm[p+1,,t] + dem_nonadm[p,,t]

    # * * 4.8.2. Admitted ----
    # Check to see that the waiting list is not empty
    if(dem_vol_adm[p,t]>0){
      dem_adm[p,,t] <- tabulate(bin = rbetabinom(n = dem_vol_adm[p,t],
                                               size = sim_bins+1,
                                               prob = demprof_adm[p,])+1,
                                nbins = sim_bins+1)
    }
    
    # Add the demand sample into the waiting list
    wl_adm[p+1,,t] <- wl_adm[p+1,,t] + dem_adm[p,,t]
  }
}

# 5. Save the simulation results ----
# ***********************************

# Create object list
obj_list <- c('sim_name', 'sim_bins', 'sim_periods', 'sim_trials',
              'wl_adm', 'wl_nonadm',
              'cs_adm', 'cs_nonadm',
              'cap_vol_adm', 'cap_vol_nonadm',
              'nonrtt_vol_adm', 'nonrtt_vol_nonadm',
              'dem_adm', 'dem_nonadm', 'dem_vol_adm', 'dem_vol_nonadm',
              'conv_adm', 'conv_nonadm', 'conv_vol_adm', 'conv_vol_nonadm',
              'rott_adm', 'rott_nonadm', 'rott_vol_adm', 'rott_vol_nonadm')

# Save human readable format of results
write.csv(data.frame(sim_name, sim_trials, sim_periods, sim_bins), paste0(outputdir, '/sim_param.csv'))
fnWrite3dArray(wl_adm, paste0(outputdir, '/wl_adm.csv'), TRUE)
fnWrite3dArray(wl_nonadm, paste0(outputdir, '/wl_nonadm.csv'), TRUE)
fnWrite3dArray(cs_adm, paste0(outputdir, '/cs_adm.csv'))
fnWrite3dArray(cs_nonadm, paste0(outputdir, '/cs_nonadm.csv'))
fnWrite3dArray(dem_adm, paste0(outputdir, '/dem_adm.csv'))
fnWrite3dArray(dem_nonadm, paste0(outputdir, '/dem_nonadm.csv'))
fnWrite3dArray(conv_adm, paste0(outputdir, '/conv_adm.csv'))
fnWrite3dArray(conv_nonadm, paste0(outputdir, '/conv_nonadm.csv'))
fnWrite3dArray(rott_adm, paste0(outputdir, '/rott_adm.csv'))
fnWrite3dArray(rott_nonadm, paste0(outputdir, '/rott_nonadm.csv'))
fnWrite2dArray(cap_vol_adm, paste0(outputdir, '//cap_vol_adm.csv'))
fnWrite2dArray(cap_vol_nonadm, paste0(outputdir, '/cap_vol_nonadm.csv'))
fnWrite2dArray(nonrtt_vol_adm, paste0(outputdir, '/nonrtt_vol_adm.csv'))
fnWrite2dArray(nonrtt_vol_nonadm, paste0(outputdir, '/nonrtt_vol_nonadm.csv'))
fnWrite2dArray(dem_vol_adm, paste0(outputdir, '/dem_vol_adm.csv'))
fnWrite2dArray(dem_vol_nonadm, paste0(outputdir, '/dem_vol_nonadm.csv'))
fnWrite2dArray(conv_vol_adm, paste0(outputdir, '/conv_vol_adm.csv'))
fnWrite2dArray(conv_vol_nonadm, paste0(outputdir, '/conv_vol_nonadm.csv'))
fnWrite2dArray(rott_vol_adm, paste0(outputdir, '/rott_vol_adm.csv'))
fnWrite2dArray(rott_vol_nonadm,paste0(outputdir, '/rott_vol_nonadm.csv'))

# Zip and clear temporary files
zip(zipfile = paste0(outputdir, '/', sim_name, '.zip'), 
    files = list.files(outputdir, full.names = TRUE),
    extras = '-Djm')

# Save the results as a machine readable RObj
save(list = obj_list,
     file = paste0(outputdir, '\\', sim_name, '.RObj'))


# Clear all data apart from outputdir and sim_name
# rm(list=ls()[!grepl('sim_name|outputdir', ls())])
