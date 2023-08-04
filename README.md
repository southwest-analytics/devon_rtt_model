# Devon Referral to Treatment (RTT) Model in R

### Author: Richard Blackwell
### Email: richard.blackwell@swahsn.com
### Date: 2023-08-04

## Variables

Note: Variables ending in `nonadm` refer to the **non-admitted pathway** and those ending in `adm` refer to the **admitted pathway**. A variable displayed as `variable_name_...` in this README signifies that both the `variable_name_nonadm` and `variable_name_adm` variables are being referred to

### Simulation variables

- `sim_name` - Name of the simulation for use in output directory and filenames
- `sim_trials` - Number of simulation runs
- `sim_periods` - Number of time periods each simulation will run for 
- `sim_bins` - Number of bins to use for the waiting list i.e. `sim_bins = 52` will monitor waits from >=0 weeks and <1 week through to >=52 weeks **NB: Bin zero will always be added so we do not need to add that into the number required**
- `sim_time_unit` - Unit of time for the model i.e. year, month, week, day 

**NB: the same unit of time will be used across the entire model, i.e. if we use week then the demand is expressed in weekly demand, the capacity in weekly capacity, the waiting list bins as weekly bins**

### Waiting list variables
- `wl_type_nonadm` & `wl_type_adm` - Type of waiting list input, either `matrix` (read direct from input file) or `dist` (created from beta-binomial distribution using parameters supplied in input file).
- `wl_size_nonadm` & `wl_size_adm` - If the `wl_type_...` is `dist` this is size of the waiting list to be created using the beta-binomial distribution.
- `wl_shape1_nonadm` & `wl_shape1_adm` - If the `wl_type_...` is `dist` this is shape parameter 1 (also know as $\alpha$ ) used to define the shape of the beta-binomial distribution.
- `wl_shape2_nonadm` & `wl_shape2_adm` - If the `wl_type_...` is `dist` this is shape parameter 2 (also know as $\beta$ ) used to define the shape of the beta-binomial distribution.
- `wl_nonadm` & `wl_adm` - Arrays of dimensions `[sim_periods+1, sim_bins+1, sim_trails]` in which to store the by bin starting waiting list i.e. period 0, and all simulations of the waiting list for each simulation trial and simulation period.

### Take-off profile variables
- `top_type_nonadm` & `top_type_adm` - Type of take-off profile input, either `matrix` (read direct from input file) or `dist` (created from beta-binomial distribution using parameters supplied in input file)
- `top_shape1_nonadm` & `top_shape1_adm` - If the `top_type_...` is `dist` this is shape parameter 1 (also know as $\alpha$ ) used to define the shape of the beta-binomial distribution
- `top_shape2_nonadm` & `top_shape2_adm` - If the `top_type_...` is `dist` this is shape parameter 2 (also know as $\beta$ ) used to define the shape of the beta-binomial distribution
- `top_nonadm` & `top_adm` - Arrays of dimensions `[sim_periods, sim_bins+1]` in which to store the take-off profiles by bin for each simulation period.

### Clock stop variables
- `cs_nonadm` & `cs_adm` - Arrays of dimensions `[sim_periods, sim_bins+1, sim_trails]` in which to store the clock stops by bin for each simulation trial and simulation period.

### Demand variables
- `dem_param_nonadm` & `dem_param_adm` - Arrays of dimensions `[sim_periods, 2]` in which to store the mean and standard deviations parameters for each simulation period used to create the `dem_vol_...` from a normal distribution
- `dem_nonadm` & `dem_adm` - Arrays of dimensions `[sim_periods, sim_trials]` used to store the demand volume for the non-admitted and admitted pathways for each period and simulation run

>### Demand variables \[DEVELOPMENT\]
> - `dem_param_nonadm` & `dem_param_adm` - Arrays of dimensions `[sim_periods, 2]` in which to store the mean and standard deviations parameters for each simulation period used to create the `dem_vol_...` from a normal distribution
> - `dem_nonadm` & `dem_adm` - Arrays of dimensions `[sim_periods, sim_trials]` used to store the demand volume for the non-admitted and admitted pathways for each period and simulation run


```mermaid
graph TD;
  A-->B;
  A-->C;
  B-->D;
  C-->D;
```
