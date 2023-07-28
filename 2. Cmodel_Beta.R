## //////////////////////////////////////////////////
#### 1-BOX MODEL ####
## --------------------------------------------------
onebox <- function( c_influx, tau, cpool0 ) {
  ## ------------------------------------------------
  ## c_influx:  flux into pool (GtC a-1)
  ## cpool0:    initial pool size (GtC)
  ## tau:       turnover (residence) time (a)
  ## ------------------------------------------------
  tauisvec <- ifelse(length(tau)==length(c_influx), TRUE, FALSE)
  
  ## determine integration length (number of time steps) from length of 'c_influx'
  len <- length(c_influx)
  
  ## initialise output variable (time series of pool size)
  out_cpool <- rep( NA, len )
  
  ## copy initial pool size to first element in output time series
  cpool <- cpool0
  
  ## integrate over each time step (this is an implementation of the differential equation)
  for (yr in seq(len) ) {
    
    ## copy current pool size to output time series
    out_cpool[yr] <- cpool
    
    ## update pool size with input and decay
    cpool <- cpool + c_influx[yr] - 1/ifelse(tauisvec, tau[yr], tau) * cpool
    
  }
  
  ## function return value is a vector containing the output time series
  return( out_cpool )
  
}

library(ggplot2)
library(tidyr)
library(gganimate)
library(gifski)

##//////////////////////////////////////////////////
## Pulse - decay
##--------------------------------------------------
nt       <- 400
cpool0    <- 3000
c_influx  <- rep( 0, nt )
c_influx[50] <- 100
tau       <- 50

df <- tibble(year = seq(nt), c_influx = c_influx) %>%
  dplyr::mutate(c_pool = onebox(c_influx = c_influx, tau = 50, cpool0 = 0))

gganim <- df %>%
  pivot_longer(starts_with("c_"), names_to = "var", values_to = "c_") %>%
  ggplot(aes(x = year, y = c_)) +
  geom_line(size = 1, color = "tomato") +
  labs(y = "Carbon (Gt C)", x = "Year") +
  facet_wrap( ~var, nrow = 2, labeller = labeller(var = c("c_influx" = "C influx", "c_pool" = "C pool")) ) +
  theme(strip.text = element_text(face = "bold")) +
  theme_grey(base_size = 20) +
  transition_reveal(year)

animate(gganim, nframes = 100, width = 700, height = 500, fps = 10, renderer = gifski_renderer("~/Desktop/MIT/Classes/R materials/C model/pulse.gif", loop = TRUE))


##//////////////////////////////////////////////////
## Spin up with two turnover rates
##--------------------------------------------------
c_influx  <- rep( 60, nt )

df <- tibble(year = seq(nt), c_influx_tau50 = c_influx, c_influx_tau60 = c_influx) %>%
  dplyr::mutate(c_pool_tau50 = onebox(c_influx = c_influx_tau50, tau = 50, cpool0 = 0),
                c_pool_tau60 = onebox(c_influx = c_influx_tau60, tau = 60, cpool0 = 0))
library(purrr)
library(stringr)
library(dplyr)

gganim <- df %>%
  pivot_longer(starts_with("c_"), names_to = "var", values_to = "c_") %>%
  mutate(tau = purrr::map_chr(var, ~ifelse(str_detect(., pattern = "_tau50$"), "50", "60"))) %>%
  mutate(var = purrr::map_chr(var, ~str_remove(., "_tau50"))) %>%
  mutate(var = purrr::map_chr(var, ~str_remove(., "_tau60"))) %>%
  ggplot(aes(x = year, y = c_, color = tau)) +
  geom_line(size = 1) +
  facet_wrap(~var, nrow = 2, labeller = labeller(var = c("c_influx" = "C influx", "c_pool" = "C pool")), scales = "free") +
  labs(y = "Carbon (Gt C)", x = "Year") +
  theme(strip.text = element_text(face = "bold")) +
  theme_grey(base_size = 20) +
  transition_reveal(year)

animate(gganim, nframes = 100, width = 700, height = 500, fps = 10, renderer = gifski_renderer("~/Desktop/MIT/Classes/R materials/C model/pulse2.gif", loop = TRUE))


##//////////////////////////////////////////////////
## Equilibrium?
##--------------------------------------------------
c_influx  <- rep( 60, nt ) + rnorm(n = nt, mean = 0, sd = 5)
df <- tibble(year = seq(nt), c_influx = c_influx) %>%
  dplyr::mutate(c_pool_tau50 = onebox(c_influx = c_influx, tau = 50, cpool0 = 0))

gganim <- df %>%
  pivot_longer(starts_with("c_"), names_to = "var", values_to = "c_") %>%
  ggplot(aes(x = year, y = c_)) +
  geom_line(size = 1, color = "tomato") +
  facet_wrap(~var, nrow = 2, labeller = labeller(var = c("c_influx" = "C influx", "c_pool" = "C pool")), scales = "free") +
  labs(y = "Carbon (Gt C)", x = "Year") +
  theme(strip.text = element_text(face = "bold")) +
  theme_grey(base_size = 20) +
  transition_reveal(year)

animate(gganim, nframes = 100, width = 700, height = 500, fps = 10, renderer = gifski_renderer("~/Desktop/MIT/Classes/R materials/C model/pulse3.gif", loop = TRUE))


##//////////////////////////////////////////////////
## Step change in input flux
##--------------------------------------------------
c_influx  <- c(rep( 60, 100 ), rep( 80, (nt-100) ))

df <- tibble(year = seq(nt), c_influx_tau50 = c_influx, c_influx_tau60 = c_influx) %>%
  dplyr::mutate(c_pool_tau50 = onebox(c_influx = c_influx_tau50, tau = 50, cpool0 = 3000),
                c_pool_tau60 = onebox(c_influx = c_influx_tau60, tau = 60, cpool0 = 3000*60/50))

gganim <- df %>%
  mutate(c_pool_tau50 = c_pool_tau50 - c_pool_tau50[1], c_pool_tau60 = c_pool_tau60 - c_pool_tau60[1]) %>%
  pivot_longer(starts_with("c_"), names_to = "var", values_to = "c_") %>%
  mutate(tau = purrr::map_chr(var, ~ifelse(str_detect(., pattern = "_tau50$"), "50", "60"))) %>%
  mutate(var = purrr::map_chr(var, ~str_remove(., "_tau50"))) %>%
  mutate(var = purrr::map_chr(var, ~str_remove(., "_tau60"))) %>%
  ggplot(aes(x = year, y = c_, color = tau)) +
  geom_line(size = 1) +
  facet_wrap(~var, nrow = 2, labeller = labeller(var = c("c_influx" = "C influx", "c_pool" = "Increase in C pool")), scales = "free") +
  labs(y = "Carbon (Gt C)", x = "Year") +
  theme(strip.text = element_text(face = "bold")) +
  theme_grey(base_size = 20) +
  transition_reveal(year)

animate(gganim, nframes = 100, width = 700, height = 500, fps = 10, renderer = gifski_renderer("~/Desktop/MIT/Classes/R materials/C model/pulse4.gif", loop = TRUE))


##//////////////////////////////////////////////////
## Step change in turnover time
##--------------------------------------------------
c_influx  <- rep( 60, nt )
vec_tau <-  c(rep( 50, 100 ), rep( 60, (nt-100) ))

df <- tibble(year = seq(nt),
             c_influx = c_influx,
             tau = vec_tau) %>%
  dplyr::mutate(c_pool = onebox(c_influx = c_influx, tau = vec_tau, cpool0 = 3000)) %>%
  dplyr::mutate(c_uptake = c_pool - lag(c_pool, 1))

gganim <- df %>%
  pivot_longer(c(tau, c_pool, c_uptake), names_to = "var", values_to = "c_") %>%
  mutate(var = factor(var, levels = c("tau", "c_pool", "c_uptake"))) %>%
  ggplot(aes(x = year, y = c_)) +
  geom_line(size = 1, color = "tomato") +
  facet_wrap(~var, nrow = 3, labeller = labeller(var = c("tau" = "tau (yr)", "c_pool" = "C pool (Gt C)", "c_uptake" = "C uptake (Gt C/yr)")), scales = "free") +
  labs(y = "", x = "Year") +
  theme(strip.text = element_text(face = "bold")) +
  theme_grey(base_size = 20) +
  transition_reveal(year)

animate(gganim, nframes = 100, width = 700, height = 500, fps = 10, renderer = gifski_renderer("~/Desktop/MIT/Classes/R materials/C model/pulse5.gif", loop = TRUE))



## execute function 'onebox' with above determined arguments
## and store output time series (function return value) to 'out1'
out1 <- onebox( c_influx, cpool0, tau )

## do the same but now with increased turnover time, store
## function return values to 'out2'
tau  <- 60
out2 <- onebox( c_influx, cpool0, tau )

####Exercises####

#1. Prescribe a pulse emission of 1000 GtC in year 100. 
# Plot the perturbation of atmospheric CO2 over time.

#2. Prescribe a step change in emissions: 
# from 0 GtC/yr before year 100 to 10 GtC/yr thereafter.

#3. Prescribe linearly increasing emissions (from 0 GtC/yr in year 1 to 20 GtC/yr in year 1000). 
# Plot atmospheric CO2 over time.

#4. Discuss the caveats of the 1-Box model.

#### 2-Box Model Atmosphere-Ocean ####

# Set parameters
lsim <- 1000 # length of simulation
# A_oc <- 3.6e+14
k_ex <- 60/270

buffer <- 100
gt2ppm <- 2.123   # conversion factor from GtC to ppmv

# 1000 GtC CO2 are emitted in year 100 into the atmosphere. 
# Plot the evolution of the atmospheric perturbation
# (in units of ppm) over the course of 1000 years.

eC <- array(0,c(lsim))

pC_a_out <- array(NA,c(lsim)) # This is the output variable for atmospheric CO2
pC_o_out <- array(NA,c(lsim)) # This is the output variable for oceanic CO2

# Initialize variables
pC_a <- 270
pC_o_init <- 270
C_o_init<- 5000 # initial ocean C-pool, in GtC
C_a  <- 270*gt2ppm
C_o <- C_o_init
pC_o <- pC_o_init

for (yr in seq(lsim)) {
  
  # Add emissions to atmosphere
  C_a <- C_a + eC[yr]
  
  # Update atmospheric CO2 partial pressure
  pC_a <- C_a/gt2ppm
  
  # Flux atmosphere -> ocean 
  f_ao <- k_ex*(pC_a - pC_o)
  
  # Update inventories
  C_a <- C_a - f_ao
  C_o <- C_o + f_ao
  
  # Update oceanic CO2 partial pressure. The partial 
  # pressure increase is scaled by the buffer factor
  pC_o <- pC_o_init + buffer*(C_o - C_o_init)/C_o_init*pC_o_init
  
  # Copy atmospheric C inventory of this year to output
  pC_a_out[yr] <- pC_a
  pC_o_out[yr] <- pC_o
  
}

par(mfcol=c(1,2))
plot(pC_a_out, main="Atm")
plot(pC_o_out, main= "Ocean")

#### Exercices ####
# Simulate and explain what happens to the system atm-ocean when there is a pulse emission of 200 GtC
# in year 100 in the atmosphere

# ----------------------------------------------------------------

# Simulate and explain what happens to the system atm-ocean when there is a step change in
# emissions (from 0 to 10 GtC/yr after yr 200)

# ----------------------------------------------------------------

# Simulate and explain what happens to our system when whe start with different 
# levels of C in our boxes

# ----------------------------------------------------------------
# Basic knowledge question: How is this related with the acidification
# of the oceans?

