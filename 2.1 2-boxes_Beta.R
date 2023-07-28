# 2-Box Model Atmosphere-Ocean

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

par(mfrow=c(1,2))
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

