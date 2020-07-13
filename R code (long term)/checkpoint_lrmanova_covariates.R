
################################################################################
## IMPORTANT NOTE: 
## Your working directory must be set to the folder containing this script file!
## Use setwd() to set your working directory.
################################################################################


# 1. use the checkpoint package -------------------------------------------

## use the checkpoint package to make sure loaded libraries are compatible
## with this code

library(checkpoint)
checkpoint("2020-06-30", r_version = "3.6.3")


# 2. load required packages -----------------------------------------------

library(openxlsx)
library(lavaan)
library(tidyverse)


# 3. read data ------------------------------------------------------------

d_raw <- read.xlsx("https://ndownloader.figshare.com/files/2026035")
d_raw <- as_tibble(d_raw)


# 4. set up data objects --------------------------------------------------

d_wide <- d_raw %>%
    select(ID, S1PP:S2NN, AGE) %>%
    mutate(AGE = scale(AGE, scale = F))


# 5. specify lavaan model -------------------------------------------------

model_age <- '
# loadings
  .PP =~ .l1*S1PP + NA*S1PP + .l2*S2PP
  .NP =~ .l1*S1NP + NA*S1NP + .l2*S2NP
  .PN =~ .l1*S1PN + NA*S1PN + .l2*S2PN
  .NN =~ .l1*S1NN + NA*S1NN + .l2*S2NN
# intercepts
  S1PP ~ .i1*1
  S2PP ~ .i2*1
  S1NP ~ .i1*1
  S2NP ~ .i2*1
  S1PN ~ .i1*1
  S2PN ~ .i2*1
  S1NN ~ .i1*1
  S2NN ~ .i2*1
  .PP ~ 0*1
  .NP ~ 0*1
  .PN ~ 0*1
  .NN ~ 0*1
# variances
  .PP ~~ 0*.PP
  .NP ~~ 0*.NP
  .PN ~~ 0*.PN
  .NN ~~ 0*.NN
# struc_coeff
  .pi0 =~ 0.25*.PP + 0.25*.NP + 0.25*.PN + 0.25*.NN
  .pi1 =~ -0.5*.PP + 0.5*.NP + -0.5*.PN + 0.5*.NN
  .pi2 =~ -0.5*.PP + -0.5*.NP + 0.5*.PN + 0.5*.NN
  .pi3 =~ -0.25*.PP + 0.25*.NP + 0.25*.PN + -0.25*.NN
# regressions
  .pi0 ~ .m0*1
  .pi1 ~ .m1*1
  .pi2 ~ .m2*1
  .pi3 ~ .m3*1
# covariate AGE 
  .pi0 ~ AGE
  .pi1 ~ AGE
  .pi2 ~ AGE
  .pi3 ~ AGE
  AGE ~~ AGE
# constraints
  .l1 + .l2 == 2
  .i1 + .i2 == 0
'


# 6. fit model ------------------------------------------------------------

fit_age <- sem(model_age, d_wide)
summary(fit_age, fit.measures = T)


# 7. reset session --------------------------------------------------------

## this reverts changes to your libraries made by the checkpoint package
uncheckpoint()
delete_checkpoint("2020-06-30")
