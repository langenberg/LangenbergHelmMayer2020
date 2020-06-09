
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
library(semnova)
library(tidyverse)


# 3. read data ------------------------------------------------------------

d_raw <- read.xlsx("https://ndownloader.figshare.com/files/2026035")
d_raw <- as_tibble(d_raw)


# 4. set up data objects --------------------------------------------------

d_wide <- d_raw %>%
  select(ID, S1PP:S2NN)


# 5. specify model --------------------------------------------------------

# idata and idesign similar to car package input
idata <- expand.grid(Peer = c("negative", "positive"),
                     Human = c("negative", "positive"))

idesign <- ~ Peer * Human

# measurement model
mmodel <- create_mmodel(
  NN = c("S1NN", "S2NN"),
  PN = c("S1PN", "S2PN"),
  NP = c("S1NP", "S2NP"),
  PP = c("S1PP", "S2PP"),
  lv_scaling = "effect"
)


# 6. fit model and print summary ------------------------------------------

fit <- semnova(
  formula = cbind(NN, PN, NP, PP) ~ 1,
  data = d_wide,
  idata = idata,
  idesign = idesign,
  mmodel = mmodel
)

summary(fit)


# 7. use a custom contrast matrix -----------------------------------------

# this matrix was used in the paper
C_matrix <- matrix(c( 1,    1,    1,    1,
                     -0.5,  0.5, -0.5,  0.5,
                     -0.5, -0.5,  0.5,  0.5,
                     -1,    1,    1,   -1),
                   nrow = 4, byrow = TRUE)

hypotheses <- list(
  intercept = 1,
  peer = 2,
  human = 3,
  peer_human = 4
)

fit <-
  lgc(
    data = d_wide,
    mmodel = mmodel,
    C_matrix = C_matrix,
    hypotheses = hypotheses
  )

summary(fit)


# 8. reset session --------------------------------------------------------

## this reverts changes to your libraries made by the checkpoint package
uncheckpoint()
delete_checkpoint("2020-04-24")
