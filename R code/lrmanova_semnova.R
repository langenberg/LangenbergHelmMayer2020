
# 1. load required packages -----------------------------------------------

library(openxlsx)
library(semnova)
library(tidyverse)


# 2. read data ------------------------------------------------------------

d_raw <- read.xlsx("https://ndownloader.figshare.com/files/2026035")
d_raw <- as_tibble(d_raw)


# 3. set up data objects --------------------------------------------------

d_wide <- d_raw %>%
  select(ID, S1PP:S2NN)


# 4. specify model --------------------------------------------------------

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


# 5. fit model and print summary ------------------------------------------

fit <- semnova(
  formula = cbind(NN, PN, NP, PP) ~ 1,
  data = d_wide,
  idata = idata,
  idesign = idesign,
  mmodel = mmodel
)

summary(fit)


# 6. use a custom contrast matrix -----------------------------------------

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
