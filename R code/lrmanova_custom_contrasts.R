
# 1. load required packages -----------------------------------------------

library(openxlsx)
library(lavaan)
library(tidyverse)
library(semnova)


# 2. read data ------------------------------------------------------------

d_raw <- read.xlsx("https://ndownloader.figshare.com/files/2026035")
d_raw <- as_tibble(d_raw)


# 3. set up data objects --------------------------------------------------

d_wide <- d_raw %>%
    select(ID, S1PP:S2NN)


# 4. first example --------------------------------------------------------

# 4.1. specify lavaan model -----------------------------------------------

model1 <- '
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
  .pi0 =~ 0*.PP + 0*.NP + 0*.PN + -3*.NN
  .pi1 =~ 1*.PP + 0*.NP + 0*.PN +  3*.NN
  .pi2 =~ 0*.PP + 1*.NP + 0*.PN + -1*.NN
  .pi3 =~ 0*.PP + 0*.NP + 1*.PN + -1*.NN
# regressions
  .pi0 ~ .m0*1
  .pi1 ~ .m1*1
  .pi2 ~ .m2*1
  .pi3 ~ .m3*1
# constraints
  .l1 + .l2 == 2
  .i1 + .i2 == 0
'


# 4.2. fit model ----------------------------------------------------------

fit1 <- sem(model1, data = d_wide)
summary(fit1)


# 4.3. using the semnova package ------------------------------------------

# Specify the contrast matrix. This must not be symmetric. The semnova will
# automatically add linearly independent rows.
C_matrix <- matrix(c(1, -1 / 3,-1 / 3,-1 / 3), nrow = 1)

# Create a measurement model
mmodel <- create_mmodel(PP = c("S1PP", "S2PP"), NP = c("S1NP", "S2NP"),
                        PN = c("S1PN", "S2PN"), NN = c("S1NN", "S2NN"))

# Create list of hypotheses. In this case, the list has only one element. It
# contains row indices of the contrast matrix that are to be compared against
# zero.
hypotheses <- list(custom = 1)

# Fit the model.
semnova1 <-
  lgc(
    data = d_wide,
    mmodel = mmodel,
    C_matrix = C_matrix,
    hypotheses = hypotheses
  )

# Print the summary.
summary(semnova1)


# 5. second example -------------------------------------------------------

# 5.1. specify lavaan model -----------------------------------------------

model2 <- '
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
  .pi0 =~ 1*.PP + 0*.NP + 0*.PN + 0*.NN
  .pi1 =~ 0*.PP + 1*.NP + 0*.PN + 0*.NN
  .pi2 =~ 1*.PP + 0*.NP + 1*.PN + 0*.NN
  .pi3 =~ 0*.PP + 0*.NP + 0*.PN + 1*.NN
# regressions
  .pi0 ~ .m0*1
  .pi1 ~ .m1*1
  .pi2 ~ .m2*1
  .pi3 ~ .m3*1
# constraints
  .l1 + .l2 == 2
  .i1 + .i2 == 0
'


# 5.2. fit model ----------------------------------------------------------

fit2 <- sem(model2, data = d_wide)
summary(fit2)



# 5.3. using the semnova package ------------------------------------------

# Specify the contrast matrix. This must not be symmetric. The semnova will
# automatically add linearly independent rows.
C_matrix <- matrix(c(1, 0, -1, 0), nrow = 1)

# Create a measurement model
mmodel <- create_mmodel(PP = c("S1PP", "S2PP"), NP = c("S1NP", "S2NP"),
                        PN = c("S1PN", "S2PN"), NN = c("S1NN", "S2NN"))

# Create list of hypotheses. In this case, the list has only one element. It
# contains row indices of the contrast matrix that are to be compared against
# zero.
hypotheses <- list(custom = 1)

# Fit the model.
semnova2 <-
  lgc(
    data = d_wide,
    mmodel = mmodel,
    C_matrix = C_matrix,
    hypotheses = hypotheses
  )

# Print the summary.
summary(semnova2)
