
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
    select(ID, S1PP:S2NN)


# 5. specify models -------------------------------------------------------

configural_model <- '
# loadings
  .PP =~ 1*S1PP + S2PP
  .NP =~ 1*S1NP + S2NP
  .PN =~ 1*S1PN + S2PN
  .NN =~ 1*S1NN + S2NN
# intercepts
  S1PP ~ 0*1
  # S2PP ~ .i2*1
  S1NP ~ 0*1
  # S2NP ~ .i2*1
  S1PN ~ 0*1
  # S2PN ~ .i2*1
  S1NN ~ 0*1
  # S2NN ~ .i2*1
  .PP ~ 0*1
  .NP ~ 0*1
  .PN ~ 0*1
  .NN ~ 0*1
# variances
  .PP ~~ 0*.PP
  .NP ~~ 0*.NP
  .PN ~~ 0*.PN
  .NN ~~ 0*.NN
# struc_h1_coeff
  .pi0 =~ 0.25*.PP + 0.25*.NP + 0.25*.PN + 0.25*.NN
  .pi1 =~ 0.25*.PP + -0.25*.NP + 0.25*.PN + -0.25*.NN
  .pi2 =~ 0.25*.PP + 0.25*.NP + -0.25*.PN + -0.25*.NN
  .pi3 =~ 0.25*.PP + -0.25*.NP + -0.25*.PN + 0.25*.NN
# regressions
  .pi0 ~ .m0*1
  .pi1 ~ .m1*1
  .pi2 ~ .m2*1
  .pi3 ~ .m3*1
'

weak_model <- '
# loadings
  .PP =~ .l1*S1PP + NA*S1PP + .l2*S2PP
  .NP =~ .l1*S1NP + NA*S1NP + .l2*S2NP
  .PN =~ .l1*S1PN + NA*S1PN + .l2*S2PN
  .NN =~ .l1*S1NN + NA*S1NN + .l2*S2NN
# intercepts
  S1PP ~ 0*1
  # S2PP ~ .i2*1
  S1NP ~ 0*1
  # S2NP ~ .i2*1
  S1PN ~ 0*1
  # S2PN ~ .i2*1
  S1NN ~ 0*1
  # S2NN ~ .i2*1
  .PP ~ 0*1
  .NP ~ 0*1
  .PN ~ 0*1
  .NN ~ 0*1
# variances
  .PP ~~ 0*.PP
  .NP ~~ 0*.NP
  .PN ~~ 0*.PN
  .NN ~~ 0*.NN
# struc_h1_coeff
  .pi0 =~ 0.25*.PP + 0.25*.NP + 0.25*.PN + 0.25*.NN
  .pi1 =~ 0.25*.PP + -0.25*.NP + 0.25*.PN + -0.25*.NN
  .pi2 =~ 0.25*.PP + 0.25*.NP + -0.25*.PN + -0.25*.NN
  .pi3 =~ 0.25*.PP + -0.25*.NP + -0.25*.PN + 0.25*.NN
# regressions
  .pi0 ~ .m0*1
  .pi1 ~ .m1*1
  .pi2 ~ .m2*1
  .pi3 ~ .m3*1
# constraints
  .l1 + .l2 == 2
'

strong_model <- '
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
# struc_h1_coeff
  .pi0 =~ 0.25*.PP + 0.25*.NP + 0.25*.PN + 0.25*.NN
  .pi1 =~ 0.25*.PP + -0.25*.NP + 0.25*.PN + -0.25*.NN
  .pi2 =~ 0.25*.PP + 0.25*.NP + -0.25*.PN + -0.25*.NN
  .pi3 =~ 0.25*.PP + -0.25*.NP + -0.25*.PN + 0.25*.NN
# regressions
  .pi0 ~ .m0*1
  .pi1 ~ .m1*1
  .pi2 ~ .m2*1
  .pi3 ~ .m3*1
# constraints
  .l1 + .l2 == 2
  .i1 + .i2 == 0
'

strict_model <- '
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
# residual variances
  S1PP ~~ .v1*S1PP
  S2PP ~~ .v2*S2PP
  S1NP ~~ .v1*S1NP
  S2NP ~~ .v2*S2NP
  S1PN ~~ .v1*S1PN
  S2PN ~~ .v2*S2PN
  S1NN ~~ .v1*S1NN
  S2NN ~~ .v2*S2NN
# struc_h1_coeff
  .pi0 =~ 0.25*.PP + 0.25*.NP + 0.25*.PN + 0.25*.NN
  .pi1 =~ 0.25*.PP + -0.25*.NP + 0.25*.PN + -0.25*.NN
  .pi2 =~ 0.25*.PP + 0.25*.NP + -0.25*.PN + -0.25*.NN
  .pi3 =~ 0.25*.PP + -0.25*.NP + -0.25*.PN + 0.25*.NN
# regressions
  .pi0 ~ .m0*1
  .pi1 ~ .m1*1
  .pi2 ~ .m2*1
  .pi3 ~ .m3*1
# constraints
  .l1 + .l2 == 2
  .i1 + .i2 == 0
'


# 6. estimate models ------------------------------------------------------

configural_fit <- sem(configural_model, data=d_wide)
weak_fit       <- sem(weak_model, data=d_wide)
strong_fit     <- sem(strong_model, data=d_wide)
strict_fit     <- sem(strict_model, data=d_wide)

summary(configural_fit, fit.measures=TRUE)
summary(weak_fit, fit.measures=TRUE)
summary(strong_fit, fit.measures=TRUE)
summary(strict_fit, fit.measures=TRUE)


# 7. prepare results ------------------------------------------------------

results <- anova(configural_fit, weak_fit, strong_fit, strict_fit)

fits <- cbind(chisq        = c(fitmeasures(configural_fit)["chisq"],
                               fitmeasures(weak_fit)["chisq"],
                               fitmeasures(strong_fit)["chisq"],
                               fitmeasures(strict_fit)["chisq"]),
              df           = c(fitmeasures(configural_fit)["df"],
                               fitmeasures(weak_fit)["df"],
                               fitmeasures(strong_fit)["df"],
                               fitmeasures(strict_fit)["df"]),
              `Pr(>Chisq)` = c(fitmeasures(configural_fit)["pvalue"],
                               fitmeasures(weak_fit)["pvalue"],
                               fitmeasures(strong_fit)["pvalue"],
                               fitmeasures(strict_fit)["pvalue"]),
              RMSEA        = c(fitmeasures(configural_fit)["rmsea"],
                               fitmeasures(weak_fit)["rmsea"],
                               fitmeasures(strong_fit)["rmsea"],
                               fitmeasures(strict_fit)["rmsea"]),
              CFI          = c(fitmeasures(configural_fit)["cfi"],
                               fitmeasures(weak_fit)["cfi"],
                               fitmeasures(strong_fit)["cfi"],
                               fitmeasures(strict_fit)["cfi"]))

rownames(fits) <- c("configural", "weak", "strong", "strict")
fits <- cbind(fits, anova(configural_fit, weak_fit, strong_fit, strict_fit)[,c(-1,-2,-3,-4)])
print(fits)

                 
# 8. reset session --------------------------------------------------------

## this reverts changes to your libraries made by the checkpoint package
uncheckpoint()
delete_checkpoint("2020-06-30")
