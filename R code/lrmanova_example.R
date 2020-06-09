
# 1. load required packages -----------------------------------------------

library(openxlsx)
library(lavaan)
library(car)
library(tidyverse)


# 2. read data ------------------------------------------------------------

d_raw <- read.xlsx("https://ndownloader.figshare.com/files/2026035")
d_raw <- as_tibble(d_raw)


# 3. set up data objects --------------------------------------------------

# select only relevant variables
d_wide <- d_raw %>%
    select(ID, S1PP:S2NN)

# reshape variables to long format
d_long <- d_wide %>%
    gather(condition, value, -ID) %>%
    mutate(item = substr(condition, 1, 2),
           condition = substr(condition, 3, 4))

# create composite scores (average across items) for regular ANOVA
d_mean_score <- d_long %>%
    group_by(ID, condition) %>%
    summarise(value = mean(value, na.rm=TRUE)) %>%
    ungroup() %>%
    spread(condition, value)


# 4. perform ANOVA --------------------------------------------------------

# 4.1. set up design objects ----------------------------------------------

# create design objects for further use
idata <- expand.grid(Peer=c("negative", "positive"), 
                     Human=c("negative","positive"))

idesign <- ~Peer*Human



# 4.2. fit model ----------------------------------------------------------

# estimate regular repeated measures ANOVA
anova_model <- lm(cbind(NN, PN, NP, PP) ~ 1, d_mean_score)
anova_fit <- Anova(anova_model, idata=idata, idesign=idesign, type="III")

# print results
summary(anova_fit)


# 5. perform L-RM-ANOVA ---------------------------------------------------

# 5.1. set up model -------------------------------------------------------

# specify L-RM-ANOVA model and constrain the intercept to be zero
intercept_h0_model <- '
# loadings
  .PP =~ 1*S1PP + .l2*S2PP
  .NP =~ 1*S1NP + .l2*S2NP
  .PN =~ 1*S1PN + .l2*S2PN
  .NN =~ 1*S1NN + .l2*S2NN
# intercepts
  S1PP ~ 0*1
  S2PP ~ .i2*1
  S1NP ~ 0*1
  S2NP ~ .i2*1
  S1PN ~ 0*1
  S2PN ~ .i2*1
  S1NN ~ 0*1
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
# constraints
  .m0 / 3 == 0
'

# specify L-RM-ANOVA model and constrain the main effect of peer to be zero
peer_h0_model <- '
# loadings
  .PP =~ 1*S1PP + .l2*S2PP
  .NP =~ 1*S1NP + .l2*S2NP
  .PN =~ 1*S1PN + .l2*S2PN
  .NN =~ 1*S1NN + .l2*S2NN
# intercepts
  S1PP ~ 0*1
  S2PP ~ .i2*1
  S1NP ~ 0*1
  S2NP ~ .i2*1
  S1PN ~ 0*1
  S2PN ~ .i2*1
  S1NN ~ 0*1
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
# constraints
  .m1 == 0
'

# specify L-RM-ANOVA model and constrain the main effect of human to be zero
human_h0_model <- '
# loadings
  .PP =~ 1*S1PP + .l2*S2PP
  .NP =~ 1*S1NP + .l2*S2NP
  .PN =~ 1*S1PN + .l2*S2PN
  .NN =~ 1*S1NN + .l2*S2NN
# intercepts
  S1PP ~ 0*1
  S2PP ~ .i2*1
  S1NP ~ 0*1
  S2NP ~ .i2*1
  S1PN ~ 0*1
  S2PN ~ .i2*1
  S1NN ~ 0*1
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
# constraints
  .m2 == 0
'

# specify L-RM-ANOVA model and constrain the interaction term to be zero
interaction_h0_model <- '
# loadings
  .PP =~ 1*S1PP + .l2*S2PP
  .NP =~ 1*S1NP + .l2*S2NP
  .PN =~ 1*S1PN + .l2*S2PN
  .NN =~ 1*S1NN + .l2*S2NN
# intercepts
  S1PP ~ 0*1
  S2PP ~ .i2*1
  S1NP ~ 0*1
  S2NP ~ .i2*1
  S1PN ~ 0*1
  S2PN ~ .i2*1
  S1NN ~ 0*1
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
# constraints
  .m3 == 0
'

# specify unconstrained L-RM-ANOVA model
h1_model <- '
# loadings
  .PP =~ 1*S1PP + .l2*S2PP
  .NP =~ 1*S1NP + .l2*S2NP
  .PN =~ 1*S1PN + .l2*S2PN
  .NN =~ 1*S1NN + .l2*S2NN
# intercepts
  S1PP ~ 0*1
  S2PP ~ .i2*1
  S1NP ~ 0*1
  S2NP ~ .i2*1
  S1PN ~ 0*1
  S2PN ~ .i2*1
  S1NN ~ 0*1
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
'

# 5.2. fit models ---------------------------------------------------------

intercept_h0_fit <- sem(intercept_h0_model, d_wide)
peer_h0_fit <- sem(peer_h0_model, d_wide)
human_h0_fit <- sem(human_h0_model, d_wide)
interaction_h0_fit <- sem(interaction_h0_model, d_wide)
h1_fit <- sem(h1_model, d_wide)


# 5.3. test for main effect intercept -------------------------------------

# 5.3.1. LRT --------------------------------------------------------------

# likelihood ratio test using lavTestLRT()
# we compare the constrained with the unconstrained model
intercept_lrt <- lavTestLRT(h1_fit, intercept_h0_fit)
# print results
intercept_lrt

# approx. F-test (LRT)
# from the chisq-stat. we can calculate an approx. F-stat.
intercept_lrt_F <- list(stat = intercept_lrt[2, "Chisq diff"] / 
                        intercept_lrt[2, "Df diff"],
                        df1  = intercept_lrt[2, "Df diff"],
                        df2  = (nrow(d_wide) - 1) * intercept_lrt[2, "Df diff"])
intercept_lrt_F$"Pr(>F)" <- 1- pf(intercept_lrt_F$stat,
                                  intercept_lrt_F$df1,
                                  intercept_lrt_F$df2)
# print results
intercept_lrt_F


# 5.3.2. Wald -------------------------------------------------------------

# Wald using lavTestWald()
# we use a wald test on the unconstrained model and a constraint
intercept_wald <- lavTestWald(h1_fit, '.m0/3 == 0')
# print results
intercept_wald

# approx. F-test (Wald)
# from the chisq-stat. we can calculate an approx. F-stat.
intercept_wald_F <- list(stat = intercept_wald$stat / intercept_wald$df,
                         df1  = intercept_wald$df,
                         df2  = (nrow(d_wide) - 1) * intercept_wald$df)
intercept_wald_F$"Pr(>F)" <- 1- pf(intercept_wald_F$stat,
                                   intercept_wald_F$df1,
                                   intercept_wald_F$df2)
# print results
intercept_wald_F


# 5.3.3. Score ------------------------------------------------------------

# Score using using lavTestScore()
intercept_score <- lavTestScore(intercept_h0_fit)
# print results
intercept_score$uni[1,]

# approx. F-test (Score)
# from the chisq-stat. we can calculate an approx. F-stat.
intercept_score_F <- list(stat = intercept_score$uni[1, "X2"] / intercept_score$uni[1, "df"],
                          df1  = intercept_score$uni[1, "df"],
                          df2  = (nrow(d_wide) - 1) * intercept_score$uni[1, "df"])
intercept_score_F$"Pr(>F)" <- 1 - pf(intercept_score_F$stat,
                                     intercept_score_F$df1,
                                     intercept_score_F$df2)
# print results
intercept_score_F



# 5.4. test for main effect peer ------------------------------------------

# 5.4.1. LRT --------------------------------------------------------------

# likelihood ratio test using lavTestLRT()
# we compare the constrained with the unconstrained model()
peer_lrt <- lavTestLRT(h1_fit, peer_h0_fit)
# print results
peer_lrt

# approx. F-test (LRT)
# from the chisq-stat. we can calculate an approx. F-stat.
peer_lrt_F <- list(stat = peer_lrt[2, "Chisq diff"] / peer_lrt[2, "Df diff"],
                   df1  = peer_lrt[2, "Df diff"],
                   df2  = (nrow(d_wide) - 1) * peer_lrt[2, "Df diff"])
peer_lrt_F$"Pr(>F)" <- 1- pf(peer_lrt_F$stat,
                             peer_lrt_F$df1,
                             peer_lrt_F$df2)
# print results
peer_lrt_F


# 5.4.2. Wald -------------------------------------------------------------

# Wald using lavTestWald()
# we use a wald test on the unconstrained model and a constraint
peer_wald <- lavTestWald(h1_fit, '.m1 == 0')
# print results
peer_wald

# approx. F-test (Wald)
# from the chisq-stat. we can calculate an approx. F-stat.
peer_wald_F <- list(stat = peer_wald$stat / peer_wald$df,
                    df1  = peer_wald$df,
                    df2  = (nrow(d_wide) - 1) * peer_wald$df)
peer_wald_F$"Pr(>F)" <- 1- pf(peer_wald_F$stat,
                              peer_wald_F$df1,
                              peer_wald_F$df2)
# print results
peer_wald_F


# 5.4.3. Score ------------------------------------------------------------

# Score using lavTestScore()
peer_score <- lavTestScore(peer_h0_fit)
# print results
peer_score$uni[1,]

# approx. F-test (Score)
# from the chisq-stat. we can calculate an approx. F-stat.
peer_score_F <- list(stat = peer_score$uni[1, "X2"] / peer_score$uni[1, "df"],
                     df1  = peer_score$uni[1, "df"],
                     df2  = (nrow(d_wide) - 1) * peer_score$uni[1, "df"])
peer_score_F$"Pr(>F)" <- 1 - pf(peer_score_F$stat,
                                peer_score_F$df1,
                                peer_score_F$df2)
# print results
peer_score_F


# 5.5. test for main effect human -----------------------------------------

# 5.5.1. LRT --------------------------------------------------------------

# likelihood ratio test using lavTestLRT()
# we compare the constrained with the unconstrained model()
human_lrt <- lavTestLRT(h1_fit, human_h0_fit)
# print results
human_lrt

# approx. F-test (LRT)
# from the chisq-stat. we can calculate an approx. F-stat.
human_lrt_F <- list(stat = human_lrt[2, "Chisq diff"] / human_lrt[2, "Df diff"],
                    df1  = human_lrt[2, "Df diff"],
                    df2  = (nrow(d_wide) - 1) * human_lrt[2, "Df diff"])
human_lrt_F$"Pr(>F)" <- 1- pf(human_lrt_F$stat,
                              human_lrt_F$df1,
                              human_lrt_F$df2)
# print results
human_lrt_F


# 5.5.2. Wald -------------------------------------------------------------

# Wald using lavTestWald()
# we use a wald test on the unconstrained model and a constraint
human_wald <- lavTestWald(h1_fit, '.m2 == 0')
# print results
human_wald

# approx. F-test (Wald)
# from the chisq-stat. we can calculate an approx. F-stat.
human_wald_F <- list(stat = human_wald$stat / human_wald$df,
                     df1  = human_wald$df,
                     df2  = (nrow(d_wide) - 1) * human_wald$df)
human_wald_F$"Pr(>F)" <- 1- pf(human_wald_F$stat,
                               human_wald_F$df1,
                               human_wald_F$df2)
# print results
human_wald_F


# 5.5.3. Score ------------------------------------------------------------

# Score using lavTestScore()
human_score <- lavTestScore(human_h0_fit)
# print results
human_score$uni[1,]

# approx. F-test (Score)
# from the chisq-stat. we can calculate an approx. F-stat.
human_score_F <- list(stat = human_score$uni[1, "X2"] / 
                      human_score$uni[1, "df"],
                      df1  = human_score$uni[1, "df"],
                      df2  = (nrow(d_wide) - 1) * 
                        human_score$uni[1, "df"])
human_score_F$"Pr(>F)" <- 1 - pf(human_score_F$stat,
                                 human_score_F$df1,
                                 human_score_F$df2)
# print results
human_score_F


# 5.6. test for interaction effect ----------------------------------------

# 5.6.1. LRT --------------------------------------------------------------

# likelihood ratio test using lavTestLRT()
# we compare the constrained with the unconstrained model()
interaction_lrt <- lavTestLRT(h1_fit, interaction_h0_fit)
# print results
interaction_lrt

# approx. F-test (LRT)
# from the chisq-stat. we can calculate an approx. F-stat.
interaction_lrt_F <- list(stat = interaction_lrt[2, "Chisq diff"] / 
                          interaction_lrt[2, "Df diff"],
                          df1  = interaction_lrt[2, "Df diff"],
                          df2  = (nrow(d_wide) - 1) * 
                            interaction_lrt[2, "Df diff"])
interaction_lrt_F$"Pr(>F)" <- 1- pf(interaction_lrt_F$stat,
                                    interaction_lrt_F$df1,
                                    interaction_lrt_F$df2)
# print results
interaction_lrt_F


# 5.6.2. Wald -------------------------------------------------------------

# Wald using lavTestWald()
# we use a wald test on the unconstrained model and a constraint
interaction_wald <- lavTestWald(h1_fit, '.m3 == 0')
# print results
interaction_wald

# approx. F-test (Wald)
# from the chisq-stat. we can calculate an approx. F-stat.
interaction_wald_F <- list(stat = interaction_wald$stat / 
                           interaction_wald$df,
                           df1  = interaction_wald$df,
                           df2  = (nrow(d_wide) - 1) * 
                             interaction_wald$df)
interaction_wald_F$"Pr(>F)" <- 1- pf(interaction_wald_F$stat,
                                     interaction_wald_F$df1,
                                     interaction_wald_F$df2)
# print results
interaction_wald_F


# 5.6.3. Score ------------------------------------------------------------

# Score using lavTestScore()
interaction_score <- lavTestScore(interaction_h0_fit)
# print results
interaction_score$uni[1,]

# approx. F-test (Score)
# from the chisq-stat. we can calculate an approx. F-stat.
interaction_score_F <- list(stat = interaction_score$uni[1, "X2"] / 
                            interaction_score$uni[1, "df"],
                            df1  = interaction_score$uni[1, "df"],
                            df2  = (nrow(d_wide) - 1) * 
                              interaction_score$uni[1, "df"])
interaction_score_F$"Pr(>F)" <- 1 - pf(interaction_score_F$stat,
                                       interaction_score_F$df1,
                                       interaction_score_F$df2)
# print results
interaction_score_F
