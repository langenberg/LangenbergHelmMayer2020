# Software Code for Estimating Latent Repeated Measures ANOVA Using the R Package semnova

## Description

This repository contains software code for the demonstration of latent repeated measures analysis of variance using the open-source R package [`semnova`](https://github.com/langenberg/semnova). The software code uses data from the article by Qu et al. (2015) which is publicly available on *PLoS ONE*.


## How to use

The repository contains two types of files. 

1. [recommended] Files in the folder `R code/` will use whatever package version is installed on the machine. There is no guarantee that these files will be compatible with future releases. These files will, however, run faster as no packages will temporarily be installed. It is recommended to use these files as long as they work.

2. Files in the folder `R code (long term)/` with the prefix `checkpoint_` use the checkpoint package. These files should always be executable as they will use the package version from June 30th, 2020.


## Trouble Shooting

In case of problems running the code, try to install the package from local. The folder `manual installation/` contains a script file to help install the `semnova` package and the `semnova` package itself.


## References

Qu, C., Ling, Y., Heynderickx, I., & Brinkman, W. P. (2015). Virtual bystanders in a language lesson: Examining the effect of social evaluation, vicarious experience, cognitive consistency and praising on students’ beliefs, self-efficacy and anxiety in a virtual reality environment. *PLoS ONE, 10*(4), 1–26. https://doi.org/10.1371/journal.pone.0125279
