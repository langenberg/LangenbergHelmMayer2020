
################################################################################
## IMPORTANT NOTE: 
## Your working directory must be set to the folder containing this script file!
## Use setwd() to set your working directory.
################################################################################

# This line will install devtools from C-RAN.
# devtools is needed to install semnova from a local file.
install.packages("devtools")

# This line will install the semnova package.
devtools::install_local("semnova_0.1-6.tar.gz")
