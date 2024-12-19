################################################################################
# USEFUL LIBRARIES
# called by main_estimation.R and main_post.R
# packages are automatically installed if missing
################################################################################

# Libraries and multi-core processing
options(warn=-1, message =-1)
required_packages <- c("rstan", "dplyr", "ggpubr", "reshape2", "broom",
                       "tidyverse", "xtable", "nleqslv", "pracma", "invgamma",
                       "ramify", "stringr", "patchwork",
                       "readxl", "EnvStats", "openxlsx",
                       "latex2exp", "scales")
for (pck in required_packages) {
  if (!(pck %in% installed.packages())) {
    install.packages(pck)
  }
  lapply(pck, library, character.only = TRUE)
}

# Stan options
rstan_options(auto_write = TRUE)
nc = parallel::detectCores()
options(mc.cores = nc)

# Scientific format for legend
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, digits=1, scientific=TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}
