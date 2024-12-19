################################################################################
# ESTIMATION LAUNCHER
# requires the installation of RStan: https://mc-stan.org/users/interfaces/rstan
################################################################################

rm(list = ls())
closeAllConnections()

# Set path in which bns_replication is located
path0 <- 'C:/Users/91358/Dropbox/Documents/Recherche/1. Research/Malthus Model'
#path0 <- 'C:/Users/paulb/Dropbox/Documents/Recherche/1. Research/Malthus Model'
#path0 <- '/home/datahub/files'

# Other paths
path1 <- paste0(path0,'/bns_replication')
path_code <- paste0(path1,'/code')
path_data <- paste0(path1,'/data')
path_raw <- paste0(path_data,'/raw')
path_trans <- paste0(path_data,'/transformed')
path_output <- paste0(path1,'/output')
path_tables <- paste0(path_output,'/tables')
path_charts <- paste0(path_output,'/charts')
path_tplots <- paste0(path_output,'/trace_plots')
path_rdata <- paste0(path_output,'/RData')

# Packages and templates
for (file in c('libraries','dataset','chart_theme','chart_functions',
               'dataset')) {
  source(paste0(path_code,'/',file,'.R'), echo = FALSE, print.eval = FALSE)
}

# Meta parameters
iters <- 10^6 # number of draws/chain
chain <- 24 # number of chains
thin0 <- ceiling(iters/(2*10^4)) # thinning of draws

# Estimation: each nt is a specification
start_time <- Sys.time()
#for (nt in c(101, 103, 105:106, 111:116, 121:123, 205, 231:235)) {
for (nt in c(105)) { # use this line to run a subset of the specs
  print(nt)
  source(paste0(path_code,'/estimation.R'), print.eval=TRUE)
}
end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(elapsed_time)

# List of specifications that can be estimated

# Notes:
  # 100<nt<200: IV specifications
  # 200<nt<300: structural specifications
  # last 2 digits should correspond across IV and structural, e.g.:
    # 105: IV, constant days, changing alpha, B1=1550-1800, B2=1810
    # 205: structural, constant days, changing alpha, B1=1550-1800, B2=1810
  # used in the paper: 101, 103, 105:106, 111:116, 121:123, 205, 231:235

# simple
# nt=101 # iv, beta=0, constant days, B1=1550-1800, B2=1810
# nt=102 # iv, beta=0, HW days, B1=1550-1800, B2=1810

# baseline
# nt=103 # iv, full model, constant days, B1=1550-1800, B2=1810
# nt=104 # iv, full model, HW days, B1=1550-1800, B2=1810

# falling alpha
# nt=105 # iv, full model, constant days, B1=1550-1800, B2=1810, changing alpha ---> baseline
# nt=106 # iv, full model, HW days, B1=1550-1800, B2=1810, changing alpha

# preferred with fixed B1
# nt=111 # B1=1550
# nt=112 # B1=1600
# nt=113 # B1=1650
# nt=114 # B1=1700
# nt=115 # B1=1750
# nt=116 # B1=1800

# preferred with different population series
# nt=121 # Broadberry's population data

# preferred with different priors
# nt=122 # looser productivity prior
# nt=123 # looser population prior

# structural
# nt=203 # structural, constant days, B1=1550-1800, B2=1810
# nt=204 # structural, HW days, B1=1550-1800, B2=1810

# structural + changing alpha
# nt=205 # structural, constant days, B1=1550-1800, B2=1810, changing alpha ---> baseline structural
# nt=206 # structural, HW days, B1=1550-1800, B2=1810, changing alpha

# baseline structural alpha with fixed B1
# nt=211 # B1=1550
# nt=212 # B1=1600
# nt=213 # B1=1650
# nt=214 # B1=1700
# nt=215 # B1=1750
# nt=216 # B1=1800

# baseline structural with different population series
# nt=221 # Broadberry's population data

# baseline structural with different priors
# nt=222 # looser productivity prior
# nt=223 # looser population prior

# baseline structural with different wage series
# nt=231 # farmers wage
# nt=232 # craftsmen wage
# nt=233 # allen wage
# nt=234 # 3 wage series
# nt=235 # annual income
