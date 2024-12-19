### LAUNCHES ESTIMATION

rm(list = ls())
closeAllConnections()

# Set path in which bns_replication is located
path0 <- 'C:/Users/paulb/Dropbox/Documents/Recherche/1. Research/Malthus Model'
#path0 <- 'C:/Users/91358/Dropbox/Documents/Recherche/1. Research/Malthus Model'
#path0 <- '/home/datahub/files/Malthus'

# Other paths
path0 <- paste0(path0,'/bns_replication')
path_code <- paste0(path0,'/code')
path_data <- paste0(path0,'/data')
path_raw <- paste0(path_data,'/raw')
path_trans <- paste0(path_data,'/transformed')
path1 <- paste0(path0,'/output')
path_tables <- paste0(path1,'/tables')
path_charts <- paste0(path1,'/charts')
path_tplots <- paste0(path1,'/trace_plots')
path_rdata <- paste0(path1,'/RData')

# Packages and templates
for (file in c('libraries','dataset','chart_theme','chart_functions','dataset')) {
  source(paste0(path_code,'/',file,'.R'), echo = FALSE, print.eval = FALSE)
}

nc <- min(nc,24)
for (nt in c(701:724,801:824)) {
  
  # Meta
  print(nt)
  iters <- 10^6 # number of draws for Stan
  chain <- nc
  thin0 <- ceiling(iters/(2*10^4))
  
  # Calls main estimation file
  source(paste0(path_code,'/estimationXX.R'), print.eval=TRUE)
  
}

# Specifications

# simple
# nt=701 # iv, beta=0, constant days, B1=1550-1800, B2=1810
# nt=702 # iv, beta=0, HW days, B1=1550-1800, B2=1810

# baseline
# nt=703 # iv, full model, constant days, B1=1550-1800, B2=1810
# nt=704 # iv, full model, HW days, B1=1550-1800, B2=1810

# falling alpha
# nt=705 # iv, full model, constant days, B1=1550-1800, B2=1810, falling alpha ---> preferred
# nt=706 # iv, full model, HW days, B1=1550-1800, B2=1810, falling alpha

# CES
# nt=707 # iv, full model, constant days, CES, B1=1550-1800, B2=1810
# nt=708 # iv, full model, HW days, CES, B1=1550-1800, B2=1810

# CES + falling alpha
# nt=709 # iv, full model, constant days, CES, B1=1550-1800, B2=1810, falling alpha
# nt=710 # iv, full model, HW days, CES, B1=1550-1800, B2=1810, falling alpha

# preferred with fixed B1
# nt=711 # B1=1550
# nt=712 # B1=1600
# nt=713 # B1=1650
# nt=714 # B1=1700
# nt=715 # B1=1750
# nt=716 # B1=1800

# preferred with different wage series
# nt=717 # farmers wage
# nt=718 # craftsmen wage
# nt=719 # allen wage
# nt=720 # 3 wage series
# nt=721 # annual income

# preferred with different population series
# nt=722 # Broadberry's population data

# preferred with different priors
# nt=723 # looser productivity prior
# nt=724 # looser population prior

# structural
# nt=801 # structural, constant days, B1=1550-1800, B2=1810
# nt=802 # structural, HW days, B1=1550-1800, B2=1810

# structural + falling alpha
# nt=803 # structural, constant days, B1=1550-1800, B2=1810, falling alpha
# nt=804 # structural, HW days, B1=1550-1800, B2=1810, falling alpha

# structural + CES
# nt=805 # structural, constant days, B1=1550-1800, B2=1810, CES
# nt=806 # structural, HW days, B1=1550-1800, B2=1810, CES

# structural + CES + falling alpha
# nt=807 # structural, constant days, B1=1550-1800, B2=1810, CES, falling alpha
# nt=808 # structural, HW days, B1=1550-1800, B2=1810, CES, falling alpha

# preferred with fixed B1
# nt=811 # B1=1550
# nt=812 # B1=1600
# nt=813 # B1=1650
# nt=814 # B1=1700
# nt=815 # B1=1750
# nt=816 # B1=1800

# preferred with different wage series
# nt=817 # farmers wage
# nt=818 # craftsmen wage
# nt=819 # allen wage
# nt=820 # 3 wage series
# nt=821 # annual income

# preferred with different population series
# nt=822 # Broadberry's population data

# preferred with different priors
# nt=823 # looser productivity prior
# nt=824 # looser population prior
