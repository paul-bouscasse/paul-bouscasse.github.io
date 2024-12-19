################################################################################
# POST-ESTIMATION CALL TO PRODUCE CHARTS AND TABLES
# uses results stored in ~/output/RData
################################################################################

rm(list = ls())

# Set path in which bns_replication is located
path0 <- 'C:/Users/91358/Dropbox/Documents/Recherche/1. Research/Malthus Model'
#path0 <- 'C:/Users/paulb/Dropbox/Documents/Recherche/1. Research/Malthus Model'
#path0 <- '/home/datahub/files'

# Other paths
path1 <- paste0(path0,'/bns_replication')
path_data0 <- paste0(path1,'/data')
path_code0 <- paste0(path1,'/code')
path_data0 <- paste0(path1,'/data')
path_raw0 <- paste0(path_data0,'/raw')
path_trans0 <- paste0(path_data0,'/transformed')
path_output0 <- paste0(path1,'/output')
path_tables0 <- paste0(path_output0,'/tables')
path_charts0 <- paste0(path_output0,'/charts')
path_tplots0 <- paste0(path_output0,'/trace_plots')
path_rdata0 <- paste0(path_output0,'/RData')
nt_smpl <- 101
nt_cnst <- 103
nt_bsl <- 105
nt_hw <- 106
nt_strct <- 205
  # difference with main_estimation.R: path names end with 0
  # subsequent files import .RData files that contain other paths
  # which would overwrite above paths if they had the same names

# Packages and templates
for (file in c('libraries','chart_theme','chart_functions')) {
  source(paste0(path_code0,'/',file,'.R'))
}

# Table- and chart-producing files
files <- c('charts_raw', 'charts_simple', 'charts_comparison',
           'tables', 'irf_figures', 'clark_experiments', 'densities',
           'to_excel','text_numbers')
for (file in files) {
  print(paste0('Post-estimation file is: ',file,'.R'))
  source(paste0(path_code0,'/',file,'.R'), print.eval=TRUE)
}
