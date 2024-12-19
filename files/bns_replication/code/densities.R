################################################################################
# DENSITY FIGURE: A3
# called by main_post.R
# uses results stored in ~/output/RData
################################################################################

library("EnvStats");library("invgamma")

# load data
load(paste0(path_rdata0,'/draws',nt_bsl))

# load templates
source(paste0(path_code0,'/chart_theme.R'))
source(paste0(path_code0,'/chart_functions.R'))

# Inverse gamma
setwd(path_charts0)
x <- seq(0,.1,.00001)
y <- matrix(0,length(x),2)
#s2e1_prior <- c(2,.00005)
y[,1] <- 2*x*dinvgamma(x^2,s2e1_prior[1],s2e1_prior[2])
y[,2] <- 2*x*dinvgamma(x^2,IG_prior[1],IG_prior[2])
df_dens <- data.frame(x,y)
colnames(df_dens) <- c('xaxis','Permanent productivity shocks','Other')
plotx <- comp_chart(df=df_dens, xaxisby=.02)
print(plotx)
save_chart(plotx, 'bns_figA3')
