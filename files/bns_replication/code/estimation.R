################################################################################
# ESTIMATION FILE
# called by main.estimation.R
################################################################################

################################################################################
### DATA IMPORT
################################################################################

# Dates
start <- 1250 # starting date of the estimation
end <- 1860 # ending date of the estimation

# Data from Excel
setwd(path_trans) # data path
data_o <- read.csv2('malthus_data.csv', header=TRUE, sep=",", fill=TRUE, fileEncoding='UTF-8-BOM') # imports data
data <- data_o[which(data_o['decade'] == start):which(data_o['decade'] == end),] # drops irrelevant dates

# Time variables
D <- as.numeric(unlist(data['decade'])) # dates
T <- length(D) # length of sample (in decades)

# Population data
N <- as.numeric(unlist(data['wrigley_pop'])) # population observations in level
N_trn <- as.numeric(unlist(data['pop_ind'])) # population trend in level
if (is.element(nt,c(121,221))) { # different population data source for some specification
  N <- as.numeric(unlist(data['broad_pop_lf']))
  N_trn <- rep(NA,length(N))
}

# Wage data
ws <- 'w_lab_10' # wage data source: Clark's building laborers for most specifications
if (is.element(nt,c(131,231))) ws <- 'w_farm_10'
if (is.element(nt,c(132,232))) ws <- 'w_craft_10'
if (is.element(nt,c(133,233))) ws <- 'allen_w'
wme <- 0 # wage measurement error dummy
nw <- 1 # number of wage series: 1 in most specification
W <- matrix(0,T,nw) # defines wage observation matrix/vector
W[,1] <- as.numeric(unlist(data[ws])) # wage data in level
W0 <- as.numeric(unlist(data_o[which(data_o['decade'] == start-10), ws])) # wage data at t=0 (for population supply at t=1)
if (is.element(nt,c(134,234))) { # wage data if several series used
  w_names <- c('w_lab_10','w_craft_10','w_farm_10')
  nw <- length(w_names)
  W <- matrix(0,T,nw)
  W0 <- rep(0,nw)
  for (j in 1:nw) {
    W[,j] <- as.numeric(unlist(data[,w_names[j]]))
    W0[j] <- as.numeric(unlist(data_o[which(data_o['decade'] == start-10),w_names[j]]))
  }
  wme <- 1
}

# Days worked data
H_ref <- 250 # default is constant
H <- rep(H_ref,T)
H0 <- H_ref
include_hme <- 0
if (nt %in% c(102,104,106,204,206)) { # HW days
  H <- as.numeric(unlist(data['d_hw'])) # days worked data
  H0 <- as.numeric(unlist(data_o[which(data_o['decade'] == start-10), 'd_hw'])) # days worked data at t=0 (for population supply at t=1)
  include_hme <- 1
}

# Income data
INC <- as.numeric(unlist(data['inc_hw']))
INC0 <- as.numeric(unlist(data_o[which(data_o['decade'] == start-10), 'inc_hw']))
if (is.element(nt,c(135,235))) {
  W[,1] <- INC/H
  W0 <- INC0/H0
}

# Interest rate data
r_mat_names <- c('r_land', 'r_rents')
nr <- length(r_mat_names)
r_mat <- matrix(0,T,nr)
r_mat0 <- rep(0,nr)
for (j in 1:nr) {
  r_mat[,j] <- as.numeric(unlist(data[,r_mat_names[j]]))
  r_mat0 <- as.numeric(unlist(data_o[which(data_o['decade'] == start-10),r_mat_names[j]]))
}
R <- as.numeric(unlist(data['interest_rate'])) # interest rate data (useless in most specifications)
R[is.na(R)] <- 5 # temporary

# Factor shares
shares <- matrix(NA,T,3)
shares[,1] <- as.numeric(unlist(data['land_share']))
shares[,2] <- as.numeric(unlist(data['capital_share']))
shares[,3] <- 1 - rowSums(shares[,1:2])

# Other data (for comparison only)
N_clark07 <- as.numeric(unlist(data['pop_07'])) # population data from Clark (2007)
N_clark10 <- as.numeric(unlist(data['pop_10'])) # population data from Clark (2010)
N_broad <- as.numeric(unlist(data['broad_pop_hf'])) # population data from Broadberry et al.
a_clark <- log(as.numeric(unlist(data['efficiency_DE']))) # productivity estimate from Clark
a_clark <- a_clark - a_clark[1] # normalization: a_1 = 0
a_clark16 <- log(as.numeric(unlist(data['efficiency_DE16']))) # productivity estimate from Clark
a_clark16 <- a_clark16 - a_clark16[1]
a_allen <- log(as.numeric(unlist(data['allen05_TFP_agriculture']))) # productivity estimate from Allen
Kg_feinstein <- as.numeric(unlist(data['capital_stock_gross'])) # gross capital estimates from Feinstein
Kn_feinstein <- as.numeric(unlist(data['capital_stock_net'])) # net capital estimates from Feinstein
kg_feinstein <- log(Kg_feinstein)
kn_feinstein <- log(Kn_feinstein)
lp_wrigley <- log(as.numeric(unlist(data['wrigley_prod']))) # productivity estimate from Clark
lp_allen <- log(as.numeric(unlist(data['allen_prod']))) # producitity estimate from Allen
rents <- log(as.numeric(unlist(data['rent_ind'])))

################################################################################
### DATA MANIPULATION
################################################################################

# Position of observations and missing data
ii_trn_n <- array(which(!is.na(N_trn)))
ii_obs_n <- array(which(!is.na(N)))
ii_mis_n <- array(which(is.na(N_trn) & is.na(N)))
ii_mis_w <- array(which(is.na(W[,1])))
ii_obs_w <- array(which(!is.na(W[,1])))
ii_mis_r <- array(which(is.na(R)))
ii_obs_r <- array(which(!is.na(R)))
ii_mis_h <- array(which(is.na(H)))
ii_obs_h <- array(which(!is.na(H)))

# Additions for additional interest rate series
ii_obs_r1 <- array(which(!is.na(r_mat[,1])))
ii_obs_r2 <- array(which(!is.na(r_mat[,2])))
ii_mis_r1 <- array(which(is.na(r_mat[,1])))
ii_mis_r2 <- array(which(is.na(r_mat[,2])))
ii_obs_inc <- array(which(!is.na(INC)))
ii_mis_inc <- array(which(is.na(INC)))

# Length of data once NAs are dropped
T_trn_n <- length(ii_trn_n)
T_mis_n <- length(ii_mis_n)
T_mis_w <- length(ii_mis_w)
T_mis_w0 <- 0
if (is.na(W0[1])) T_mis_w0 <- 1
T_mis_r <- length(ii_mis_r)
T_mis_r1 <- length(ii_mis_r1)
T_mis_r2 <- length(ii_mis_r2)
T_mis_h <- length(ii_mis_h)
T_mis_h0 <- as.numeric(is.na(H0))
T_mis_inc <- length(ii_mis_inc)
T_mis_inc0 <- as.numeric(is.na(INC0))
T_irf <- 42 # for IRF (slide 33-34)
T_irf2 <- length(seq(from=1750,to=end,by=10)) # for IRF (slide 21-22 of the appendix)

# Transformation of the data
n_trn <- log(as.vector(na.omit(N_trn))) # logs and drops NAs
n_obs <- log(as.vector(na.omit(N*10^6))) # logs and drops NAs, adjusting for millions
m <- which(H*W[,1]==min(H*W[,1], na.rm=TRUE)) # minimum of labor supply over period
norm <- W[m,1]*H[m] # argmin
W <- W/norm # normalization of income to 1 at its minimum
W0 <- W0/norm # same normalization as for t<1
h_obs <- log(as.vector(na.omit(H))) # logs and drops NAs
w_obs <- log(as.matrix(na.omit(W))) # logs and drops NAs
w0 <- log(as.array(W0))
h0 <- log(as.vector(na.omit(H0)))
inc_obs <- log(as.vector(na.omit(INC)/min(na.omit(INC))*H_ref))
inc0 <- log(as.vector(na.omit(INC0)/min(na.omit(INC))*H_ref))
r_obs <- as.vector(na.omit(R))/100 # percent
r_obs_mat <- r_mat/100
r_obs1 <- as.vector(na.omit(r_obs_mat[,1]))
r_obs2 <- as.vector(na.omit(r_obs_mat[,2]))

################################################################################
### BREAKS AND PRIORS
################################################################################

# Population level prior
# finds mean and std of population level parameter (psi) such that 95% of 1310 population is between 4.5 and 6 million people
fpop <- function(x) { # function that returns the quantiles minus desired level
  eq1 <- x[1] - qnorm(.975)*x[2] + n_trn[which(D==1310)] - log(4.5*10^6)
  eq2 <- x[1] + qnorm(.975)*x[2] + n_trn[which(D==1310)] - log(6*10^6)
  return(c(eq1,eq2))
}
if (T_trn_n>0) {
  sol <- nleqslv(c(11,.1),fpop) # sets function to 0
  E_b <- sol$x[1] # prior mean
  std_b <- sol$x[2] # std mean
} else {
  E_b <- 0
  std_b <- 1
}
if (is.element(nt,c(123,223))) std_b <- 10

# Capital
include_K <- 1 # include capital dummy
nal <- 2 # dimension of alpha

# Supply block
include_full <- 1
if (nt %in% c(101:102)) include_full <- 0

# slope given?
include_slope <- 1
if (nt>200) include_slope <- 0
if (include_slope) {
  
  # OLS/IV
  bef_iv <- 1340
  aft_iv <- 1360
  end_ols <- 1500
  T_ols <- length(D[D<=end_ols])
  df_raw <- data.frame(D = D, w = rep(NA,T), h = rep(NA,T), n_trn = rep(NA,T), n_obs = rep(NA,T))
  df_raw[ii_obs_w,'w'] <- w_obs[ii_obs_w]
  df_raw[ii_obs_h,'h'] <- h_obs[ii_obs_h]
  df_raw[ii_trn_n,'n_trn'] <- n_trn
  df_raw[ii_obs_n,'n_obs'] <- n_obs
  df_raw['l_trn'] <- df_raw$n_trn + df_raw$h
  df_raw['l_obs'] <- df_raw$n_obs + df_raw$h
  if (!is.element(nt,c(121,221))) {
    mod_ols <- lm(w ~ l_trn, df_raw[D<=end_ols,])
    delta_iv <- -(df_raw$w[D==aft_iv] - df_raw$w[D[ii_obs_w]==bef_iv])/(df_raw$l_trn[D==aft_iv] - df_raw$l_trn[D==bef_iv])
  }
  else delta_iv <- -(df_raw$w[D==aft_iv] - df_raw$w[D[ii_obs_w]==bef_iv])/(df_raw$l_obs[D==aft_iv-10] - df_raw$l_obs[D==bef_iv])
  
  # set (alpha,beta)
  slope0 <- delta_iv
  include_K <- include_full
  
  # raw series
  if (!is.element(nt,c(121,221))) {
    a_raw <- rep(NA,T)
    a_raw[ii_trn_n] = slope0[1]*df_raw$l_trn[ii_trn_n]
    a_raw[ii_obs_n] = slope0[1]*df_raw$l_trn[ii_obs_n]
    a_raw <- a_raw - a_raw[!is.na(a_raw)][1]
    a_raw[ii_obs_n] <- a_raw[ii_obs_n] - a_raw[ii_obs_n[1]] + a_raw[ii_trn_n[length(ii_trn_n)]]
  }
  
} else {
  slope0 <- 10^(-6)
}

# Priors
IG_prior <- c(3,.005)
s2e1_prior <- c(3,.001)
s2e2_prior <- IG_prior
if (is.element(nt,c(122,222))) s2e1_prior <- s2e2_prior
alpha_prior <- c(0,1)

# Capital-related priors (useless in most specifications)
delta_prior <- c(0,.2)
r_prior <- c(.03,.15)

# XI_1 prior
p_prior <- c(0,.5) # prior for probability of plage
muXI_prior <- c(.5,.9) # prior for mean of plague

# Falling alpha?
init <- min(D[!is.na(kn_feinstein) & !is.na(rents)])
include_Z <- 0
ii_alpha <- 1:T
ii_drft <- integer(0)
if (is.element(nt,c(105:106,111:116,121:123,
                    205:206,211:216,221:223,231:235))) {
  include_Z <- 1
  ii_alpha <- which(D<init)
  ii_drft <- which(D>=init)
}

# Breaks
breaks1 <- which(D==1550):which(D==1800)
breaks2 <- which(D==1810)
if (is.element(nt,c(111,211))) breaks1 <- which(D==1550)
if (is.element(nt,c(112,212))) breaks1 <- which(D==1600)
if (is.element(nt,c(113,213))) breaks1 <- which(D==1650)
if (is.element(nt,c(114,214))) breaks1 <- which(D==1700)
if (is.element(nt,c(115,215))) breaks1 <- which(D==1750)
if (is.element(nt,c(116,216))) breaks1 <- which(D==1800)

# Other transformations
dim_b <- length(breaks1)*length(breaks2)
Be <- matrix(0,4,dim_b)
Be[1,] <- 1
for (j in 1:length(breaks1)) {
  cols0 <- (j-1)*length(breaks2) + (1:length(breaks2))
  Be[2,cols0] <- breaks1[j]
  Be[3,cols0] <- breaks2
}
Be[4,] <- T+1
Be[3,Be[2,]==Be[3,]] <- T+1
cntr_b <- .001

# Detect # of breaks for each break combination
vB0 <- rep(NA, dim_b)
vB <- rep(NA,dim_b)
vB0[Be[2,]==Be[4,]] <- 0
vB0[Be[2,]<Be[3,] & Be[3,]==Be[4,]] <- 1
vB0[Be[2,]<Be[3,] & Be[3,]<Be[4,]] <- 2
B <- length(unique(vB0))
for (b in 1:B) {
  vB[vB0==sort(unique(vB0))[b]] <- b
}

# Plague breaks
Bx1 <- c(1, which(D==1680), T+1) # breaks for plague shocks
Bx2 <- Bx1 # breaks for symmetric population shocks
nBe <- size(Be)[1] - 2 # number of productivity breaks
nBx1 <- length(Bx1) - 2
nBx2 <- length(Bx2) - 2
Bex <- sort(unique(c(Be,Bx2)))
nBex <- length(Bex) - 2

# Last info for drift
nme <- 2 + include_hme + wme + include_K + 2*include_Z # dimension of measurement error parameters
ii_drft_m1 <- ii_drft - 1 # drift coordinates lagged by 1
if (include_Z==0) ii_drft_m1 <- integer(0)
T_drft <- length(ii_drft)
cntr <- nal+1 # concentration parameter for Dirichlet

# (alpha,beta) parameters
include_apar <- 1 - include_slope

# check Malmquist index
include_check <- 1

################################################################################
### ESTIMATION
################################################################################

# Lists
  # parameters to be returned
para <- c('alpha', 'slope', 'gamma', 'phi',  'psi', 'omega', 'betap', 'mu_a', 'se1', 'se2',  'p',
          'mu_XI', 'nu_XI', 'sx2', 'si', 'nu', 'sh', 'piv')
  # data as list to be passed to Stan
data0 <- list(T=T, include_K=include_K,
              include_full=include_full, include_slope=include_slope, include_apar=include_apar, include_Z=include_Z,
              include_hme=include_hme, include_check=include_check,
              nal=nal, alphapp=colMeans(shares[D<1600,1:2]),
              T_drft=T_drft, ii_alpha=ii_alpha, ii_drft=ii_drft, ii_drft_m1=ii_drft_m1,
              nBe=nBe, Be=Be, nBx1=nBx1, Bx1=Bx1, nBx2=nBx2, Bx2=Bx2, nBex=nBex, Bex=Bex,
              B=B, vB=as.array(vB),
              T_trn_n=T_trn_n, T_mis_n=T_mis_n, T_mis_h=T_mis_h, T_mis_w=T_mis_w,
              T_mis_inc=T_mis_inc,
              ii_trn_n=ii_trn_n, ii_mis_n=ii_mis_n, ii_obs_n=ii_obs_n,
              n_trn=n_trn, n_obs=n_obs,
              ii_obs_w=ii_obs_w, ii_mis_w=ii_mis_w, w_obs=w_obs, nw=nw, wme=wme, nme=nme,
              w0=w0, T_mis_w0, h0=h0, T_mis_h0=T_mis_h0, lag_w=0,
              ii_obs_h=ii_obs_h, ii_mis_h=ii_mis_h, ii_obs_inc=ii_obs_inc, ii_mis_inc=ii_mis_inc,
              h_obs=h_obs, inc_obs=inc_obs,
              rent_obs=as.array(rents[ii_drft]), k_obs=as.array(kn_feinstein[ii_drft]),
              cntr=cntr, E_b=E_b, std_b=std_b, slope0=as.array(slope0), IG_prior=IG_prior,
              s2e1_prior=s2e1_prior, s2e2_prior=s2e2_prior,
              delta_prior=delta_prior, p_prior=p_prior, muXI_prior=muXI_prior, r_prior=r_prior,
              r_obs=r_obs, T_mis_r=T_mis_r, ii_mis_r=ii_mis_r, ii_obs_r=ii_obs_r,
              r_obs1=r_obs1, r_obs2=r_obs2, T_mis_r1=T_mis_r1, T_mis_r2=T_mis_r2,
              ii_obs_r1=ii_obs_r1, ii_obs_r2=ii_obs_r2, ii_mis_r1=ii_mis_r1, ii_mis_r2=ii_mis_r2,
              ii_mis_inc=ii_mis_inc,
              dim_b=dim_b, cntr_b=cntr_b,
              t_1350=which(D==1350), T_irf=T_irf, t_1750=which(D==1750), T_irf2=T_irf2)

# Estimation
setwd(path_code)
file0 <- 'malthus_model.stan'
malthus_fit <- stan(file = file0, data = data0, iter = iters, chains = chain,
                    thin = thin0, seed = 1312) # Stan call
malthus_sum_full <- summary(malthus_fit) # extracts summary of results
malthus_sum <- summary(malthus_fit, pars = para) # summary for certain parameters
print(malthus_fit, pars = para) # prints summary
summary(get_elapsed_time(malthus_fit)) # prints elapsed time

################################################################################
### TABLES
################################################################################

# Production parameters summary
malthus_sum_a <- summary(malthus_fit, pars = c('mu_a','se1','se2')) # imports results

# Population summary
  # main parameters
malthus_sum_n1 <- summary(malthus_fit, pars = c('alpha','gamma','omega'))
  # population supply parameters
malthus_sum_n2 <- summary(malthus_fit, pars = c('p','mu_XI','nu_XI','sx2','si[1]','si[2]','nu[1]','nu[2]'))
  # days worked parameters
if (include_hme) malthus_sum_h <- summary(malthus_fit, pars = c('sh','si[3]','nu[3]'))

################################################################################
### CHARTS: RESULT PREPARATION
################################################################################

# Extracts mean and confidence bands from draws
CI <- .9 # 90% confidence interval
draws <- list()
moments <- list() # list containing results
median <- list()
par_list <- c('alpha','phi','slope') # parameters
lat_list <- c('atilde','e1','e2','n','N','iota','w','h','H') # latent variables
oth_list <- c('piv') # IRF simulated in Stan
if (include_full) {
  lat_list <- c(lat_list,'XI1','xi','w_ss','WoW_ss','WoW_ss2')
  par_list <- c(par_list,'gamma','omega','betap')
  oth_list <- c(oth_list,'N_irf2')
}
if (include_K==1) {
  par_list <- c(par_list,'slope','delta')
  lat_list <- c(lat_list,'r','k','rents') # interest rate
}
lat_list <- c(lat_list,'mlq','alphat')
if (include_check) {
  lat_list <- c(lat_list,'mlq_check','w_check')
  if (include_K) lat_list <- c(lat_list,'k_check')
}
if (include_K==1) lat_list <- c(lat_list,'betat')
ext_list <- c(par_list,lat_list,oth_list)
draws <- rstan::extract(malthus_fit, pars=ext_list, permuted=TRUE) # extract draws from Stanfit object
for (par0 in ext_list) {
  moments[[par0]] <- cbind(unlist(sapply(as.data.frame(draws[par0]), FUN=mean)), # mean
                           unlist(sapply(as.data.frame(draws[par0]), FUN=quantile, probs=.5)), # median
                           unlist(sapply(as.data.frame(draws[par0]), FUN=quantile, probs=(1-CI)/2)), # lower bound
                           unlist(sapply(as.data.frame(draws[par0]), FUN=quantile, probs=1-(1-CI)/2))) # upper bound
  colnames(moments[[par0]]) <- c('mean','median','lb','ub')
}
moments[['iota']][ii_mis_n,] <- NA

# Malmquist check
if (include_check) {
  check_mlq = sum(abs(moments$mlq_check - moments$mlq))
  if (check_mlq>1e-6) {
    print(paste0("CHECK MALMQUIST INDEX"))
  }
}

# Computes density of some parameters
dens <- list()
dens[['slope']] <- density(draws$slope, from=alpha_prior[1], to=alpha_prior[2])
if (include_full) {
  dens[['gamma']] <- density(draws$gamma, from=-.2, to=.2)
  dens[['omega']] <- density(draws$omega, from=-.2, to=.2)
}
if (include_K==0) dens[['alpha']] <- density(draws$alpha, from=alpha_prior[1], to=alpha_prior[2])
if (include_K==1) {
  dens[['alpha']] <- density(draws$alpha[,1], from=alpha_prior[1], to=alpha_prior[2])
  dens[['beta']] <- density(draws$alpha[,2], from=alpha_prior[1], to=alpha_prior[2])
  dens[['delta']] <- density(draws$delta, from=0, to=.2)
}

# Other series for comparison
base <- 1300
a_allen <- a_allen - a_allen[which(D==base)] + moments$atilde[which(D==base),'mean']
base <- 1600
a_clark <- a_clark - a_clark[which(D==base)] + moments$atilde[which(D==base),'mean']
a_clark16 <- a_clark16 - a_clark16[which(D==base)] + moments$atilde[which(D==base),'mean']
#lp_wrigley <- interp1(D,lp_wrigley) - lp_wrigley[which(D==base)] + moments$atilde[which(D==base),'mean']
lp_wrigley <- lp_wrigley - lp_wrigley[which(D==base)] + moments$atilde[which(D==base),'mean']
base <- 1600
#lp_allen <- interp1(D,lp_allen) - lp_allen[which(D==base)] + moments$atilde[which(D==base),'mean']
lp_allen <- lp_allen - lp_allen[which(D==base)] + moments$atilde[which(D==base),'mean']

# Data frame for charts
chart <- data.frame(decade = D, moments[lat_list],
                    a_allen = a_allen, a_clark = a_clark, a_clark16 = a_clark16,
                    lp_wrigley = lp_wrigley, lp_allen = lp_allen,
                    N_broad = N_broad, N_clark07 = N_clark07, N_clark10 = N_clark10)
if (include_full==0) chart <- data.frame(chart, a_raw=a_raw)

# Population IRF: figure 16
if (include_full) {
  irf2_axis <- seq(from=1750,to=1860,by=10)
  chart_irf2 <- data.frame(axis = irf2_axis, actual=moments$N[which(D==1750):T,'mean'],
                           moments$N_irf2)
}

################################################################################
### SAVE RESULTS
################################################################################

# Trace plots (to check convergence)
setwd(path_tplots)
plotx <- traceplot(malthus_fit, pars = c('alpha','piv')) +
  scale_x_continuous(breaks = seq(iters/2,iters,by = iters/2)) +
  theme(text = element_text(size = 10, family = font_fam))
ggsave(paste0('malthus',nt,'_trace.png'),  plot=plotx, dpi=100, width=6.5, height=7, units='in')
rm(plotx)

# Results
rm('draws')
rm('malthus_fit')
setwd(path_rdata)
save(list = ls(all.names = TRUE), file = paste0('draws',nt), envir = .GlobalEnv)
