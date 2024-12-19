################################################################################
# IRF FIGURES: 14, H1-4
# called by main_post.R
# uses results stored in ~/output/RData
################################################################################

nt <- nt_hw

# RData
setwd(path_rdata0)
load(paste0("draws",nt))

# Chart template and function
source(paste0(path_code0,'/chart_theme.R'), echo=FALSE)
source(paste0(path_code0,'/chart_functions.R'), echo=FALSE)

# IRF function
# see derivations in appendix H
irf3 <- function(alpha,beta,omega,phi,mu_a,p1,betap1,betap2,gamma,delta,T2) {
  
  # Inputs:
    # parameters: notations of the paper
    # T2: horizon of simulation
  # Outputs: list of dataframes with results of simulation
  
  # Kappa
  n_alpha <- length(alpha)
  n_mu <- length(mu_a)
  n2 <- max(n_alpha,n_mu)
  kappa <- rep(0, length=n2)
  mumkappa <- kappa
  phiv <- kappa
  for (j in 1:n2) {
    phiv[j] <- beta/(1-beta)*log(beta) + log(1-alpha[min(j,n_alpha)]-beta)
    kappa[j] <- alpha[min(j,n_alpha)]*(omega + gamma*(phiv[j] + (1-alpha[min(j,n_alpha)]-beta)/(1-beta)*moments$h[Be[2],'mean'])
                                       - beta*gamma/(1-beta)*log(moments$r[Be[2],'mean'] + delta)
                                       + p1*(digamma(betap1)-digamma(betap1+betap2)))
    mumkappa[j] <- mu_a[min(j,n_mu)] - kappa[j]
  }
  
  # Simulation
  a_n <- array(0, dim=c(T2,n2)) # a-alpha*n
  w <- a_n # w
  dw <- a_n # w_t - w_{t-1}
  W <- a_n # W
  n <- a_n  # only meaningful if mu_a = 0
  n3 <- n
  N <- a_n # N
  n[1,] <- kappa[1]*(1-beta)/(alpha[1]*gamma)/alpha[1]
  a_n[1,] <- (alpha-alpha[1])*(moments$h[Be[2],'mean'] + n[1,]) - alpha*n[1,]
  #a_n[1,] <- - alpha*n[1,]
  w[1,] <- phiv + 1/(1-beta)*(a_n[1,] - alpha*moments$h[Be[2],'mean']) - beta/(1-beta)*log(moments$r[Be[2],'mean'] + delta)
  dw[1,] <- w[1,] - w[1,1]
  for (t in 1:T2) {
    if (t>1) {
      n[t,] <- n[t-1,] + omega + gamma*(w[t-1,] + moments$h[Be[2],'mean']) + p1*(digamma(betap1)-digamma(betap1+betap2))
      a_n[t,] <- mumkappa + (1-alpha*gamma-beta)/(1-beta)*a_n[t-1,]
      w[t,] <- phiv + 1/(1-beta)*(a_n[t,] - alpha*moments$h[Be[2],'mean']) - beta/(1-beta)*log(moments$r[Be[2],'mean'] + delta)
      dw[t,] <- w[t,]-w[t-1,]
    }
    n3[t,] <- -a_n[t,]/alpha
    W[t,] = exp(w[t,]-w[1,1])
    N[t,] <- exp(n[t,] - n[1,])
  }
  
  # Results in data frames
  irf_axis = 10*(0:(T2-1))
  df_list <- list()
  df_list[['a_n']] <- data.frame(a_n)
  df_list[['w']] <- data.frame(w)
  df_list[['W']] <- data.frame(W)
  df_list[['dw']] <- data.frame(dw)
  df_list[['n']] <- data.frame(n)
  df_list[['N']] <- data.frame(N)
  for (var in names(df_list)) {
    df_list[[var]] <- data.frame(irf_axis,df_list[[var]])
    if (n_mu>1) colnames(df_list[[var]]) <- c('xaxis',paste0("\u03BC\u2090=",round(mu_a,2)))
    if (n_alpha>1) colnames(df_list[[var]]) <- c('xaxis',paste0("\u03B1=",round(alpha,2)))
  }
  df_list[['mumkappa']] <- mumkappa
  df_list[['kappa']] <- kappa
  df_list[['dw']] <- df_list[['dw']][2:51,]
  return(df_list)
  
}

# Parameters
T2 <- 101
T3 <- 10^5+1
alpha <- malthus_sum_full$summary[paste0("alphat[",1,"]"),"mean"]
beta <- malthus_sum_full$summary[paste0("betat[",1,"]"),"mean"]
gamma <- malthus_sum_full$summary["gamma[1]","mean"]
omega <- malthus_sum_full$summary["omega[1]","mean"]
delta <- malthus_sum_full$summary["delta[1]","mean"]
phi <- malthus_sum_full$summary["phi[1]","mean"]
p1 <- malthus_sum_full$summary["p[1]","mean"]
betap1 <- malthus_sum_full$summary["betap[1]","mean"]
betap2 <- malthus_sum_full$summary["betap[2]","mean"]
yaxis <- c(1,cumprod(rep(10,5)))

# Chart: mu_a in level
setwd(path_charts0)
mu_a <- c(.01,malthus_sum_full$summary["mu_a[2]","mean"],
          .05, malthus_sum_full$summary["mu_a[3]","mean"], .1,.2)
mu_a <- sort(mu_a)
df_list <- irf3(alpha,beta,omega,phi,mu_a,p1,betap1,betap2,gamma,delta,T3)
kappa <- df_list['kappa']$kappa
mumkappa <- df_list['mumkappa']$mumkappa

# Check IRF converges to formula
print(paste0('Should be 0: ',round(sum(abs(exp(mu_a/alpha/gamma) - df_list[['W']][T3,1+(1:length(mu_a))])))))
print(paste0('Should be 0: ',round(sum(abs((mu_a-kappa)*(1-beta)/alpha/gamma - df_list[['a_n']][T3,1+(1:length(mu_a))])))))

# Figure 14
yaxis2 <- c(1,cumprod(rep(4,5)))
if (include_hme==0) {
  alphav <- seq(0,2,by=.0001)
  xaxisby0 <- .2
} else {
  alphav <- seq(0,.5,by=.0001)
  xaxisby0 <- .05
}
ssW <- matrix(0,length(alphav),length(mu_a)+1)
ssW[,1] <- alphav
for (j in 2:dim(ssW)[2]) {
  ssW[,j] <- exp(mu_a[j-1]/alphav/gamma)
}
ssW <- data.frame(ssW)
colnames(ssW) <- c('xaxis',paste0("\u03BC\u2090=",round(mu_a,2)))
plotx <- comp_chart(df=ssW, xaxisby=xaxisby0) +
  scale_y_continuous(breaks=yaxis2, trans='log2', limits=c(1,1000)) +
  theme(axis.title.x=element_text(size=10, family=font_fam, face='plain'),
        axis.title.y=element_text(size=10, family=font_fam, face='plain')) +
  labs(x='\u03b1', y='Ratio')
print(plotx)
save_chart(plotx, 'bns_fig14')

# Figures H1-2
for (var in c('dw','W')) {
  if (dim(df_list[[var]])[1]>T2) df_list[[var]] <- df_list[[var]][1:T2,]
  plotx <- comp_chart(df=df_list[[var]], xaxisby=100)
  if (var=='dw') {
    plotx <- plotx + scale_x_continuous(breaks=seq(from=100, to=500, by=100), expand=c(.01,0))
    save_chart(plotx, 'bns_figH1')
  }
  if (var=='W') {
    plotx <- plotx + scale_y_continuous(breaks=yaxis, trans='log2') # log scale
    save_chart(plotx, 'bns_figH2')
  }
}

# Changing alpha
alphav <- c(moments$alphat[1,1], .4, .3, .25, moments$alphat[T,1])
df_list2 <- irf3(alphav,beta,omega,phi,0,p1,betap1,betap2,gamma,delta,T3)
kappav <- df_list2['kappa']$kappa

# Check IRF converges to formula
print(paste0('Should be 0: ',round(sum(abs(exp(0/alphav/gamma) - df_list2[['W']][T3,1+(1:length(alphav))])))))
print(paste0('Should be 0: ',round(sum(abs((0-kappav)*(1-beta)/alphav/gamma - df_list2[['a_n']][T3,1+(1:length(alphav))])))))
print(paste0('Should be 0: ',
             round(sum(abs(
               exp(kappav*(1-beta)/alphav^2/gamma + (alphav-alpha[1])*(moments$h[Be[2],'mean'] + df_list2[['n']][1,2])/alphav)
               /exp(kappav[1]*(1-beta)/alphav[1]^2/gamma)
               - df_list2[['N']][T3,1+(1:length(alphav))])))
))

# Figures H3-4
for (var in c('W','N')) {
  df_list2[[var]] <- df_list2[[var]][1:T2,-2]
  if (dim(df_list2[[var]])[1]>T2) df_list2[[var]] <- df_list2[[var]][1:T2,]
  plotx <- comp_chart(df=df_list2[[var]], xaxisby=100)
  if (var=='W') {
    plotx <- plotx # log scale
    save_chart(plotx, 'bns_figH3')
  }
  if (var=='N') {
    plotx <- plotx + scale_y_continuous(breaks=seq(from=1,to=10,by=3)) # log scale
    save_chart(plotx, 'bns_figH4')
  }
}
