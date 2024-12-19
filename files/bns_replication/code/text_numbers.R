################################################################################
# NUMBERS CITED IN THE TEXT
# called by main_post.R
# uses results stored in ~/output/RData
################################################################################

setwd(path_rdata0)

print('Introduction')
load(paste0("draws",nt_bsl))
round(exp(chart$a_allen[chart$decade==1850] - chart$a_allen[chart$decade==1500]) - 1,2)
round(exp(moments$mlq[which(D==1850)] - moments$mlq[which(D==1500)]) - 1,2)

print('Section 2.2')
load(paste0("draws",nt_smpl))
round(moments$alpha[,'mean'],2)

print('Section 2.3')
load(paste0("draws",nt_smpl))
round(sum(moments$piv[D[Be[2,]]<1640,'mean']),2)
round(sum(moments$piv[D[Be[2,]]<1680,'mean']),2)
round(moments$alpha[1,'mean']*0.6,1)

print('Section 2.5')
load(paste0("draws",nt_smpl))
round(exp(moments$mlq[which(D==1680),'mean'] - moments$mlq[which(D==1600),'mean']) - 1,2)
round(exp(moments$mlq[which(D==1640),'mean'] - moments$mlq[which(D==1600),'mean'])^(10/(1640-1600)) - 1,3)
round(exp(moments$mlq[which(D==1680),'mean'] - moments$mlq[which(D==1640),'mean'])^(10/(1680-1640)) - 1,3)
round(exp(moments$mlq[which(D==1810),'mean'] - moments$mlq[which(D==1680),'mean'])^(10/(1810-1680)) - 1,3)

print('Section 4.1')
load(paste0("draws",nt_bsl))
round(malthus_sum$summary[paste0('alpha[',c(1,2),']'),c('mean','sd')],2)
round(moments$alphat[T,'mean'],2)
round(moments$betat[T,'mean'],2)
round(1-moments$alphat[1,'mean']-moments$betat[1,'mean'],2)
round(1-moments$alphat[T,'mean']-moments$betat[T,'mean'],2)

print('Section 4.2')
load(paste0("draws",nt_hw))
print(round(malthus_sum_a$summary[paste0('mu_a[',1:3,']'),c('mean','sd')],2))

print('Section 5.1: constant days')
load(paste0("draws",nt_bsl))
round(100*(exp(moments$w[which(D==1440),'mean'] - moments$w[which(D==1270),'mean'])-1))
round(moments$w[which(D==1440),'mean'] - moments$w[which(D==1270),'mean'],2)
round(moments$gamma[,'mean'] * (moments$w[which(D==1440),'mean'] - moments$w[which(D==1270)]),2)

print('Section 5.1: HW days')
load(paste0("draws",nt_hw))
round(moments$gamma[,'mean'] * (moments$w[which(D==1440),'mean'] + moments$h[which(D==1440),'mean'] - moments$w[which(D==1270)] - moments$h[which(D==1270)]),2)
round(moments$gamma[,'mean'] * log(2),2)

print('Section 5.1: half life numbers')
for (nt in c(nt_bsl,nt_hw)) {
  load(paste0("draws",nt))
  for (t in c(1,T)) {
    x <- log(.5)/log((1-moments$alphat[t,"mean"]*malthus_sum_full$summary["gamma[1]","mean"]-moments$betat[t,"mean"])/(1-moments$betat[t,"mean"]))
    print(round(10*x))
  }
}

print('Section 5.2: steady state wage numbers')
load(paste0("draws",nt_hw))
for (t in c(1,T)) {
  print(round(moments$alphat[t,"mean"],2))
  print(round(exp(malthus_sum$summary['mu_a[2]','mean']/moments$alphat[t,"mean"]/moments$gamma[1,"mean"])))
  print(round(exp(malthus_sum$summary['mu_a[3]','mean']/moments$alphat[t,"mean"]/moments$gamma[1,"mean"])))
}
print(round(1/moments$WoW_ss[T,c('median')],2))

print('Section 5.3')
round(moments$N[which(D==1740),'mean'])
round(moments$N[which(D==1860),'mean'])
round((moments$N[which(D==1860),'mean']/moments$N[which(D==1740),'mean'])^(10/(1860-1740)) - 1,3)

print('Appendix H: decrease labor share')
setwd(path_rdata0)
load(paste0("draws",nt_hw))
labor_share <- rep(NA,T)
for (t in c(1,T)) {
  labor_share[t] <- (1 - malthus_sum_full$summary[paste0("alphat[",t,"]"),"mean"]
                     - malthus_sum_full$summary[paste0("betat[",1,"]"),"mean"])
  print(paste0('alpha[',t,']'))
  print(round(malthus_sum_full$summary[paste0("alphat[",t,"]"),"mean"],2))
  print(paste0('labor share'))
  print(round(labor_share[t],2))
}
print(round(labor_share[T] / labor_share[1] - 1, 2))
