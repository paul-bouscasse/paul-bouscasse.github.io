################################################################################
# FIGURES BASED ON SINGLE SPECIFICATION
# called by main_post.R
# uses results stored in ~/output/RData
################################################################################

setwd(path_charts0)

################################################################################
### Figures based on baseline specification: 1, 2, 11, 17, A4, A9, A10
################################################################################

# results from baseline specification
load(paste0(path_rdata0,'/draws',nt_bsl))
source(paste0(path_code0,'/chart_theme.R'))
source(paste0(path_code0,'/chart_functions.R'))

# Figure 1
comp_df <- list() # list definition
a_clark16 <- a_clark16 - a_clark16[which(D==1300)] + moments$mlq[which(D==1300),'mean']
comp_df[['tfp']] <- data.frame(D, moments$mlq[,'mean'], a_allen, a_clark16)
colnames(comp_df[['tfp']]) <- c('xaxis', 'Our baseline', 'Allen (2005)', 'Clark (2016)*')
plotx <- comp_chart(df=comp_df[['tfp']], xaxisby=50)
save_chart(plotx,'bns_fig1')

# Figure 2

# data
Be2 <- Be
colsel <- D[Be[2,]]==1600 & D[Be[3,]]==1810
if (length(colsel)>1) {
  if (sum(colsel)>0) Be2 <- matrix(Be[,colsel], 4, 1)
}
chart['l.mean'] <- chart['h.mean'] + chart['n.mean']

# adjustments to label positions
chart <- label_chart(chart, include_apar, bef_iv, aft_iv)
chart[chart$decade==1250,'nudge_x'] <- -.01
chart[chart$decade==1250,'nudge_y'] <- -.05
chart[chart$decade==1340,'nudge_x'] <- -.1
chart[chart$decade==1340,'nudge_y'] <- -.02
chart[chart$decade==1360,'nudge_x'] <- -.12
chart[chart$decade==1360,'nudge_y'] <- 0
chart[chart$decade==1600,'nudge_x'] <- -.02
chart[which(chart$decade %in% c(1300,1450,1800,1860)),'nudge_x'] <- .1
chart[chart$decade==1730,'nudge_x'] <- 0
chart[which(chart$decade %in% c(1250,1600,1730)),'nudge_y'] <- .045

# plot
plotx <- mmc_chart(df=chart, var=c('l.mean','w.mean'),
                   lab=c("Log labor supply","Log real wages"), Be=Be2)
save_chart(plotx, 'bns_fig2')

# Figure 11
iab <- D>=1700
chart1 <- ci_chart(df=data.frame(xaxis=D[iab], moments[['alphat']][iab,c('mean','lb','ub')]), xaxisby=50) +
  ggtitle(TeX("$\\alpha_t$")) + tit_theme + ylim(0,.7)
chart2 <- ci_chart(df=data.frame(xaxis=D[iab], moments[['betat']][iab,c('mean','lb','ub')]), xaxisby=50) +
  ggtitle(TeX("$\\beta_t$")) + tit_theme + ylim(0,.7)
plotx <- chart1 + chart2
save_chart(plotx, 'bns_fig11')

# Figure 17
comp_df[['N']] <- data.frame(D, moments$N[,'mean'], N_clark07, N_clark10,N_broad)
comp_df[['N']] <- comp_df[['N']][1:which(D==1540),]
colnames(comp_df[['N']]) <- c('xaxis',paste0('Spec. ',nt), 'Clark (2007)', 'Clark (2010)','Broadberry et al. (2015)')
colnames(comp_df[['N']])[2] <- 'Our baseline'
plotx <- comp_chart(df=comp_df[['N']], xaxisby=50)
save_chart(plotx,'bns_fig17')

# Figure A.4
chart1 <- ci_chart(df=data.frame(xaxis=D, moments[['k']][,c('mean','lb','ub')]), xaxisby=100) +
  ggtitle(TeX("Capital")) + tit_theme + ylim(-1,3)
chart2 <- ci_chart(df=data.frame(xaxis=D, moments[['rents']][,c('mean','lb','ub')]), xaxisby=100) +
  ggtitle(TeX("Rents")) + tit_theme + ylim(-1,3)
plotx <- chart1 + chart2
save_chart(plotx, 'bns_figA4')

# Figure A.9
chart_df <- data.frame(xaxis=D, moments[['xi']][,c('mean','lb','ub')])
plotx <- ci_chart(df=chart_df, xaxisby=50, 'mean')
save_chart(plotx, 'bns_figA9')

# Figure A.10
chart_df <- data.frame(xaxis=D, moments[['iota']][,c('mean','lb','ub')])
plotx <- ci_chart(df=chart_df, xaxisby=50, 'mean')
save_chart(plotx, 'bns_figA10')

################################################################################
### Figures based on simple specification: 5, 6, 7
################################################################################

# Results from simple specification
load(paste0(path_rdata0,'/draws',nt_smpl))
source(paste0(path_code0,'/chart_theme.R'))
source(paste0(path_code0,'/chart_functions.R'))

# Figure 5
breaks <- unique(D[Be[2,]])
chart_df <- data.frame(xaxis=breaks, mean=moments[['piv']][,'mean'])
plotx <- ts_chart(df=chart_df, 'mean', xaxisby=50) + ylim(0,.2)
save_chart(plotx, 'bns_fig5')

# Figure 6
chart_df <- data.frame(xaxis=D, moments[['mlq']][,c('mean','lb','ub')])
plotx <- ci_chart(df=chart_df, xaxisby=50, 'mean')
save_chart(plotx, 'bns_fig6')

# Figure 7
comp_df[['w']] <- data.frame(D, moments$mlq[,'mean'],  moments$w[,'mean']-moments$w[1,'mean'])
colnames(comp_df[['w']]) <- c('xaxis','Productivity', 'Real wages')
plotx <- comp_chart(df=comp_df[['w']], xaxisby=50)
save_chart(plotx, 'bns_fig7')

################################################################################
### Figures based on variable days specification: 15, 16
################################################################################

# Results from simple specification
load(paste0(path_rdata0,'/draws',nt_hw))
source(paste0(path_code0,'/chart_theme.R'))
source(paste0(path_code0,'/chart_functions.R'))

# Figure 15
moments[['W_ss_oW']] <- 1/moments[['WoW_ss']] # W_ss/W = 1/(W/W_ss)
chart_df <- data.frame(xaxis=D, moments[['W_ss_oW']][,c('median','lb','ub')])
plotx <- ci_chart(df=chart_df, xaxisby=50, line='median') + scale_y_continuous(trans='log10')
save_chart(plotx, 'bns_fig15')

# Figure 16
plotx <- comp_ci_chart(df=chart_irf2, lab=c('Predicted','Actual'))
save_chart(plotx, 'bns_fig16')
