################################################################################
# FIGURES BASED ON SEVERAL SPECIFICATIONS: 10, 13, A2, A5, A6, A7, A8
# called by main_post.R
# uses results stored in ~/output/RData
################################################################################

# x-axis
D <- seq(from=1250,to=1860,by=10)

# specifications
spec_a <- list(c(101,103,105),       #fig10
              c(105,106,205),        #fig13
              c(205,231:235),        #figA5
              c(105,121),            #figA6
              c(111:116),            #figA7
              c(105,122,123))        #figA8
spec_pi <- list(c(103,105,106,205))  #figA2

# legends
legend_a <- list(c('Simple', 'Constant \u03B1, \u03B2',                         #fig10
                  'Baseline (changing \u03B1, \u03B2)'),
                c('Baseline', 'Variable days worked', 'Structural slope'),      #fig13
                c('Builders (baseline)', 'Farmers', 'Craftsmen', 'Allen (2007)',
                  '3 series', 'Annual income'),                                 #figA5
                c('Clark (baseline)', 'Broadberry et al.'),                     #figA6
                c('B1=1550', 'B1=1600', 'B1=1650', 'B1=1700',
                  'B1=1750', 'B1=1800'),                                        #figA7
                c('Baseline', 'Productivity shocks', 'Population'))             #figA8
legend_pi <- list(c('Constant \u03B1, \u03B2',                                  #figA2
                    'Baseline (changing \u03B1, \u03B2)',
                    'Baseline (changing \u03B1, \u03B2) + variable days worked',
                    'Baseline (changing \u03B1, \u03B2) + structural'))

# productivity figure numbers
nfig_a <- c('10', '13', 'A5', 'A6', 'A7', 'A8')
nfig_pi <- c('A2')

# Productivity results
df_a <- list()
for (k in 1:length(spec_a)) {
  
  # Data frames
  df_a[[k]] <- data.frame(D)
  
  for (nt in spec_a[[k]]) {
    
    # Data
    load(paste0(path_rdata0,"/draws",nt))
    
    # Warning message to ensure run has enough
    if (iters<10^6) print(paste0('Not enough draws in specification ',nt,': iters=',iters))
    
    # Productivity and population
    df_a[[k]] <- data.frame(df_a[[k]], moments$mlq[,'mean'])
    
  }
  
  # For legend
  colnames(df_a[[k]]) <- c('xaxis', legend_a[[k]])
  
  # renormalization for builders/farmers picture
  if (nfig_a[k]=='A5') {
    norm_pos <- which(df_a[[k]]['xaxis']==1600)
    norm <- as.numeric(df_a[[k]][norm_pos,legend_a[[k]][1]] - df_a[[k]][norm_pos,legend_a[[k]]])
    df_a[[k]][legend_a[[k]]] <- df_a[[k]][legend_a[[k]]] + matrix(1,dim(df_a[[k]])[1],1)%*%norm
  }

}

# Probability results
df_pi <- list()
for (k in 1:length(spec_pi)) {
  
  for (nt in spec_pi[[k]]) {
    
    # Data
    load(paste0(path_rdata0,"/draws",nt))
    if (nt==spec_pi[[k]][1]) {
      df_pi[[k]] <- data.frame(D[Be[2,]])
      colnames(df_pi[[k]]) <- 'xaxis'
    }
    
    # Warning message to ensure run has enough
    if (iters<10^6) print(paste0('Not enough draws in specification ',nt,': iters=',iters))

    # Break proba
    df_pi0 <- data.frame(D[Be[2,]], moments$piv[,'mean'])
    df_pi0 <- aggregate(df_pi0[,2], list(df_pi0[,1]), sum)
    colnames(df_pi0) <- c('xaxis',nt)
    df_pi[[k]] <- merge(df_pi[[k]], df_pi0, by='xaxis', all=TRUE)
    
  }
  
  # For legend
  colnames(df_pi[[k]]) <- c('xaxis', legend_pi[[k]])
  
  # Delete specification with single possible break
  df_pi[[k]] <- df_pi[[k]][colSums(!is.na(df_pi[[k]]))>1]
  df_pi[[k]][is.na(df_pi[[k]])] <- 0
  
}

### CHARTS
source(paste0(path_code0,'/chart_theme.R'), echo=FALSE)
source(paste0(path_code0,'/chart_functions.R'), echo=FALSE)
setwd(path_charts0)

# Productivity
for (k in 1:length(spec_a)) {
  plotx <- comp_chart(df_a[[k]],50)
  ggsave(paste0('bns_fig',nfig_a[k],'.png'), plot=plotx,
         width=chart_width, height=chart_height, units="in")
}

# Break proba
for (k in 1:length(spec_pi)) {
  plotx <- comp_chart(df_pi[[k]],50)
  ggsave(paste0('bns_fig',nfig_pi[k],'.png'), plot=plotx,
         width=chart_width, height=chart_height, units="in")
}
