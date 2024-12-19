################################################################################
# TABLES
# called by main_post.R
# uses results stored in ~/output/RData
################################################################################

################################################################################
### Tables 1 and 4
################################################################################

# Table 4
setwd(path_tables0)
sink(file = 'bns_tab4.tex') # open table file
cat('\\begin{tabular}{rrrrr} \n \\hline \\hline \n')
cat('& Mean & St Dev & \\hspace{5pt} 2.5\\% & 97.5\\% \\\\ \n \\hline \n')
for (nt in c(nt_smpl,nt_cnst,nt_bsl)) {
  
  # Load results
  load(paste0(path_rdata0,'/draws',nt))
  
  # Productivity tables
  if (size(Be)[2]>1) {
    s1 <- 1
    s2 <- 2
    s3 <- 3
  } else {
    s1 <- paste0('t<',D[Be[2]])
    s2 <- paste0(D[Be[2]],'\\leq t <',D[Be[3]])
    s3 <- paste0('t \\geq ',D[Be[3]])
  }
  rownames(malthus_sum_a$summary) <- c(paste0('$\\mu_{a,',s1,'}$'),paste0('$\\mu_{a,',s2,'}$'),
                                       paste0('$\\mu_{a,',s3,'}$'),
                                       paste0('$\\sigma_{\\epsilon_1,',s1,'}$'), paste0('$\\sigma_{\\epsilon_1,',s2,'}$'),
                                       paste0('$\\sigma_{\\epsilon_1,',s3,'}$'),
                                       paste0('$\\sigma_{\\epsilon_2,',s1,'}$'), paste0('$\\sigma_{\\epsilon_2,',s2,'}$'),
                                       paste0('$\\sigma_{\\epsilon_2,',s3,'}$'))
  
  # Subtitle
  if (nt==nt_smpl) {
    subtit <- 'Simple'
    tab1 <- malthus_sum_a$summary[1:3,c(1,3,4,8)]
  }
  if (nt==nt_cnst) subtit <- 'Constant $\\alpha$, $\\beta$'
  if (nt==nt_bsl) subtit <- 'Baseline'
    
  # Table
  cat(paste0('\\multicolumn{5}{c}{',subtit,'} \\\\ \n \\hline \n'))
  print.xtable(xtable(malthus_sum_a$summary[1:3,c(1,3,4,8)]),
               floating=FALSE, only.contents=TRUE,
               include.colnames=FALSE, hline.after=NULL,
               sanitize.rownames.function=identity)
  cat('[0.5em] \n \\hline \n')
  
}
cat('\\end{tabular} \n')
sink(file = NULL) # closes file

# Table 1 is the top of table 4
sink(file = paste0('bns_tab1.tex'))
cat('\\begin{tabular}{rrrrr} \n \\hline \\hline \n')
cat('& Mean & St Dev & \\hspace{5pt} 2.5\\% & 97.5\\% \\\\ \n \\hline \n')
print.xtable(xtable(tab1),
             floating=FALSE, only.contents=TRUE,
             include.colnames=FALSE, hline.after=NULL,
             sanitize.rownames.function=identity)
cat('[0.5em] \n \\hline \n')
cat('\\end{tabular} \n')
sink(file = NULL) # closes file

################################################################################
### Tables 2, 3
################################################################################

# Tables 2-3 are hard-coded: original file stored in ~/data/tables_hard_coded
for (j in 2:3) {
  file.copy(from = paste0(path_data0,'/tables_hard_coded/bns_tab',j,'.tex'),
            to = paste0(path_tables0,'/bns_tab',j,'.tex'), overwrite = TRUE)
}

################################################################################
### Table 5
################################################################################

setwd(path_rdata0)

# x-axis
D <- seq(from=1250,to=1860,by=10)

# gamma table
spec <- list(c(nt_bsl,nt_hw))
legend <- list(c('Baseline (constant days worked)', 'Variable days worked'))

# data
tab_gamma <- list()
for (k in 1:length(spec)) {
  for (nt in spec[[k]]) {
    
    # load data
    load(paste0("draws",nt))
    
    # gamma table
    if (nt==spec[[k]][1]) tab_gamma <- malthus_sum$summary["gamma[1]",c(1,3,4,8)]
    else tab_gamma <- rbind(tab_gamma, malthus_sum$summary["gamma[1]",c(1,3,4,8)])
    
  }
  
  # row names
  rownames(tab_gamma) <- legend[[k]]
  
}

# tables
setwd(path_tables0)
sink(file = paste0('bns_tab5.tex')) # open table file
cat('\\begin{tabular}{rrrrr} \n \\hline \\hline \n')
cat('& Mean & St Dev & \\hspace{5pt} 2.5\\% & 97.5\\% \\\\ \n \\hline \n')
print.xtable(xtable(tab_gamma),
             floating=FALSE, only.contents=TRUE,
             include.colnames=FALSE, hline.after=NULL,
             sanitize.rownames.function=identity)
cat('\\hline \n')
cat('\\end{tabular} \n')
sink(file = NULL) # closes file

################################################################################
### Table A1
################################################################################

# Load results
setwd(path_rdata0)
load(paste0('draws',nt_bsl))
setwd(path_tables0)

# row names for production parameters
rownames(malthus_sum_n1$summary) <- c('$\\alpha$','$\\beta$','$\\gamma$','$\\omega$')

# row names for sigmas
if (size(Be)[2]>1) {
  s1 <- 1
  s2 <- 2
  s3 <- 3
} else {
  s1 <- paste0('t<',D[Be[2]])
  s2 <- paste0(D[Be[2]],'\\leq t <',D[Be[3]])
  s3 <- paste0('t \\geq ',D[Be[3]])
}
rownames(malthus_sum_a$summary) <- c(paste0('$\\mu_{a,',s1,'}$'),paste0('$\\mu_{a,',s2,'}$'),
                                     paste0('$\\mu_{a,',s3,'}$'),
                                     paste0('$\\sigma_{\\epsilon_1,',s1,'}$'), paste0('$\\sigma_{\\epsilon_1,',s2,'}$'),
                                     paste0('$\\sigma_{\\epsilon_1,',s3,'}$'),
                                     paste0('$\\sigma_{\\epsilon_2,',s1,'}$'), paste0('$\\sigma_{\\epsilon_2,',s2,'}$'),
                                     paste0('$\\sigma_{\\epsilon_2,',s3,'}$'))

# row names for population parameters
rownames(malthus_sum_n2$summary) <- c('$\\pi_{t<1680}$','$\\pi_{t \\geq 1680}$',
                                      '$\\mu_{\\xi_1}$','$\\nu_{\\xi_1}$',
                                      '$\\sigma_{\\xi_2}$',
                                      '$\\sigma_{n,t<1540}$','$\\sigma_{n,t \\geq 1540}$',
                                      '$\\nu_{n,t<1540}$','$\\nu_{n,t \\geq 1540}$')

# variable days parameters (not used if table is produced for baseline)
if (include_hme) rownames(malthus_sum_h$summary) <- c('$\\sigma_h$','$\\tilde{\\sigma}_{h}$','$\\nu_{h}$')

# Table with all parameters
malthus_sum_par <- rbind(malthus_sum_n1$summary, malthus_sum_a$summary[4:9,],
                         malthus_sum_n2$summary)

# Create subtitles
addtorow <- list()
addtorow$pos <- list(0,4,10,15,19)
addtorow$command <- c('Main','Productivity Shock','Population','Population Measurement Error')
if (include_hme) {
  malthus_sum_par <- rbind(malthus_sum_par, malthus_sum_h$summary)
  addtorow$pos <- append(addtorow$pos,22)
  addtorow$command <- append(addtorow$command, 'Days Worked')
}
addtorow$command <- paste0('\\multicolumn{5}{l}{\\textit{',addtorow$command,' Parameters}} \\\\')
addtorow$command <- c(addtorow$command,'')
addtorow$command[2:length(addtorow$command)] <- paste0('[0.5em] ',addtorow$command[2:length(addtorow$command)])

# Save table
sink(file = paste0('bns_tabA1.tex'))
cat('\\begin{tabular}{rrrrr} \n \\hline \\hline \n')
cat('& Mean & St Dev & \\hspace{5pt} 2.5\\% & 97.5\\% \\\\ \n \\hline \n')
print.xtable(xtable(malthus_sum_par[,c(1,3,4,8)]),
             floating=FALSE, only.contents=TRUE,
             include.colnames=FALSE, hline.after=NULL,
             add.to.row = addtorow, sanitize.rownames.function=identity)
cat('\\hline \n')
cat('\\end{tabular} \n')
sink(file=NULL)
