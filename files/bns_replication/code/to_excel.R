################################################################################
# PRODUCES EXCEL FILE WITH RESULTS POSTED ON AUTHORS' WEBPAGES
# called by main_post.R
# uses results stored in ~/output/RData
################################################################################

setwd(path_rdata0)

# Data
file_xlsx <- 'bns_estimates.xlsx'
sheetNames <- c('simple','constant_alpha_beta','baseline',
                'variable_days','structural')
colnames0 <- c('mean','median','p5','p95')
k <- 0
for (nt in c(nt_smpl,nt_cnst,nt_bsl,nt_hw,nt_strct)) {
  
  k <- k + 1
  load(paste0("draws",nt))
  
  # time series data
  df_xlsx <- data.frame('decade'=D)
  if (nt==nt_smpl) {
    varlist <- c('mlq','N')
  } else {
    varlist <- c(varlist, c('k','rents'))
    if (include_Z) varlist <- c(varlist, c('alphat','betat'))
  }
  for (var in varlist) {
    var2 <- var
    if (length(colnames(moments[[var]]))==3) colnames(moments[[var]]) <- paste0(var2,'_',colnames0[c(1,3,4)])
    if (length(colnames(moments[[var]]))==4) colnames(moments[[var]]) <- paste0(var2,'_',colnames0)
    df_xlsx <- cbind(df_xlsx, moments[[var]])
  }
  
  # add probability
  df_p <- data.frame('decade'=D[Be[2,]], 'P_B1'=moments$piv[,'mean'])
  df_xlsx <-merge(df_xlsx, df_p, by='decade', all=TRUE)
  
  # build workbook
  if (k==1) wb <- buildWorkbook(df_xlsx, sheetName=sheetNames[k], row.names=FALSE)
  else {
    addWorksheet(wb, sheetName=sheetNames[k])
    writeData(wb, sheet=sheetNames[k], x=df_xlsx)
  }
  addStyle(wb, sheet=sheetNames[k], style=createStyle(numFmt="0.00"), rows=2:(dim(df_xlsx)[1]+1), cols=2:dim(df_xlsx)[2], gridExpand=TRUE)
  
}

# Notes
col1 <- c('Productivity, population, capital, rent and parameter estimates from "When Did Growth Begin?"',
          paste0('This version: ',Sys.Date()),
          NA,
          'SHEETS', sheetNames, '',
          'VARIABLES', 'mlq', 'N', 'k', 's', 'alpha_t', 'beta_t', 'P_B1', '',
          'POSTERIOR MOMENTS', 'mean', 'median', 'p5', 'p95')
col2 <- c(NA, NA, NA, NA,
          'Simple (section 2)',
          'Constant \u03B1, \u03B2 (figures 10, A.2)',
          'Baseline (changing \u03B1, \u03B2) (figures 10, 13, A.2)',
          'Variable days worked (figures 13, A.2)',
          'Structural slope (figures 13, A.2)',
          NA,NA,
          'Permanent component of the Malmquist index (natural logarithm scale, 1250 normalized to 0)',
          'Population (million)',
          'Capital (natural logarithm scale, 1250 normalized to 0)',
          'Rents (natural logarithm scale, 1250 normalized to 0)',
          'Land parameter',
          'Capital parameter',
          'Probability of first break',
          NA,NA,
          'Mean',
          'Median',
          '5th percentile',
          '95th percentile')
notesheet <- data.frame(col1, col2)

# Excel file
setwd(path_output0)
addWorksheet(wb, sheetName='notes')
writeData(wb, sheet='notes', x=notesheet, colNames=FALSE)
saveWorkbook(wb, file_xlsx, overwrite=TRUE)
