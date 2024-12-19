################################################################################
# FUNCTIONS WHICH PRODUCE PLOTS
# called by main_estimation.R and main_post.R
################################################################################

# Plots single time series
ts_chart <- function(df, yvar, xaxisby) {
  
  # Inputs:
    # df: dataframe for chart with fields xaxis and yvar
    # yvar: variable to be plotted
    # xaxisby: space between ticks on xaxis
  # Output: plotx
  
  xaxis <- df['xaxis']
  baxis <- seq(from=min(xaxis),to=max(xaxis),by=xaxisby)
  plotx <- ggplot(df, aes_string(x='xaxis', y=yvar)) +
    mytheme +
    geom_line(size=thick_line) +
    scale_x_continuous(breaks = baxis, labels = baxis, position = 'bottom', expand = c(.01,0))
  return(plotx)
  
}

# Plots time series with confidence interval
ci_chart <- function(df, xaxisby, line='mean') {
  
  # Inputs:
    # df: dataframe for chart with fields xaxis, mean/median, lb and ub (lower and upper bounds)
    # xaxisby: space between ticks on xaxis
  # Output: plotx
  
  xaxis <- df['xaxis']
  baxis <- seq(from=min(xaxis),to=max(xaxis),by=xaxisby)
  plotx <- ggplot(df, aes_string(x='xaxis', y=line, ymin='lb', ymax='ub')) +
    geom_ribbon(fill='grey80') + geom_line(size=thick_line) +
    scale_x_continuous(breaks = baxis, labels = baxis, position = 'bottom', expand = c(.01,0)) +
    mytheme
  return(plotx)
  
}

# Plots time series with confidence interval, and compares it to another time series (actual)
comp_ci_chart <- function(df, lab, line='mean') {
  
  # Inputs:
    # df: dataframe for chart with fields xaxis, mean, lb and ub (lower and upper bounds), actual
    # lab: labels for mean and actual
  # Output: plotx
  
  plotx <- ggplot(df, aes(axis)) +
    geom_ribbon(aes(ymin=lb, ymax=ub), fill='grey80')
  if (line=='mean') plotx <- plotx + geom_line(aes(y=mean, linetype=lab[1]), size=thick_line)
  if (line=='median') plotx <- plotx + geom_line(aes(y=median, linetype=lab[1]), size=thick_line)
  plotx <- plotx +
    geom_line(aes(y=actual, linetype=lab[2]), size=thick_line) + mytheme +
    theme(axis.title = element_blank(), legend.position = 'bottom',
        legend.key.width = unit(2,'line'), legend.margin=margin(t=0, r=0, b=0, l=0),
        legend.title = element_blank(), legend.key = element_blank()) +
    scale_x_continuous(position = 'bottom', expand = c(.01,0)) +
    scale_colour_manual('Black') + scale_linetype_manual(values = c('dashed','solid'))
  
}

# Plots several time series
comp_chart <- function(df, xaxisby) {
  
  # Inputs:
    # df: dataframe for chart with fields xaxis and some variables
      # note: all variables in df will be plotted
    # xaxisby: space between ticks on xaxis
  # Output: plotx
  
  # Some series are not continuous and needs to show as dots
  shape2 <- shape0
  typ2 <- typ0
  for (series in c('Wrigley (1985)', 'Allen (2000)','Allen (2005)','Return on land',
                   'Return on rent charges')) {
    shape2[which(colnames(df)==series)-1] <- 18
    typ2[which(colnames(df)==series)-1] <- 'blank'
  }
  shape2[which(colnames(df)=='Return on land')-1] <- 19
  mytyp2 <- scale_linetype_manual(values = typ2)
  myshape2 <- scale_shape_manual(values = shape2)
  
  # Chart
  xaxis <- df['xaxis']
  sub_chart <- melt(df, id='xaxis') # reformats data frame
  baxis <- seq(from=min(xaxis),to=max(xaxis),by=xaxisby)
  plotx <- ggplot(sub_chart, aes(x=xaxis, y=value, color=variable, linetype=variable, shape=variable)) +
    mytheme + geom_point(size=thick_point) + geom_path(size=thick_line) +
    theme(axis.title = element_blank(), legend.position = 'bottom',
          legend.key.width = unit(2,'line'), legend.margin=margin(t=0, r=0, b=2, l=0),
          legend.title = element_blank(), legend.key = element_blank()) +
    scale_x_continuous(breaks = baxis, labels = baxis, position = 'bottom', expand = c(.01,0)) +
    mycol + mytyp2 + myshape2
  if (dim(df)[2]>3) {
    plotx <- plotx + theme(legend.spacing.y = unit(.1,'cm')) +
      guides(color=guide_legend(nrow=2,byrow=TRUE))
  }
  return(plotx)
  
}

# Plots prior and posterior
pp_chart <- function(df, xaxis_bl) {
  
  # Inputs:
    # df: dataframe for chart with fields xaxis, prior, posterior
    # xaxis_bl: breaks and labels for x axis
  # Output: plotx
  
  xaxis <- df['xaxis']
  sub_chart <- melt(pp_df[[par0]], id='xaxis')
  plotx <- ggplot(sub_chart, aes(x=xaxis, y=value, color=variable, linetype=variable)) +
    mytheme + geom_line(size=thick_line) +
    theme(axis.title = element_blank(), legend.position = 'bottom',
          legend.key.width = unit(2,'line'), legend.margin=margin(t=0, r=0, b=0, l=0),
          legend.title = element_blank(), legend.key = element_blank()) +
    scale_x_continuous(breaks=xaxis_bl, labels=xaxis_bl, position = 'bottom', expand = c(.01,0)) +
    scale_colour_manual(values = c('grey70','Black')) + scale_linetype_manual(values = c('dashed','solid'))
  
  return(plotx)
  
}

# Chart labels
label_chart <- function(df, include_apar, bef_iv, aft_iv) {
  
  # Labels
  df$label <- NA
  label_dates <- c(1250,1300,1450,1600,1730,1800,1860)
  if (include_apar==0)
    label_dates <- sort(c(label_dates, c(bef_iv,aft_iv)))
  df[which(df$decade %in% label_dates),'label'] <- df[which(df$decade %in% label_dates),'decade']
  
  # Nudge for labels
  df$nudge_x <- 0
  df$nudge_y <- 0
  
  return(df)
  
}

# Plots unicolor Malthus curve
umc_chart <- function(df,var,lab) {
  
  # Inputs
    # df: data frame containing data
    # var: vector containing name of x and y
    # labels: 
  # Output: chart
  
  plotx <- mmc_chart(df=df,var=var,lab=lab,Be=c(1,T+1)) # special case of mmc_chart
  return(plotx)
  
}

# Plots multicolor Malthus curve
mmc_chart <- function(df, var, lab, Be) {
  
  # Inputs
  # df: data frame containing data
  # var: vector containing name of x and y variables
  # labels: labels of x and y variables
  # Be: color breaks (same format as Be in the main code)
  # Output: chart
  
  var0 <- str_remove_all(var,'`') # to avoid ` in legend
  mmc_col <- c("gray40","gray80","Black",'red') # colors
  if (length(Be)==2) mmc_col <- 'Black'
  
  # Periods for color
  df['periods'] <- NA # for dots
  if (is.null(dim(Be))) {
    dimBe1 <- length(Be)
    J <- sum(Be<size(df)[1])
  } else {
    dimBe1 = dim(Be)[1]
    J <- sum(Be[,1]<size(df)[1])
  }
  for (j in 1:J) {
    per <- Be[j]:(Be[j+1]-1)
    df[paste0('y',j)] <- NA
    df[per,paste0('y',j)] <- df[per,var0[2]] # for line
    if (j>1) df[per[1]-1,paste0('y',j)] <- df[per[1]-1,var0[2]]
    df[per,'periods'] <- paste0('p',j)
  }
  
  # Chart
  plotx <- ggplot(df, aes_string(x=var[1], y=var[2], col='periods')) +
    mytheme + labs(x=lab[1], y=lab[2]) + geom_point(size=thick_point) + scale_colour_manual(values = mmc_col)
  for (j in 1:J) {
    plotx <- plotx + geom_path(aes_string(y=paste0('y',j)), size=thick_line, col=mmc_col[j])
  }
  plotx <- plotx + geom_text(aes(label=label), nudge_x=df$nudge_x, nudge_y=df$nudge_y, colour='black', size=label_size, family=font_fam, check_overlap=TRUE) +
    guides(col=FALSE, shape=FALSE, size=FALSE)
  return(plotx)
  
}

# Saves chart
save_chart <- function(plotx, name) {
  
  # Inputs:
    # plotx: plot to be saved
    # name: name of plot
  
  nm <- paste0(name,'.png')
  ggsave(nm, plot=plotx,
         width=chart_width, height=chart_height, units='in')
  if (!is.null(dev.list())) dev.off()
  
}
