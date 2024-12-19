################################################################################
# FIGURES BASED ON RAW DATA
# called by main_post.R
# uses dataset stored in ~/data/transformed/malthus_data.csv
################################################################################

# Dates
start <- 1250
end <- 1860
D <- seq(from=start, to=end, by=10)
T <- length(D)
Be <- c(1, which(D==1450), which(D==1630), T+1)

# Birth/death data
bd_name <- paste0(path_raw0,'/MalthusFigures.xlsx')
bd_data <- read_excel(bd_name, sheet='Birth and Death Rates')
bd_data['decade'] <- floor(bd_data['Age']/10)*10
bd_data <- bd_data[c('decade','Birth Rate Combined','Death Rate Combined')]
bd_col <- c('xaxis','Birth rate (\U2030)','Death rate (\U2030)')
colnames(bd_data) <- bd_col
bd_data <- aggregate(bd_data, by=list(bd_data$xaxis), FUN=mean)
bd_data <- bd_data[!is.na(bd_data[3]),bd_col]

# Data
setwd(path_trans0)
data <- read.csv2('malthus_data.csv', header=TRUE, sep=',', fill=TRUE, fileEncoding='UTF-8-BOM') # imports data
data <- data[which(data['decade'] == start):which(data['decade'] == end),]
data2 <- read_excel(paste0(path_raw0,'/Wages 2014.xlsx'), sheet='Decadal')
data2 <- data2[which(data2['Decade'] == start):which(data2['Decade'] == end),]
df_w <- as.data.frame(data2[c('Building Laborers, d/day', 'Farm Laborers, d/day',
                              'Building Craftsmen, d/day', 'Cost of Living (1860s=100)')])
for (j in 1:3) {
  df_w[j] <- df_w[j]/df_w[4]
}
df_w <- df_w[1:3]
df_w <- df_w/df_w[dim(df_w)[1],1]*100
Wa <- as.numeric(unlist(data['allen_w']))
Wa[1:which(D==1760)] <- NA
D <- as.numeric(unlist(data['decade']))
N_trn <- as.numeric(unlist(data['pop_ind']))
N_obs <- as.numeric(unlist(data['wrigley_pop']))
N_trn <- N_trn/N_trn[which(D==1520)]*N_obs[which(D==1540)]
N_clark10 <- as.numeric(unlist(data['pop_10']))
H <- as.numeric(unlist(data['d_hw']))
W <- as.numeric(unlist(data['w_lab_10']))
INC <- as.numeric(unlist(data['inc_hw']))
a_clark <- log(as.numeric(unlist(data['efficiency_DE'])))
a_allen <- log(as.numeric(unlist(data['allen05_TFP_agriculture'])))
lp_wrigley <- log(as.numeric(unlist(data['wrigley_prod'])))
lp_allen <- log(as.numeric(unlist(data['allen_prod'])))
r <- as.numeric(unlist(data['interest_rate']))
r[is.na(r)] <- 5
r_mat_names <- c('r_land', 'r_rents')
nr <- length(r_mat_names)
r_mat <- matrix(0,T,nr)
r_mat0 <- rep(0,nr)
for (j in 1:nr) {
  r_mat[,j] <- as.numeric(unlist(data[,r_mat_names[j]]))
}
#r_mat[is.na(r_mat)] <- 5 # temporary
r_mat <- r_mat/100
rents <- as.numeric(unlist(data['rent_ind']))
Kg_feinstein <- as.numeric(unlist(data['capital_stock_gross'])) # gross capital estimates from Feinstein
Kn_feinstein <- as.numeric(unlist(data['capital_stock_net'])) # net capital estimates from Feinstein
kg_feinstein <- log(Kg_feinstein)
kn_feinstein <- log(Kn_feinstein)
rents2 <- rents
rents2 <- rents/rents[which(D==1860)]*100

# Transformation of the data
T <- length(D)
n_clark10 <- log(N_clark10*10^6)
w <- log(W)
inc <- log(INC)
a_clark <- a_clark - a_clark[1] # normalization

# Number for Allen
print('Number for Allen')
exp(a_allen[which(D==1850)] - a_allen[which(D==1500)]) - 1

# Data for charts
df_chart <- data.frame(D, n_clark10, w, inc)
  # first dataframe: decade, population (Clark 2010), real wages
for (series in list(lp_wrigley,lp_allen)) {
  series <- interp1(D,series) - series[min(which(!is.na(series)))] + a_clark[min(which(!is.na(series)))]
  df_chart <- data.frame(df_chart, series)
    # adds labor producitivy from Wrigley and Allen
    # normalized such that equal to Clark for the first observation
}
for (series in list(a_allen, a_clark)) {
  series <- series - series[min(which(!is.na(series)))] + a_clark[min(which(!is.na(series)))]
  df_chart <- data.frame(df_chart, series)
    # adds TFP from Allen and Clark to data frame
}
for (series in list(W, r, rents)) {
  series <- series/series[which(D==1860)]*100
  df_chart <- data.frame(df_chart, series)
    # adds price series
}
df_chart <- data.frame(df_chart, df_w, Wa)
df_chart <- data.frame(df_chart, N_trn, N_obs, H, r_mat[,1], r_mat[,2])
df_chart <- data.frame(df_chart, Kn_feinstein)
  # adds population trend, observations and days worked

# Column names
prod <- c("Wrigley (1985)", "Allen (2000)", "Allen (2005)", "Clark (2010)")
prices <- c("Wages", "Interest rates", "Rents")
pop <- c("Trend from Clark (2007b)","Population from Wrigley et al. (1997)")
wages <- c('Builders','Farmers','Craftsmen', 'Allen (2007)')
ir <- c("Return on land", "Return on rent charges")
legend <- c("xaxis", "Population", "Real wages", "Annual incomes", prod, prices, wages, pop, "Days worked", ir, "Capital")
colnames(df_chart) <- legend

##################################################################

### CHARTS
  # functions are defined in chart_functions.R

setwd(path_charts0)

# Wages
plotx <- ts_chart(df=df_chart, yvar='Wages', xaxisby=50)
save_chart(plotx,'bns_fig3')

# Population
plotx <- comp_chart(df=df_chart[c('xaxis',pop)], xaxisby=50)
save_chart(plotx,'bns_fig4')

# Interest rates
plotx <- comp_chart(df=df_chart[c('xaxis',ir)], xaxisby=50)
save_chart(plotx,'bns_fig8')

# Post 1760
df2 <- df_chart[D>=1760,c('xaxis','Rents','Capital')]
plot1 <- ts_chart(df=df2, yvar='Rents', xaxisby=50) + ggtitle('Rents') +
  theme(plot.margin = margin(5.5,7,5.5,5.5)) + tit_theme
plot2 <- ts_chart(df=df2, yvar='Capital', xaxisby=50) + ggtitle('Capital') +
  theme(plot.margin = margin(5.5,5,5.5,7.5)) + tit_theme
plotx <-  plot1 + plot2
save_chart(plotx,'bns_fig9')

# Days
plotx <- ts_chart(df=df_chart, yvar='`Days worked`', xaxisby=50)
save_chart(plotx,'bns_fig12')

# Wages
plotx <- comp_chart(df=df_chart[c('xaxis',wages)], xaxisby=50)
save_chart(plotx,'bns_figA1')
