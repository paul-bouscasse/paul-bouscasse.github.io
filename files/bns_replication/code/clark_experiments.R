################################################################################
# FIGURES FOR APPENDIX G
# called by main_post.R
# uses results stored in ~/output/RData
################################################################################

################################################################################
### CLARK SERIES
################################################################################

setwd(path_raw0)

# Import wage, rents and price from Clark 
factors <- read_xlsx('England NNI - Clark - 2015.xlsx',  sheet = 'Decadal') %>% 
  slice(-1) %>% 
  rename(rent_clark =  `Land rents`,
         wage_clark = `Male average Wage`,
         P_DE = `Price Index - Domestic Expenditure`) %>% 
  select(Decade, P_DE, rent_clark, wage_clark) %>% 
  mutate_all(~as.numeric(.))

# Import tax series from Clark
tax <- read_xlsx("clark10.xlsx", sheet = "clark10_t33") %>% 
  select(Decade, `Indirect Tax Share`) %>% rename(itax_share_clark = `Indirect Tax Share`)

# Import factor shares series from Clark
shares <- read_xlsx("clark10.xlsx", sheet = "clark10_t13") %>% 
  rename(
    wage_share_clark = Wage,
    land_share_clark = Land,
    capital_share_clark = Capital
  )

# Import interest rate series from Clark + efficiency index reported by Clark 
interest_rate <- read.csv(paste0(path_trans0, "/malthus_data.csv"), sep = ",") %>% 
  rename(Decade = decade,
         interest_clark = interest_rate) %>% 
  select(Decade, interest_clark, efficiency_DE) %>% 
  mutate(interest_clark = ifelse(is.na(interest_clark), 5, interest_clark)) # Same assumption as Clark for missing values 

# Clark (2016)
share_names <- paste0(c('wage','land','capital','itax'),'_share_clark16')
price_names <- paste0(c('wage','rent','interest'),'_clark16')
clark16_1 <- read_xlsx("GDP-Efficiency 2023.xlsx", sheet = "Efficiency", skip=1) %>%
  rename(efficiency_DE16 = `A-DE`, wage_clark16 = `real w`, rent_clark16 = `real rent`, interest_clark16 = `Cap Return`) %>%
  rename(wage_share_clark16 = `share wages`, land_share_clark16 = `share land`, capital_share_clark16 = `share cap`, itax_share_clark16 = `indirect tax share`)
clark16_2 <- read_xlsx("GDP-Efficiency 2023.xlsx", sheet = "nni") %>% rename(nni = `NNI (m. pounds)`) %>% select(Year,nni)
clark16 <- merge(clark16_1, clark16_2, by="Year") %>%
  mutate_at(vars(all_of(share_names)), ~.*nni) %>%
  mutate(Decade = floor(Year/10)*10) %>% group_by(Decade) %>% filter(Decade>1240) %>% filter(Decade<1870) %>%
  mutate_at(vars(wage_clark16, rent_clark16), ~log(.)) %>%
  summarize_at(vars(all_of(c('efficiency_DE16','nni',price_names,share_names))), funs(mean(., na.rm=TRUE))) %>% ungroup() %>%
  mutate_at(vars(all_of(share_names)), ~./nni)

# Merge 
clark <- interest_rate %>% 
  left_join(shares) %>% 
  left_join(tax) %>% 
  left_join(factors) %>%
  left_join(clark16)

################################################################################
### BNS SERIES
################################################################################

# Rent and interest rate series from BNS
load(paste0(path_rdata0,'/draws',nt_bsl))

# Extract BNS estimates 
malthus <- tibble(
  Decade = D,
  interest_malthus = moments$r[1:62],
  a = moments$mlq[1:62],
  rent_malthus = moments$rents[1:62],
  land_share_malthus = c((moments$alphat[1:(T-1)] + moments$alphat[2:T])/2, NA),
  capital_share_malthus = c((moments$betat[1:(T-1)] + moments$betat[2:T])/2, NA),
  wage_share_malthus = 1 - land_share_malthus - capital_share_malthus
)

# Wage series used in BNS: nominal wage of building laborers 
wage_malthus <- read_xlsx("Wages 2014.xlsx", sheet = "Decadal") %>% 
  slice(-1) %>% 
  rename(wage_malthus = 'Building Laborers, d/day') %>% 
  select(Decade, wage_malthus) %>% 
  mutate_all(~as.numeric(.)) %>% 
  filter(Decade >= 1250)

# Merge series used in BNS and in Clark
data <- malthus %>% 
  left_join(wage_malthus) %>% 
  left_join(clark) %>% 
  mutate_at(vars(interest_malthus), ~ .*100) 

################################################################################
### CREATION OF NEW SERIES
################################################################################

#### Compute dual formula as in Figure 5 and 10 ####
dual <- data %>% 
  mutate_at(vars(rent_clark, wage_clark, wage_malthus), ~ ./P_DE) %>% 
  mutate_at(vars(rent_clark, wage_clark, wage_malthus), ~ log(./.[1])) %>% 
  mutate_at(vars(interest_clark, interest_clark16, interest_malthus), ~log(.+3)) %>% 
  mutate_at(vars(interest_clark, interest_clark16, interest_malthus), ~ . - .[1]) %>% 
  mutate(
    # Dual with all series used by Clark (Figures 5 and 10)
    `Dual with Clark series` = capital_share_clark*interest_clark + land_share_clark*
      rent_clark + wage_share_clark*wage_clark - log(1 - itax_share_clark)
  ) %>% 
  select(Decade, `Dual with Clark series`)


#### Compute chain with initial factor shares #### 
aux_chain <- data %>% 
  mutate_at(vars(rent_clark, wage_clark, wage_malthus), ~ ./P_DE) %>% 
  mutate_at(vars(rent_clark, wage_clark, wage_malthus), ~ log(./.[62]*100)) %>% 
  mutate_at(vars(interest_clark, interest_clark16, interest_malthus), ~log(.+3)) %>% 
  mutate_at(vars(interest_clark, interest_clark16, interest_malthus), ~ . - .[62] + log(100))

# Initial expenditure shares
level_chain <- tibble(
  Decade = data$Decade,
  `Replication Clark (2010)` = NA,
  `Replication Clark (2016)*` = NA,
  `Clark (Time Aggregated/Average Shares)` = NA,
  `Dual Malthus series` = NA,
  `Dual Malthus factor shares` = NA,
  `Dual Malthus series and factor shares` = NA,
  `#2` = NA,
  `#3`= NA,
  `#2+(#3-#2)` = NA,
  `#2+(#4-#2)` = NA,
  `#2+(#3-#2)+(#4-#2)` = NA,
  `#5` = NA,
  `Interaction` = NA,
)

# Shares
aux_chain$wage_share_avg <- c((aux_chain$wage_share_clark16[1:(T-1)] + aux_chain$wage_share_clark16[2:T])/2, NA)
aux_chain$land_share_avg <- c((aux_chain$land_share_clark16[1:(T-1)] + aux_chain$land_share_clark16[2:T])/2, NA)
aux_chain$capital_share_avg <- c((aux_chain$capital_share_clark16[1:(T-1)] + aux_chain$capital_share_clark16[2:T])/2, NA)

for (t in (1:61)) {
  
  # Replication Clark (2010)
  a_clark_rep <- aux_chain$wage_share_clark[t]*aux_chain$wage_clark[t] + aux_chain$land_share_clark[t] * 
    aux_chain$rent_clark[t] + aux_chain$capital_share_clark[t]*aux_chain$interest_clark[t] - 
    log(1-aux_chain$itax_share_clark[t])  
  
  # a_t+1 computed with initial factor shares
  aprime_clark_rep <- aux_chain$wage_share_clark[t]*aux_chain$wage_clark[t+1] + aux_chain$land_share_clark[t] * 
    aux_chain$rent_clark[t+1] + aux_chain$capital_share_clark[t]*aux_chain$interest_clark[t+1] -
    log(1-aux_chain$itax_share_clark[t+1]) 
  
  level_chain$`Replication Clark (2010)`[t+1] <- (aprime_clark_rep - a_clark_rep)
  
  # Chain with Clark (2016) series
  a_clark16 <- aux_chain$wage_share_clark16[t]*aux_chain$wage_clark16[t] + aux_chain$land_share_clark16[t] * 
    aux_chain$rent_clark16[t] + aux_chain$capital_share_clark16[t]*aux_chain$interest_clark16[t] - 
    log(1-aux_chain$itax_share_clark16[t])  
  
  # a_t+1 computed with initial factor shares
  aprime_clark16 <- aux_chain$wage_share_clark16[t]*aux_chain$wage_clark16[t+1] + aux_chain$land_share_clark16[t] * 
    aux_chain$rent_clark16[t+1] + aux_chain$capital_share_clark16[t]*aux_chain$interest_clark16[t+1] -
    log(1-aux_chain$itax_share_clark16[t+1]) 
  
  level_chain$`Replication Clark (2016)*`[t+1] <- (aprime_clark16 - a_clark16)
  
  # Chain with Clark series
  a_clark <- aux_chain$wage_share_avg[t]*aux_chain$wage_clark16[t] + aux_chain$land_share_avg[t] * 
    aux_chain$rent_clark16[t] + aux_chain$capital_share_avg[t]*aux_chain$interest_clark16[t] - 
    log(1-aux_chain$itax_share_clark16[t])
  
  # a_t+1 computed with initial factor shares
  aprime_clark <- aux_chain$wage_share_avg[t]*aux_chain$wage_clark16[t+1] + aux_chain$land_share_avg[t] * 
    aux_chain$rent_clark16[t+1] + aux_chain$capital_share_avg[t]*aux_chain$interest_clark16[t+1] -
    log(1-aux_chain$itax_share_clark16[t+1]) 
  
  level_chain$`Clark (Time Aggregated/Average Shares)`[t+1] <- (aprime_clark - a_clark)
  
  # Chain with Malthus series
  a_malthus_series <- aux_chain$wage_share_avg[t]*aux_chain$wage_malthus[t] + aux_chain$land_share_avg[t] * 
    aux_chain$rent_malthus[t] + aux_chain$capital_share_avg[t]*aux_chain$interest_malthus[t] - 
    log(1-aux_chain$itax_share_clark16[t])  
  
  aprime_malthus_series <- aux_chain$wage_share_avg[t]*aux_chain$wage_malthus[t+1] + aux_chain$land_share_avg[t] * 
    aux_chain$rent_malthus[t+1] + aux_chain$capital_share_avg[t]*aux_chain$interest_malthus[t+1] -
    log(1-aux_chain$itax_share_clark16[t+1]) 
  
  level_chain$`Dual Malthus series`[t+1] <-  (aprime_malthus_series - a_malthus_series)
  
  # Chain with Malthus factor shares
  a_malthus_factor <- aux_chain$wage_share_malthus[t]*aux_chain$wage_clark16[t] + aux_chain$land_share_malthus[t] * 
    aux_chain$rent_clark16[t] + aux_chain$capital_share_malthus[t]*aux_chain$interest_clark16[t] - 
    log(1-aux_chain$itax_share_clark16[t])  
  
  aprime_malthus_factor <- aux_chain$wage_share_malthus[t]*aux_chain$wage_clark16[t+1] + aux_chain$land_share_malthus[t] * 
    aux_chain$rent_clark16[t+1] + aux_chain$capital_share_malthus[t]*aux_chain$interest_clark16[t+1] -
    log(1-aux_chain$itax_share_clark16[t+1]) 
  
  level_chain$`Dual Malthus factor shares`[t+1] <- (aprime_malthus_factor - a_malthus_factor)
  
  # Chain with Malthus factor shares
  a_malthus_fs <- aux_chain$wage_share_malthus[t]*aux_chain$wage_malthus[t] + aux_chain$land_share_malthus[t] * 
    aux_chain$rent_malthus[t] + aux_chain$capital_share_malthus[t]*aux_chain$interest_malthus[t] - 
    log(1-aux_chain$itax_share_clark16[t])  
  
  aprime_malthus_fs <- aux_chain$wage_share_malthus[t]*aux_chain$wage_malthus[t+1] + aux_chain$land_share_malthus[t] * 
    aux_chain$rent_malthus[t+1] + aux_chain$capital_share_malthus[t]*aux_chain$interest_malthus[t+1] -
    log(1-aux_chain$itax_share_clark16[t+1]) 
  
  level_chain$`Dual Malthus series and factor shares`[t+1] <- (aprime_malthus_fs - a_malthus_fs)
  
}

# Cumulative sum
level_chain[is.na(level_chain)] <- 0
level_chain <- level_chain %>% 
  mutate_at(vars(`Replication Clark (2010)`, `Replication Clark (2016)*`, `Clark (Time Aggregated/Average Shares)`, `Dual Malthus series`,
                 `Dual Malthus factor shares`, `Dual Malthus series and factor shares`), ~
              cumsum(.))

# merge all in one dataset
t_norm <- 1600
output <- data %>% 
  left_join(dual) %>% 
  left_join(level_chain) %>% 
  # Compute factor price series in real terms, with the same normalization as in the writeup
  mutate_at(vars(rent_clark, wage_clark, wage_malthus), ~ ./P_DE) %>% 
  mutate_at(vars(rent_clark, wage_clark, wage_malthus), ~ log(./.[1])) %>% 
  mutate_at(vars(rent_clark16, wage_clark16), ~ .-.[1]) %>% 
  select(Decade, interest_malthus, interest_clark, wage_malthus, wage_clark,
         rent_malthus, rent_clark, capital_share_malthus, capital_share_clark,
         wage_share_malthus, wage_share_clark, land_share_malthus, land_share_clark,
         wage_clark16, rent_clark16, interest_clark16,
         wage_share_clark16, land_share_clark16, capital_share_clark16,
         a, efficiency_DE, efficiency_DE16, `Dual with Clark series`, `Clark (Time Aggregated/Average Shares)`,
         `Replication Clark (2010)`, `Replication Clark (2016)*`,
         `Dual Malthus series`,  `Dual Malthus factor shares`,
         `Dual Malthus series and factor shares`,
         `#2`, `#2+(#3-#2)`, `#2+(#4-#2)`, `#2+(#3-#2)+(#4-#2)`, `#5`, `Interaction`) %>% 
  # Apply the same normalization for productivity estimates to match malthus estimates in 1300
  mutate_at(vars(efficiency_DE, efficiency_DE16), ~log(.)) %>% 
  mutate_at(vars(efficiency_DE, efficiency_DE16, `Dual with Clark series`,
                `Replication Clark (2010)`,`Replication Clark (2016)*`,
                 `Clark (Time Aggregated/Average Shares)`, `Dual Malthus series`,
                 `Dual Malthus factor shares`,
                 `Dual Malthus series and factor shares`), 
            ~ . - .[which(Decade==t_norm)] + a[which(Decade==t_norm)]) %>%
  rename(
    xaxis = Decade,
    `Clark (2010)` = efficiency_DE,
    `Clark (2016)*` = efficiency_DE16,
    `Our baseline` = a,
    `Clark (Time Aggregated/Average Shares)` = `Clark (Time Aggregated/Average Shares)`,
    `Dual with Clark's factor prices and our factor shares` = `Dual Malthus factor shares`,
    `Dual with our factor prices and Clark's factor shares` = `Dual Malthus series`,
    `Dual with our factor prices and our factor shares` = `Dual Malthus series and factor shares`
  ) %>% mutate(rent_clark16 = rent_clark16 - rent_clark16[xaxis==1600] + rent_clark[xaxis==1600])


################################################################################
### CHARTS
################################################################################

source(paste0(path_code0,'/chart_theme.R'), echo=FALSE)
source(paste0(path_code0,'/chart_functions.R'), echo=FALSE)
setwd(path_charts0)

# Figure G1
df0 <- output %>% select(xaxis, `Our baseline`, `Clark (2010)`, `Clark (2016)*`, `Clark (Time Aggregated/Average Shares)`)
plotx <- comp_chart(df0,50)
ggsave(paste0('bns_figG1.png'), plot=plotx,
       width=chart_width, height=chart_height, units="in")

# Figure G2
df0 <- output %>% select(xaxis, `Our baseline`, `Clark (Time Aggregated/Average Shares)`, `Dual with our factor prices and our factor shares`)
colnames(df0)[3] <- 'Dual with Clark\'s factor prices and Clark\'s factor shares'
plotx <- comp_chart(df0,50)
ggsave(paste0('bns_figG2.png'), plot=plotx,
       width=chart_width, height=chart_height, units="in")

# Figure G3
df0 <- output %>% select(xaxis, `Our baseline`, `Clark (Time Aggregated/Average Shares)`, `Dual with our factor prices and Clark's factor shares`)
colnames(df0)[3] <- 'Dual with Clark\'s factor prices and Clark\'s factor shares'
plotx <- comp_chart(df0,50)
ggsave('bns_figG3.png', plot=plotx,
       width=chart_width, height=chart_height, units="in")

# Figure G4
col0 <- c("Black","grey70","grey70")
mycol <- scale_colour_manual(values = col0)
plotp <- function(df,name0,title0,leg0) { # function to produce charts
  if (name0=='rent') cols0 <- c('xaxis',paste0(name0,'_',c('malthus','clark','clark16')))
  else cols0 <- c('xaxis',paste0(name0,'_',c('malthus','clark16')))
  df0 <- subset(df, select=cols0)
  if (title0=='Rents before 1700') df0 <- df0[df0$xaxis<=1700,]
  if (name0=='rent') colnames(df0) <- c('xaxis','Ours','Clark (2010)','Clark (2016)*')
  else colnames(df0) <- c('xaxis','Ours','Clark (2016)*')
  plotx <- comp_chart(df0,100) +
    ggtitle(title0)  + theme(legend.position = leg0)
}
typ0 <- c("solid","solid")
mytyp <- scale_linetype_manual(values = typ0)
plotx <- plotp(output,'wage','Wages',"none") + plotp(output,'interest','Interest rate',"none")
typ0 <- c("solid","dashed","solid")
mytyp <- scale_linetype_manual(values = typ0)
plotx <- plotx + plotp(output,'rent','Rents',"none") + plotp(output,'rent','Rents before 1700',"none")
ggsave(paste0('bns_figG4.png'), plot=plotx,
       width=chart_width, height=chart_height, units="in")
ggsave(paste0('bns_figG4_legend.png'), plot=plotp(output,'rent','Rents',"bottom") + guides(colour = guide_legend(nrow = 1)),
       width=chart_width, height=chart_height, units="in")

# restore color code
source(paste0(path_code0,'/chart_theme.R'))

# Figure G5
df0 <- output %>% select(xaxis, `Our baseline`, `Clark (Time Aggregated/Average Shares)`, `Dual with Clark's factor prices and our factor shares`)
colnames(df0)[3] <- 'Dual with Clark\'s factor prices and Clark\'s factor shares'
plotx <- comp_chart(df0,50)
ggsave(paste0('bns_figG5.png'), plot=plotx,
       width=chart_width, height=chart_height, units="in")

# Figure G6
df0 <- output %>%
  select(xaxis, capital_share_malthus, capital_share_clark16,
         wage_share_malthus, wage_share_clark16,
         land_share_malthus, land_share_clark16)
colnames(df0) <- c('xaxis','Capital (ours)','Capital (Clark, 2016*)',
                   'Labor (ours)', 'Labor (Clark, 2016*)',
                   'Land (ours)', 'Land (Clark, 2016*)')
plotx <- comp_chart(df0,50)
ggsave(paste0('bns_figG6.png'), plot=plotx,
       width=chart_width, height=chart_height, units="in")

# numbers for text
print('Section G.3')
nb_rents <- aux_chain %>% filter(Decade %in% c(1250,1500)) %>% select(rent_clark, rent_clark16)
round(exp(nb_rents[2,] - nb_rents[1,]) - 1, 2)
