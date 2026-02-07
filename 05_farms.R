
# function to define the farms
cdfFUN <- function(data, x, y){
  d <- data %>% dplyr::filter(Sector == {{x}}) %>% dplyr::select(y)
  d <- d[, 1]
  d <- d[d>0]
  return(d)
}
# don't run
# plot sizes of utilized farmland for municipalities with at least 1 farm
# cdfFUN(data = farms1, x = "Sicilia", y = "hutilized", z = "region")

# don't run
# plot sizes of utilized farmland for municipalities in the Nebrodi region with at least 1 farm
# cdfFUN(data = farms1, x = "Y", y = "hutilized", z = "nebrodi")

# average plot size of utilized farmland for municipalities with at least 1 farm
size <- cdfFUN(data = equities, x = NULL, y = "Market_Value_Billions")



# sum the total number of farms
total_number_of_farms <- function(x){
  cat("This number is the number reported by the agricultural census for all of Italy:\n")
  farms <- sum(x)
  print(farms)
}
total_number_of_farms(farms1$ftotal)











# function to define the farms
cdfFUN <- function(data, x, y, z){
  d <- data %>% dplyr::filter(region == {{x}}) %>% dplyr::filter(y > 0) %>% dplyr::select(y)
  d <- d[, 1]
  d <- d[d>0]
  return(d)
}
# don't run
# plot sizes of utilized farmland for municipalities with at least 1 farm
# cdfFUN(data = farms1, x = "Sicilia", y = "hutilized", z = "region")

# don't run
# plot sizes of utilized farmland for municipalities in the Nebrodi region with at least 1 farm
# cdfFUN(data = farms1, x = "Y", y = "hutilized", z = "nebrodi")

# average plot size of utilized farmland for municipalities with at least 1 farm
d <- cdfFUN(data = farms1, x = "Sicilia", y = "havgutilized", z = "region")


# maximum likelihood estimation of the distribution of farmland --------------------------
mle <- function(distribution, nsim = 100, k = NULL, xlab){
  
  # replication
  set.seed(15092022) # Huddy's birthday
  
  # plot dimensions
  par(mfrow = c(1, 1))
  
  # distribution
  d <- sort(distribution)
  
  # mle estimation to power law distributions
  require("poweRlaw") # see https://www.rdocumentation.org/packages/poweRlaw/versions/0.70.6
  mle = poweRlaw::conpl(d)
  
  # don't run
  # manually set the structural cut-off for the power law distribution
  # mle$setXmin(k) # cut off
  
  # estimate the structural cut-off for power law distribution
  xmin <- poweRlaw::estimate_xmin(mle)$xmin
  mle$setXmin(xmin)
  
  # scaling parameter
  gamma <- poweRlaw::estimate_pars(mle)$pars
  mle$setPars(gamma)
  
  # structural cut-off values for bootstrapping
  
  # structural cut-off
  xmin <- round(xmin, digits = 0)
  
  # maximum value of the statistical distribution
  kn <- max(d); kn <- round(kn, digits = 0)
  
  # bootstrap procedure to compute 95% confidence intervals for the estimate of the scaling parameter
  boots = poweRlaw::bootstrap(
    m = mle, 
    no_of_sims = nsim, 
    # don't run
    # this code estimates the decay parameter throughout the distribution
    # xmins = seq(xmin, kn, 1), # estimates xmins in the distribution
    xmins = xmin,
    # xmax = kn,
    threads = 10, # more threads speed up the procedure
    distance = "reweight", # the distance statistic is used to calculate p-values ... reweight because distribution is likely not i.i.d.
    seed = 20110210 # Halle's birthday
  )
  
  # confidence intervals
  gamma_boots <- boots$bootstraps[, 3]
  lo <- stats::quantile(gamma_boots, 0.025, na.rm = TRUE)
  hi <- stats::quantile(gamma_boots, 0.975, na.rm = TRUE)
  
  # compute p-value
  p = poweRlaw::bootstrap_p(
    m = mle, 
    no_of_sims = nsim,
    # don't run
    # this code computes error in decay parameter for any plausible structural cut-off k throughout the statistical distribution
    # xmins = seq(xmin, kn, 1), # estimates xmins in the distribution
    xmins = xmin,
    # xmax = kn,
    threads = 10, # more threads speed up the procedure
    distance = "reweight",
    seed = 20110210 # Halle's birthday
  )
  p <- p$p # p-value
  
  # full empirical distribution for plotting
  d_full <- d[d > 0]  # all data
  n_full <- length(d_full)
  ccdf_full <- (n_full:1) / n_full
  
  # tail of the empirical distribution for plotting
  d_tail <- d[d >= xmin]
  n <- length(d_tail)
  ccdf_empirical <- (n:1) / n
  
  # define confidence intervals
  x_seq <- seq(xmin, kn, length.out = 200)
  
  # compute CCDF values for power law
  scale_factor <- sum(d >= xmin) / length(d_full)
  ccdf <- function(y){ # formula: P(X > x) = (x/xmin)^(1-alpha)
    p_x <- scale_factor * (x_seq/xmin)^(1 - y)
  }
  ccdf_mu <- ccdf(gamma)
  ccdf_hi <- ccdf(hi)
  ccdf_lo <- ccdf(lo)
  
  # Manual log-log plot
  plot(d_full, ccdf_full, 
       log = "xy",
       pch = 21, bg = "white", col = "black", lwd = 2,
       panel.first = grid(),
       xlim = c(10^0, 10^2),
       # ylim = c(10^-3, 10^0),
       xlab = xlab, 
       ylab = "Cumulative Distribution Function (CDF)",
       axes = FALSE, frame = TRUE
  )
  labels <- sapply(0:2, function(i) as.expression(bquote(10^.(i))))
  axis(1, at = 10^(0:2), labels = labels)
  labels <- sapply(-4:0, function(i) as.expression(bquote(10^.(i))))
  axis(2, at = 10^(-4:0), labels = labels)
  
  # shaded confidence band
  polygon(c(x_seq, rev(x_seq)), 
          c(ccdf_lo, rev(ccdf_hi)), 
          col = rgb(1, 0, 0, 0.2), 
          border = NA
  )
  
  # fitted statistical distribution
  lines(x_seq, ccdf_mu, lwd = 3, col = 2)
  
  # vertical line at xmin to show where power law starts
  abline(v = xmin, col = "blue", lty = 2, lwd = 2)
  
  # slope of the fitted statistical distribution
  # legend("topright", legend = bquote(gamma == .(round(gamma, 2))), bty = "n", cex = 1.2)
  # slope of the fitted statistical distribution with 95% CI
  # slope of the fitted statistical distribution with 95% CI and p-value
  legend("topright", 
         legend = c(
           bquote(gamma == .(round(-gamma, 2)) ~ "[" * .(round(-hi, 2)) * "," ~ .(round(-lo, 2)) * "]"),
           bquote(italic(p) == .(round(p, 3)))
         ),
         bty = "n", cex = 1.2)
  
  # hypothesis test
  cat("interpretation: \n\n")
  cat("null hypothesis: statistical distribution resembles the power law distribution.\n\n")
  cat("alternate hypothesis: statistical distribution does not resemble the power law distribution.\n\n")
  cat("statistical significance (p < 0.05) provides evidence to reject the null hypothesis.\n\n")
  cat("p = "); cat(p)
}
mle(distribution = d, xlab = "Average number of hectares of utilized farmland")





# ... close .R script ----------------------------------------------------------




# cumulative distribution function ---------------------------------------------
cdf <- function(data = farms1, y = havgutilized, group = province){
  
  # call pipe to workspace
  `%>%` <- magrittr::`%>%`
  
  # required packages
  require(dplyr)
  
  # calculate empirical cdf for different groups
  cdf <- data %>%
    # grouping variable
    dplyr::group_by({{group}}) %>%
    # calculate the cdf of each group
    dplyr::mutate( cdf = stats::ecdf( {{y}} )( {{y}} ) ) %>%
    # ungroup
    dplyr::ungroup()
  
  # organize and restructure the data
  cdf <- cdf %>%
    # drop zeros
    dplyr::filter( {{y}} != 0) %>%
    # grouping variable
    dplyr::group_by( {{group}} ) %>%
    # include observation number
    dplyr::mutate(obs = dplyr::row_number()) %>%
    # ungroup
    dplyr::ungroup() %>%
    # order columns
    dplyr::select( {{group}}, obs,  {{y}}, cdf ) %>%
    # arrange observations 
    dplyr::arrange( {{group}}, obs, {{y}} )
  
  # return
  return(cdf)
}
cdf(data = farms1, y = havgutilized, group = province)





################################################################################






#
farms2 <- farms1 %>%
  dplyr::group_by(province) %>%
  dplyr::mutate(
    htotal = sum(htotal),
    hutilized = sum(hutilized),
    hunutilized = sum(hunutilized),
    ftotal = sum(ftotal),
    futilized = sum(futilized),
    funutilized = sum(funutilized),
    altimetric = sum(altimetric),
    popleg = sum(popleg),
    popres = sum(popres),
    sqkilo = sum(sqkilo),
    hectares = sum(hectares),
    dairycows = sum(dairycows),
    cattle = sum(cattle),
    buffalo = sum(buffalo),
    horses = sum(horses),
    sheep = sum(sheep),
    goats = sum(goats),
    pigs = sum(pigs),
    poultry = sum(poultry),
    livestock = sum(livestock),
    itotal = sum(itotal),
    ibuilding = sum(ibuilding),
    ilivestock = sum(ilivestock),
    imechanization = sum(imechanization),
    iphytosanitary = sum(iphytosanitary),
    iplanting = sum(iplanting),
    iproductivity = sum(iproductivity),
    icarting = sum(icarting),
    `arson, building` = sum(`arson, building`),
    `arson, forest` = sum(`arson, forest`),
    `arson, vandalism of property` = sum(`arson, vandalism of property`),
    extortion = sum(extortion),
    kidnapping = sum(kidnapping),
    `mafia association` = sum( `mafia association`),
    `mafia homicides` = sum(`mafia homicides`),
    `usury` = sum(`usury`)
  )

# drop columns 
farms2 <- dplyr::select(
  farms2, 
  -comune, 
  -havgtotal, 
  -havgutilized, 
  -havgunutilized, 
  -altimetric, 
  -altitude, 
  -coastal, 
  -urbanization, 
  -mountain, 
  -nebrodi, 
  -pd.futilized, 
  -pd.funutilized, 
  -avg.livestock, 
  -pd.livestock, 
  -pd.poultry, 
  -rank, 
  -case, 
  -code
)

# drop duplicates
farms2 <- unique(farms2)



# calculate the average size of a farm in each municipality --------------------
farms2 <- farms2 %>%
  # total farmland
  dplyr::mutate(havgtotal = log(htotal/ftotal)) %>% # log total hectares : total number of farms
  dplyr::mutate(havgtotal = exp(havgtotal)) %>%  # exponentiate
  dplyr::mutate(havgtotal = ifelse(is.nan(havgtotal), 0, havgtotal)) %>% # = 0
  dplyr::mutate(havgtotal = ifelse(is.infinite(havgtotal), 0, havgtotal)) %>% # = 0
  dplyr::mutate(havgtotal = ifelse(is.na(havgtotal), 0, havgtotal)) %>% # = 0
  # utilized farmland
  dplyr::mutate(havgutilized = log(hutilized/futilized)) %>% # log total hectares : total number of farms
  dplyr::mutate(havgutilized = exp(havgutilized)) %>%  # exponentiate
  dplyr::mutate(havgutilized = ifelse(is.nan(havgutilized), 0, havgutilized)) %>% # = 0
  dplyr::mutate(havgutilized = ifelse(is.infinite(havgutilized), 0, havgutilized)) %>% # = 0
  dplyr::mutate(havgutilized = ifelse(is.na(havgutilized), 0, havgutilized)) %>% # = 0
  # unutilized farmland
  dplyr::mutate(havgunutilized = log(hunutilized/funutilized)) %>% # log total hectares : total number of farms
  dplyr::mutate(havgunutilized = exp(havgunutilized)) %>%  # exponentiate
  dplyr::mutate(havgunutilized = ifelse(is.nan(havgunutilized), 0, havgunutilized)) %>% # = 0
  dplyr::mutate(havgunutilized = ifelse(is.infinite(havgunutilized), 0, havgunutilized)) %>% # = 0
  dplyr::mutate(havgunutilized = ifelse(is.na(havgunutilized), 0, havgunutilized)) # = 0




# calculate the population density of farms with utilized farmland in each municipality ------------------------------
farms2 <- dplyr::mutate(farms2, pd.futilized = futilized/sqkilo) # number of farms per squared kilometer
farms2$pd.futilized[is.infinite(farms2$pd.futilized)] <- 0
Hmisc::label(farms2$pd.futilized) <- "number of farms with utilized farmland per squared kilometer"

# calculate the population density of farms with unutilized farmland in each municipality ------------------------------
farms2 <- dplyr::mutate(farms2, pd.funutilized = funutilized/sqkilo) # number of farms per squared kilometer
farms2$pd.funutilized[is.infinite(farms2$pd.funutilized)] <- 0
Hmisc::label(farms2$pd.funutilized) <- "number of farms with unutilized farmland per squared kilometer"


#
farms2$pd.extortion <- farms2$extortion/farms2$sqkilo



