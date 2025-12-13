



# calculate average farm size for all of Italy ---------------------------------

# calculate the quantile - cdf = 80%
farms.italy <- farms1 %>%
  dplyr::mutate(q50 = stats::quantile(havgutilized, probs = 0.50)) %>% # calculate size of 50% of farms 
  dplyr::mutate(q80 = stats::quantile(havgutilized, probs = 0.80)) # calculate size of 80% of farms

# plot
ggplot2::ggplot(farms.italy, ggplot2::aes(havgutilized)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) + # geom = "area"
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(size = 12)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 100, 10)) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::ylab("Cumulative Distribution Function (CDF)") +
  ggplot2::xlab("Average size of farm (hectares of utulized farmland) in municipalities") +
  # mark 50% of farms
  ggplot2::geom_vline(data = farms.italy, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of farms
  ggplot2::geom_vline(data = farms.italy, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = farms.italy, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2264 50% of farms:\n ", round(q50, digits = 0), "ha"))) + 
  ggplot2::geom_label(data = farms.italy, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2264 80% of farms:\n ", round(q80, digits = 0), "ha"))) 





# histogram of the size of the average parcel of farmland across regions
ggplot2::ggplot(farms1, ggplot2::aes(havgutilized)) +
  ggplot2::stat_ecdf(geom = "step", col = "black", alpha = 1.00) +
  ggplot2::theme_bw() + 
  ggplot2::facet_wrap(~ region, nrow = 4, ncol = 5) +
  ggplot2::xlim(0, 100) +
  # ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::ylab("Empiricial Cumulative Distribution Function (ECDF)") +
  ggplot2::xlab("Average size of farm (hectares of utulized farmland) in municipalities")



# histogram of the size of the average parcel of farmland for provinces in Sicily

# mark the quantiles on the x-axis

# don't run
# quantiles
# q <- c(0.00, 0.25, 0.50, 0.75, 1.00)

# calculate the quantile - cdf = 80%
farms.sicily <- farms1 %>%
  dplyr::filter(region == "Sicilia") %>%
  dplyr::group_by(province) %>%
  dplyr::mutate(q50 = stats::quantile(havgutilized, probs = 0.50)) %>% # calculate size of 50% of farms 
  dplyr::mutate(q80 = stats::quantile(havgutilized, probs = 0.80)) # calculate size of 80% of farms

# plot
ggplot2::ggplot(subset(farms1, region == "Sicilia"), ggplot2::aes(havgutilized)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) + # geom = "area"
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~ province, nrow = 3, ncol = 3) +
  ggplot2::theme(text = ggplot2::element_text(size = 20)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 100, 10)) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::ylab("Cumulative Distribution Function (CDF)") +
  ggplot2::xlab("Average size of farm (hectares of utulized farmland) in municipalities") +
  # mark 50% of farms
  ggplot2::geom_vline(data = farms.sicily, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of farms
  ggplot2::geom_vline(data = farms.sicily, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = farms.sicily, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2264 50% of farms:\n ", round(q50, digits = 0), "ha"))) + 
  ggplot2::geom_label(data = farms.sicily, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2264 80% of farms:\n ", round(q80, digits = 0), "ha"))) 




# calculate the quantile - cdf = 80%
farms.nebrodi <- farms1 %>%
  dplyr::filter(nebrodi == "Y") %>%
  dplyr::mutate(q50 = stats::quantile(havgutilized, probs = 0.50)) %>% # calculate size of 50% of farms 
  dplyr::mutate(q80 = stats::quantile(havgutilized, probs = 0.80)) # calculate size of 80% of farms
  
# plot
ggplot2::ggplot(subset(farms1, nebrodi == "Y"), ggplot2::aes(havgutilized)) +
  ggplot2::stat_ecdf(geom = "step", linewidth = 1, col = "black", alpha = 1.00) + # geom = "area"
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(size = 12)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 100, 10)) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::ylab("Cumulative Distribution Function (CDF)") +
  ggplot2::xlab("Average size of farm (hectares of utulized farmland) in municipalities") +
  # mark 50% of farms
  ggplot2::geom_vline(data = farms.nebrodi, ggplot2::aes(xintercept = q80), linewidth = 1, linetype = "dashed", color = "red") +
  # mark 80% of farms
  ggplot2::geom_vline(data = farms.nebrodi, ggplot2::aes(xintercept = q50), linewidth = 1, linetype = "dashed", color = "red") +
  # label the lines
  ggplot2::geom_label(data = farms.nebrodi, mapping = ggplot2::aes(x = q50, y = 0.25, label = paste("\u2264 50% of farms:\n ", round(q50, digits = 0), "ha"))) + 
  ggplot2::geom_label(data = farms.nebrodi, mapping = ggplot2::aes(x = q80, y = 0.50, label = paste("\u2264 80% of farms:\n ", round(q80, digits = 0), "ha"))) 



# cumulative degree distribution -----------------------------------------------
cdf <- function(data){
  
  
  # calculate cdf
  cdf <- stats::ecdf(d$d) # calculate cdf function
  cdf <- cdf(d$d)         # pass degree distribution through cdf function 
  cdf <- data.frame(cdf)    # into dataset
  # join
  d <- cbind(d, cdf)
  cdf <- sort(data)
  colnames(cdf) <- c("y")
  cdf <- data.frame(cdf)
  cdf <- dplyr::filter(cdf, y != 0)
  cdf$degree <- rownames(cdf); cdf$degree <- as.numeric(cdf$degree)
  return(cdf)
}
cdf_mob <- cdf( intergraph::asIgraph(g_mob) )
cdf_org <- cdf(u_org)
cdf_off <- cdf(u_off)
cdf_nar <- cdf(u_nar)
cdf_uno <- cdf(u_uno)
cdf_geo <- cdf(u_geo)



#########################

# call pipe to workspace
`%>%` <- magrittr::`%>%`

# plot sizes of utilized farmland for municipalities with >= 1 farms
d <- farms1 %>% dplyr::filter(region == "Sicilia") %>%
  dplyr::filter(hutilized != 0) %>%
  dplyr::select(hutilized)
d <- as.numeric(d$hutilized)

# plot sizes of utlized farmland for municipalities in the Nebrodi region with >= 1 farms
d <- farms1 %>% dplyr::filter(nebrodi == "Y") %>%
  dplyr::filter(hutilized != 0) %>%
  dplyr::select(hutilized)
d <- as.numeric(d$hutilized)

# average plot size of utilized farmland for municipalities with >= 1 farms
davg <- farms1 %>% dplyr::filter(region == "Sicilia") %>%
  dplyr::filter(havgutilized != 0) %>%
  dplyr::select(havgutilized)
davg <- as.numeric(davg$havgutilized)



# function to estimate degree scaling exponents by mle --------------------------
mle <- function(d, k){
  
  # replication
  set.seed(15092022) # Huddy's birthday
  
  # distribution
  d <- sort(d)
  kn = max(d) # upper range in the weighted degree distribution
  
  # mle estimation to power law distributions
  require("poweRlaw") # see https://www.rdocumentation.org/packages/poweRlaw/versions/0.70.6
  mle = poweRlaw::conpl(d)
  
  # structural cut-off
  xmin <- poweRlaw::estimate_xmin(mle)
  mle$setXmin(xmin) # set
  
  # scaling parameter
  gamma <- poweRlaw::estimate_pars(mle)$pars
  mle$setPars(gamma)
 
  # return
  return(mle)
}
result01 <- mle(d = d, k = NULL)

plot01 <- plot(result01)

plot(result01)
lines(result01)




plot(log(farms1$futilized[farms1$nebrodi == "Y"]), log(farms1$havgutilized[farms1$nebrodi == "Y"]))

# function to estimate degree scaling exponents by mle --------------------------
mle <- function(d, k){
  
  # replication
  set.seed(15092022) # Huddy's birthday
  
  # distribution
  d <- sort(d)
  kn = max(d) # upper range in the weighted degree distribution

  # mle estimation to power law distributions
  require("poweRlaw") # see https://www.rdocumentation.org/packages/poweRlaw/versions/0.70.6
  mle = poweRlaw::conpl(d)
  # mle$setXmin(k) # cut off
  #  don't run ... to estimate the structural cut-off, run this code chunk
  xmin <- poweRlaw::estimate_xmin(mle)
  mle$setXmin(xmin)
  
  # decay exponent for the distribution
  gamma <- poweRlaw::estimate_pars(mle)$pars; Y <- round(gamma, digits = 2)
  print(paste("decay exponent, gamma:", Y))
  mle$setPars(gamma) # set decay parameter to object
  
  # bootstrap procedure to compute 95% CIs i.e., uncertainties to the gamma parameter estimates
  boots = poweRlaw::bootstrap(
    m = mle, 
    no_of_sims = 100, 
    # xmins = xmin,
    # don't run ... this code estimates the decay parameter throughout the degree distribution
    # xmins = seq(xmin, kn, 1), # estimates xmins in the degree sequence = k
    xmins = xmin, # estimates xmins in the degree sequence = k
    xmax = kn,
    threads = k * 3, # more threads speed up the procedure
    distance = "reweight", # the distance statistic is used to calculate p-values ... reweight because degree is not iid
    seed = 20110210 # Halle's birthday
    )
  
  # std deviations to the bootstraps
  sd <- sd(boots$bootstraps[, 3]) # std deviation
  sd <- sd/sqrt(length(sd))       # standard error
  hi <- gamma + (2 * sd)
  lo <- gamma - (2 * sd)
  
  # print hi, lo range to gamma
  hi <- round(hi, digits = 2) # round
  print(paste("hi gamma", hi))
  lo <- round(lo, digits = 2) # round
  print(paste("lo gamma", lo))
  
  # compute p-value
  p = poweRlaw::bootstrap_p(
    m = mle, 
    no_of_sims = 100,
    # xmins = k,
    # don't run ... this code computes error in decay parameter for any ...
    # ... plausible structural cut-off k throughout the degree distribution
    # xmins = seq(xmin, kn, 1), # estimates xmins in the degree sequence = k
    xmins = xmin, # estimates xmins in the degree sequence = k
    xmax = kn,
    threads = k * 3, # more threads speed up the procedure
    distance = "reweight",
    seed = 20110210 # Halle's birthday
    )
  p <- p$p # p-value
  print(paste("p-value", p))
}
result01 <- mle(d = d, k = NULL)


  # Family membership ties
  mle_02_k01 <- mle(g = u_org, k =  1)
  mle_02_k02 <- mle(g = u_org, k =  2)
  mle_02_k03 <- mle(g = u_org, k =  3)
  mle_02_k04 <- mle(g = u_org, k =  4)
  mle_02_k05 <- mle(g = u_org, k =  5)
  mle_02_k06 <- mle(g = u_org, k =  6)
  mle_02_k07 <- mle(g = u_org, k =  7)
  mle_02_k08 <- mle(g = u_org, k =  8)
  mle_02_k09 <- mle(g = u_org, k =  9)
  mle_02_k10 <- mle(g = u_org, k = 10)
  
  # crime racketeering
  mle_03_k01 <- mle(g = u_off, k =  1)
  mle_03_k02 <- mle(g = u_off, k =  2)
  mle_03_k03 <- mle(g = u_off, k =  3) 
  mle_03_k04 <- mle(g = u_off, k =  4)
  mle_03_k05 <- mle(g = u_off, k =  5)
  mle_03_k06 <- mle(g = u_off, k =  6)
  mle_04_k07 <- mle(g = u_off, k =  7)
  mle_04_k08 <- mle(g = u_off, k =  8)
  mle_04_k09 <- mle(g = u_off, k =  9)
  mle_04_k10 <- mle(g = u_off, k = 10)
  
  # street-level heroin dealing
  mle_04_k01 <- mle(g = u_nar, k = 1)
  mle_04_k02 <- mle(g = u_nar, k = 2)
  mle_04_k03 <- mle(g = u_nar, k = 3) 
  mle_04_k04 <- mle(g = u_nar, k = 4)
  mle_04_k05 <- mle(g = u_nar, k = 5)
  mle_04_k06 <- mle(g = u_nar, k = 6)
  mle_04_k07 <- mle(g = u_nar, k = 7)
  mle_04_k08 <- mle(g = u_nar, k = 8) 
  mle_04_k09 <- mle(g = u_nar, k = 9)
  mle_04_k10 <- mle(g = u_nar, k = 10)
  
  # unions
  mle_05_k01 <- mle(g = u_uno, k =  1)
  mle_05_k02 <- mle(g = u_uno, k =  2)
  mle_05_k03 <- mle(g = u_uno, k =  3) 
  mle_05_k04 <- mle(g = u_uno, k =  4)
  mle_05_k05 <- mle(g = u_uno, k =  5)
  mle_05_k06 <- mle(g = u_uno, k =  6)
  mle_05_k07 <- mle(g = u_uno, k =  7)
  mle_05_k08 <- mle(g = u_uno, k =  8)
  mle_05_k09 <- mle(g = u_uno, k =  9)
  mle_05_k10 <- mle(g = u_uno, k = 10)
  
  # neighborhoods
  mle_06_k01 <- mle(g = u_geo, k =  1)
  mle_06_k02 <- mle(g = u_geo, k =  2)
  mle_06_k03 <- mle(g = u_geo, k =  3) 
  mle_06_k04 <- mle(g = u_geo, k =  4)
  mle_06_k05 <- mle(g = u_geo, k =  5)
  mle_06_k06 <- mle(g = u_geo, k =  6)
  mle_06_k07 <- mle(g = u_geo, k =  7)
  mle_06_k08 <- mle(g = u_geo, k =  8) 
  mle_06_k09 <- mle(g = u_geo, k =  9)
  mle_06_k10 <- mle(g = u_geo, k = 10)
  

  
# ... close .R script ----------------------------------------------------------

  
  
  
  #
  farms1 <- dplyr::arrange(farms1, dplyr::desc(havgutilized))
  comune <- dplyr::arrange(farms1, havgutilized, code, comune, province, region)
  cdf <- stats::ecdf(farms1$havgutilized)
  cdf <- cdf(farms1$havgutilized)
  cdf <- data.frame(cdf)
  cdf <- cbind(cdf, comune)
  colnames(cdf) <- c("cdf", "havg")
  
  #
  cdf <- stats::ecdf(farms1$havgutilized)
  farms1 <- farms1 %>%
    dplyr::group_by(province) %>%
    dplyr::mutate(cdf = cdf(havgutilized))

