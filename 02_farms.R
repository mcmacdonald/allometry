# ------------------------------------------------------------------------------

# this .r script contains code to analyze the farmland across municipalities:
  
  # 1) constructs measures for farms across municipalities
  
  # 2) allometric analysis of the size of farmland and susceptibility to extortion and mafia victimization

# ------------------------------------------------------------------------------


# histogram of the size of the average parcel of farmland
ggplot2::ggplot(subset(farms1, nebrodi %in% "Y" & hutilized > 0), ggplot2::aes(havgutilized)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count / sum(count))), fill = "white", colour = "black", alpha = 1.00) +
  ggplot2::theme_bw() + 
  ggplot2::xlim(0, 50) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::ylab("Frequency") +
  ggplot2::xlab("Average size of farm, hectares of utulized farmland")

# histogram of the size of the average parcel of farmland
ggplot2::ggplot(subset(farms1, region %in% "Sicilia" & hutilized > 0), ggplot2::aes(havgutilized)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count / sum(count))), fill = "white", colour = "black", alpha = 1.00) +
  ggplot2::theme_bw() + 
  ggplot2::xlim(0, 50) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::ylab("Frequency") +
  ggplot2::xlab("Average size of farm, hectares of utulized farmland")

# histogram of the size of the average parcel of farmland
ggplot2::ggplot(subset(farms1, hutilized > 0 & hutilized > 50), ggplot2::aes(havgutilized)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count / sum(count))), fill = "white", colour = "black", alpha = 1.00) +
  ggplot2::theme_bw() + 
  ggplot2::facet_wrap(~ region, nrow = 4, ncol = 5) +
  ggplot2::xlim(0, 50) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::ylab("Frequency") +
  ggplot2::xlab("Average size of farm, hectares of utulized farmland")


# histogram of the size of the average parcel of farmland
ggplot2::ggplot(subset(farms1, hunutilized > 0 & hunutilized > 50), ggplot2::aes(havgunutilized)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count / sum(count))), fill = "white", colour = "black", alpha = 1.00) +
  ggplot2::theme_bw() + 
  ggplot2::facet_wrap(~ region, nrow = 4, ncol = 5) +
  ggplot2::xlim(0, 50) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::ylab("Frequency") +
  ggplot2::xlab("Average size of farm, hectares of unutulized farmland")


# histogram of the size of the average parcel of farmland
ggplot2::ggplot(data = farms1) +
  ggplot2::geom_histogram(ggplot2::aes(x = hutilized, y = ggplot2::after_stat(count / sum(count))), fill = "white", colour = "black", alpha = 1.00) +
  # ggplot2::geom_histogram(ggplot2::aes(x = htotal, y = ggplot2::after_stat(count / sum(count))), fill = "black", colour = "black", alpha = 0.80) +
  ggplot2::theme_bw() + 
  ggplot2::facet_wrap(~ region, nrow = 4, ncol = 5) +
  ggplot2::xlim(0, 50) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::ylab("Frequency") +
  ggplot2::xlab("Average size of farm, hectares of utulized farmland")




# 1) the relationship between the number of farms and the hectares of farmland

# estimate the scale of slopes that summarize the relationship between density of farmland and total hectares

# don't run
# fit linear model for each region
# models = farms1 %>% dplyr::group_by(region) %>% dplyr::do(model = stats::lm(ln_htotal ~ ln_ftotal, data = .))
# models$model

# fit linear models for each region
models <- by(data = farms1, INDICES = list(farms1$region), FUN = function(x) {
  model <- stats::lm(log(hutilized + 1) ~ log(futilized + 1), data = x)
  stats::coef(model)
    }
  )
do.call("rbind", models)

# plot lines of best fit for different regions -------------------------------------------------
ggplot2::ggplot(farms1, ggplot2::aes(futilized, hutilized)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ region, nrow = 4, ncol = 5) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Hectares of utilized farmland (log scale)") +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with utilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())

# plot lines of best fit for different regions -------------------------------------------------
ggplot2::ggplot(farms1, ggplot2::aes(futilized, pd.livestock)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ region, nrow = 4, ncol = 5) +
  ggplot2::theme_bw() +
  # ggplot2::ylab("Head of livestock per square kilometer of utlized farmland") +
  ggplot2::ylab(expression(paste("Head of livestock per ", km^{2}, " of utlized farmland (log scale)"))) +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with utilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())



# plot lines of best fit for different regions -------------------------------------------------
ggplot2::ggplot(farms1, ggplot2::aes(futilized, iproductivity)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ region, nrow = 4, ncol = 5) +
  ggplot2::theme_bw() +
  # ggplot2::ylab("Head of livestock per square kilometer of utlized farmland") +
  ggplot2::ylab(expression(paste("Farms with investments in soil productivity"))) +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with utilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())

# plot lines of best fit for different regions -------------------------------------------------
ggplot2::ggplot(farms1, ggplot2::aes(funutilized, iproductivity)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ region, nrow = 4, ncol = 5) +
  ggplot2::theme_bw() +
  # ggplot2::ylab("Head of livestock per square kilometer of utlized farmland") +
  ggplot2::ylab(expression(paste("Farms with investments in soil productivity"))) +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with unutilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())




# 2) the relationship between the number of farms and the heads of livestock per squared kilometer



# estimate the scale of slopes that summarize the relationship between density of farmland and total hectares
models <- by(data = farms1, INDICES = list(farms1$region), FUN = function(x) {
  model <- stats::lm(log(pd.livestock + 1) ~ log(futilized + 1), data = x)
  stats::coef(model)
    }
  )
do.call("rbind", models)

# plot lines of best fit for different regions -------------------------------------------------
ggplot2::ggplot(farms1, ggplot2::aes(futilized, pd.livestock)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ region, nrow = 4, ncol = 5) +
  ggplot2::theme_bw() +
  # ggplot2::ylab("Head of livestock per square kilometer of utlized farmland") +
  ggplot2::ylab(expression(paste("Head of livestock per ", km^{2}, " of utlized farmland (log scale)"))) +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with utilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())




# estimate the scale of slopes that summarize the relationship between density of farmland and total hectares
models <- by(data = farms1, INDICES = list(farms1$region), FUN = function(x) {
  model <- stats::lm(log(livestock + 1) ~ log(futilized + 1), data = x)
  stats::coef(model)
    }
  )
do.call("rbind", models)

# plot lines of best fit for different regions -------------------------------------------------
ggplot2::ggplot(farms1, ggplot2::aes(futilized, livestock)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ region, nrow = 4, ncol = 5) +
  ggplot2::theme_bw() +
  # ggplot2::ylab("Head of livestock per square kilometer of utlized farmland") +
  ggplot2::ylab(expression(paste("Head of livestock per on utlized farmland (log scale)"))) +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with utilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())



#
# drop duplicates
farms1 <- unique(farms1)

# case number for sorting
farms1 <- dplyr::mutate(farms1, case = dplyr::row_number(farms1))

# sort by the size of the farm
farms1 <- dplyr::arrange(farms1, dplyr::desc(htotal))

# sort by the size of the farm
farms1 <- dplyr::arrange(farms1, case)

# drop cases that have no values
farms1 <- dplyr::filter(farms1, !is.na(htotal))

# sum the total number of famrs
sum(farms1$ftotal) # this number is the number reported by the agricultural census for all of Italy



# rank order the size of farms
farms1 <- farms1 %>% dplyr::mutate(rank = dplyr::dense_rank(dplyr::desc(htotal)))
farms1 <- farms1 %>% dplyr::arrange(rank)







# province-level analysis ---------------------------------------------------------

# data for municipalities in Sicily
sicily <- dplyr::filter(farms1, region == "Sicilia")

# histogram of the size of the average parcel of farmland
ggplot2::ggplot(subset(sicily, hutilized > 0), ggplot2::aes(havgutilized)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count / sum(count))), fill = "white", colour = "black", alpha = 1.00) +
  ggplot2::facet_wrap(~ province, nrow = 3, ncol = 3) +
  ggplot2::theme_bw() + 
  ggplot2::xlim(0, 50) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::ylab("Frequency") +
  ggplot2::xlab("Average size of farm, hectares of utulized farmland")

# histogram of the size of the average parcel of farmland
ggplot2::ggplot(subset(sicily, hutilized > 0), ggplot2::aes(havgunutilized)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count / sum(count))), fill = "white", colour = "black", alpha = 1.00) +
  ggplot2::facet_wrap(~ province, nrow = 3, ncol = 3) +
  ggplot2::theme_bw() + 
  ggplot2::xlim(0, 50) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::ylab("Frequency") +
  ggplot2::xlab("Average size of farm, hectares of utulized farmland")

# plot lines of best fit for different provinces -------------------------------------------------
ggplot2::ggplot(sicily, ggplot2::aes(futilized, hutilized)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ province, nrow = 3, ncol = 3) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Hectares of utilized farmland (log scale)") +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with utilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())

# plot lines of best fit for different provinces -------------------------------------------------
ggplot2::ggplot(sicily, ggplot2::aes(funutilized, hunutilized)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ province, nrow = 3, ncol = 3) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Hectares of unutilized farmland (log scale)") +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with unutilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())

# plot lines of best fit for different province -------------------------------------------------
ggplot2::ggplot(sicily, ggplot2::aes(futilized, havgutilized)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ province, nrow = 3, ncol = 3) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Hectares of utilized farmland (log scale)") +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with utilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())

# plot lines of best fit for different province -------------------------------------------------
ggplot2::ggplot(sicily, ggplot2::aes(futilized, iproductivity)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ province, nrow = 3, ncol = 3) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Hectares of utilized farmland (log scale)") +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with utilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())

# plot lines of best fit for different province -------------------------------------------------
ggplot2::ggplot(sicily, ggplot2::aes(funutilized, iproductivity)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ province, nrow = 3, ncol = 3) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Hectares of utilized farmland (log scale)") +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with utilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())

# plot lines of best fit for different province -------------------------------------------------
ggplot2::ggplot(sicily, ggplot2::aes(livestock, ilivestock)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ province, nrow = 3, ncol = 3) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Hectares of utilized farmland (log scale)") +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with utilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())



# plot lines of best fit for different province -------------------------------------------------
ggplot2::ggplot(sicily, ggplot2::aes(futilized, imechanization)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ province, nrow = 3, ncol = 3) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Hectares of utilized farmland (log scale)") +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with utilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())



# plot lines of best fit for different province -------------------------------------------------
ggplot2::ggplot(sicily, ggplot2::aes(funutilized, imechanization)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ province, nrow = 3, ncol = 3) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Hectares of utilized farmland (log scale)") +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Farms with utilized farmland (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())

# plot lines of best fit for different province -------------------------------------------------
ggplot2::ggplot(sicily, ggplot2::aes(havgutilized, imechanization)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ province, nrow = 3, ncol = 3) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Farms with investments in mechanization (log scale)") +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Average size of utilized farmland, hectares (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())


# plot lines of best fit for different province -------------------------------------------------
ggplot2::ggplot(farms1, ggplot2::aes(havgutilized, imechanization)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ region, nrow = 4, ncol = 5) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Farms with investments in mechanization (log scale)") +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Average size of utilized farmland, hectares (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())


# plot lines of best fit for different province -------------------------------------------------
ggplot2::ggplot(farms1, ggplot2::aes(havgutilized, imechanization)) + 
  ggplot2::geom_point(fill = "white", colour = "black", alpha = 0.10) + 
  ggplot2::geom_smooth(formula = "y~x-0", method = 'lm', se = TRUE, col = "red", fill = "red") +
  ggplot2::facet_wrap(~ region, nrow = 4, ncol = 5) +
  ggplot2::theme_bw() +
  ggplot2::ylab("Farms with investments in mechanization (log scale)") +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::xlab("Average size of utilized farmland, hectares (log scale)") +
  ggplot2::scale_x_log10(labels = scales::label_number())
