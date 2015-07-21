# Master Thesis Project - Extreme Value Theory
# We use here QQ-plots to assess graphically the
# goodness of fit of our extreme distributions for
# the high ( above high quantile) and low (below low
# quantile) log-returns

# Clear the environment
rm(list=ls())

# Close all already open graphic windows
graphics.off()

# Sourcing the auxiliary files
source("loadStockData_plain.r")

# Remark : the csv uses the metastock data format 
#          7 columns : - Ticker (identifier of the stock
#                       and stockmarket on which it is listed)
#					   - Date (yyyymmdd)
#					   - Open
#					   - High
#					   - Low
#					   - Close
#					   - Volume
# By default, the highest price is used i.e. column 3.
CHOICE <- 3

# Loading the original data for the five stocks then
# getting the LVMH and the Total data
df_plain <- loadStockData_plain(CHOICE)

# Fitting
	# Computing the 95 % - quantiles  [97.5 % - quantile in the case of the LVMH stock]
	# and the 5 % quantiles for each of the stocks
	highQuantiles <- c(quantile(df_plain[[1]],0.95), quantile(df_plain[[2]],0.95), quantile(df_plain[[3]],0.975),
					quantile(df_plain[[4]],0.95), quantile(df_plain[[5]],0.95))
	lowQuantiles <- c(quantile(df_plain[[1]],0.05), quantile(df_plain[[2]],0.05), quantile(df_plain[[3]],0.05),
					quantile(df_plain[[4]],0.05), quantile(df_plain[[5]],0.05))	
									
	# Getting the data above the high threshold and below the low threshold
	data_bnp_H <- df_plain[[1]]
	data_bnp_H <- data_bnp_H[data_bnp_H > highQuantiles[1]]
	
	data_bnp_L <- df_plain[[1]]
	data_bnp_L <- data_bnp_L[data_bnp_L < lowQuantiles[1]]

	data_carrefour_H <- df_plain[[2]]
	data_carrefour_H <- data_carrefour_H[data_carrefour_H > highQuantiles[2]]

	data_carrefour_L <- df_plain[[2]]
	data_carrefour_L <- data_carrefour_L[data_carrefour_L < lowQuantiles[2]]
	
	data_lvmh_H <- df_plain[[3]]
	data_lvmh_H <- data_lvmh_H[data_lvmh_H > highQuantiles[3]]
	
	data_lvmh_L <- df_plain[[3]]
	data_lvmh_L <- data_lvmh_L[data_lvmh_L < lowQuantiles[3]]

	data_sanofi_H <- df_plain[[4]]
	data_sanofi_H <- data_sanofi_H[data_sanofi_H > highQuantiles[4]]

	data_sanofi_L <- df_plain[[4]]
	data_sanofi_L <- data_sanofi_L[data_sanofi_L < lowQuantiles[4]]

	data_total_H <- df_plain[[5]]
	data_total_H <- data_total_H[data_total_H > highQuantiles[5]]

	data_total_L <- df_plain[[5]]
	data_total_L <- data_total_L[data_total_L < lowQuantiles[5]]
	
	# Computing the log returns for the abive-high-threshold data
	data_bnp_H <- diff(log(data_bnp_H))
	data_carrefour_H <- diff(log(data_carrefour_H))
	data_lvmh_H <- diff(log(data_lvmh_H))
	data_sanofi_H <- diff(log(data_sanofi_H))
	data_total_H <- diff(log(data_total_H))
	# Computing the minus log returns for the below-low-threshold data
	data_bnp_L <- diff(log(data_bnp_L))
	data_carrefour_L <- diff(log(data_carrefour_L))
	data_lvmh_L <- diff(log(data_lvmh_L))
	data_sanofi_L <- diff(log(data_sanofi_L))
	data_total_L <- diff(log(data_total_L))

	# Fitting the data, and getting the corresponding location, scale and 
	# shape parameters respectively, for each stock
	# Fitting the log-returns for the above-high-threshold data
	bnp_fit_H <- (gev.fit(data_bnp_H))
	carrefour_fit_H <- gev.fit(data_carrefour_H)
	lvmh_fit_H <- gev.fit(data_lvmh_H)
	sanofi_fit_H <- gev.fit(data_sanofi_H)
	total_fit_H <- gev.fit(data_total_H)
	# Fitting the minus log-returns for the below-low-threshold data
	bnp_fit_L <- (gev.fit(data_bnp_L))
	carrefour_fit_L <- gev.fit(data_carrefour_L)
	lvmh_fit_L <- gev.fit(data_lvmh_L)
	sanofi_fit_L <- gev.fit(data_sanofi_L)
	total_fit_L <- gev.fit(data_total_L)	
	
	# Saving the GEV parameters for later use
	params_bnp_H <- bnp_fit_H$mle
	params_carrefour_H <- carrefour_fit_H$mle
	params_lvmh_H <- lvmh_fit_H$mle
	params_sanofi_H <- sanofi_fit_H$mle
	params_total_H <- total_fit_H$mle
	
	params_bnp_L <- bnp_fit_L$mle
	params_carrefour_L <- carrefour_fit_L$mle
	params_lvmh_L <- lvmh_fit_L$mle
	params_sanofi_L <- sanofi_fit_L$mle
	params_total_L <- total_fit_L$mle

# Drawing the QQ-plots
	# BNP stock
	
		N_bnp_H <- length(data_bnp_H)
		qVect_bnp_H <- (N_bnp_H + 1 - 1:N_bnp_H)/(N_bnp_H + 1)
		N_bnp_L <- length(data_bnp_L)
		qVect_bnp_L <- (N_bnp_L + 1 - 1:N_bnp_L)/(N_bnp_L + 1)
		
		# Ordering the samples
		data_bnp_H <- sort(data_bnp_H)
		data_bnp_L <- sort(data_bnp_L, decreasing = TRUE)
		
		# Getting the quantiles of the fitting distributions
		gev_bnp_H <- qgev(qVect_bnp_H ,params_bnp_H[3],params_bnp_H[1],params_bnp_H[2])
		gev_bnp_L <- qgev(qVect_bnp_L ,params_bnp_L[3],params_bnp_L[1],params_bnp_L[2])
		
		# Plotting
		quartz()
    	png(file = "QQ_BNP_High.png")
		plot(qVect_bnp_H,gev_bnp_H, main = "QQ-plot BNP Paribas, above \n95 %-quantile data", xlab ="", ylab = "")
		abline(lm(gev_bnp_H~qVect_bnp_H), col = "red")
		dev.off()
		graphics.off()
		
		quartz()
    	png(file = "QQ_BNP_Low.png")
		plot(qVect_bnp_L,gev_bnp_L, main = "QQ-plot BNP Paribas, below \n5 %-quantile data", xlab ="", ylab = "")
		abline(lm(gev_bnp_L~qVect_bnp_L), col = "blue")
		dev.off()
		graphics.off()
		
	# Carrefour stock
	
		N_carrefour_H <- length(data_carrefour_H)
		qVect_carrefour_H <- (N_carrefour_H + 1 - 1:N_carrefour_H)/(N_carrefour_H + 1)
		N_carrefour_L <- length(data_carrefour_L)
		qVect_carrefour_L <- (N_carrefour_L + 1 - 1:N_carrefour_L)/(N_carrefour_L + 1)
		
		# Ordering the samples
		data_carrefour_H <- sort(data_carrefour_H)
		data_carrefour_L <- sort(data_carrefour_L, decreasing = TRUE)
		
		# Getting the quantiles of the fitting distributions
		gev_carrefour_H <- qgev(qVect_carrefour_H ,params_carrefour_H[3],params_carrefour_H[1],params_carrefour_H[2])
		gev_carrefour_L <- qgev(qVect_carrefour_L ,params_carrefour_L[3],params_carrefour_L[1],params_carrefour_L[2])
		
		# Plotting
		quartz()
    	png(file = "QQ_Carrefour_High.png")
		plot(qVect_carrefour_H,gev_carrefour_H, main = "QQ-plot Carrefour, above \n95 %-quantile data", xlab ="", ylab = "")
		abline(lm(gev_carrefour_H~qVect_carrefour_H), col = "red")
		dev.off()
		graphics.off()
		
		quartz()
    	png(file = "QQ_Carrefour_Low.png")
		plot(qVect_carrefour_L,gev_carrefour_L, main = "QQ-plot Carrefour, below \n5 %-quantile data", xlab ="", ylab = "")
		abline(lm(gev_carrefour_L~qVect_carrefour_L), col = "blue")
		dev.off()
		graphics.off()

	# LVMH stock
	
		N_lvmh_H <- length(data_lvmh_H)
		qVect_lvmh_H <- (N_lvmh_H + 1 - 1:N_lvmh_H)/(N_lvmh_H + 1)
		N_lvmh_L <- length(data_lvmh_L)
		qVect_lvmh_L <- (N_lvmh_L + 1 - 1:N_lvmh_L)/(N_lvmh_L + 1)
		
		# Ordering the samples
		data_lvmh_H <- sort(data_lvmh_H)
		data_lvmh_L <- sort(data_lvmh_L, decreasing = TRUE)
		
		# Getting the quantiles of the fitting distributions
		gev_lvmh_H <- qgev(qVect_lvmh_H ,params_lvmh_H[3],params_lvmh_H[1],params_lvmh_H[2])
		gev_lvmh_L <- qgev(qVect_lvmh_L ,params_lvmh_L[3],params_lvmh_L[1],params_lvmh_L[2])
		
		# Plotting
		quartz()
    	png(file = "QQ_LVMH_High.png")
		plot(qVect_lvmh_H,gev_lvmh_H, main = "QQ-plot LVMH, above \n95 %-quantile data", xlab ="", ylab = "")
		abline(lm(gev_lvmh_H~qVect_lvmh_H), col = "red")
		dev.off()
		graphics.off()
		
		quartz()
    	png(file = "QQ_LVMH_Low.png")
		plot(qVect_lvmh_L,gev_lvmh_L, main = "QQ-plot LVMH, below \n5 %-quantile data", xlab ="", ylab = "")
		abline(lm(gev_lvmh_L~qVect_lvmh_L), col = "blue")
		dev.off()
		graphics.off()

	# Sanofi stock
	
		N_sanofi_H <- length(data_sanofi_H)
		qVect_sanofi_H <- (N_sanofi_H + 1 - 1:N_sanofi_H)/(N_sanofi_H + 1)
		N_sanofi_L <- length(data_sanofi_L)
		qVect_sanofi_L <- (N_sanofi_L + 1 - 1:N_sanofi_L)/(N_sanofi_L + 1)
		
		# Ordering the samples
		data_sanofi_H <- sort(data_sanofi_H)
		data_sanofi_L <- sort(data_sanofi_L, decreasing = TRUE)
		
		# Getting the quantiles of the fitting distributions
		gev_sanofi_H <- qgev(qVect_sanofi_H ,params_sanofi_H[3],params_sanofi_H[1],params_sanofi_H[2])
		gev_sanofi_L <- qgev(qVect_sanofi_L ,params_sanofi_L[3],params_sanofi_L[1],params_sanofi_L[2])
		
		# Plotting
		quartz()
    	png(file = "QQ_Sanofi_High.png")
		plot(qVect_sanofi_H,gev_sanofi_H, main = "QQ-plot Sanofi, above \n95 %-quantile data", xlab ="", ylab = "")
		abline(lm(gev_sanofi_H~qVect_sanofi_H), col = "red")
		dev.off()
		graphics.off()
		
		quartz()
    	png(file = "QQ_Sanofi_Low.png")
		plot(qVect_sanofi_L,gev_sanofi_L, main = "QQ-plot Sanofi, below \n5 %-quantile data", xlab ="", ylab = "")
		abline(lm(gev_sanofi_L~qVect_sanofi_L), col = "blue")
		dev.off()
		graphics.off()

	# Total stock
	
		N_total_H <- length(data_total_H)
		qVect_total_H <- (N_total_H + 1 - 1:N_total_H)/(N_total_H + 1)
		N_total_L <- length(data_total_L)
		qVect_total_L <- (N_total_L + 1 - 1:N_total_L)/(N_total_L + 1)
		
		# Ordering the samples
		data_total_H <- sort(data_total_H)
		data_total_L <- sort(data_total_L, decreasing = TRUE)
		
		# Getting the quantiles of the fitting distributions
		gev_total_H <- qgev(qVect_total_H ,params_total_H[3],params_total_H[1],params_total_H[2])
		gev_total_L <- qgev(qVect_total_L ,params_total_L[3],params_total_L[1],params_total_L[2])
		
		# Plotting
		quartz()
    	png(file = "QQ_Total_High.png")
		plot(qVect_total_H,gev_total_H, main = "QQ-plot Total, above \n95 %-quantile data", xlab ="", ylab = "")
		abline(lm(gev_total_H~qVect_total_H), col = "red")
		dev.off()
		graphics.off()
		
		quartz()
    	png(file = "QQ_Total_Low.png")
		plot(qVect_total_L,gev_total_L, main = "QQ-plot Total, below \n5 %-quantile data", xlab ="", ylab = "")
		abline(lm(gev_total_L~qVect_total_L), col = "blue")
		dev.off()
		graphics.off()	