# Master Thesis Project - Extreme Value Theory
# We use here QQ-plots to assess graphically the
# goodness of fit of our extreme distributions

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
	# Computing the 95 % - quantiles for each of the stocks
	quantiles <- c(quantile(df_plain[[1]],0.95), quantile(df_plain[[2]],0.95), quantile(df_plain[[3]],0.975),
					quantile(df_plain[[4]],0.95), quantile(df_plain[[5]],0.95))
					
	# Getting the data above the threshold
	data_bnp <- df_plain[[1]]
	data_bnp <- data_bnp[data_bnp > quantiles[1]]

	data_carrefour <- df_plain[[2]]
	data_carrefour <- data_carrefour[data_carrefour > quantiles[2]]

	data_lvmh <- df_plain[[3]]
	data_lvmh <- data_lvmh[data_lvmh > quantiles[3]]

	data_sanofi <- df_plain[[4]]
	data_sanofi <- data_sanofi[data_sanofi > quantiles[4]]

	data_total <- df_plain[[5]]
	data_total <- data_total[data_total > quantiles[5]]

	# Fitting the data, and getting the corresponding location, scale and 
	# shape parameters respectively, for each stock
	bnp_fit <- (gev.fit(data_bnp))
	carrefour_fit <- gev.fit(data_carrefour)
	lvmh_fit <- gev.fit(data_lvmh)
	sanofi_fit <- gev.fit(data_sanofi)
	total_fit <- gev.fit(data_total)
	
	# Saving the GEV parameters for later use
	params_bnp <- bnp_fit$mle
	params_carrefour <- carrefour_fit$mle
	params_lvmh <- lvmh_fit$mle
	params_sanofi <- sanofi_fit$mle
	params_total <- total_fit$mle

# Drawing the QQ-plots
N <- dim(df_plain)[1]
qVect <- (N + 1 - 1:N)/(N + 1)

	# Ordering the samples (increasing sort)
	data_bnp <- sort(df_plain[[1]])
	data_carrefour <- sort(df_plain[[2]])
	data_lvmh <- sort(df_plain[[3]])
	data_sanofi <- sort(df_plain[[4]])
	data_total <- sort(df_plain[[5]])
	
	# Getting the quantiles of the fitting distributions
	gev_bnp <- qgev(qVect,params_bnp[3],params_bnp[1],params_bnp[2])
	gev_carrefour <- qgev(qVect,params_carrefour[3],params_carrefour[1],params_carrefour[2])
	gev_lvmh <- qgev(qVect,params_lvmh[3],params_lvmh[1],params_lvmh[2])
	gev_sanofi <- qgev(qVect,params_sanofi[3],params_sanofi[1],params_sanofi[2])
	gev_total <- qgev(qVect,params_total[3],params_total[1],params_total[2])
	
	# Plotting for all the Data
	quartz()
    png(file = "QQ_BNP.png")
	plot(qVect,gev_bnp)
	dev.off()
	graphics.off()

	quartz()
	png(file = "QQ_Carrefour.png")
	plot(qVect, gev_carrefour)
	dev.off()
	graphics.off()

	quartz()
	png(file = "QQ_LVMH.png")
	plot(qVect, gev_lvmh)
	dev.off()
	graphics.off()

	quartz()
	png(file = "QQ_Sanofi.png")
	plot(qVect, gev_sanofi)
	dev.off()
	graphics.off()

	quartz()
	png(file = "QQ_Total.png")
	plot(qVect, gev_total)
	dev.off()
	graphics.off()
	
	# Plotting only for the high quantiles
		
		# Computing the 95 % - quantiles for each of the stocks
		quantiles <- c(quantile(df_plain[[1]],0.95), quantile(df_plain[[2]],0.95), quantile(df_plain[[3]],0.975),
					quantile(df_plain[[4]],0.95), quantile(df_plain[[5]],0.95))
					
		# Getting the data above the threshold
		data_bnp <- df_plain[[1]]
		data_bnp <- data_bnp[data_bnp > quantiles[1]]

		data_carrefour <- df_plain[[2]]
		data_carrefour <- data_carrefour[data_carrefour > quantiles[2]]

		data_lvmh <- df_plain[[3]]
		data_lvmh <- data_lvmh[data_lvmh > quantiles[3]]

		data_sanofi <- df_plain[[4]]
		data_sanofi <- data_sanofi[data_sanofi > quantiles[4]]

		data_total <- df_plain[[5]]
		data_total <- data_total[data_total > quantiles[5]]
		
		# BNP
		N_BNP <- length(data_bnp)
		qVectBNP <- (N_BNP + 1 - 1:N_BNP)/(N_BNP + 1)
		
		data_bnp <- sort(data_bnp)
		gev_bnp <- qgev(qVectBNP,params_bnp[3],params_bnp[1],params_bnp[2])
		
		quartz()
    	png(file = "QQ_BNP2.png")
		plot(qVectBNP,gev_bnp, main = "QQ-plot BNP Paribas", xlab ="", ylab = "")
		abline(lm(gev_bnp~qVectBNP), col = "red")
		dev.off()
		graphics.off()
		
		# Carrefour
		N_Carrefour <- length(data_carrefour)
		qVectCarrefour <- (N_Carrefour + 1 - 1:N_Carrefour)/(N_Carrefour + 1)
		
		data_carrefour <- sort(data_carrefour)
		gev_carrefour <- qgev(qVectCarrefour,params_carrefour[3],params_carrefour[1],params_carrefour[2])
		
		quartz()
    	png(file = "QQ_Carrefour2.png")
		plot(qVectCarrefour,gev_carrefour, main = "QQ-plot Carrefour", xlab ="", ylab = "")
		abline(lm(gev_carrefour~qVectCarrefour), col = "red")
		dev.off()
		graphics.off()
		
		# LVMH
		N_LVMH <- length(data_lvmh)
		qVectLVMH <- (N_LVMH + 1 - 1:N_LVMH)/(N_LVMH + 1)
		
		data_lvmh <- sort(data_lvmh)
		gev_lvmh <- qgev(qVectLVMH,params_lvmh[3],params_lvmh[1],params_lvmh[2])
		
		quartz()
    	png(file = "QQ_LVMH2.png")
		plot(qVectLVMH,gev_lvmh, main = "QQ-plot LVMH", xlab ="", ylab = "")
		abline(lm(gev_lvmh~qVectLVMH), col = "red")
		dev.off()
		graphics.off()	
	
		# Sanofi
		N_Sanofi <- length(data_sanofi)
		qVectSanofi <- (N_Sanofi + 1 - 1:N_Sanofi)/(N_Sanofi + 1)
		
		data_sanofi <- sort(data_sanofi)
		gev_sanofi <- qgev(qVectSanofi,params_sanofi[3],params_sanofi[1],params_sanofi[2])
		
		quartz()
    	png(file = "QQ_Sanofi2.png")
		plot(qVectSanofi,gev_sanofi, main = "QQ-plot Sanofi", xlab ="", ylab = "")
		abline(lm(gev_sanofi~qVectSanofi), col = "red")
		dev.off()
		graphics.off()	
	
		# Total
		N_Total <- length(data_total)
		qVectTotal <- (N_Total + 1 - 1:N_Total)/(N_Total + 1)
		
		data_total <- sort(data_total)
		gev_total <- qgev(qVectTotal,params_total[3],params_total[1],params_total[2])
		
		quartz()
    	png(file = "QQ_Total2.png")
		plot(qVectTotal,gev_total, main = "QQ-plot Total", xlab ="", ylab = "")
		abline(lm(gev_total~qVectTotal), col = "red")
		dev.off()
		graphics.off()		

