cv.bal.psa <- function (covariates, treatment, propensity, strata = NULL, int = NULL, 
		  tree = FALSE, minsize = 2, universal.psd = TRUE, trM = 0, 
		  absolute.es = TRUE, trt.value = NULL, use.trt.var = FALSE, 
		  verbose = FALSE, xlim = NULL, plot.strata = TRUE, ...) 
{
	X <- covariates
	treat.lev <- sort(unique(treatment))
	if (is.null(trt.value)) 
		trt.value = treat.lev[2]
	if (!is.null(trt.value)) {
		if ((trt.value != treat.lev[2]) & (trt.value != treat.lev[1])) {
			stop("WARNING: trt.value as defined does not match a treatment level")
		}
		if (trt.value == treat.lev[1]) {
			treat.lev <- treat.lev[c(2, 1)]
		}
	}
	cstrat <- cstrata.psa(treatment = treatment, propensity = propensity, 
						  strata = strata, int = int, tree = tree, minsize = minsize, 
						  graphic = FALSE)
	shom <- cstrat$Original.Strata
	som <- cstrat$Used.Strata
	psct <- cstrat$strata
	XX <- cbind(X, treatment, psct)
	names.cov <- colnames(X)
	n.cov <- length(names.cov)
	names.strata <- colnames(som)
	n.strata <- length(names.strata)
	uess = matrix(0, nrow = n.cov, ncol = 2)
	effect.size.ji = matrix(0, nrow = n.cov, ncol = n.strata)
	effect.size.ji.adj = matrix(0, nrow = n.cov, ncol = n.strata)
	var.cov.by.strattreat = matrix(0, nrow = n.cov, ncol = 2 * 
								   	n.strata)
	mean.diff <- matrix(0, nrow = n.cov, ncol = n.strata)
	mean.diff.adj = matrix(0, nrow = n.cov, ncol = n.strata)
	sd.adj <- matrix(0, nrow = n.cov, ncol = n.strata)
	sd.un <- rep(0, n.cov)
	mean.diff.unadj <- rep(0, nrow = n.cov)
	for (j in 1:n.cov) {
		for (i in 1:n.strata) {
			ha = XX[XX[, n.cov + 2] == names.strata[i], c(j, 
														  n.cov + 1)]
			mean.diff[j, i] = (mean(ha[ha[, 2] == treat.lev[2], 
									   1], trim = trM) - mean(ha[ha[, 2] == treat.lev[1], 
									   						  1], trim = trM))
			mean.diff.adj[j, i] = mean.diff[j, i] * som[3, i]/sum(som[3, 
																	  ])
			if (use.trt.var) {
				var.cov.by.strattreat[j, i] = var(ha[ha[, 2] == 
													 	treat.lev[2], 1])
				var.cov.by.strattreat[j, i + n.strata] = var(ha[ha[, 
																   2] == treat.lev[2], 1])
				sd.adj[j, i] = sd(ha[ha[, 2] == treat.lev[2], 
									 1])
			}
			else {
				var.cov.by.strattreat[j, i] = var(ha[ha[, 2] == 
													 	treat.lev[1], 1])
				var.cov.by.strattreat[j, i + n.strata] = var(ha[ha[, 
																   2] == treat.lev[2], 1])
				sd.adj[j, i] = sqrt((var.cov.by.strattreat[j, 
														   i] + var.cov.by.strattreat[j, i + n.strata])/2)
			}
		}
		mean.diff.unadj[j] = (mean(XX[XX[, n.cov + 1] == treat.lev[2], 
									  j], trim = trM) - mean(XX[XX[, n.cov + 1] == treat.lev[1], 
									  						  j], trim = trM))
		if (use.trt.var) {
			sd.un[j] = sd(XX[XX[, n.cov + 1] == treat.lev[2], 
							 j])
		}
		else {
			sd.un[j] = sqrt((var(XX[XX[, n.cov + 1] == treat.lev[1], 
									j]) + var(XX[XX[, n.cov + 1] == treat.lev[2], 
												 j]))/2)
		}
		uess[j, 1] = if (sd.un[j] > 0) {
			mean.diff.unadj[j]/sd.un[j]
		}
		else {
			0
		}
		if (universal.psd == TRUE) {
			sd.adj[j, ] = sd.un[j]
		}
		for (i in 1:n.strata) {
			effect.size.ji[j, i] = if (sd.adj[j, i] > 0) {
				mean.diff[j, i]/sd.adj[j, i]
			}
			else {
				0
			}
			effect.size.ji.adj[j, i] = if (sd.adj[j, i] > 0) {
				mean.diff.adj[j, i]/sd.adj[j, i]
			}
			else {
				0
			}
		}
		uess[j, 2] = sum(effect.size.ji.adj[j, ])
	}
	n.strata2 = n.strata * 2
	sd.un <- matrix(sd.un, ncol = 1)
	rownames(sd.un) <- names.cov
	dimnames(uess) = list(names.cov, c("stES_unadj", "stES_adj"))
	dimnames(mean.diff) = list(names.cov, names.strata)
	dimnames(mean.diff.adj) = list(names.cov, names.strata)
	dimnames(effect.size.ji) = list(names.cov, names.strata)
	mean.diff.unadj <- matrix(mean.diff.unadj, ncol = 1)
	dimnames(mean.diff.unadj) = list(names.cov, "mean.diff_unadj")
	dimnames(var.cov.by.strattreat) = list(names.cov, paste("cellvar", 
															1:n.strata2))
	if (absolute.es == TRUE) {
		effect.size.ji = abs(effect.size.ji)
		mean.diff = abs(mean.diff)
		mean.diff.adj = abs(mean.diff.adj)
		mean.diff.unadj = abs(mean.diff.unadj)
		uess = abs(uess)
	}
	se = order(uess[, 1])
	se2 = order(uess[, 1], decreasing = TRUE)
	sd.un = as.matrix(sd.un[se2, ])
	colnames(sd.un) <- "st.dev.unadj"
	ord.uess = uess[se, ]
	ord.uess.2 = uess[se2, ]
	effect.size.ji1 = effect.size.ji[se, ]
	effect.size.ji2 = effect.size.ji[se2, ]
	mean.diff.adj = mean.diff.adj[se2, ]
	mean.diff.unadj = mean.diff.unadj[se2, ]
	var.cov.by.strattreat = var.cov.by.strattreat[se2, ]
	mean.diff = mean.diff[se2, ]
	colnames(effect.size.ji2) = letters[1:n.strata]
	colnames(som) = letters[1:n.strata]
	colnames(mean.diff) = letters[1:n.strata]
	colnames(mean.diff.adj) = letters[1:n.strata]
	colnames(var.cov.by.strattreat) = c(paste(letters[1:n.strata], 
											  "_", treat.lev[1], sep = ""), paste(letters[1:n.strata], 
											  									"_", treat.lev[2], sep = ""))
	final = cbind(ord.uess, effect.size.ji1)
	if (is.null(xlim)) {
		xlim <- 1.1 * range(final)
		if (xlim[1] > 0) 
			xlim[1] <- 0
	}
	y.coord <- matrix(rep(1:n.cov, (2 + n.strata)), nrow = n.cov)
	main <- "Standardized Covariate Effect Sizes \nw/ & w/o PS adjustment"
	if (absolute.es == TRUE) {
		main <- paste("Absolute", main)
	}
	plot(final, y.coord, xlim = xlim, ylim = NULL, axes = FALSE, 
		 sub = "Open circles are stES-unadj; Closed circles are stES-adj", 
		 xlab = paste("Standardized Effect Sizes: treatment", 
		 			 treat.lev[2], "- treatment ", treat.lev[1]), ylab = " ", 
		 main = main, font = 2, cex = 0, ...)
	axis(1, font = 2, las = 1)
	axis(2, 1:n.cov, dimnames(ord.uess)[[1]], font = 2, las = 1, 
		 tick = FALSE, cex.axis = 0.79)
	points(final[, 1], y.coord[, 1], col = "dark red", pch = 21)
	lines(final[, 1], y.coord[, 1], col = "dark red", lwd = 1.5)
	points(final[, 2], y.coord[, 2], col = "blue", pch = 19)
	lines(final[, 2], y.coord[, 2], col = "blue", lwd = 1.5)
	if (plot.strata) {
		for (m in 3:(2 + n.strata)) {
			points(final[, m], y.coord[, m], col = "blue", pch = 94 + 
				   	m, cex = 0.66)
		}
	}
	abline(v = 0, lty = 2, lwd = 1.2)
	abline(v = seq(round(xlim[1] - 1, 0), round(xlim[2] + 1, 
												0), 0.5), lty = 3, lwd = 0.7)
	box()
	sd.ESs = apply(effect.size.ji2, 1, sd)
	final2 = round(cbind(ord.uess.2, effect.size.ji2, sd.ESs), 
				   2)
	out <- list(shom, som, round(mean.diff.adj, 2), round(mean.diff.unadj, 
														  2), final2, treat.lev, round(cbind(sd.un, var.cov.by.strattreat), 
														  							 2))
	names(out) <- c("original.strata", "strata.used", "mean.diff.strata.wtd", 
					"mean.diff.unadj", "effect.sizes", "treatment.levels", 
					"effects.strata.treatment")
	if (verbose) {
		return(out)
	}
	else {
		return(invisible(out))
	}
}
