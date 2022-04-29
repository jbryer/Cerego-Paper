# Perform matching and stratification results. Returns as a list with three elements: 
# * match.out - the reuslts of match.results which wraps Matchby
# * strata.out - the results of circ.psa
# * summary - a data frame with the summaries of the above PSA methods
psa.cerego <- function(df, 
					   df.complete, 
					   out.cols, 
					   treat.cols, 
					   nStrata = 5,
					   by.cols = c('Ethnicity','Gender'),
					   caliper = 0.15, 
					   replace = FALSE, 
					   ties = FALSE, 
					   estimand = 'ATE',
					   M = 1,
					   treat.minutes = 5) {
	stopifnot(length(out.cols) == length(treat.cols))
	quizzes <- list()
	for(i in seq_along(out.cols)) {
		out.col <- out.cols[i]
		treat.col <- treat.cols[i]
		treat.rows <- which(
			!is.na(df[,out.col]) & 
			!is.na(df[,treat.col]) & 
			df[,treat.col] > treat.minutes)
		control.rows <- which(
			!is.na(df[,out.col]) & 
			!is.na(df[,treat.col]) & 
			df[,treat.col] <= treat.minutes) 
		rows <- c(treat.rows, control.rows)
		
		# Treatment students who did not use the treatment for this outcome
		holdouts <- which(
			!is.na(df[,out.col]) & 
			!is.na(df[,treat.col]) &
			df[,treat.col] <= treat.minutes & 
			df$Treat)
		
		strata <- cut(df.complete$ps, 
					  quantile(df.complete$ps, seq(0,1,1/nStrata)), 
					  include.lowest = TRUE, 
					  labels = LETTERS[1:nStrata])
		quizzes[[out.col]]$match.out <- match.results(
			df.complete[rows,],
			outcome = df[rows,out.col],
			treat = df[rows,treat.col] > treat.minutes,
			by.cols = by.cols,
			caliper = caliper,
			replace = replace,
			ties = ties, 
			estimand = 'ATE',
			M = M)
		quizzes[[out.col]]$strata.out <- circ.psa(
			df[rows,out.col],
			df[rows,treat.col] > treat.minutes,
			strata[rows]
		)
		quizzes[[out.col]]$summary <- data.frame(
			Outcome = out.col,
			Estimate = quizzes[[i]]$match.out$est, 
			Estimate.Standardized = quizzes[[i]]$match.out$est / sd(df[,out.col], na.rm=TRUE),
			se = quizzes[[i]]$match.out$se.standard,
			nMatches = quizzes[[i]]$match.out$wnobs,
			nTreat = length(unique(quizzes[[i]]$match.out$index.treated)),
			nControl = length(unique(quizzes[[i]]$match.out$index.control)),
			t = quizzes[[i]]$match.out$est / quizzes[[i]]$match.out$se.standard,
			p = 2 * (1 - pnorm(abs(quizzes[[i]]$match.out$est / quizzes[[i]]$match.out$se.standard))),
			Estimate.Strata = quizzes[[i]]$strata.out$ATE,
			se.Strata = quizzes[[i]]$strata.out$se.wtd,
			t.Strata = quizzes[[i]]$strata.out$approx.t,
			p.Strata = 2 * (1 - pt(quizzes[[i]]$strata.out$approx.t, df=quizzes[[i]]$strata.out$df)),
			nTreat.Strata = sum(df[rows,treat.col] > treat.minutes),
			meanTime = mean(df[ df[,treat.col] > treat.minutes, treat.col ]),
			medianTime = median(df[ df[,treat.col] > treat.minutes, treat.col ]),
			nHoldout = length(holdouts),
			holdoutMean = mean(df[holdouts,out.col]),
			treatMean = mean(df[treat.rows,out.col]),
			controlMean = mean(df[control.rows,out.col])
		)
	}
	return(quizzes)
}
