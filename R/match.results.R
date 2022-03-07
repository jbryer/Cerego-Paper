# Return summary of results from Matchby
match.results <- function(df,
						  outcome,
						  treat, 
						  print.level = 0 ,
						  by.cols = c('Ethnicity','Gender'), ...) {
	rows <- !is.na(outcome)
	outcome <- outcome[rows]
	treat <- treat[rows]
	df <- df[rows,]
	by <- list()
	for(j in by.cols) {
		by[[j]] <- df[,j]
	}
	match.out <- Matchby(outcome, treat, df$ps, by=by,
						 print.level=print.level, ...)
	return(match.out)
}

