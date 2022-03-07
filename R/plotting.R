cerego_matching_plot <- function(results.summary, error_bar_factor = 1.96) {
	results.summary$Sig <- cut(
		results.summary$p, 
		breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
		labels = c('p < 0.001', 'p < 0.01', 'p < 0.05', 'p > 0.05'))
	results.summary$Sig.Strata <- cut(
		results.summary$p.Strata, 
		breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
		labels = c('p < 0.001', 'p < 0.01', 'p < 0.05', 'p > 0.05'))
	
	results.summary$Holdout_Diff <- results.summary$treat - results.summary$holdoutMean
	results.summary[results.summary$Outcome == 'FinalAverage',]$Holdout_Diff <- NA
	
	ggplot(results.summary, aes(x=Outcome, y=Estimate)) + 
		geom_errorbar(aes(ymax=(Estimate + error_bar_factor * se), 
						  ymin=(Estimate - error_bar_factor * se)), 
					  color='black', alpha=0.5, width=0) +
		geom_hline(yintercept=0) +
		geom_text(aes(label=paste0('n = ', nTreat, '\nAvg Time\n= ', round(meanTime), ' mins')), 
				  y=-8, size=3) +
		geom_text(aes(label=prettyNum(Estimate, digits=3)), 
				  size=3, hjust=-0.5) +
		geom_point(aes(y = Holdout_Diff), 
				   position = position_nudge(x = -0.25), 
				   color = 'maroon',
				   shape = 8) +
		geom_point(aes(color=Sig, shape=Sig), size=4) + 
		# scale_x_discrete(limits = c(paste0('Quiz', 1:8), 'FinalAverage'),
		# 				 labels = c(paste0('Quiz ', 1:8), 'Final Average')) +
		scale_color_manual('Significance Level', values=c('p < 0.001' = '#386cb0', 
														  'p < 0.01' = '#fdc086', 
														  'p < 0.05' = '#7fc97f', 
														  'p > 0.05' = 'grey70'),
						   drop=FALSE) +
		scale_shape_manual('Significance Level', values=c('p < 0.001' = 17, 
														  'p < 0.01' = 16, 
														  'p < 0.05' = 15, 
														  'p > 0.05' = 1),
						   drop=FALSE) +
		# ylim(c(-10,25)) +
		expand_limits(y = -10) +
		xlab('') + ylab('Average Treatment Effect') +
		theme_minimal() +
		theme(legend.position = 'bottom')
}

cerego_strata_plot <- function(results.summary, error_bar_factor = 1.96) {
	results.summary$Sig <- cut(
		results.summary$p, 
		breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
		labels = c('p < 0.001', 'p < 0.01', 'p < 0.05', 'p > 0.05'))
	results.summary$Sig.Strata <- cut(
		results.summary$p.Strata, 
		breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
		labels = c('p < 0.001', 'p < 0.01', 'p < 0.05', 'p > 0.05'))
	
	results.summary$Holdout_Diff <- results.summary$treat - results.summary$holdoutMean
	results.summary[results.summary$Outcome == 'FinalAverage',]$Holdout_Diff <- NA
	
	ggplot(results.summary, aes(x=Outcome, y=Estimate.Strata)) + 
		geom_errorbar(aes(ymax=(Estimate.Strata + error_bar_factor * se.Strata), 
						  ymin=(Estimate.Strata - error_bar_factor * se.Strata)), 
					  color='black', alpha=0.5, width=0) +
		geom_hline(yintercept=0) +
		geom_text(aes(label=paste0('n = ', nTreat.Strata, '\nAvg Time\n= ', 
								   round(meanTime), ' mins')), 
				  y=-8, size=3) +
		geom_text(aes(label=prettyNum(Estimate.Strata, digits=3)), 
				  size=3, hjust=-0.5) +
		geom_point(aes(color=Sig.Strata, shape=Sig.Strata), size=4) + 
		# scale_x_discrete(limits = c(paste0('Quiz', 1:8), 'FinalAverage'),
		# 				 labels = c(paste0('Quiz ', 1:8), 'Final Average')) +
		scale_color_manual('Significance Level', values=c('p < 0.001' = '#386cb0', 
														  'p < 0.01' = '#fdc086', 
														  'p < 0.05' = '#7fc97f', 
														  'p > 0.05' = 'grey70'),
						   drop=FALSE) +
		scale_shape_manual('Significance Level', values=c('p < 0.001' = 17, 
														  'p < 0.01' = 16, 
														  'p < 0.05' = 15, 
														  'p > 0.05' = 1),
						   drop=FALSE) +
		# ylim(c(-10,25)) + 
		expand_limits(y = -10) +
		xlab('') + ylab('Average Treatment Effect') +
		theme_minimal() +
		theme(legend.position = 'bottom')
}
