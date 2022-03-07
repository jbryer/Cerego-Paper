missing.plot <- function(df) {
	tab <- data.frame(Observed=numeric(ncol(df)), Missing=numeric(ncol(df)), row.names=names(df))
	for(i in seq_len(ncol(df))) {
		tmp <- prop.table(table(is.na(df[,i])))
		tab[names(df)[i],]$Observed <- ifelse(is.na(tmp['FALSE']), 0, tmp['FALSE'])
		tab[names(df)[i],]$Missing <- ifelse(is.na(tmp['TRUE']), 0, tmp['TRUE'])
	}
	tab$Variable <- row.names(tab)
	tab <- melt(tab, id.vars='Variable')
	names(tab) <- c('Variable', 'Missing', 'value')
	ggplot(tab[nrow(tab):1,], aes(x=Variable, y=value, fill=Missing, order=Missing)) + 
		geom_bar(stat='identity', position='fill') + 
		geom_text(data=tab[tab$Missing == 'Missing',],
				  aes(label=paste0(round(value*100, digits=2), '%')), hjust=0) +
		xlab('') + ylab('Percent') + 
		scale_fill_brewer('Missing') +
		coord_flip()
}
