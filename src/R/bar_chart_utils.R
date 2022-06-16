# function to make bar charts

select_and_summarize <- function(usedf, cols, selections) {
	# select frome a dataframe and summarize elements for bar chart

	Nsample <- "Nsample"

	# group the data to get the total number in each subset
	out <- usedf %>% 
		group_by_at(selections, .drop=FALSE) %>% 
		mutate(Nsample = n()) %>%
		group_by_at(append(selections, c(Nsample)), .drop=FALSE) %>%
		summarise_at(vars(one_of(cols)), sum, na.rm=TRUE)

	return(out)

}

calculate_pct_and_sig <- function(usedf, cols, selections, mortality) {
	
	if (mortality){
		# We want the denominator to be the sum of all elements for all selections except the last in the list
		lived <- usedf[usedf$Outcome == "Lived",]
		died <- usedf[usedf$Outcome == "Died",]

		# calculate percentages and errors
		pctdf <- died
		sigdf <- died
		numdf <- died
		dendf <- died

	} else {

		# calculate percentages and errors
		pctdf <- usedf
		sigdf <- usedf
		numdf <- usedf
		dendf <- usedf
	}
	
	# calculate the percentage and error (using error propagation)
	# for mortality, we want the denominator to be the sum of all elements for all selections except the last in the list
	# for non-mortality, simply use the Nsample value for the denominator

	for (cc in cols){
		num <- pctdf[cc]
		ifelse(mortality, den <- lived[cc] + died[cc], den <- usedf$Nsample)
		pctdf[cc] <- round(100.*( num/den ), ndigits)
		sigdf[cc] <- round(100.*( num/den**2. + (num/den**2.)**2.*den )**0.5, ndigits)
		numdf[cc] <- num
		dendf[cc] <- den
		# assuming no error on denominator
		#sigdf1[cc] <- 100.*num**0.5/den
	}
	
	return(list("pct" = pctdf, "sig" = sigdf, "num" = numdf, "den" = dendf))
}

cbind_and_pivot <- function(psdf, plot_type, selections, mortality){

	# reorder this table so that it is easier to use with ggplot

	pctdf <- psdf$pct
	sigdf <- psdf$sig
	numdf <- psdf$num
	dendf <- psdf$den

	out <- pctdf
	out <- pivot_longer(data = out,
	   cols = -append(selections, c("Nsample")),
	   names_to = plot_type,
	   values_to = "percent")


	tmpdf <- pivot_longer(data = sigdf,
	   cols = -append(selections, c("Nsample")),
	   names_to = plot_type,
	   values_to = "sig_percent")
	
	tmpdfn <- pivot_longer(data = numdf,
	   cols = -append(selections, c("Nsample")),
	   names_to = plot_type,
	   values_to = "number")

	tmpdfd <- pivot_longer(data = dendf,
	   cols = -append(selections, c("Nsample")),
	   names_to = plot_type,
	   values_to = "sample")
	
	out <- cbind(out, sig_percent=tmpdf$sig_percent, number=tmpdfn$number, sample=tmpdfd$sample)

	# select the columns we want
	out <- out[, append(selections, c(plot_type, "percent", "sig_percent", "number", "sample"))]


	return(out)

}

add_tooltips <- function(usedf, selections){
	# define the tooltip
	text <- ""
	for (ss in selections){
		text <- paste0(text, '<b>',str_replace_all(ss,'_',' '), ': </b>', usedf[[ss]], '<br/>')
	}
	usedf$tooltip <- paste0(text, '<b>percent : </b>',usedf$percent, ' +/- ', usedf$sig_percent,' %',
		'<br/><b>number : </b>', usedf$number,
		'<br/><b>sample size: </b>', usedf$sample)

	return(usedf)
}

set_fill_patterns <- function(usedf, col){

	col_vals <- unlist(unique(usedf[col]), use.names = FALSE)
	col_patterns <- patterns[(1:length(col_vals))]
	names(col_patterns) <- col_vals
	col_angles <- pattern_angles[(1:length(col_vals))]
	names(col_angles) <- col_vals

	return(list("patterns" = col_patterns, "angles" = col_angles))
}

prep_bar_chart_data <- function(usedf, plot_type, cols, agg1, agg2, df1=NULL, df1m=NULL){

	############################################
	# percentage within each group given the selections
	selections <- c(agg1)
	if (agg2 != "None") selections <- append(selections, agg2)

	if (is.null(df1)) df1 <- select_and_summarize(usedf, cols, selections)
	psdf <- calculate_pct_and_sig(df1, cols, selections, FALSE)
	outdf <- cbind_and_pivot(psdf, plot_type, selections)
	outdf <- add_tooltips(outdf, selections)

	############################################
	# mortality percentage given the selections
	selectionsm <- selections
	if (agg1 != "Outcome" && agg2 != "Outcome") selectionsm <- append(selectionsm, "Outcome")
	if (is.null(df1m)) df1m <- select_and_summarize(usedf, cols, selectionsm)
	psdfm <- calculate_pct_and_sig(df1m, cols, selectionsm, TRUE)
	outdfm <- cbind_and_pivot(psdfm, plot_type, selectionsm)
	outdfm <- add_tooltips(outdfm, selections)


	############################################
	# set the patterns
	agg2_patterns <- c("None" = "none")
	if (agg2 != "None") agg2_patterns <- set_fill_patterns(outdf, agg2)

	return(list("df" = outdf, "dfm" = outdfm, "patterns" = agg2_patterns))

}

single_aggregate_bar_plot <- function(usedf, usedfm, plot_type, agg1){
	f <- ggplot(usedf, aes_string(fill=agg1,  y="percent", x=plot_type, tooltip="tooltip")) + 
		geom_bar(stat = "identity", position = "dodge", color="black") + 
		labs(x = str_replace_all(plot_type,"_"," "), y = "Overall Percentage")
		#labs(x = "", y = "Overall Percentage")

	fm <- ggplot(usedfm, aes_string(fill=agg1,  y="percent", x=plot_type, tooltip="tooltip")) + 
		geom_bar(stat = "identity", position = "dodge", color="black") + 
		#labs(x = plot_type, y = "Mortality Percentage")	
		labs(x = "", y = "Mortality Percentage")	

	return(list("f" = f, "fm" = fm))
}

double_aggregate_bar_plot <- function(usedf, usedfm, plot_type, agg1, agg2, agg2_patterns){

	f <- ggplot(usedf, aes_string(fill=agg1, pattern=agg2, pattern_angle=agg2, y="percent", x=plot_type, tooltip="tooltip")) + 
		geom_bar_pattern(stat = "identity", position = "dodge",
					   color = "black", 
					   pattern_fill = "black",
					   pattern_density = 0.1,
					   pattern_spacing = 0.025,
					   pattern_key_scale_factor = 0.6) +
		scale_pattern_manual(values = agg2_patterns$patterns) +
		scale_pattern_angle_manual(values = agg2_patterns$angles) +
		labs(x = str_replace_all(plot_type,"_"," "), y = "Overall Percentage", pattern = agg2) + 
		#labs(x = "", y = "Overall Percentage", pattern = agg2) + 
		guides(pattern = guide_legend(override.aes = list(fill = "white")),
			  fill = guide_legend(override.aes = list(pattern = "none")))
	
	fm <- ggplot(usedfm, aes_string(fill=agg1, pattern=agg2, pattern_angle=agg2, y="percent", x=plot_type, tooltip="tooltip")) + 
		geom_bar_pattern(stat = "identity", position = "dodge",
					   color = "black", 
					   pattern_fill = "black",
					   pattern_density = 0.1,
					   pattern_spacing = 0.025,
					   pattern_key_scale_factor = 0.6) +
		scale_pattern_manual(values = agg2_patterns$patterns) +
		scale_pattern_angle_manual(values = agg2_patterns$angles) +
		#labs(x = plot_type, y = "Mortality Percentage", pattern = agg2) + 
		labs(x = "", y = "Mortality Percentage", pattern = agg2) + 
		guides(pattern = guide_legend(override.aes = list(fill = "white")),
			  fill = guide_legend(override.aes = list(pattern = "none")))

	return(list("f" = f, "fm" = fm))	
}

generate_bar_plot <- function(usedf, plot_type, cols, agg1, agg2, brush1, brush2, df1=NULL , df1m=NULL){


	bardf = prep_bar_chart_data(usedf, plot_type, cols, agg1, agg2, df1, df1m)
	usedf1 = bardf$df
	usedf1m = bardf$dfm
	agg2_patterns = bardf$patterns

	# create the plot

	#https://stackoverflow.com/questions/62393159/how-can-i-add-hatches-stripes-or-another-pattern-or-texture-to-a-barplot-in-ggp
	#WHY DO I USE position_dodge(0.9) IN THE ERROBAR PLOT??

	############################################
	# top panel (f1) shows percent in each group
	# bottom panel (f1m) shows mortality percent in each group

	# use the brush to set the range
	range1 <- c("x" = NULL, "y" = NULL)
	range2 <- c("x" = NULL, "y" = NULL)
	if (!is.null(brush1)) {
		range1$x <- c(brush1$xmin, brush1$xmax)
		range1$y <- c(brush1$ymin, brush1$ymax)
	} 

	#brush2 <- input$organ_support_bar_plot_mortality_brush
	if (!is.null(brush2)) {
		range2$x <- c(brush2$xmin, brush2$xmax)
		range2$y <- c(brush2$ymin, brush2$ymax)
	} 

	# set the plot range if not brushed
	if (is.null(range1$x)) range1$x <- c(0.5, length(cols)+0.5)
	#if (is.null(range1$y)) range1$y <- c(0, min(1.1*max((usedf1$percent + usedf1$sig_percent), na.rm=TRUE), 100.))
	if (is.null(range1$y)) range1$y <- c(0, 100.)

	if (is.null(range2$x)) range2$x <- c(0.5, length(cols)+0.5)
	#if (is.null(range2$y)) range2$y <- c(0, min(1.1*max((usedf1m$percent + usedf1m$sig_percent), na.rm=TRUE), 100.))
	if (is.null(range2$y)) range2$y <- c(0, 100.)

	# I don't think there's a clean way to do this without an if statement
	ifelse(agg2 == "None",
		fig <- single_aggregate_bar_plot(usedf1, usedf1m, plot_type, agg1),
		fig <- double_aggregate_bar_plot(usedf1, usedf1m, plot_type, agg1, agg2, agg2_patterns)
	)

	# these are common to either type of plot so can use here (outside of if statement)
	f1 <- fig$f +  
		scale_fill_brewer(palette = colors[[agg1]]) +
		geom_errorbar(aes(ymin = percent - sig_percent, ymax = percent + sig_percent), width=.2, position=position_dodge(.9)) +
		coord_cartesian(xlim = range1$x, ylim = range1$y, expand = FALSE) + 
		theme_bw() + 
		#theme(legend.position = "none")
		theme(legend.position = "bottom")

	f1m <- fig$fm +  
		scale_fill_brewer(palette = colors[[agg1]]) +
		geom_errorbar(aes(ymin = percent - sig_percent, ymax = percent + sig_percent), width=.2, position=position_dodge(.9)) +
		coord_cartesian(xlim = range2$x, ylim = range2$y, expand = FALSE) + 
		theme_bw() + 
		#theme(legend.position = "bottom")
		theme(legend.position = "none")


	# fix some labels
	# legend
	f1$labels$fill <- str_replace_all(f1$labels$fill, "_", " ")
	f1$labels$pattern <- str_replace_all(f1$labels$pattern, "_", " ")
	f1$labels$pattern_angle <- str_replace_all(f1$labels$pattern_angle, "_", " ")
	# x axis labels
	fix_labels_all <- Vectorize(function(x) {
		str_replace_all(x, "_", " ")
	})
	f1 <- f1 + scale_x_discrete(labels = fix_labels_all)
	f1m <- f1m + scale_x_discrete(labels = fix_labels_all)


	return(list("overall" = f1, "mortality" = f1m))

}

