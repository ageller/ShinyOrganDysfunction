# function to create the summary stats text
create_summary_table <- function(usedf){

	died <- usedf[usedf$Outcome == "Died",]
	return(div(
		hr(style = "border-top: 1px solid #000000;"),
		HTML(
			paste0("<span style='font-size:20px'><b>Summary Statistics</b></span><br/>",
				"<b>Total selected patients</b> : ",nrow(usedf),"<br/>",
				"<b>Percent of total population</b> : ",format(round(100.*nrow(usedf)/nrow(df), ndigits), nsmall = ndigits),"%<br/>",
				"<b>Total selected patients who died</b> : ",nrow(died),"<br/>",
				"<b>Percent mortality of selected patients</b> : ",format(round(100.*nrow(died)/nrow(usedf), ndigits), nsmall = ndigits), "%")
			)
		)
	)
}

# apply data selections (called from selectData below)
apply_data_selections <- function(usedf, selections){

	outdf <- usedf

	for (ss in names(selections)){
		ifelse(selections[[ss]] == "Any", select <- as.factor(c("No", "Yes")), select <- selections[[ss]])
		outdf <- outdf[ outdf[[ss]] %in% select, ]
	}

	# print(nrow(usedf))
	# print(nrow(outdf))
	# print(anti_join(usedf, outdf))

	return(outdf)

}


# function to compile all the selections and apply them
select_data <- function(input){
	selections <- list("Age_Group" = input$AgeGroupCheckbox,
		"Gender" = input$GenderCheckbox,
		"Season_Admission" = input$SeasonCheckbox,
		"Malignancy" = input$malignancyRadiobutton,
		"Transplant" = input$transplantRadiobutton,
		"Technology_Dependence" = input$technologyDependenceRadiobutton,
		"MOD1" = input$MOD1Radiobutton,
		"MOD3" = input$MOD3Radiobutton
		)

	for (oo in organs){
		selections[[oo]] <- c(input[[paste0(oo, "Radiobutton")]])
	}

	return(apply_data_selections(df, selections))
}

# functions for tooltips
#########################NOTE THIS HAS MORTALITY HARD CODED
create_tooltip <- function(hover, plot, index){
	tooltip <- NULL
	reset <- TRUE
	if (is.numeric(hover$y)){

		# find the nearest bar and only show if the cursor is within the bar
		bar_plot_data <- layer_data(plot, i = 1L)
		foo <- which.min(abs(bar_plot_data$x - hover$x))

		if (!is.na(bar_plot_data$y[[foo]])){
			if (bar_plot_data$y[[foo]] > hover$y) {
				reset <- FALSE

				if (foo != bar_index[index]){
					bar_index[index] <<- foo

					# create the tooltip
					tooltip <- set_tooltip(hover$coords_css$x + 10, hover$coords_css$y + 10, bar_plot_data$tooltip[[bar_index[index]]])

				}
			} 
		} 
	}

	if (reset && bar_index[index] > 0) {
		bar_index[index] <<- -1
		tooltip <- ""
	}


	return(tooltip)
}
set_tooltip <- function(x,y,content){
	style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); padding:10px;",
					"left:", x, "px; top:", y, "px;")

	# actual tooltip created as wellPanel
	tooltip <- 	wellPanel(style = style, div(HTML(content)))
	return(tooltip)	
}
set_bardiv <- function(hover, bar_data){
	# get the bounds
	#xconv <- hover$coords_css$x/hover$x
	#print(hover)
	xconv <- (hover$range$right - hover$range$left)/(hover$domain$right - hover$domain$left)
	xmin <- (bar_data$xmin - hover$domain$left)*xconv + hover$range$left
	xmax <- (bar_data$xmax - hover$domain$left)*xconv + hover$range$left
	print(paste("x", hover$x, "css_x", hover$coords_css$x, "css_x/x",hover$coords_css$x/hover$x,"conv", xconv,"xmin", bar_data$xmin, "xmin_conv", xmin, "width", xmax - xmin))
	ymin <- hover$coords_css$y + 10
	ymax <- hover$coords_css$y + 50

	style <- paste0("position:absolute; z-index:100; background-color: rgba(0, 0, 0, 0); border: 4px solid black; border-radius:0;",
					"left:", xmin, "px; top:", ymin, "px; width:",xmax - xmin, "px; height:", ymax - ymin, "px")

	# actual tooltip created as wellPanel
	div <- 	wellPanel(style = style, div(""))
	return(div)	
}
