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