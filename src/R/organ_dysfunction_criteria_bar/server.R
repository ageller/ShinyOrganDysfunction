# server module 
# for the organ dysfunction criteria figure

organ_dysfunction_criteria_server <- function(id){

	moduleServer(
		id,
		function(input, output, session){

			# generate two separate plots, each will be zoomable
			organ_dysfunction_criteria_plots <- reactiveValues(overall = NULL, mortality = NULL)

			# when button is clicked, select the data and update plots object
			observe({
				input$updatePlot 
				input$mainPanelTabSelected
				isolate({
					if (input$mainPanelTabSelected == 3 & plot_needs_update[strtoi(input$mainPanelTabSelected)]){
						hide("organ_dysfunction_criteria_bar_plot_overall")
						hide("organ_dysfunction_criteria_bar_plot_mortality")
						hide("summary_table")
						withProgress(message = 'Generating figure 3', value = 0, {

							# include this here as well so that it doesn't proceed to try to make the plot 
							# (is there a way to do this without repeating code??)
							validate(
								need(input$AgeGroupCheckbox, message = 'Please select at least one Age Group.'),
								need(input$GenderCheckbox, message = 'Please select at least one Gender.'),
								need(input$SeasonCheckbox, message = 'Please select at least one Season.'),
								need(input$organ_dysfunction_criteria_day_checkbox, message = 'Please select at least one day.'),
							)

							# take the selection on the data (<<- is "super assign" to update the global variable)
							usedf <- selected_df

							# for a given criteria do the following:
							# 1. create a temp df that doesn't have the organ columns
							# 2. add back the organ columns as numeric for just that criteria (with 0 if the organ doesn't exist?)
							# 3. summarize for the different organ types and add column for the criteria
							# then append these summary dfs for all the criteria and run through the rest of my functions

							usedf <- usedf[ , !(names(usedf) %in% organs)]

							df1 <- data.frame(matrix(ncol = length(organs)+1, nrow = 0))
							df1m <- data.frame(matrix(ncol = length(organs)+2, nrow = 0))
							for (cc in criteria){
								
								# for each organ type create a new column that has 0 or 1 if any day had that dysfunction
								# include only the days the user selected
								for (oo in organs){
									str_list <- paste(cc, oo, paste0("Day", input$organ_dysfunction_criteria_day_checkbox), sep="_")
									foo <- select(usedf, matches(paste(str_list, collapse="|")))
									usedf[[oo]] <- ifelse(rowSums(foo, na.rm = TRUE) == 0, 0, 1)
								}

								foo <- select_and_summarize(usedf, organs, c())
								foo$criteria <- cc
								df1 <- rbind(df1, foo)
								
								bar <- select_and_summarize(usedf, organs, c("Outcome"))
								bar$criteria <- cc
								df1m <- rbind(df1m, bar)
							}

							incProgress(0.3, detail = "creating bar plot")

							# create the plots and save them in the plots object
							foo <- generate_bar_plot(usedf, "Organ_Dysfunction_Criteria_Comparison", organs, "criteria", "None", input$organ_dysfunction_criteria_bar_plot_overall_brush, input$organ_dysfunction_criteria_bar_plot_mortality_brush, df1, df1m)

							organ_dysfunction_criteria_plots$overall <- foo$overall
							organ_dysfunction_criteria_plots$mortality <- foo$mortality

							output$organ_dysfunction_criteria_bar_plot_overall <- renderPlot(organ_dysfunction_criteria_plots$overall)
							output$organ_dysfunction_criteria_bar_plot_mortality <- renderPlot(organ_dysfunction_criteria_plots$mortality)

							incProgress(0.7)

						})
						show("organ_dysfunction_criteria_bar_plot_overall")
						show("organ_dysfunction_criteria_bar_plot_mortality")
						show("summary_table")
						plot_needs_update[strtoi(input$mainPanelTabSelected)] <<- FALSE
					}
				})
			})


			# for tooltips
			observe({
				tooltip <- create_bar_tooltip(input$organ_dysfunction_criteria_bar_plot_mortality_hover, organ_dysfunction_criteria_plots$mortality, "mortality", input$dimensions)
				if (!is.null(tooltip)) output$organ_dysfunction_criteria_bar_plot_mortality_hover_tooltip <- renderUI(tooltip)
			})

			observe({
				tooltip <- create_bar_tooltip(input$organ_dysfunction_criteria_bar_plot_overall_hover, organ_dysfunction_criteria_plots$overall, "overall", input$dimensions)
				if (!is.null(tooltip)) output$organ_dysfunction_criteria_bar_plot_overall_hover_tooltip <- renderUI(tooltip)
			})



			# validate the checkboxes 
			output$organ_dysfunction_criteria_day_error <- renderText({
				validate(
					need(input$organ_dysfunction_criteria_day_checkbox, message = 'Please select at least one day.'),
				)
			})
		}
	)

}
