# server module 
# for the organ dysfunction timeseries figure

organ_dysfunction_timeseries_server <- function(id){

	moduleServer(
		id,
		function(input, output, session){

			# generate two separate plots, each will be zoomable
			organ_dysfunction_timeseries_plots <- reactiveValues(overall = NULL, mortality = NULL)

			# when button is clicked, select the data and update plots object
			observe({
				input$updatePlot 
				input$mainPanelTabSelected
				isolate({
					if (input$mainPanelTabSelected == 4){
						withProgress(message = 'Generating figure 4', value = 0, {

							# include this here as well so that it doesn't proceed to try to make the plot 
							# (is there a way to do this without repeating code??)
							validate(
								need(input$AgeGroupCheckbox, message = 'Please select at least one Age Group.'),
								need(input$GenderCheckbox, message = 'Please select at least one Gender.'),
								need(input$SeasonCheckbox, message = 'Please select at least one Season.'),
								need(input$organ_dysfunction_timeseries_criteria_checkbox, message = 'Please select at least one criteria'),
								need(input$organ_dysfunction_timeseries_organs_checkbox, message = 'Please select at least one organ.'),
							)

							# take the selection on the data
							usedf <- selected_df

							# do the following:
							# 1. create a temp df that doesn't have the organ columns
							# 2. add back the organ columns as numeric for just the selected criteria (with 0 if the organ doesn't exist?)
							# 3. summarize for the different organ types on a given day, add column for the day
							# 4. append these summary dfs for all the days 
							# 5. subtract from Ntotal the number of patients who were not alive on that day
							# 6. run through the rest of my functions

							df1 <- data.frame(matrix(ncol = length(input$organ_dysfunction_timeseries_organs_checkbox)+2, nrow = 0))

							incProgress(0.1, detail = "summarizing data")

							for (dd in days){

								# for each organ type create a new column that has 0 or 1 if it had a failure on that day
								for (oo in input$organ_dysfunction_timeseries_organs_checkbox){
									foo <- select(usedf, matches(paste(input$organ_dysfunction_timeseries_criteria_checkbox,oo,paste0("Day", dd), sep="_")))
									# this would convert count those who died as having that organ failure.  I don't think that is correct.
									# foo[is.na(foo)] = 1
									usedf[[oo]] <- ifelse(rowSums(foo, na.rm = TRUE) == 0, 0, 1)
									
								}

								foo <- select_and_summarize(usedf, input$organ_dysfunction_timeseries_organs_checkbox, c("Outcome"))
								foo$day <- dd

								#count the number of rows with all nans so that I can subtract that from the total sample later on
								cname <- paste0("allNAN",dd)
							    fee <- select(usedf, contains(criteria) & contains(paste0("Day",dd)) & -contains("Count"))
								usedf[[cname]] <- ifelse(rowSums(is.na(fee)) == ncol(fee), 1, 0)
								bar <- select_and_summarize(usedf, c(cname), c("Outcome"))
								#print(bar)
								foo$Nsample <- foo$Nsample - bar[bar$Outcome == "Died",][[cname]]
															

								df1 <- rbind(df1, foo)
							}

							incProgress(0.3, detail = "creating line plot")

							foo <- generate_timeseries_line_plot(df1, "organ", input$organ_dysfunction_timeseries_organs_checkbox, organ_colors, input$organ_dysfunction_timeseries_line_plot_overall_brush, input$organ_dysfunction_timeseries_line_plot_mortality_brush)

							organ_dysfunction_timeseries_plots$overall <- foo$overall
							organ_dysfunction_timeseries_plots$mortality <- foo$mortality

							output$organ_dysfunction_timeseries_line_plot_overall <- renderPlot(organ_dysfunction_timeseries_plots$overall)
							output$organ_dysfunction_timeseries_line_plot_mortality <- renderPlot(organ_dysfunction_timeseries_plots$mortality)

							# update the plot title
							txt <- paste("Organ dysfunction timeseries for the", paste(input$organ_dysfunction_timeseries_criteria_checkbox, collapse = ", "), "criteria")
							output$organ_dysfunction_timeseries_plot_title <- renderText(txt)

							incProgress(0.6)

						})
					}
				})
			})


			# for tooltips
			observe({
				tooltip <- create_point_tooltip(input$organ_dysfunction_timeseries_line_plot_mortality_hover, organ_dysfunction_timeseries_plots$mortality, "mortality")
				if (!is.null(tooltip)) output$organ_dysfunction_timeseries_line_plot_mortality_hover_tooltip <- renderUI(tooltip)
			})

			observe({
				tooltip <- create_point_tooltip(input$organ_dysfunction_timeseries_line_plot_overall_hover, organ_dysfunction_timeseries_plots$overall, "overall")
				if (!is.null(tooltip)) output$organ_dysfunction_timeseries_line_plot_overall_hover_tooltip <- renderUI(tooltip)
			})



			# validate the checkboxes 
			output$organ_dysfunction_timeseries_error <- renderText({
				validate(
					need(input$organ_dysfunction_timeseries_criteria_checkbox, message = 'Please select at least one criteria.'),
					need(input$organ_dysfunction_timeseries_organs_checkbox, message = 'Please select at least one organ.'),
				)
			})
		}
	)

}