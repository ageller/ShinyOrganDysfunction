# server module 
# functions used by multiple other modules

shared_server <- function(id){

	moduleServer(
		id,
		function(input, output, session){

			# reset button
			observeEvent(input$resetInputs, {
				reset("AgeGroupCheckbox")
				reset("GenderCheckbox")
				reset("SeasonCheckbox")
				reset("malignancyRadiobutton")
				reset("transplantRadiobutton")
				reset("technologyDependenceRadiobutton")
				reset("MOD1Radiobutton")
				reset("MOD3Radiobutton")

				for (oo in organs){
					reset(paste0(oo, "Radiobutton"))
				}
			})

			# validate the checkboxes
			output$checkboxError <- renderText({
				validate(
					need(input$AgeGroupCheckbox, message = 'Please select at least one Age Group.'),
					need(input$GenderCheckbox, message = 'Please select at least one Gender.'),
					need(input$SeasonCheckbox, message = 'Please select at least one Season.'),
				)
			})

			# take a subset of the data and create the shared summary table 
			observe({
				input$updatePlot 
				isolate({

					# include this here as well so that it doesn't proceed to try to make the plot 
					# (is there a way to do this without repeating code??)
					validate(
						need(input$AgeGroupCheckbox, message = 'Please select at least one Age Group.'),
						need(input$GenderCheckbox, message = 'Please select at least one Gender.'),
						need(input$SeasonCheckbox, message = 'Please select at least one Season.'),
					)

					# take the selection on the data (<<- is "super assign" to update the global variable)
					selected_df <<- select_data(df, input)
					selected_dfFull <<- select_data(dfFull, input)

					output$summary_table <- renderUI(create_summary_table(selected_df))
					

					# update this plot
					plot_needs_update[strtoi(input$mainPanelTabSelected)] <<- TRUE

					# only update the different panels if the demographic selections changed
					if (input_changed) {
						plot_needs_update <<- rep(TRUE,6)
						input_changed <<- FALSE
					}

				})
			}) 

			# check if any of the inputs from the demographics selections have changed
			observe({
				input$AgeGroupCheckbox 
				input$GenderCheckbox 
				input$SeasonCheckbox 
				input$malignancyRadiobutton 
				input$transplantRadiobutton 
				input$technologyDependenceRadiobutton 
				input$MOD1Radiobutton 
				input$MOD3Radiobutton 
				isolate({
					input_changed <<- TRUE
				})
			})

			# don't show the summary table in the credits tab
			observe({
				input$mainPanelTabSelected
				isolate({
					if (input$mainPanelTabSelected == 7) {
						hide("summary_table")
					} else {
						show("summary_table")
					}
				})
			})

		}
	)
}