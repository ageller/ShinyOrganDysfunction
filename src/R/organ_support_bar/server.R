# server module 
# for the organ support type figure

organ_support_server <- function(id){

	moduleServer(
		id,
		function(input, output, session){

			# generate two separate plots, each will be zoomable
			organ_support_plots <- reactiveValues(overall = NULL, mortality = NULL)

			# when button is clicked, select the data and update plots object
			observe({
				input$updatePlot 
				input$mainPanelTabSelected
				isolate({
					if (input$mainPanelTabSelected == 1){
						withProgress(message = 'Generating figure', value = 0, {

							# include this here as well so that it doesn't proceed to try to make the plot 
							# (is there a way to do this without repeating code??)
							valid <- validate(
								need(input$AgeGroupCheckbox, message = 'Please select at least one Age Group.'),
								need(input$GenderCheckbox, message = 'Please select at least one Gender.'),
								need(input$SeasonCheckbox, message = 'Please select at least one Season.'),
								need(input$organ_bar_agg1 != input$organ_bar_agg2, message = 'Please select different values for each Aggregation Group.'),
							)

							# take the selection on the data (<<- is "super assign" to update the global variable)
							usedf <- selected_df

							incProgress(0.3, detail = "creating bar plot")

							# create the plots and save them in the plots object
							foo <- generate_bar_plot(usedf, "Organ_Support_Type", c("Mechanical_Ventilation", "Vasoactives", "NPPV", "ECMO", "CRRT"), input$organ_bar_agg1, input$organ_bar_agg2, input$organ_support_bar_plot_overall_brush, input$organ_support_bar_plot_mortality_brush)

							organ_support_plots$overall <- foo$overall
							organ_support_plots$mortality <- foo$mortality

							output$organ_support_bar_plot_overall <- renderPlot(organ_support_plots$overall)
							output$organ_support_bar_plot_mortality <- renderPlot(organ_support_plots$mortality)

							# update the plot title
							txt <- paste("Organ support type aggregated by",str_replace_all(input$organ_bar_agg1,"_", " "))
							if (input$organ_bar_agg2 != "None") txt <- paste(txt, "and", str_replace_all(input$organ_bar_agg2,"_", " "))
							output$organ_support_plot_title <- renderText(txt)


						})
					}
				})
			})


			# for tooltips
			observe({
				tooltip <- create_bar_tooltip(input$organ_support_bar_plot_mortality_hover, organ_support_plots$mortality, "mortality")
				if (!is.null(tooltip)) output$organ_support_bar_plot_mortality_hover_tooltip <- renderUI(tooltip)
			})

			observe({
				tooltip <- create_bar_tooltip(input$organ_support_bar_plot_overall_hover, organ_support_plots$overall, "overall")
				if (!is.null(tooltip)) output$organ_support_bar_plot_overall_hover_tooltip <- renderUI(tooltip)
			})



			# validate aggregation dropdowns
			output$organ_bar_agg_error <- renderText({
				validate(
					need(input$organ_bar_agg1 != input$organ_bar_agg2, message = 'Please select different values for each Aggregation Group.'),
				)
			})

		}
	)

}
