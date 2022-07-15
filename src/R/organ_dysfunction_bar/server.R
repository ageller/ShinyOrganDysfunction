# server module 
# for the organ dysfunction figure

organ_dysfunction_server <- function(id){

	moduleServer(
		id,
		function(input, output, session){

			# generate two separate plots, each will be zoomable
			organ_dysfunction_plots <- reactiveValues(overall = NULL, mortality = NULL)

			# when button is clicked, select the data and update plots object
			observe({
				input$updatePlot 
				input$mainPanelTabSelected 
				isolate({
					if (input$mainPanelTabSelected == 2){
						withProgress(message = 'Generating figure 2', value = 0, {
							# include this here as well so that it doesn't proceed to try to make the plot 
							# (is there a way to do this without repeating code??)
							validate(
								need(input$AgeGroupCheckbox, message = 'Please select at least one Age Group.'),
								need(input$GenderCheckbox, message = 'Please select at least one Gender.'),
								need(input$SeasonCheckbox, message = 'Please select at least one Season.'),
								need(input$organ_bar_agg1 != input$organ_bar_agg2, message = 'Please select different values for each Aggregation Group.'),
							)

							usedf <- selected_df

							# these were created as Yes/No factors, so convert them to numeric for the bar plot
							for (oo in organs){
								if (is.factor(usedf[, oo])) usedf[, oo] <- as.character(usedf[, oo])
								usedf[, oo][usedf[, oo] == "Yes"] <- "1"
								usedf[, oo][usedf[, oo] == "No"] <- "0"
								usedf[, oo] <- as.numeric(usedf[, oo])
							}

							incProgress(0.3, detail = "creating bar plot")

							# create the plots and save them in the plots object
							foo <- 	generate_bar_plot(usedf, "Organ_Dysfunction_Type", organs, input$organ_bar_agg1, input$organ_bar_agg2, input$organ_dysfunction_bar_plot_overall_brush, input$organ_dysfunction_bar_plot_mortality_brush)

							organ_dysfunction_plots$overall <- foo$overall
							organ_dysfunction_plots$mortality <- foo$mortality

							output$organ_dysfunction_bar_plot_overall <- renderPlot(organ_dysfunction_plots$overall)
							output$organ_dysfunction_bar_plot_mortality <- renderPlot(organ_dysfunction_plots$mortality)

							# update the plot title
							txt <- paste("Organ dysfunction type aggregated by",str_replace_all(input$organ_bar_agg1,"_", " "))
							if (input$organ_bar_agg2 != "None") txt <- paste(txt, "and", str_replace_all(input$organ_bar_agg2,"_", " "))
							output$organ_dysfunction_plot_title <- renderText(txt)

							incProgress(0.7)

						})
					}
				})
			})

			# for tooltips
			observe({
				tooltip <- create_bar_tooltip(input$organ_dysfunction_bar_plot_mortality_hover, organ_dysfunction_plots$mortality, "mortality")
				if (!is.null(tooltip)) output$organ_dysfunction_bar_plot_mortality_hover_tooltip <- renderUI(tooltip)
			})

			observe({
				tooltip <- create_bar_tooltip(input$organ_dysfunction_bar_plot_overall_hover, organ_dysfunction_plots$overall, "overall")
				if (!is.null(tooltip)) output$organ_dysfunction_bar_plot_overall_hover_tooltip <- renderUI(tooltip)
			})


			# validate the aggregation dropdowns
			output$organ_bar_agg_error <- renderText({
				validate(
					need(input$organ_bar_agg1 != input$organ_bar_agg2, message = 'Please select different values for each Aggregation Group.'),
				)
			})
		}
	)

}
