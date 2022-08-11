# server module 
# for the demographics figure

demographics_sankey_server <- function(id){

	moduleServer(
		id,
		function(input, output, session){

			# when button is clicked, select the data and update plots object
			observe({
				input$updatePlot
				input$mainPanelTabSelected 
				isolate({
					if (input$mainPanelTabSelected == 5 & plot_needs_update[strtoi(input$mainPanelTabSelected)]){
						hide("demographics_sankey_plot")
						hide("summary_table")
						withProgress(message = 'Generating figure 6', value = 0, {

							# include this here as well so that it doesn't proceed to try to make the plot 
							# (is there a way to do this without repeating code??)
							validate(
								need(input$AgeGroupCheckbox, message = 'Please select at least one Age Group.'),
								need(input$GenderCheckbox, message = 'Please select at least one Gender.'),
								need(input$SeasonCheckbox, message = 'Please select at least one Season.'),
								need(input$demographics_characteristics_sankey_checkbox, message = 'Please select at least one characteristic'),
							)

							# define the columns that we need
							cols <- append(input$demographics_characteristics_sankey_checkbox, input$demographics_organs_sankey_checkbox)
							cols <- append(cols, "Outcome")

							# select only the columns that we need
							usedf <- select(selected_df, one_of(cols))

							# convert everything to characters
							for (cc in cols){
								usedf[[cc]] <- as.character(usedf[[cc]])
							}

							# summarize    
							number <- "number"
							usedf <- usedf %>% 
								group_by_at(cols, .drop=FALSE) %>% 
								mutate(number = n()) %>%
								group_by_at(append(cols, c(number)), .drop=FALSE) %>%
								summarise_at(vars(one_of(cols)), sum, na.rm=TRUE)

							incProgress(0.3, detail = "creating nodes dataframe")

							# get all the unique nodes
							# but first, replace the values that are just "yes"/"no" with more meaningful values
							nodev <- c()
							for (cc in cols){
								if (any(usedf[[cc]] == "Yes")){
									replacer <- str_replace(cc,"_"," ")
									if (cc %in% organs){
										replacer <- paste(replacer, "Failure")
									}
									usedf[[cc]][usedf[[cc]] == "Yes"]  = paste("Has",replacer)
									usedf[[cc]][usedf[[cc]] == "No"]  = paste("No",replacer)
								}
								nodev <- append(nodev, unique(usedf[[cc]]))
							}
							nodes <- data.frame(node = c(0:(length(nodev)-1)), name = nodev)

							incProgress(0.3, detail = "creating links dataframe")

							# Next I need to create the links
							tmpdf <- usedf
							# convert the names to numbers as in nodes
							i <- 1
							for (i in c(1:(length(nodev)))){
								tmpdf <- data.frame(lapply(tmpdf, function(x){ x[x %in% nodes$name[i]] <- nodes$node[i]; x}))
							}
							# add an extra Outcome column that I will keep for color coding
							tmpdf$Outcome2 <- usedf$Outcome
							# take each set of 2 columns as source and target
							# There is probably some fancy r pivot way of doing this...
							links <- data.frame(matrix(ncol = 4, nrow = 0))
							for (i in c(1:(length(cols) - 1))){
								foo <- select(tmpdf, one_of(cols[i:(i+1)]) | matches('number') | matches('Outcome2') )
								names(foo)[1] <- "source"
								names(foo)[2] <- "target"
								links <- rbind(links, foo)
							}
							names(links)[4] <- "Outcome"

							# try to reduce this data set
							links <- links %>% group_by(source, target, Outcome) %>% 
								summarise(
									number = sum(number)
								)
							# links <- data.frame(links[order(links$Outcome), ])


							#make sure these are integers
							links$source <- as.integer(links$source)
							links$target <- as.integer(links$target)
							links$number <- as.integer(links$number)

							incProgress(0.3, detail = "creating sankey figure")

							output$demographics_sankey_plot <- renderSankeyNetwork({
								fig <- generate_sankey_plot(nodes, links, 0)
							})


						})
						show("demographics_sankey_plot")
						show("summary_table")
						plot_needs_update[strtoi(input$mainPanelTabSelected)] <<- FALSE
					}

				})	
			})


			# # for tooltips
			# observe({
			# 	tooltip <- create_point_tooltip(input$organ_dysfunction_timeseries_line_plot_mortality_hover, organ_dysfunction_timeseries_plots$mortality, "mortality")
			# 	if (!is.null(tooltip)) output$organ_dysfunction_timeseries_line_plot_mortality_hover_tooltip <- renderUI(tooltip)
			# })

			# observe({
			# 	tooltip <- create_point_tooltip(input$organ_dysfunction_timeseries_line_plot_overall_hover, organ_dysfunction_timeseries_plots$overall, "overall")
			# 	if (!is.null(tooltip)) output$organ_dysfunction_timeseries_line_plot_overall_hover_tooltip <- renderUI(tooltip)
			# })

			# validate the checkboxes 
			output$demographics_characteristics_sankey_error <- renderText({
				validate(
					need(input$demographics_characteristics_sankey_checkbox, message = 'Please select at least one characteristic'),
				)
			})

		}
	)

}