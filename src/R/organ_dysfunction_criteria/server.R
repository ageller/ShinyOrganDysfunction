# server module 
# for the orgain dysfunction criteria figure

organ_dysfunction_criteria_server <- function(id){

	moduleServer(
		id,
		function(input, output, session){

			# generate two separate plots, each will be zoomable
			organ_dysfunction_criteria_ranges_overall <- reactiveValues(x = NULL, y = NULL)
			organ_dysfunction_criteria_ranges_mortality <- reactiveValues(x = NULL, y = NULL)
			organ_dysfunction_criteria_plots <- reactiveValues(overall = NULL, mortality = NULL, summary = NULL)


			# function to create the plots
			create_plot <- function(usedf){
				# check if there is any brushing for zoom
				brush_overall <- input$organ_dysfunction_criteria_bar_plot_overall_brush
				if (!is.null(brush_overall)) {
					organ_dysfunction_criteria_ranges_overall$x <- c(brush_overall$xmin, brush_overall$xmax)
					organ_dysfunction_criteria_ranges_overall$y <- c(brush_overall$ymin, brush_overall$ymax)

				} else {
					organ_dysfunction_criteria_ranges_overall$x <- NULL
					organ_dysfunction_criteria_ranges_overall$y <- NULL
				}

				brush_mortality <- input$organ_dysfunction_criteria_bar_plot_mortality_brush
				if (!is.null(brush_mortality)) {
					organ_dysfunction_criteria_ranges_mortality$x <- c(brush_mortality$xmin, brush_mortality$xmax)
					organ_dysfunction_criteria_ranges_mortality$y <- c(brush_mortality$ymin, brush_mortality$ymax)

				} else {
					organ_dysfunction_criteria_ranges_mortality$x <- NULL
					organ_dysfunction_criteria_ranges_mortality$y <- NULL
				}


				# for a given criteria do the following:
				# 1. create a temp df that doesn't have the organ columns
				# 2. add back the organ columns as numeric for just that criteria (with 0 if the organ doesn't exist?)
				# 3. summarize for the different organ types and add column for the criteria
				# then append these summary dfs for all the criteria and run through the rest of my functions

				usedf <- df[ , !(names(df) %in% organs)]

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

				generate_bar_plot(usedf, "Organ_Dysfunction_Criteria_Comparison", organs, "criteria", "None", organ_dysfunction_criteria_ranges_overall, organ_dysfunction_criteria_ranges_mortality, df1, df1m)

			}



			# when button is clicked, select the data and update plots object
			observe({
				input$updatePlot 
				isolate({

					# include this here as well so that it doesn't proceed to try to make the plot 
					# (is there a way to do this without repeating code??)
					validate(
						need(input$AgeGroupCheckbox, message = 'Please select at least one Age Group.'),
						need(input$GenderCheckbox, message = 'Please select at least one Gender.'),
						need(input$SeasonCheckbox, message = 'Please select at least one Season.'),
						need(input$organ_dysfunction_criteria_day_checkbox, message = 'Please select at least one day.'),
					)

					# take the selection on the data (<<- is "super assign" to update the global variable)
					usedf <- select_data(input)

					# create the plots and table and save them in the plots object
					foo <- create_plot(usedf)
					organ_dysfunction_criteria_plots$overall <- foo$overall
					organ_dysfunction_criteria_plots$mortality <- foo$mortality
					organ_dysfunction_criteria_plots$summary <- create_summary_table(usedf)

				})
			})



			# tooltips
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


			observe({
				hover <- input$organ_dysfunction_criteria_bar_plot_mortality_hover
				reset <- TRUE
				if (is.numeric(hover$y)){

					# find the nearest bar and only show if the cursor is within the bar
					bar_plot_data <- layer_data(organ_dysfunction_criteria_plots$mortality, i = 1L)
					foo <- which.min(abs(bar_plot_data$x - hover$x))

					if (!is.na(bar_plot_data$y[[foo]])){
						if (bar_plot_data$y[[foo]] > hover$y) {
							reset <- FALSE

							if (foo != bar_index_mortality){
								bar_index_mortality <<- foo

								# add the tooltip
								output$organ_dysfunction_criteria_bar_plot_mortality_hover_tooltip <- renderUI(set_tooltip(hover$coords_css$x + 10, hover$coords_css$y + 10, bar_plot_data$tooltip[[bar_index_mortality]]))

							}
						} 
					} 
				}

				if (reset) {
					output$organ_dysfunction_criteria_bar_plot_mortality_hover_tooltip <- renderUI("")
					bar_index_mortality <<- -1
				} 
			})

			observe({
				hover <- input$organ_dysfunction_criteria_bar_plot_overall_hover
				reset <- TRUE
				if (is.numeric(hover$y)){

					# find the nearest bar and only show if the cursor is within the bar
					bar_plot_data <- layer_data(organ_dysfunction_criteria_plots$overall, i = 1L)
					foo <- which.min(abs(bar_plot_data$x - hover$x))

					if (!is.na(bar_plot_data$y[[foo]])){

						if (bar_plot_data$y[[foo]] > hover$y) {
							reset <- FALSE

							if (foo != bar_index_overall){
								bar_index_overall <<- foo

								# add the tooltip
								output$organ_dysfunction_criteria_bar_plot_overall_hover_tooltip <- renderUI(set_tooltip(hover$coords_css$x + 10, hover$coords_css$y + 10, bar_plot_data$tooltip[[bar_index_overall]]))

								# add the div to highlight the selected bar (not working)
								# output$organ_dysfunction_criteria_bar_plot_overall_hover_div <- renderUI(set_bardiv(hover, bar_plot_data[bar_index_overall,]))
							}
						} 
					} 
				} 

				if (reset) {
					output$organ_dysfunction_criteria_bar_plot_overall_hover_tooltip <- renderUI("")
					# output$organ_dysfunction_criteria_bar_plot_overall_hover_div <- renderUI("")
					bar_index_overall <<- -1
				}

			})

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

			# outputs

			# plots
			output$organ_dysfunction_criteria_bar_plot_mortality <- renderPlot({
				input$updatePlot
				isolate({
					organ_dysfunction_criteria_plots$mortality 
				})
			})
			output$organ_dysfunction_criteria_bar_plot_overall <- renderPlot({
				input$updatePlot
				isolate({
					organ_dysfunction_criteria_plots$overall 	
				})
			})

			# summary table
			output$summary_table <- renderUI({
				input$updatePlot
				isolate({
					organ_dysfunction_criteria_plots$summary
				})
			})

			# validate the checkboxes and aggregation dropdowns
			output$checkboxError <- renderText({
				validate(
					need(input$AgeGroupCheckbox, message = 'Please select at least one Age Group.'),
					need(input$GenderCheckbox, message = 'Please select at least one Gender.'),
					need(input$SeasonCheckbox, message = 'Please select at least one Season.'),
				)
			})
			output$organ_dysfunction_criteria_day_error <- renderText({
				validate(
					need(input$organ_dysfunction_criteria_day_checkbox, message = 'Please select at least one day.'),
				)
			})
		}
	)

}