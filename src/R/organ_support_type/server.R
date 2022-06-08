organ_support_type_server <- function(id){

	moduleServer(
		id,
		function(input, output, session){

			# generate two separate plots, each will be zoomable
			ranges_overall <- reactiveValues(x = NULL, y = NULL)
			ranges_mortality <- reactiveValues(x = NULL, y = NULL)
			plots <- reactiveValues(overall = NULL, mortality = NULL, summary = NULL)



			# Plot title
			plot_title <- function(){
				txt <- paste("Organ support type aggregated by",str_replace_all(input$agg1,"_", " "))
				if (input$agg2 != "None") txt <- paste(txt, "and", str_replace_all(input$agg2,"_", " "))
				return(txt)
			}
			output$plot_title <- renderText({
				input$updatePlot
				isolate({
					plot_title()
				})
			})



			# function to compile all the selections and apply them
			selectData <- function(){
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

				return(applyDataSelections(df, selections))
			}


			# function to create the plots
			create_plot <- function(usedf){
				# check if there is any brushing for zoom
				brush_overall <- input$organ_support_bar_plot_overall_brush
				if (!is.null(brush_overall)) {
					ranges_overall$x <- c(brush_overall$xmin, brush_overall$xmax)
					ranges_overall$y <- c(brush_overall$ymin, brush_overall$ymax)

				} else {
					ranges_overall$x <- NULL
					ranges_overall$y <- NULL
				}

				brush_mortality <- input$organ_support_bar_plot_mortality_brush
				if (!is.null(brush_mortality)) {
					ranges_mortality$x <- c(brush_mortality$xmin, brush_mortality$xmax)
					ranges_mortality$y <- c(brush_mortality$ymin, brush_mortality$ymax)

				} else {
					ranges_mortality$x <- NULL
					ranges_mortality$y <- NULL
				}

				generate_bar_plot(usedf, "Organ_Support_Type", c("Mechanical_Ventilation", "Vasoactives", "NPPV", "ECMO", "CRRT"), input$agg1, input$agg2, ranges_overall, ranges_mortality)

			}

			# function to create the summary stats text
			summary_table <- function(usedf){

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


			# validate the checkboxes and aggregation dropdowns
			output$checkboxError <- renderText({
				validate(
					need(input$AgeGroupCheckbox, message = 'Please select at least one Age Group.'),
					need(input$GenderCheckbox, message = 'Please select at least one Gender.'),
					need(input$SeasonCheckbox, message = 'Please select at least one Season.'),
				)
			})
			output$aggError <- renderText({
				validate(
					need(input$agg1 != input$agg2, message = 'Please select different values for each Aggregation Group.'),
				)
			})

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
						need(input$agg1 != input$agg2, message = 'Please select different values for each Aggregation Group.'),
					)

					# take the selection on the data (<<- is "super assign" to update the global variable)
					usedf <- selectData()

					# create the plots and table and save them in the plots object
					foo <- create_plot(usedf)
					plots$overall <- foo$overall
					plots$mortality <- foo$mortality
					plots$summary <- summary_table(usedf)

				})
			})

			# set the output for the plots
			output$organ_support_bar_plot_mortality <- renderPlot({
				input$updatePlot
				isolate({
					plots$mortality 
				})
			})
			output$organ_support_bar_plot_overall <- renderPlot({
				input$updatePlot
				isolate({
					plots$overall 	
				})
			})


			output$summary_table <- renderUI({
				input$updatePlot
				isolate({
					plots$summary
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
				hover <- input$organ_support_bar_plot_mortality_hover
				reset <- TRUE
				if (is.numeric(hover$y)){

					# find the nearest bar and only show if the cursor is within the bar
					bar_plot_data <- layer_data(plots$mortality, i = 1L)
					foo <- which.min(abs(bar_plot_data$x - hover$x))

					if (!is.na(bar_plot_data$y[[foo]])){
						if (bar_plot_data$y[[foo]] > hover$y) {
							reset <- FALSE

							if (foo != bar_index_mortality){
								bar_index_mortality <<- foo

								# add the tooltip
								output$organ_support_bar_plot_mortality_hover_tooltip <- renderUI(set_tooltip(hover$coords_css$x + 10, hover$coords_css$y + 10, bar_plot_data$tooltip[[bar_index_mortality]]))

							}
						} 
					} 
				}

				if (reset) {
					output$organ_support_bar_plot_mortality_hover_tooltip <- renderUI("")
					bar_index_mortality <<- -1
				} 
			})

			observe({
				hover <- input$organ_support_bar_plot_overall_hover
				reset <- TRUE
				if (is.numeric(hover$y)){

					# find the nearest bar and only show if the cursor is within the bar
					bar_plot_data <- layer_data(plots$overall, i = 1L)
					foo <- which.min(abs(bar_plot_data$x - hover$x))

					if (!is.na(bar_plot_data$y[[foo]])){

						if (bar_plot_data$y[[foo]] > hover$y) {
							reset <- FALSE

							if (foo != bar_index_overall){
								bar_index_overall <<- foo

								# add the tooltip
								output$organ_support_bar_plot_overall_hover_tooltip <- renderUI(set_tooltip(hover$coords_css$x + 10, hover$coords_css$y + 10, bar_plot_data$tooltip[[bar_index_overall]]))

								# add the div to highlight the selected bar (not working)
								# output$organ_support_bar_plot_overall_hover_div <- renderUI(set_bardiv(hover, bar_plot_data[bar_index_overall,]))
							}
						} 
					} 
				} 

				if (reset) {
					output$organ_support_bar_plot_overall_hover_tooltip <- renderUI("")
					# output$organ_support_bar_plot_overall_hover_div <- renderUI("")
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
		}
	)

}