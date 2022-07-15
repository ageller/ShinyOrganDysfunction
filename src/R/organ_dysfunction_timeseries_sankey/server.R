# server module 
# for the organ dysfunction timeseries figure

organ_dysfunction_timeseries_sankey_server <- function(id){

	moduleServer(
		id,
		function(input, output, session){

			# when button is clicked, select the data and update plots object
			observe({
				input$updatePlot 
				input$mainPanelTabSelected
				isolate({
					if (input$mainPanelTabSelected == 5){
						withProgress(message = 'Generating figure', value = 0, {

							# include this here as well so that it doesn't proceed to try to make the plot 
							# (is there a way to do this without repeating code??)
							validate(
								need(input$AgeGroupCheckbox, message = 'Please select at least one Age Group.'),
								need(input$GenderCheckbox, message = 'Please select at least one Gender.'),
								need(input$SeasonCheckbox, message = 'Please select at least one Season.'),
							)

							# take the selection on the data
							usedf <- select(selected_dfFull, matches('Outcome') | (contains(input$organ_dysfunction_timeseries_sankey_criteria_radio) & contains(input$organ_dysfunction_timeseries_sankey_organs_radio)))
							usedf$Outcome <- as.character(usedf$Outcome)


							# replace all NA values with -1
							usedf[is.na(usedf)] <- -1

							incProgress(0.1, detail = "creating nodes dataframe")

							# create the nodes dataframe (nodevals will contain numeric values used to generate the string node names)
							nodevals <- data.frame(matrix(ncol = 2, nrow = 0))
							names(nodevals) <- c("day", "score")
							for (dd in days){
								for (ss in scores){
									nodevals[nrow(nodevals) + 1,] <- c(dd, ss)
								}
							}
							nodev <- c("Lived", "Died", paste("Day", nodevals$day, "Score", nodevals$score)) #using this for constructing the links
							nodes <- data.frame(node = c(0:(length(nodev)-1)), name = nodev)
							nodevals["score"][nodevals["score"] == -1] <- "D"
							nodev2 <- c("Lived", "Died",nodevals$score) #using this for labels on the plot
							nodes2 <- data.frame(node = c(0:(length(nodev)-1)), name = nodev2)
							
							incProgress(0.1, detail = "creating links dataframe")

							# create the links dataframe
							# this will be fully numeric with values that point to the strings in the nodes dataframe
							links <- data.frame(matrix(ncol = 4, nrow = 0))
							names(links) <- c("source", "target", "number", "Outcome")
							#Is there a better way to do this without these nested loop??
							maxInc <- 2.*(length(days) - 1)*length(scores)*length(scores)
							for (oo in c("Lived", "Died")){
								for (dd1 in days[1:(length(days)-1)]){
									dd2 <- as.numeric(dd1) + 1
									for (ss1 in scores){
										#select all rows that have the give score on day dd
										col1 <- paste(input$organ_dysfunction_timeseries_sankey_criteria_radio, input$organ_dysfunction_timeseries_sankey_organs_radio, paste0("Day",dd1), sep = "_")
										col2 <- paste(input$organ_dysfunction_timeseries_sankey_criteria_radio, input$organ_dysfunction_timeseries_sankey_organs_radio, paste0("Day",dd2), sep = "_")
										#loop through scores again to find the scores for the following day
										for (ss2 in scores){
											# ########## TO DO ############### This needs some check to make sure that the criteria as the given organ
											foo <- filter(usedf, usedf[[col1]] == ss1 & usedf[[col2]] == ss2 & usedf$Outcome == oo)
											#print(paste(col1, col2, ss1, ss2, oo, nrow(foo)))
											if (nrow(foo) > 0){
												source <- nodes[nodes$name == paste("Day",dd1,"Score",ss1), ]$node
												target <- nodes[nodes$name == paste("Day",dd2,"Score",ss2), ]$node
												links[nrow(links) + 1,] <- c(source, target, nrow(foo), oo)
											}

											incProgress(0.7/maxInc)

										}
									}
								}
							}

							# add the final links to those who lived and died
							for (oo in c("Lived", "Died")){
								dd <- 7
								for (ss in scores){
									#select all rows that have the give score on day dd
									col <- paste(input$organ_dysfunction_timeseries_sankey_criteria_radio, input$organ_dysfunction_timeseries_sankey_organs_radio, paste0("Day",dd), sep = "_")
									foo <- filter(usedf, usedf[[col]] == ss & usedf$Outcome == oo)
									if (nrow(foo) > 0){
										source <- nodes[nodes$name == paste("Day",dd,"Score",ss), ]$node
										target <- nodes[nodes$name == oo, ]$node
										links[nrow(links) + 1,] <- c(source, target, nrow(foo), oo)
									}
								}
							}

							#make sure these are integers
							links$source <- as.integer(links$source)
							links$target <- as.integer(links$target)
							links$number <- as.integer(links$number)

							incProgress(0.1, detail = "creating sankey diagram")

							output$organ_dysfunction_timeseries_sankey_plot <- renderSankeyNetwork({
								fig <- generate_sankey_plot(nodes2, links, 0)

								# add some custom formatting
								onRender(fig,
									'
									function(el,x) {

										//add a margin at the bottom so that it doesnt overlap the table
										el.style.marginBottom = "50px";

										// remove all the existing labels for the scores
										d3.selectAll(".node text").filter(d => (d.name != "Lived" && d.name != "Died")).remove();

										// add the labels back in but centered 
										//https://stackoverflow.com/questions/72770709/r-align-and-center-text-using-foreignobject-and-sankeynetwork
										d3.selectAll(".node").filter(d => (d.name != "Lived" && d.name != "Died" && d.dy > 0))
											.append("foreignObject")
											.attr("width",  d => Math.max(d.dx, 40))
											.attr("height", d => Math.max(d.dy, 20))
											.html((d,i) => "<div style=\\"height: 100%; display: flex; align-items: center; flex-direction: row; justify-content: left; text-align: left; padding-left:1px\\">" + d.name);
									  
										// add additional labels for the Day numbers
										// https://stackoverflow.com/questions/66813278/how-to-add-columnn-titles-in-a-sankey-chart-networkd3
										var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
										cols_x.forEach((d, i) => {
											if (i < 7){
												d3.select(el).select("svg")
												.append("text")
												.attr("x", d)
												.attr("y", 12)
												.text("Day " + (i + 1));
											}
										})
										  
									}
									'
								)

							})

							# update the plot title
							txt <- paste(input$organ_dysfunction_timeseries_sankey_criteria_radio, "scores over time for", input$organ_dysfunction_timeseries_sankey_organs_radio)
							output$organ_dysfunction_timeseries_sankey_plot_title <- renderText(txt)

						})

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



		}
	)

}