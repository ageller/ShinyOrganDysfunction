# Main panel for displaying outputs  
# for the organ dysfunction timeseries sankey figure 

organ_dysfunction_timeseries_sankey_main <- function(id){

	ns <- NS(id)

	tagList(
		# text describing plot 
		h3(textOutput(ns("organ_dysfunction_timeseries_sankey_plot_title"))),

		div(
			style = "position:relative",
			sankeyNetworkOutput(ns("organ_dysfunction_timeseries_sankey_plot"))
		)

		# plots (Note that the legend changes the size; so even making them both same height is not exact)
		# div(
		# 	style = "position:relative",
		# 	plotOutput(ns("organ_dysfunction_timeseries_sankey_plot_mortality"),
		# 		height = "350px",
		# 		width = "100%",
		# 		hover = hoverOpts(
		# 			id = ns("organ_dysfunction_timeseries_sankey_plot_mortality_hover"),
		# 			delay = 10,
		# 			delayType = "debounce"
		# 		),
		# 		brush = brushOpts(
		# 			id = ns("organ_dysfunction_timeseries_sankey_plot_mortality_brush"),
		# 			resetOnNew = TRUE
		# 		),
		# 	),
		# 	htmlOutput(ns("organ_dysfunction_timeseries_sankey_plot_mortality_hover_tooltip"))
		# ),



	)
}
