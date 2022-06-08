# Main panel for displaying outputs  
# for the organ support type figure 

organ_support_type_main <- function(id){

	ns <- NS(id)

	tagList(
		# text describing plot 
		h3(textOutput(ns("plot_title"))),

		# plots (Note that the legend changes the size; so even making them both same height is not exact)
		div(
			style = "position:relative",
			plotOutput(ns("organ_support_bar_plot_mortality"),
				height = "350px",
				width = "100%",
				hover = hoverOpts(
					id = ns("organ_support_bar_plot_mortality_hover"),
					delay = 10,
					delayType = "debounce"
				),
				brush = brushOpts(
					id = ns("organ_support_bar_plot_mortality_brush"),
					resetOnNew = TRUE
				),
			),
			htmlOutput(ns("organ_support_bar_plot_mortality_hover_tooltip"))
		),


		div(
			style = "position:relative",
			plotOutput(ns("organ_support_bar_plot_overall"),
				height = "300px",
				width = "100%",
				hover = hoverOpts(
					id = ns("organ_support_bar_plot_overall_hover"),
					delay = 10,
					delayType = "debounce"
				),
				brush = brushOpts(
					id = ns("organ_support_bar_plot_overall_brush"),
					resetOnNew = TRUE
				)
			),
			htmlOutput(ns("organ_support_bar_plot_overall_hover_tooltip")),
			# htmlOutput(ns("organ_support_bar_plot_overall_hover_div")),
		),


		htmlOutput(ns("summary_table")),
	)
}
