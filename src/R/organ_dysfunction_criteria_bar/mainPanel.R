# Main panel for displaying outputs  
# for the organ dysfunction type figure 

organ_dysfunction_criteria_main <- function(id){

	ns <- NS(id)

	tagList(
		# text describing plot 
		h3("Organ dysfunction criteria comparison"),

		# plots (Note that the legend changes the size; so even making them both same height is not exact)
		div(
			style = "position:relative",
			plotOutput(ns("organ_dysfunction_criteria_bar_plot_mortality"),
				height = "350px",
				width = "100%",
				hover = hoverOpts(
					id = ns("organ_dysfunction_criteria_bar_plot_mortality_hover"),
					delay = 10,
					delayType = "debounce"
				),
				brush = brushOpts(
					id = ns("organ_dysfunction_criteria_bar_plot_mortality_brush"),
					resetOnNew = TRUE
				),
			),
			htmlOutput(ns("organ_dysfunction_criteria_bar_plot_mortality_hover_tooltip"))
		),


		div(
			style = "position:relative",
			plotOutput(ns("organ_dysfunction_criteria_bar_plot_overall"),
				height = "300px",
				width = "100%",
				hover = hoverOpts(
					id = ns("organ_dysfunction_criteria_bar_plot_overall_hover"),
					delay = 10,
					delayType = "debounce"
				),
				brush = brushOpts(
					id = ns("organ_dysfunction_criteria_bar_plot_overall_brush"),
					resetOnNew = TRUE
				)
			),
			htmlOutput(ns("organ_dysfunction_criteria_bar_plot_overall_hover_tooltip")),
		),


	)
}
