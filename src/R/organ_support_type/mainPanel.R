# Main panel for displaying outputs  
# for the organ support type figure 
mainPanel(

	# text describing plot 
	h3(textOutput("plot_title")),

	# plots (Note that the legend changes the size; so even making them both same height is not exact)
	div(
		style = "position:relative",
		plotOutput("organ_support_bar_plot_mortality",
			height = "350px",
			width = "100%",
			hover = hoverOpts(
				id = "organ_support_bar_plot_mortality_hover",
				delay = 10,
				delayType = "debounce"
			),
			brush = brushOpts(
				id = "organ_support_bar_plot_mortality_brush",
				resetOnNew = TRUE
			),
		),
		htmlOutput("organ_support_bar_plot_mortality_hover_tooltip")
	),


	div(
		style = "position:relative",
		plotOutput("organ_support_bar_plot_overall",
			height = "300px",
			width = "100%",
			hover = hoverOpts(
				id = "organ_support_bar_plot_overall_hover",
				delay = 10,
				delayType = "debounce"
			),
			brush = brushOpts(
				id = "organ_support_bar_plot_overall_brush",
				resetOnNew = TRUE
			)
		),
		htmlOutput("organ_support_bar_plot_overall_hover_tooltip"),
		# htmlOutput("organ_support_bar_plot_overall_hover_div"),
	),


	htmlOutput("summary_table"),


)