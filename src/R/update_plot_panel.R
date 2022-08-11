update_plot_sidebar <- function(id){

	ns <- NS(id)


	tagList(
		hr(style = "border-top: 1px solid #000000;"),
		h4("3. Click the button below to update the plot."),
		actionButton(ns("updatePlot"), "Update Plot"),

	)
}

zoom_info_sidebar <- function(id){
	ns <- NS(id)

	tagList(
		hr(style = "border-top: 1px solid #000000;"),

		p("To zoom, click and drag over the desired area on the plot in the right panel to create a zoom box.  (You can click outside the box to reset.)  When satisfied, click the 'Update Plot' button above to redefine the plot axes according to your zoom box.  Each plot panel can have a separate zoom.  If no zoom box is defined, clicking 'Update Plot' will reset the axes to the default."),
	)
}
