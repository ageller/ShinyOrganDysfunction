# Sidebar panel for inputs 
# for the organ support type figure 
organ_support_type_sidebar <- function(id){

	ns <- NS(id)

	tagList(
		h4("1. Select the subset of the data to include."),
		tabsetPanel(

			tabPanel("Age Group",
				div(
					style = "height:300px;overflow-y:auto;",
					checkboxGroupInput(ns("AgeGroupCheckbox"), "",
						ages,
						selected = ages
					)
				)
			),
			tabPanel("Gender",
				div(
					style = "height:300px;overflow-y:auto;",
					checkboxGroupInput(ns("GenderCheckbox"), "",
						genders,
						selected = genders
					)
				)
			),
			tabPanel("Season",
				div(
					style = "height:300px;overflow-y:auto;",
					checkboxGroupInput(ns("SeasonCheckbox"), "",
						seasons,
						selected = seasons
					)
				)
			),
			tabPanel("Organs",
				div(
					style = "height:300px;overflow-y:auto;",
					p("All options in this tab are based on the PODIUM criteria."),
					radioButtons(ns("MOD1Radiobutton"), "MOD on day 1",
						choices = as.factor(c("Any", "No", "Yes")),
						selected = "Any",
						inline = TRUE
					),
					radioButtons(ns("MOD3Radiobutton"), "MOD by day 3",
						choices = as.factor(c("Any", "No", "Yes")),
						selected = "Any",
						inline = TRUE
					),
					p("The buttons below select patients with a specific organ failure on any day."),
					lapply(1:length(organs), function(i) {
						oo <- organs[i]
						radioButtons(ns(paste0(oo, "Radiobutton")), paste("Had", oo, "failure"),
							choices = as.factor(c("Any", "No", "Yes")),
							selected = "Any",
							inline = TRUE
						)
					})
				)
			),
			tabPanel("Comorbidities",
				div(
					style = "height:300px;overflow-y:auto;",
					radioButtons(ns("malignancyRadiobutton"), "Had Malignancy",
						append(as.factor("Any"), malignancies),
						selected = "Any",
						inline = TRUE
					),
					radioButtons(ns("transplantRadiobutton"), "Had Tranplant",
						append(as.factor("Any"), transplants),
						selected = "Any",
						inline = TRUE
					),
					radioButtons(ns("technologyDependenceRadiobutton"), "Had Technology Dependence",
						append(as.factor("Any"), technologyDependencies),
						selected = "Any",
						inline = TRUE
					)
				)
			),
		),

		div(
			style = "min-height:20px; margin-bottom:10px",
			textOutput(ns("checkboxError"))
		),
		actionButton(ns("resetInputs"), "Reset All Section 1 Selections"),

		hr(style = "border-top: 1px solid #000000;"),
		h4("2. Specify how to aggregate the data."),
		selectInput(
			ns("agg1"), "First Aggregation Group", 
			c("Age Group" = "Age_Group", 
				"Outcome" = "Outcome",
				"Season Admission" = "Season_Admission",
				"Gender" = "Gender",
				"Malignancy" = "Malignancy",
				"Transplant" = "Transplant",
				"Technology Dependence" = "Technology_Dependence"
			),
			selected = "Age Group"
		),
		selectInput(
			ns("agg2"), "Second Aggregation Group (optional)", 
			c("Age Group" = "Age_Group", 
				"Outcome" = "Outcome",
				"Season Admission" = "Season_Admission",
				"Gender" = "Gender",
				"Malignancy" = "Malignancy",
				"Transplant" = "Transplant",
				"Technology Dependence" = "Technology_Dependence",
				"None" = "None"
			),
			selected = "None"
		),

		div(
			style = "min-height:20px;",
			textOutput(ns("aggError"))
		),

		hr(style = "border-top: 1px solid #000000;"),
		h4("3. Click the button below to update the plot."),
		actionButton(ns("updatePlot"), "Update Plot"),

		hr(style = "border-top: 1px solid #000000;"),

		p("To zoom, click and drag over the desired area on the plot in the left panel to create a zoom box.  (You can click outside the box to reset.)  When satisfied, click the 'Update Plot' button above to redefine the plot axes according to your zoom box.  Each plot panel can have a separate zoom.  If no zoom box is defined, clicking 'Update Plot' will reset the axes to the default."),
	)
}