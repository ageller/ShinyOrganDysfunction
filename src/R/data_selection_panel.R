# Sidebar panel for inputs 
# for selecting data based on demographics (used for all plots)

data_selection_sidebar <- function(id){

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
					p("The buttons below select patients with a specific organ dysfunction on any day."),
					lapply(1:length(organs), function(i) {
						oo <- organs[i]
						radioButtons(ns(paste0(oo, "Radiobutton")), paste("Had", oo, "dysfunction"),
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
					radioButtons(ns("transplantRadiobutton"), "Had Transplant",
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
	)
}
