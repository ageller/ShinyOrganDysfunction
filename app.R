# for tabs: https://shiny.rstudio.com/articles/tabsets.html

library(dplyr)
library(tidyr)
library(stringr)

library(ggplot2)
library(ggpattern)
library(ggpubr)
#library(plotly)

# start with the binary version
df <- read.csv('data/od_viz_binary.csv')

# add a column to have strings for lived vs. died 
df <- df %>%
	mutate(Outcome = case_when(
		Died == 0 ~ "Lived",
		Died == 1 ~ "Died",
	)) %>%
	mutate(Malignancy = case_when(
		Malignancy == 0 ~ "No",
		Malignancy == 1 ~ "Yes",
	)) %>%
	mutate(Transplant = case_when(
		Transplant == 0 ~ "No",
		Transplant == 1 ~ "Yes",
	)) %>% 
	mutate(Technology_Dependence = case_when(
		Technology_Dependence == 0 ~ "No",
		Technology_Dependence == 1 ~ "Yes",
	)) %>%
# MOD (“multiple organ dysfunction”) criteria
# “MOD on day 1” : Having 2 or more (out of 9) organ dysfunctions based on the PODIUM criteria on day 1 
	mutate(MOD1 = case_when(
		PODIUM_Count_Day1 < 2 ~ "No",
		PODIUM_Count_Day1 >= 2 ~ "Yes",
	)) %>%
#“MOD by day 3” : Having 2 or more (out of 9) organ dysfunctions based on the PODIUM criteria at any point during days 1 to 3.
	mutate(MOD3 = case_when(
		(PODIUM_Count_Day1 < 2 & PODIUM_Count_Day2 < 2 & PODIUM_Count_Day3 < 2) ~ "No",
		(PODIUM_Count_Day1 >= 2 | PODIUM_Count_Day2 >= 2 | PODIUM_Count_Day3 >= 2) ~ "Yes",
	))

# set all the relevant columns as factors
factor_cols = c("Age_Group", "Outcome", "Season_Admission", "Gender", "Malignancy", "Transplant", "Technology_Dependence")
for (ff in factor_cols){
	df[, ff] <- as.factor(df[, ff])
}


# create vectors for the checkboxes
ages <- sort(unlist(unique(df$Age_Group), use.names = FALSE))
outcomes <- sort(unlist(unique(df$Outcome), use.names = FALSE))
genders <- sort(unlist(unique(df$Gender), use.names = FALSE))
seasons <- sort(unlist(unique(df$Season_Admission), use.names = FALSE))
malignancies <- sort(unlist(unique(df$Malignancy), use.names = FALSE))
transplants <- sort(unlist(unique(df$Transplant), use.names = FALSE))
technologyDependencies <- sort(unlist(unique(df$Technology_Dependence), use.names = FALSE))

# get the unique organ types
foo <- colnames(select(df, contains("Day")))
foo <- strsplit(foo, '_')
foo <- sapply(foo,"[[", 2)
organs <- unlist(unique(foo), use.names = FALSE)
organs <- organs[!organs %in% 'Count']

# for each organ type create a new column that has 0 or 1 if any day had that failure
for (cc in organs){
	foo <- select(df, contains(cc))
	df[[cc]] <- ifelse(rowSums(foo, na.rm = TRUE) == 0, "No", "Yes")
}

# define standard colors for each aggregate
colors = c("Age_Group" = "Blues", 
			"Outcome" = "Reds",
			"Season_Admission" = "Greens",
			"Gender" = "Oranges",
			"Malignancy" = "Purples",
			"Transplant" = "PuRd",
			"Technology_Dependence" = "YlOrBr"
			)

patterns <- c("none", "stripe", "crosshatch", "circle", "stripe", "crosshatch", "stripe")
pattern_angles <- c(0, 45, 45, 0, 0, 0, -45)
#patterns <- c("stripe", "none", "crosshatch", "circle", "image", "placeholder", "magick", "gradient", "plasma")


# number of digits to show for numerical text
ndigits <- 1


# Define UI 
ui <- fluidPage(

	# App title 
	headerPanel("Pediatric Organ Dysfunction Explorer"),

	# Sidebar panel for inputs 
	sidebarPanel(

		h4("1. Select the subset of the data to include."),
		tabsetPanel(

			tabPanel("Age Group",
				div(
					style = "height:200px;overflow-y:auto;",
					checkboxGroupInput("AgeGroupCheckbox", "",
						ages,
						selected = ages
					)
				)
			),
			tabPanel("Gender",
				div(
					style = "height:200px;overflow-y:auto;",
					checkboxGroupInput("GenderCheckbox", "",
						genders,
						selected = genders
					)
				)
			),
			tabPanel("Season",
				div(
					style = "height:200px;overflow-y:auto;",
					checkboxGroupInput("SeasonCheckbox", "",
						seasons,
						selected = seasons
					)
				)
			),
			tabPanel("Organs",
				div(
					style = "height:200px;overflow-y:auto;",
					radioButtons("MOD1Checkbox", "MOD on day 1",
						choices = c("Any", "No", "Yes"),
						selected = "Any",
						inline = TRUE
					),
					radioButtons("MOD3Checkbox", "MOD by day 3",
						choices = c("Any", "No", "Yes"),
						selected = "Any",
						inline = TRUE
					),
					p("The buttons below select patients with a specific organ failure on any day from any criteria."),
					lapply(1:length(organs), function(i) {
						oo <- organs[i]
						radioButtons(paste0(oo, "Checkbox"), paste("Had", oo, "failure"),
							choices = c("Any", "No", "Yes"),
							selected = "Any",
							inline = TRUE
						)
					})
				)
			),
			tabPanel("Misc.",
				div(
					style = "height:200px;overflow-y:auto;",
					radioButtons("malignancyCheckbox", "Had Malignancy",
						append("Any", malignancies),
						selected = "Any",
						inline = TRUE
					),
					radioButtons("transplantCheckbox", "Had Tranplant",
						append("Any", transplants),
						selected = "Any",
						inline = TRUE
					),
					radioButtons("technologyDependenceCheckbox", "Had Technology Dependence",
						append("Any", technologyDependencies),
						selected = "Any",
						inline = TRUE
					)
				)
			),
		),


		hr(style = "border-top: 1px solid #000000;"),
		h4("2. Specify how to aggregate the data."),
		selectInput(
			"agg1", "First Aggregation Group", 
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
			"agg2", "Second Aggregation Group (optional)", 
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

		hr(style = "border-top: 1px solid #000000;"),
		h4("3. Click the button below to update the plot."),
		actionButton("updatePlot", "Update Plot"),
	),


	# Main panel for displaying outputs 
	mainPanel(

		# text describing plot 
		h3(textOutput("plot_title")),

		p("To zoom, click and drag over the desired area to create a zoom box.  (You can click outside the box to reset.)  When satisfied, click the 'Update Plot' button in the left panel to redefine the plot axes according to your zoom box.  Each plot panel can have a separate zoom.  If no zoom box is defined, clicking 'Update Plot' will reset the axes to the default."),

		# plots (Note that the legend changes the size; so even making them both same height is not exact)
		div(
			style = "position:relative",
			plotOutput("life_support_bar_plot_mortality",
				height = "350px",
				width = "100%",
				hover = hoverOpts(
					id = "life_support_bar_plot_mortality_hover",
					delay = 10,
					delayType = "debounce"
				),
				brush = brushOpts(
					id = "life_support_bar_plot_mortality_brush",
					resetOnNew = TRUE
				),
			),
			htmlOutput("life_support_bar_plot_mortality_hover_info")
		),

		div(
			style = "position:relative",
			plotOutput("life_support_bar_plot_overall",
				height = "300px",
				width = "100%",
				hover = hoverOpts(
					id = "life_support_bar_plot_overall_hover",
					delay = 10,
					delayType = "debounce"
				),
				brush = brushOpts(
					id = "life_support_bar_plot_overall_brush",
					resetOnNew = TRUE
				)
			),
			htmlOutput("life_support_bar_plot_overall_hover_info"),
		),


		htmlOutput("summary_table"),


	)
)




# Define server logic 
server <- function(input, output) {
	# generate two separate plots, each will be zoomable
	ranges_overall <- reactiveValues(x = NULL, y = NULL)
	ranges_mortality <- reactiveValues(x = NULL, y = NULL)
	plots <- reactiveValues(overall = NULL, mortality = NULL, summary = NULL)



	# Plot title
	plot_title <- function(){
		txt <- paste("Life support type aggregated by",str_replace(input$agg1,"_", " "))
		if (input$agg2 != "None") txt <- paste(txt, "and", str_replace(input$agg2,"_", " "))
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
			"Malignancy" = input$malignancyCheckbox,
			"Transplant" = input$transplantCheckbox,
			"Technology_Dependence" = input$technologyDependenceCheckbox,
			"MOD1" = input$MOD1Checkbox,
			"MOD3" = input$MOD3Checkbox
			)

		for (oo in organs){
			selections[[oo]] <- c(input[[paste0(oo, "Checkbox")]])
		}

		return(applyDataSelections(df, selections))
	}


	# function to create the plots
	create_plot <- function(usedf){
		# check if there is any brushing for zoom
		brush_overall <- input$life_support_bar_plot_overall_brush
		if (!is.null(brush_overall)) {
			ranges_overall$x <- c(brush_overall$xmin, brush_overall$xmax)
			ranges_overall$y <- c(brush_overall$ymin, brush_overall$ymax)

		} else {
			ranges_overall$x <- NULL
			ranges_overall$y <- NULL
		}

		brush_mortality <- input$life_support_bar_plot_mortality_brush
		if (!is.null(brush_mortality)) {
			ranges_mortality$x <- c(brush_mortality$xmin, brush_mortality$xmax)
			ranges_mortality$y <- c(brush_mortality$ymin, brush_mortality$ymax)

		} else {
			ranges_mortality$x <- NULL
			ranges_mortality$y <- NULL
		}

		generate_bar_plot(usedf, "Life_Support_Type", c("Mech_Ventilation", "Vasoactives", "NPPV", "ECMO", "CRRT"), input$agg1, input$agg2, ranges_overall, ranges_mortality)

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

	# when button is clicked, select the data and update plots object
	observe({
		input$updatePlot 
		isolate({
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
	output$life_support_bar_plot_mortality <- renderPlot({
		input$updatePlot
		isolate({
			plots$mortality
		})
	})
	output$life_support_bar_plot_overall <- renderPlot({
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
	# https://shiny.rstudio.com/gallery/plot-interaction-basic.html
	# https://gitlab.com/-/snippets/16220
	output$life_support_bar_plot_mortality_hover_info <- renderUI({
		hover <- input$life_support_bar_plot_mortality_hover
		if (is.numeric(hover$y)){

			style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); padding:10px;",
							"left:", hover$coords_css$x + 10, "px; top:", hover$coords_css$y + 10, "px;")

			# actual tooltip created as wellPanel
			wellPanel(
				style = style,
				div(
					HTML(
						paste(format(round(hover$y, ndigits), nsmall = ndigits), "%")
					)
				)

			)
		}
	})
	output$life_support_bar_plot_overall_hover_info <- renderUI({
		hover <- input$life_support_bar_plot_overall_hover
		if (is.numeric(hover$y)){

			style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); padding:10px;",
							"left:", hover$coords_css$x + 10, "px; top:", hover$coords_css$y + 10, "px;")

			# actual tooltip created as wellPanel
			wellPanel(
				style = style,
				div(
					HTML(
						paste(format(round(hover$y, ndigits), nsmall = ndigits), "%")
					)
				)

			)
		}
	})



}

applyDataSelections <- function(usedf, selections){

	outdf <- usedf

	for (ss in names(selections)){
		ifelse(selections[[ss]] == "Any", select <- c("No", "Yes"), select <- selections[[ss]])
		outdf <- outdf[ outdf[[ss]] %in% select, ]
	}

	return(outdf)

}

select_and_summarize <- function(usedf, cols, selections) {
	# select frome a dataframe and summarize elements for bar chart

	Nsample <- "Nsample"

	# group the data to get the total number in each subset
	out <- usedf %>% 
		group_by_at(selections, .drop=FALSE) %>% 
		mutate(Nsample = n()) %>%
		group_by_at(append(selections, c(Nsample)), .drop=FALSE) %>%
		summarise_at(vars(one_of(cols)), sum, na.rm=TRUE)

	return(out)

}

calculate_pct_and_sig <- function(usedf, cols, selections, mortality) {
	
	if (mortality){
		# We want the denominator to be the sum of all elements for all selections except the last in the list
		lived <- usedf[usedf$Outcome == "Lived",]
		died <- usedf[usedf$Outcome == "Died",]

		# calculate percentages and errors
		pctdf <- died
		sigdf <- died
		
	} else {

		# calculate percentages and errors
		pctdf <- usedf
		sigdf <- usedf
	}
	
	# calculate the percentage and error (using error propagation)
	# for mortality, we want the denominator to be the sum of all elements for all selections except the last in the list
	# for non-mortality, simply use the Nsample value for the denominator

	for (cc in cols){
		num <- pctdf[cc]
		ifelse(mortality, den <- lived[cc] + died[cc], den <- usedf$Nsample)
		pctdf[cc] <- 100.*( num/den )
		sigdf[cc] <- 100.*( num/den**2. + (num/den**2.)**2.*den )**0.5
		# assuming no error on denominator
		#sigdf1[cc] <- 100.*num**0.5/den
	}
	
	return(list("pct" = pctdf, "sig" = sigdf))
}

cbind_and_pivot <- function(psdf, plot_type, selections){

	# reorder this table so that it is easier to use with ggplot

	pctdf <- psdf$pct
	sigdf <- psdf$sig

	out <- pctdf
	out <- pivot_longer(data = out,
	   cols = -append(selections, c("Nsample")),
	   names_to = plot_type,
	   values_to = "percent")


	tmpdf <- pivot_longer(data = sigdf,
	   cols = -append(selections, c("Nsample")),
	   names_to = plot_type,
	   values_to = "sig_percent")

	out <- cbind(out, sig_percent=tmpdf$sig_percent)

	# select the columns we want
	out <- out[, append(selections, c(plot_type, "percent", "sig_percent"))]

	return(out)

}

set_fill_patterns <- function(usedf, col){

	col_vals <- unlist(unique(usedf[col]), use.names = FALSE)
	col_patterns <- patterns[(1:length(col_vals))]
	names(col_patterns) <- col_vals
	col_angles <- pattern_angles[(1:length(col_vals))]
	names(col_angles) <- col_vals

	return(list("patterns" = col_patterns, "angles" = col_angles))
}

prep_bar_chart_data <- function(usedf, plot_type, cols, agg1, agg2){

	############################################
	# percentage within each group given the selections
	selections <- c(agg1)
	if (agg2 != "None") selections <- append(selections, agg2)

	df1 <- select_and_summarize(usedf, cols, selections)
	psdf <- calculate_pct_and_sig(df1, cols, selections, FALSE)
	outdf <- cbind_and_pivot(psdf, plot_type, selections)

	############################################
	# mortality percentage given the selections
	if (agg1 != "Outcome" && agg2 != "Outcome") selections <- append(selections, "Outcome")
	df1m <- select_and_summarize(usedf, cols, selections)
	psdfm <- calculate_pct_and_sig(df1m, cols, selections, TRUE)
	outdfm <- cbind_and_pivot(psdfm, plot_type, selections)


	############################################
	# set the patterns
	agg2_patterns <- c("None" = "none")
	if (agg2 != "None") agg2_patterns <- set_fill_patterns(outdf, agg2)

	return(list("df" = outdf, "dfm" = outdfm, "patterns" = agg2_patterns))

}

single_aggregate_bar_plot <- function(usedf, usedfm, plot_type, agg1){
	f <- ggplot(usedf, aes_string(fill=agg1,  y="percent", x=plot_type)) + 
		geom_bar(stat = "identity", position = "dodge", color="black") + 
		labs(x = plot_type, y = "Overall Percentage")
		#labs(x = "", y = "Overall Percentage")

	fm <- ggplot(usedfm, aes_string(fill=agg1,  y="percent", x=plot_type)) + 
		geom_bar(stat = "identity", position = "dodge", color="black") + 
		#labs(x = plot_type, y = "Mortality Percentage")	
		labs(x = "", y = "Mortality Percentage")	

	return(list("f" = f, "fm" = fm))
}

double_aggregate_bar_plot <- function(usedf, usedfm, plot_type, agg1, agg2, agg2_patterns){

	f <- ggplot(usedf, aes_string(fill=agg1, pattern=agg2, pattern_angle=agg2, y="percent", x=plot_type)) + 
		geom_bar_pattern(stat = "identity", position = "dodge",
					   color = "black", 
					   pattern_fill = "black",
					   pattern_density = 0.1,
					   pattern_spacing = 0.025,
					   pattern_key_scale_factor = 0.6) +
		scale_pattern_manual(values = agg2_patterns$patterns) +
		scale_pattern_angle_manual(values = agg2_patterns$angles) +
		labs(x = plot_type, y = "Overall Percentage", pattern = agg2) + 
		#labs(x = "", y = "Overall Percentage", pattern = agg2) + 
		guides(pattern = guide_legend(override.aes = list(fill = "white")),
			  fill = guide_legend(override.aes = list(pattern = "none")))
	
	fm <- ggplot(usedfm, aes_string(fill=agg1, pattern=agg2, pattern_angle=agg2, y="percent", x=plot_type)) + 
		geom_bar_pattern(stat = "identity", position = "dodge",
					   color = "black", 
					   pattern_fill = "black",
					   pattern_density = 0.1,
					   pattern_spacing = 0.025,
					   pattern_key_scale_factor = 0.6) +
		scale_pattern_manual(values = agg2_patterns$patterns) +
		scale_pattern_angle_manual(values = agg2_patterns$angles) +
		#labs(x = plot_type, y = "Mortality Percentage", pattern = agg2) + 
		labs(x = "", y = "Mortality Percentage", pattern = agg2) + 
		guides(pattern = guide_legend(override.aes = list(fill = "white")),
			  fill = guide_legend(override.aes = list(pattern = "none")))

	return(list("f" = f, "fm" = fm))	
}

generate_bar_plot <- function(usedf, plot_type, cols, agg1, agg2, range1, range2){


	bardf = prep_bar_chart_data(usedf, plot_type, cols, agg1, agg2)
	usedf1 = bardf$df
	usedf1m = bardf$dfm
	agg2_patterns = bardf$patterns

	# create the plot

	#https://stackoverflow.com/questions/62393159/how-can-i-add-hatches-stripes-or-another-pattern-or-texture-to-a-barplot-in-ggp
	#WHY DO I USE position_dodge(0.9) IN THE ERROBAR PLOT??

	############################################
	# top panel (f1) shows percent in each group
	# bottom panel (f1m) shows mortality percent in each group

	# set the plot range if not brushed
	if (is.null(range1$x)) range1$x <- c(0.5, length(cols)+0.5)
	if (is.null(range1$y)) range1$y <- c(0, 1.1*max((usedf1$percent + usedf1$sig_percent)))

	if (is.null(range2$x)) range2$x <- c(0.5, length(cols)+0.5)
	if (is.null(range2$y)) range2$y <- c(0, 1.1*max((usedf1m$percent + usedf1m$sig_percent)))

	# I don't think there's a clean way to do this without an if statement
	ifelse(agg2 == "None",
		fig <- single_aggregate_bar_plot(usedf1, usedf1m, plot_type, agg1),
		fig <- double_aggregate_bar_plot(usedf1, usedf1m, plot_type, agg1, agg2, agg2_patterns)
	)

	# these are common to either type of plot so can use here (outside of if statement)
	f1 <- fig$f +  
		scale_fill_brewer(palette = colors[[agg1]]) +
		geom_errorbar(aes(ymin = percent - sig_percent, ymax = percent + sig_percent), width=.2, position=position_dodge(.9)) +
		coord_cartesian(xlim = range1$x, ylim = range1$y, expand = FALSE) + 
		theme_bw() + 
		#theme(legend.position = "none")
		theme(legend.position = "bottom")

	f1m <- fig$fm +  
		scale_fill_brewer(palette = colors[[agg1]]) +
		geom_errorbar(aes(ymin = percent - sig_percent, ymax = percent + sig_percent), width=.2, position=position_dodge(.9)) +
		coord_cartesian(xlim = range2$x, ylim = range2$y, expand = FALSE) + 
		theme_bw() + 
		#theme(legend.position = "bottom")
		theme(legend.position = "none")


	return(list("overall" = f1, "mortality" = f1m))

	#ggarrange(f1, f1m, ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")
	# p <- ggarrange(f1, f1m, ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")
	# ggplotly(p, tooltip = "text")

}

shinyApp(ui, server)