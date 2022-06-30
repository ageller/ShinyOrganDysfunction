#read in and intialize the data

# function to read in and intialize the data file
init_data <- function(fname){
	# start with the binary version
	df <- read.csv(fname)

	# replace any NA values with 0
	# df[is.na(df)] = 0
	# remove any rows that are all NA


	# rename columns?
	names(df)[names(df) == 'Mech_Ventilation'] <- 'Mechanical_Ventilation'

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
			(is.na(PODIUM_Count_Day1) | is.na(PODIUM_Count_Day2) | is.na(PODIUM_Count_Day3)) ~ "Yes",
		))

	# get the unique organ types
	#foo <- colnames(select(df, contains("Day")))
	foo <- colnames(select(df, contains("PODIUM")))
	foo <- strsplit(foo, '_')
	foo <- sapply(foo,"[[", 2)
	organs <- unlist(unique(foo), use.names = FALSE)
	organs <- organs[!organs %in% 'Count']

	# for each organ type create a new column that has "No" or "Yes" if any day had that dysfunction
	for (cc in organs){
		foo <- select(select(df, contains(cc)), contains("PODIUM"))
		df[[cc]] <- ifelse(rowSums(foo, na.rm = TRUE) == 0, "No", "Yes")
	}


	# set all the relevant columns as factors
	factor_cols = c("Age_Group", "Outcome", "Season_Admission", "Gender", "Malignancy", "Transplant", "Technology_Dependence", "MOD1", "MOD3")
	for (oo in organs){
		factor_cols <- append(factor_cols, oo)
	}
	for (ff in factor_cols){
		df[, ff] <- as.factor(df[, ff])
	}

	return(list("df" = df, "organs" = organs))

}

# read in the binary data
foo <- init_data('src/data/od_viz_binary.csv')
df <- foo$df
organs <- foo$organs

# read in the full data
foo <- init_data('src/data/od_viz.csv')
dfFull <- foo$df


# create vectors for the checkboxes
ages <- sort(unlist(unique(df$Age_Group), use.names = FALSE))
outcomes <- sort(unlist(unique(df$Outcome), use.names = FALSE))
genders <- sort(unlist(unique(df$Gender), use.names = FALSE))
seasons <- sort(unlist(unique(df$Season_Admission), use.names = FALSE))
malignancies <- sort(unlist(unique(df$Malignancy), use.names = FALSE))
transplants <- sort(unlist(unique(df$Transplant), use.names = FALSE))
technologyDependencies <- sort(unlist(unique(df$Technology_Dependence), use.names = FALSE))


# conversions to use later
name_conversion <- c("Age_Group" = "Age Group", 
				"Outcome" = "Outcome",
				"Season_Admission" = "Season Admission",
				"Gender" = "Gender",
				"Malignancy" = "Malignancy",
				"Transplant" = "Transplant",
				"Technology_Dependence" = "Technology Dependence")


criteria <- c("pSOFA", "PELOD2", "PODIUM", "IPSCC")

# define standard colors for each aggregate
colors = c("Age_Group" = "Blues", 
			"Outcome" = "Reds",
			"Season_Admission" = "Greens",
			"Gender" = "Oranges",
			"Malignancy" = "Purples",
			"Transplant" = "PuRd",
			"Technology_Dependence" = "YlOrBr",
			"criteria" = "YlGn"
			)

# define standard colors for each organ
organ_colors = c("Resp" = "darkblue", 
            "CV" = "maroon",
            "Hepatic" = "lightgoldenrod3",
            "Coag" = "purple",
            "Heme" = "firebrick2",
            "Renal" = "darkgreen",
            "Endo" = "hotpink",
            "Immuno" = "dimgrey"
            )

days <- c("1","2","3","4","5","6","7")

scores <- c(0,1,2,3,4, -1) #using -1 for NA

patterns <- c("none", "stripe", "crosshatch", "circle", "stripe", "crosshatch", "stripe")
pattern_angles <- c(0, 45, 45, 0, 0, 0, -45)
#patterns <- c("stripe", "none", "crosshatch", "circle", "image", "placeholder", "magick", "gradient", "plasma")


# number of digits to show for numerical text
ndigits <- 1

# for tooltips
plot_element_index <- c("mortality" = -1, "overall" = -1)

# will hold the selected data
selected_df <- df
selected_dfFull <- dfFull