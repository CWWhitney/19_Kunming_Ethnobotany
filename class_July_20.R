#Notes from class on ethnobotany and R
#July 20th 2019: Plotting: Making functions ####

#Plotting high dimensional data####
#pull the hortibonn git repository from github
# https://github.com/hortibonn/Plotting-High-Dimensional-Data

#Make data frame

DF <- data.frame(variable = as.factor(1:10),
                 value = sample(10, replace = TRUE))

#Make standard error

se <- function(x) sqrt(var(x)/length(x)) 

#create Radial plot

library(ggplot2)
ggplot(DF, aes(variable, value, fill = variable)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_errorbar(aes(ymin = value - se(DF$value), 
                    ymax = value + se(DF$value), 
                    color = variable), 
                width = .2) + 
  scale_y_continuous(breaks = 0:nlevels(DF$variable)) +
  theme_minimal() +
  coord_polar()

#Making functions ####

#Read about how to use functions in R
# https://nicercode.github.io/guides/functions/
# https://www.dummies.com/programming/r/how-to-use-a-function-in-r/
# basically it is constructed like function (argument_list)  {body}

#Choosing a good function name####
#Naming things is very important. Make sure you choose unique and memorable names
#Choose a unique name by calling it in R to see if there are results. 
#I will choose 'Radialplot_skinnyblue'
Radialplot_skinnyblue
#returned 'Error: object 'Radialplot_skinnyblue' not found' 
#which means it does not yet exist, at least not in the libraries I have


#pull a function from a project for an example
library(ethnobotanyR)
Radial_plot #returns a long list of commands starting with 'function(data, analysis)'

#copy the result and assign it to your new function name

Radialplot_skinnyblue <- function(data, analysis) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Package \"magrittr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  value <-  meltURdata <- URdata <- URs <- sp_name <- informant <- URps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  #add error stops with validate_that
  assertthat::validate_that("informant" %in% colnames(data), msg = "The required column called \"informant\" is missing from your data. Add it.")
  assertthat::validate_that("sp_name" %in% colnames(data), msg = "The required column called \"sp_name\" is missing from your data. Add it.")
  
  assertthat::validate_that(is.factor(data$informant), msg = "The \"informant\" is not a factor variable. Transform it.")
  assertthat::validate_that(is.factor(data$sp_name), msg = "The \"sp_name\" is not a factor variable. Transform it.")
  
  assertthat::validate_that(all(sum(dplyr::select(data, -informant, -sp_name)>0)) , msg = "The sum of all UR is not greater than zero. Perhaps not all uses have values or are not numeric.")
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  data_complete<-data[stats::complete.cases(data), ]
  #message about complete cases
  assertthat::see_if(length(data_complete) == length(data), msg = "Some of your observations included \"NA\" and were removed. Consider using \"0\" instead.")
  
  Radial_plot_data <- analysis(data) #create subset-able data
  
  names(Radial_plot_data)[length(names(Radial_plot_data))]<-"value" 
  
  Radial_plot <- 
    ggplot2::ggplot(Radial_plot_data, ggplot2::aes(x = sp_name, y = value, fill = sp_name)) +
    ggplot2::geom_bar(width = 0.5, stat = "identity", color = "blue") +
    ggplot2::scale_y_continuous(breaks = 0:nlevels(Radial_plot_data$sp_name), position = "right") +
    ggplot2::coord_polar() + 
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.x=ggplot2::element_blank())+
    ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank())+
    ggplot2::geom_text(ggplot2::aes(label=value), position=ggplot2::position_dodge(width=0.9), vjust=-0.25)+
    ggplot2::theme(legend.position = "none") 
  
  print(Radial_plot)
}

#Above I changed the color in the ggplot2::geom_bar options to "blue"
# I also changed ggplot2::geom_bar options to width = 0.5

Radial_plot(ethnobotanydata, URs)
Radialplot_skinnyblue(ethnobotanydata, URs)



