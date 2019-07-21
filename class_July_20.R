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

#Class July 20 2019

#Make function red####
#copy and paste after calling 'ethnoChord' in the Console
New_Chord_function_red<- function(data) { #use the '<-' to assign function to new object
  if (!requireNamespace("reshape", quietly = TRUE)) {
    stop("Package \"reshape\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("circlize", quietly = TRUE)) {
    stop("Package \"circlize\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Package \"magrittr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  sp_name <- informant <- value <- strwidth <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
  #Melt ethnobotany data
  mat <- reshape::melt(data, id=c("informant","sp_name")) %>% dplyr::filter(value >=1)%>%
    dplyr::arrange(dplyr::desc(informant)) %>%  dplyr::arrange(dplyr::desc(sp_name)) %>% dplyr::select(2:3)
  
  #Create chord plot
  
  circlize::chordDiagram(mat, annotationTrack = "grid", 
                         preAllocateTracks = list(track.height = max(graphics::strwidth(unlist(dimnames(mat))))))
  circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
    xlim = circlize::get.cell.meta.data("xlim")
    xplot = circlize::get.cell.meta.data("xplot")
    ylim = circlize::get.cell.meta.data("ylim")
    sector.name = circlize::get.cell.meta.data("sector.index")
    
    if(abs(xplot[2] - xplot[1]) < 20) {
      circlize::circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                            niceFacing = TRUE, adj = c(0, 0.5), col = "red")
    } else {
      circlize::circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                            niceFacing = TRUE, adj = c(0, 0.5), col = "red")
    }
  }, bg.border = NA)
  
  print("Chord diagram for each use related to each species in the data set")
}

#Running new red function
New_Chord_function_red(ethnobotanydata)

#Copy paste from R example ####

#Use built-in ethnobotany data example
ethnoChord(ethnobotanydata)

#Generate random dataset of three informants uses for four species
eb_data <- data.frame(replicate(10,sample(0:1,20,rep=TRUE)))
names(eb_data) <- gsub(x = names(eb_data), pattern = "X", replacement = "Use_")  
eb_data$informant<-sample(c('User_1', 'User_2', 'User_3'), 20, replace=TRUE)
eb_data$sp_name<-sample(c('sp_1', 'sp_2', 'sp_3', 'sp_4'), 20, replace=TRUE)
ethnoChord(eb_data)

#Make function blue####
#copy and paste after calling 'ethnoChord' in the Console
New_Chord_function_blue<- function(data) { #use the '<-' to assign function to new object
  if (!requireNamespace("reshape", quietly = TRUE)) {
    stop("Package \"reshape\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("circlize", quietly = TRUE)) {
    stop("Package \"circlize\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Package \"magrittr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  sp_name <- informant <- value <- strwidth <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
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
  
  #Melt ethnobotany data
  mat <- reshape::melt(data, id=c("informant","sp_name")) %>% dplyr::filter(value >=1)%>%
    dplyr::arrange(dplyr::desc(informant)) %>%  dplyr::arrange(dplyr::desc(sp_name)) %>% dplyr::select(2:3)
  
  #Create chord plot
  
  circlize::chordDiagram(mat, annotationTrack = "grid", 
                         preAllocateTracks = list(track.height = max(graphics::strwidth(unlist(dimnames(mat))))))
  circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
    xlim = circlize::get.cell.meta.data("xlim")
    xplot = circlize::get.cell.meta.data("xplot")
    ylim = circlize::get.cell.meta.data("ylim")
    sector.name = circlize::get.cell.meta.data("sector.index")
    
    if(abs(xplot[2] - xplot[1]) < 20) {
      circlize::circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                            niceFacing = TRUE, adj = c(0, 0.5), col = "blue")
    } else {
      circlize::circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                            niceFacing = TRUE, adj = c(0, 0.5), col = "blue")
    }
  }, bg.border = NA)
  
  print("Chord diagram for each use related to each species in the data set")
}

#Running new blue function
New_Chord_function_blue(ethnobotanydata)

#Running all functions, original, red and blue####
ethnoChord(ethnobotanydata)
New_Chord_function_red(ethnobotanydata)
New_Chord_function_blue(ethnobotanydata)

#Formulas and R script for assignments ####

a<-1+1
b<-3*7
a+b

b<-"fred"
b

#Use functions for calculations####

#Function to calculate URs ####

new_URs_function <- function(data) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("Package \"magrittr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  URdata <- URs <- sp_name <- informant <- URps <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  #add error stops with validate_that
  assertthat::validate_that("informant" %in% colnames(data), msg = "The required column called \"informant\" is missing from your data. Add it.")
  assertthat::validate_that("sp_name" %in% colnames(data), msg = "The required column called \"sp_name\" is missing from your data. Add it.")
  
  assertthat::validate_that(is.factor(data$informant), msg = "The \"informant\" is not a factor variable. Transform it.")
  assertthat::validate_that(is.factor(data$sp_name), msg = "The \"sp_name\" is not a factor variable. Transform it.")
  
  assertthat::validate_that(all(sum(dplyr::select(data, -informant, -sp_name)>0)) , msg = "The sum of all UR is not greater than zero. Perhaps not all uses have values or are they are not numeric.")
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  data_complete<-data[stats::complete.cases(data), ]
  #message about complete cases
  assertthat::see_if(length(data_complete) == length(data), msg = "Some of your observations included \"NA\" and were removed. Consider using \"0\" instead.")
  
  URdata <- data #create subset-able data
  
  URdata$URps <- dplyr::select(URdata, -informant, -sp_name) %>% rowSums()
  URs <- URdata %>% dplyr::group_by(sp_name) %>%
    dplyr::summarize (URs = mean(URps))%>% #change from 'sum' to 'mean'
    dplyr::arrange(-URs) 
  
  print(as.data.frame(URs))
}

#Run new URs formula (wrong with 'mean' instead of 'sum')
new_URs_function(ethnobotanydata)

