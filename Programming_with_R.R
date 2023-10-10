# Hello, this program is for didactic purposes to learn quickly
# how to operate with R.

# To RUN the code you can select the lines and press "RUN" or CTRL+ENTER
print('hello') #this should give you a "hello" in the console

# You can comment lines with the '#' symbol, these lines 
# will  be ignored from the program. You can select and comment
# multiple lines with CTRL+shift+C


#----------------------------------------------------------------
# 1) Declare the libraries that you need to use. RStudio allows 
#    to install automatically, but otherwise you can just run
#    install.packages("NAME_OF_THE_LIBRARY")
# - the libraries must be run before running the other commands -
library(tidyverse) #this library includes dataframes etc...
library(ggplot2)   #this library includes plotting features
library(readxl)    #this library allows to import data from excel
#---------------------------------------------------------------


#---------------------------------------------------------------
# 2) Import data from txt, csv or excel  files
getwd() #this will give you the working directory. Make sure to include
        #your excel files in the working directory

#    This command will create an 'object' called Data_Mixer_Raw
#    and fill this object with the content that read_excel gives
Data_Mixer_Raw <- read_excel("2023.08.09_301-202_Macros_Yellow-Red_ForAnalysis.xlsx")

head(Data_Mixer_Raw, 6) 
# if everything is good, the console will print the first 
# 6 lines of the dataframe you just created
#---------------------------------------------------------------


#---------------------------------------------------------------
# 3) Statistical analysis
# For example, calculating the averages and the Mixing Index for the dataset
# We create a dataframe with the processed data: 
# '%>%' will continue the command to the following line

First_example <- Data_Mixer_Raw %>%
  group_by(chip) %>% #this will group the data for the different values of chip
  summarise(N= n(), .groups = "keep") #this will summarize the groups 
head(First_example, 6)
# in this example, we added the variable N that will contain the result of  the 
# function n(). this function will show the number of elements for each group


# now we do the real analysis
AVG_Intensity <- Data_Mixer_Raw %>% 
  group_by(chip, dye_color, conc, pos_chip) %>%  
  summarise(N = n(), Intensity_AVG = mean(intensity),
            # Intensity_Var = sum((Intensity-Intensity_AVG)^2) /n(), 
            Intensity_SD = sd(intensity), 
            Intensity_CV = Intensity_SD/Intensity_AVG,
            Chip_MI = 1. - Intensity_CV, .groups = "keep")
head(AVG_Intensity, 6)
# with these lines we:
# - created and filled the dataframe "AVG_Intensity" with the processed version
#   of Data_Mixer_Raw
# - Data_Mixer_Raw was first grouped by its variables chip, dye_color, conc and pos_chip
#   and then summarised the content of each group.
# we used summarise to introduce additional variables: the average, SD and MI.
#---------------------------------------------------------------


#---------------------------------------------------------------
# 4) Plotting

# example: plotting the data in First_example with the number of values in
# function of the chip name
ggplot(data = First_example)+
  aes(x = chip, y = N)+
  geom_point()

# now you can play with variables and functions to get different results, for example
# you can replace the function geom_points with geom_bar (uncomment next lines)
# ggplot(data = First_example)+
#   aes(x = chip, y = N)+
#   geom_bar(stat = "identity") #this will  give a barplot


# --------------------------Mixer plots------------------------
# before plotting we introduce a coupke variables to scale the second axis.
# this is beacause we want to overlap different plots and it is slightly more complicated
q <- - 0.2
coeff <- (1. - q)/ 500.

# now we open an svg file where the plot will  be saved.
# to just plot we can comment this line or simply not select it
svg("2023-08-09_Intensity_vs_Concentration.svg", width = 10, height = 5) #open file


