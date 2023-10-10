# library(tidyverse)
# library(dslabs)
# library(ggplot2)
# library(viridis)
# library(readxl)
# library(RColorBrewer)
# library(forcats)
# library(ggforce)
# library(plotly)
# library(packcircles)
# library(scatterplot3d)
# library(htmlwidgets)
library(tidyverse) #this library includes dataframes etc...
library(ggplot2)   #this library includes plotting features
library(readxl)    #this library allows to import data from excel


#-----------------------------Importing dataset---------------------------------
Data_Mixer_Raw <- read_excel("2023_Mixer_Analysis.xlsx")

head(Data_Mixer_Raw, 6)



#-------------------------------Data analysis-----------------------------------
AVG_Intensity <- Data_Mixer_Raw %>% 
  group_by(chip, dye_color, conc, pos_chip, ratio) %>% 
  summarise(N = n(), Intensity_AVG = mean(intensity),
            # Intensity_Var = sum((Intensity-Intensity_AVG)^2) /n(), 
            Intensity_SD = sd(intensity), 
            Intensity_CV = Intensity_SD/Intensity_AVG,
            Chip_MI = 1. - Intensity_CV, .groups = "keep")
head(AVG_Intensity, 6)

AVG_301 <- AVG_Intensity %>%
  filter(chip == 301)
  head(AVG_301, 6)

# ---------------------------------Plotting-------------------------------------
# before plotting we introduce a couple variables to scale the second axis.
# this is because we want to overlap different plots and it is slightly more complicated
q <- - 0.2
coeff <- (1. - q)/ 500.


#generate plot
ggplot(data = AVG_Intensity)+
  aes(x = as.factor(ratio), y = Intensity_AVG, fill= dye_color)+
  facet_wrap(~ chip)+  #facet_wrap divides the values in relation to the chip
  geom_errorbar(aes(ymin = Intensity_AVG - 100, #geom_errorbar draws errorbars between ymin and ymax
                    ymax = Intensity_AVG + Intensity_SD), 
                width = .2,
                position = position_dodge(width = 0.9))+
  geom_bar(stat = "identity", position = "dodge")+ #geom_bar plots the 'Intensity_AVG' values
  geom_point(aes(y= (Chip_MI - q)/ coeff, group = dye_color, colour = dye_color),
             position = position_dodge(width = 0.9), size = 5)+
  # geom_point plots the 'Chip_MI values with scale correction (q and coeff)
  
  #--------graphical elements of the plot:
  scale_y_continuous(name = "Average Intensity", sec.axis = sec_axis( trans=~.*coeff + q, name="Mixing Index"))+
  xlab("Concentration")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0),
        strip.background =element_rect(fill='White', color = "Lightblue"), #this is the box around donor names
        panel.background =element_rect(fill='White', color = "Lightblue"), #this is the box around the stacks
        panel.grid.major = element_line(color = 'Lightblue'),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values = c("#a52826", "yellow"),
                    labels = c("red", "yellow"))+
  scale_color_manual(values = c("#a52826", "yellow"),
                     labels= c("red", "yellow"))


# ----------------------------------Saving--------------------------------------


write.csv(AVG_Intensity, "C:/Users/Folder/Mixer_analysis.csv")


# now we open an svg file where the plot will  be saved.
svg("v301_Mixer_Plot.svg", width = 10, height = 5) #open file


#generate plot
ggplot(data = AVG_301)+
  aes(x = as.factor(ratio), y = Intensity_AVG, fill= dye_color)+
  geom_errorbar(aes(ymin = Intensity_AVG - 100, #geom_errorbar draws errorbars between ymin and ymax
                    ymax = Intensity_AVG + Intensity_SD), 
                width = .2,
                position = position_dodge(width = 0.9))+
  geom_bar(stat = "identity", position = "dodge")+ #geom_bar plots the 'Intensity_AVG' values
  geom_point(aes(y= (Chip_MI - q)/ coeff, group = dye_color, colour = dye_color),
             position = position_dodge(width = 0.9), size = 5)+
  # geom_point plots the 'Chip_MI values with scale correction (q and coeff)
  
  #--------graphical elements of the plot:
  scale_y_continuous(name = "Average Intensity", sec.axis = sec_axis( trans=~.*coeff + q, name="Mixing Index"))+
  xlab("Ratio Red:Green")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0),
        strip.background =element_rect(fill='White', color = "Lightblue"), #this is the box around donor names
        panel.background =element_rect(fill='White', color = "Lightblue"), #this is the box around the stacks
        panel.grid.major = element_line(color = 'Lightblue'),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values = c("#a52826", "green"),
                    labels = c("Red", "Green"))+
  scale_color_manual(values = c("#a52826", "green"),
                     labels= c("Red", "Green"))
dev.off() #this command is  necessary to close the svg file


 
 
#-------------------------------OTHER GRAPHICS---------------------------------- 
#open svg file
#svg("Intensity_vs_Positionv302.svg", width = 10, height = 5) #open file
ggplot(data = AVG_Intensity)+
  aes(x = as.factor(Position_x), y = Intensity_AVG, fill= dye_color)+
  facet_wrap(~ Concentration, ncol = 5)+
  geom_errorbar(aes(ymin = Intensity_AVG - 0.1, 
                    ymax = Intensity_AVG + Intensity_SD), 
                width = .2,
                position = position_dodge(width = 0.9))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_line(aes(y= (Chip_MI+0.5)/ coeff, group = dye_color, colour = dye_color),
            position = position_dodge(width = 0.9),
            size= 1)+
  geom_point(aes(y= (Chip_MI+0.5)/ coeff, group = dye_color),
             position = position_dodge(width = 0.9))+
  #graphical section
  scale_y_continuous(name = "Average Intensity", sec.axis = sec_axis( trans=~.*coeff-0.5, name="Mixing Index"))+
  xlab("Position along x")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0),
        #axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0),
        #        axis.title.y = element_blank(),
        #axis.title.x = element_blank(),
        #        panel.border = element_blank(),
        #        strip.background = element_blank(),   #this is the box around donor names
        strip.background =element_rect(fill='White', color = "Lightblue"), #this is the box around donor names
        panel.background =element_rect(fill='White', color = "Lightblue"), #this is the box around the stacks
        panel.grid.major = element_line(color = 'Lightblue'),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values = c("#a52826", "#67BD62"),
                    labels = c("red", "green"))+
  scale_color_manual(values = c("#a52826", "#67BD62"),
                     labels= c("red", "green"))
#dev.off()





#----VIOLIN VERSION-----------------
svg("Intensity_vs_Position_violin.svg", width = 10, height = 5) #open file
ggplot(data= Data_Mixer_Raw, 
       aes(x= as.factor(conc), 
           y = intensity, fill= dye_color)) +
  geom_violin()+
  facet_wrap(~ chip, ncol = 5)+
  ylab("Intensity")+
  xlab("Position in the chip")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0),
        #axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0),
        #        axis.title.y = element_blank(),
        #axis.title.x = element_blank(),
        #        panel.border = element_blank(),
        #        strip.background = element_blank(),   #this is the box around donor names
        strip.background =element_rect(fill='White', color = "Lightblue"), #this is the box around donor names
        panel.background =element_rect(fill='White', color = "Lightblue"), #this is the box around the stacks
        panel.grid.major = element_line(color = 'Lightblue'),
        panel.grid.minor = element_blank(),
        legend.position = "none")+
  scale_fill_manual(values = c("#a52826", "#67BD62"),
                    labels = c("red", "green"))+
  scale_color_manual(values = c("#a52826", "#67BD62"),
                     labels= c("red", "green"))

  # scale_fill_brewer(limits = c("red", "green"),
  #                   direction=-1,
  #                   name = 'Genus',
  #                   palette = "RdYlBu")
dev.off()



#3D plot
Mixer_Filtered <- dplyr::filter(Data_Mixer_Raw, chip == '301', conc == '80')


fig2 <- plot_ly(Mixer_Filtered, type = "scatter3d", mode="markers",
                x = Mixer_Filtered$pos_x,
                y = Mixer_Filtered$pos_y, 
                z = Mixer_Filtered$intensity,
                color = Mixer_Filtered$intensity,
                marker = list(size = 5)) %>%
  
  plotly::layout(scene = list(xaxis = list(title = 'X [mm]', 
                                           tickfont = list(size = 16), 
                                           titlefont= list(size= 28)),
                              yaxis = list(title = 'Y [mm]', tickfont = list(size = 16), 
                                           titlefont= list(size= 28)),
                              zaxis = list(title = 'Intensity', 
                                           range= c(0,600),
                                           tickfont = list(size = 16), 
                                           titlefont= list(size= 28))),
                 annotations = list(x = 1.2,
                                    y = 1.05,
                                    text = 'fluorescence intensity',
                                    xref = 'paper',
                                    yref = 'paper',
                                    showarrow = FALSE))
fig2

