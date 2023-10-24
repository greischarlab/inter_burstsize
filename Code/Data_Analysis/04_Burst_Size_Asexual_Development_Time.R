###I look at the asexual development time and the burst size to see if
###there is any relationship between them.
###This is the plot making figure

library(here)
library(ggplot2)
library(ggbeeswarm)
mal_dat <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))

subsetted_mal_dat <-   mal_dat[,c("Plasmodium.species",
                                  "Average",
                                  "Lower",
                                  "Upper",
                                  "Duration",
                                  "OrderHost")]


subsetted_mal_dat <- subsetted_mal_dat[is.na(subsetted_mal_dat $Duration) == FALSE 
                                       & subsetted_mal_dat $Duration != "",]
subsetted_mal_dat[subsetted_mal_dat$Plasmodium.species == 'circumflexum',]$Plasmodium.species<- "circumflexum_1"

subsetted_mal_dat[subsetted_mal_dat$Plasmodium.species == 'circumflexum_1',]$Duration <- 24

#circumflexum_2 
circ_2 <- data.frame("Plasmodium.species" = "circumflexum_2",
           "Average" = 16,
           "Lower" = 8,
           "Upper" = 30,
           "Duration" = 48,
           "OrderHost" = 'avian')
  
###24/36
subsetted_mal_dat[subsetted_mal_dat$Plasmodium.species == 'lophurae',]$Plasmodium.species<- "lophurae_1"

subsetted_mal_dat[subsetted_mal_dat$Plasmodium.species == 'lophurae_1',]$Duration <- 24


#lophurae_2 
loph_2 <- data.frame("Plasmodium.species" = "lophurae_2",
                     "Average" = 13,
                     "Lower" = 8,
                     "Upper" = 18,
                     "Duration" = 36,
                     "OrderHost" = 'avian')

Full_Mal_Duration_Data<-
      rbind(subsetted_mal_dat,
      circ_2,
      loph_2)

Full_Mal_Duration_Data$Duration <- as.numeric(Full_Mal_Duration_Data$Duration)
Full_Mal_Duration_Data$Average <- as.numeric(Full_Mal_Duration_Data$Average)

Full_Mal_Duration_Data$R0 <- Full_Mal_Duration_Data$Average ^ (1/(Full_Mal_Duration_Data$Duration/24))
      

ggplot(Full_Mal_Duration_Data, 
       aes(x = as.factor(Duration), 
           y = Average))+
  geom_boxplot(aes(color = as.factor(Duration)))+
  geom_beeswarm(aes(fill = as.factor(Duration)),
                    size = 2, shape = 21, color = 'black')+
    scale_color_viridis(option = 'turbo', discrete = TRUE)+
  scale_fill_viridis(option = 'turbo', discrete = TRUE)+
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,5))+
  xlab("Duration (Hours)")+
  ylab("R0")+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text= element_text(size = 15, color = 'black'),
        axis.title = element_text(size = 16, color = 'black'))
 
ggsave(here("Figure","Data_Analysis","Burst_Size_AesxualDuration_Figure.pdf"),
       width = 7.0, height = 3, units = 'in')

###R0 Figure

ggplot(na.omit(Full_Mal_Duration_Data), 
       aes(x = as.factor(Duration), 
           y = R0))+
  geom_boxplot(aes(color = as.factor(Duration)))+
  geom_beeswarm(aes(fill = as.factor(Duration)),
                size = 2, shape = 21, color = 'black')+
  scale_color_viridis(option = 'turbo', discrete = TRUE)+
  scale_fill_viridis(option = 'turbo', discrete = TRUE)+
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,5))+
  xlab("Duration (Hours)")+
  ylab("R0")+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text= element_text(size = 15, color = 'black'),
        axis.title = element_text(size = 16, color = 'black'))


ggsave(here("Figure","Data_Analysis","Burst_Size_AesxualDuration_Figure_R0.pdf"),
       width = 7.0, height = 3, units = 'in')
