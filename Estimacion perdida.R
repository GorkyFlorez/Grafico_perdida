

df2 <- data.frame(
                  Año=rep(c("1999", "2004", "2009", "2014", "2018")),
                  Perdida=c(27.3, 27.5, 27, 25.4, 24))

head(df2)
library(ggplot2)
Barplot= ggplot(df2, aes(x = Año, y = Perdida))+
  geom_col(fill = "#2a9d8f", width = 0.6)+
  geom_text(aes(label = Perdida), vjust = 1.6, color = "white", size=2)+
  scale_y_continuous(limits = c(0, 35), expand = c(0, 0))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5),
        axis.text.y  = element_text(face="bold", color="black",
                                    family="serif",size=7),
        axis.text.x = element_blank(),
        axis.title = element_text(color="black",size = 9),
        legend.position = "none",
        plot.caption = element_text(size = 8, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = margin(t = 15)),
       )+
  labs(x = "", y = expression('Superficie de bosque km '^2),
       title = "",caption = "")


# Packages
library(ggplot2)
library(dplyr)



# create data
time <- as.numeric(rep(seq(1,5),each=5))  # x Axis
value <- c(0.10, 0.17, 0.20, 0.40, 0.6,
           0.20, 0.30, 0.64, 0.80, 0.70,
           0.25, 0.50, 0.68, 0.93, 0.92,
           0.28, 0.84, 1.10, 1.10, 1.32,
           0.28, 0.94, 1.30, 1.30, 1.62)               # y Axis
group <- c("Bosque secundario", "Agricultura", "Area de extraccion minera", "Agua de disposicion residual" ,"Bosque",
           "Bosque secundario", "Agricultura", "Area de extraccion minera", "Agua de disposicion residual" ,"Bosque",
           "Bosque secundario", "Agricultura", "Area de extraccion minera", "Agua de disposicion residual" ,"Bosque",
           "Bosque secundario", "Agricultura", "Area de extraccion minera", "Agua de disposicion residual" ,"Bosque",
           "Bosque secundario", "Agricultura", "Area de extraccion minera", "Agua de disposicion residual" ,"Bosque")      
data <- data.frame(time, value, group)

data 

# stacked area chart
Areaplot= ggplot(data, aes(x=time, y=value, fill=group)) + 
  geom_area()+
  scale_x_discrete(limit = c("1", "2","3","4","5"), expand = c(0, 0),
                   labels = c("1999", "2004", "2009", "2014", "2018"))+
  scale_y_continuous(limits = c(0, 8), expand = c(0, 0))+
  theme_bw()+
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5),
        axis.text.y  = element_text(face="bold", color="black",
                                    family="serif",size=7),
        axis.text.x  = element_text(face="bold", color="black",
                                    family="serif",size=7),
        axis.title = element_text(size = 9),
        legend.position = c(0.35, 0.75),
        legend.text=element_text(size=6, family="serif"),
        legend.title = element_text(size=6, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.2,"cm"), #ancho de cuadrados de referencia 
        plot.caption = element_text(size = 8, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = margin(t = 15)),
        plot.background = element_rect(fill = NA, color=NA),
    
  )+
  labs(x = "Año", y = expression('Perdida de bosque km'^2),
       title = "",caption = "")+
  scale_fill_manual(values = c("#ffd60a", "#0077b6","#e85d04", "#6a994e","#1dd3b0"),
                    name="") 

library(cowplot)
Expo = ggdraw() +
  coord_equal(xlim = c(0, 10), ylim = c(0, 16), expand = FALSE) +
  
  draw_plot(Barplot , width = 8, height = 8,x = 0, y = 7)+
  draw_plot(Areaplot, width = 8, height = 8,x = 0.1, y = 1.1)+
  
  
  
  theme(panel.background = element_rect(fill = "white"))

Expo

ggsave(plot=Expo ,"Grafico estadistico.png",units = "cm",width = 10, #ancho
       height = 16, #alargo
       dpi=1200)
