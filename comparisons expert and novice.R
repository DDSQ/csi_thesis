library( tidyr )
library( dplyr )
library( caret )
library( lattice )
library( ggplot2 ) 
library(gridExtra)

ced <- read.csv("ced.csv")
eycd <- read.csv('eyced.csv')



ced$X <- NULL
eycd$X <- NULL


ced$measure <- as.factor(ced$measure)
ced$experience <- as.factor(ced$experience)

eycd$measure <- as.factor(eycd$measure)
eycd$experience <- as.factor(eycd$experience)



ceddur <- ced[(ced$measure == "evidence") | (ced$measure == "non evidence" ) ,]
cednum <- ced[(ced$measure == " evidence") | (ced$measure == " non evidence" ) ,]


eydur <- eycd[(eycd$measure == "evidence") | (eycd$measure == "non evidence" ) ,]
eynum <- eycd[(eycd$measure == " evidence") | (eycd$measure == " non evidence" ) ,]


### ---------
cb1 <- ggplot(ceddur, aes(x = measure, y = value, fill = experience, label = (value)))+
  geom_boxplot(position = 'dodge')+
  theme(axis.ticks.x = element_blank())+ 
  labs(x = 'Change blindness', y = 'fixation duration')+
  scale_fill_discrete(guide = FALSE)


cb2 <- ggplot(cednum, aes(x = measure, y = value, fill = experience, label = (value)))+
  geom_boxplot(position = 'dodge')+
  theme(axis.ticks.x = element_blank())+ 
  labs(x = 'Change blindness',y = 'number of fixations')+
  scale_fill_discrete(guide = FALSE)

# -------------------------------

ey1 <- ggplot(eydur, aes(x = measure, y = value, fill = experience, label = (value)))+
  geom_boxplot(position = 'dodge')+
  theme(axis.ticks.x = element_blank())+ 
  labs(x = "Crime scene inspection", y = 'fixation duration')+
  scale_fill_discrete(guide = FALSE)

ey2 <- ggplot(eynum, aes(x = measure, y = value, fill = experience, label = (value)))+
  geom_boxplot(position = 'dodge')+
  theme(axis.ticks.x = element_blank())+
  labs(x = "Crime scene inspection", y = 'number of fixations')+
  scale_fill_discrete(guide = FALSE)

grid.arrange(ey1,ey2,cb1,cb2, nrow = 2)

