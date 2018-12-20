library('dplyr')
library( tidyr )
library( dplyr )
library( caret )
library( lattice )
library( ggplot2 ) 
library(gridExtra)
 # -------


change <- read.csv('change.csv')
change$X <- NULL
# set to factor and numeric
change$score<- as.numeric(change$score)
change$image <- as.factor(change$image)
change$measure <- as.factor(change$measure)



im1 = ggplot(data = change[change$image == 1,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs(y= 'score', title = as.character(1))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),         axis.text.x = element_blank(),         axis.ticks.x = element_blank())
  

im2 <- ggplot(data = change[change$image == 2,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs( y= 'score', title = as.character(2))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),         axis.text.x = element_blank(),         axis.ticks.x = element_blank())


im3 <- ggplot(data = change[change$image == 3,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs( y= 'score', title = as.character(3))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  theme(axis.title.x = element_blank(),         axis.text.x = element_blank(),         axis.ticks.x = element_blank())
  
  
im5 <- ggplot(data = change[change$image == 5,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs( y= 'score', title = as.character(5))+
  scale_fill_discrete(name = "Train/Test performance",guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),         axis.text.x = element_blank(),         axis.ticks.x = element_blank())


im6 <- ggplot(data = change[change$image == 6,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs( y= 'score', title = as.character(6))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),         axis.text.x = element_blank(),         axis.ticks.x = element_blank())


im7 <- ggplot(data = change[change$image == 7,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs( y= 'score', title = as.character(7))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),         axis.text.x = element_blank(),         axis.ticks.x = element_blank())


im8 <- ggplot(data = change[change$image == 8,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs( y= 'score', title = as.character(8))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),         axis.text.x = element_blank(),         axis.ticks.x = element_blank())
  
im9 <- ggplot(data = change[change$image == 9,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs( y= 'score', title = as.character(9))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),         axis.text.x = element_blank(),         axis.ticks.x = element_blank())

im10 <- ggplot(data = change[change$image == 10,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs( y= 'score', title = as.character(10))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),         axis.text.x = element_blank(),         axis.ticks.x = element_blank())

im11 <- ggplot(data = change[change$image == 11,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs( y= 'score', title = as.character(11))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),         axis.text.x = element_blank(),         axis.ticks.x = element_blank())

im12 <- ggplot(data = change[change$image == 12,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs( y= 'score', title = as.character(12))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),         axis.text.x = element_blank(),         axis.ticks.x = element_blank())

im13 <- ggplot(data = change[change$image == 13,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs( y= 'score', title = as.character(13))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),         axis.text.x = element_blank(),         axis.ticks.x = element_blank())

im14 <- ggplot(data = change[change$image == 14,], aes(x = measure, y = score, fill = measure, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs(y= 'score', title = as.character(14))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

grid.arrange(im1,im2,im3,im5,im6,im7,im8,im9,im10,im11,im12,im13,im14, ncol = 5, top ="Scores per image on change blindness task")


change

loceye = read.csv('csi_im.csv', sep = ',')

loceye$image <- as.factor(loceye$image)
loceye$model <- as.factor(loceye$model)
loceye$score <- as.numeric(loceye$score)


p11 = ggplot(data = loceye[loceye$image == 1.1,], aes(x = model, y = score, fill = model, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs(y= 'score', title = as.character(1.1))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p12 <- ggplot(data = loceye[loceye$image == 1.2,], aes(x = model, y = score, fill = model, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs(y= 'score', title = as.character(1.2))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p21 <- ggplot(data = loceye[loceye$image == 2.1,], aes(x = model, y = score, fill = model, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs(y= 'score', title = as.character(2.1))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p22 <- ggplot(data = loceye[loceye$image == 2.2,], aes(x = model, y = score, fill = model, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs(y= 'score', title = as.character(2.2))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p31 <- ggplot(data = loceye[loceye$image == 3.1,], aes(x = model, y = score, fill = model, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs(y= 'score', title = as.character(3.1))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p32 <- ggplot(data = loceye[loceye$image == 3.2,], aes(x = model, y = score, fill = model, label = (score)))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11), axis.title = element_text(size = 10))+
  geom_text(size = 3, nudge_y = 0.05)+
  theme_classic()+
  labs(y= 'score', title = as.character(3.2))+
  scale_fill_discrete(name = "Train/Test performance", guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


grid.arrange(p11,p12,p21,p22,p31,p32, nrow = 2, ncol = 4, top = 'Scores per image on crime scene inspection task')

