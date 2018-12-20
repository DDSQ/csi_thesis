#libraries
library('dplyr')
library( tidyr )
library( dplyr )
library( caret )
library( lattice )
library( ggplot2 ) 
library(gridExtra)

# loading datasets
cd_knn <- read.csv('cdknn.csv')
csi_knn <- read.csv('csiknn.csv')


#delete redundant X's
csi_knn$X <-NULL
cd_knn$X <- NULL

# round the numbers
cd_knn$score = format(cd_knn$score, digits = 2)
csi_knn$score = format(csi_knn$score, digits = 2)

# format to numeric again
cd_knn$score <- as.numeric(cd_knn$score)
csi_knn$score <- as.numeric(csi_knn$score)


#factorize
csi_knn$fase <- as.factor(csi_knn$fase)
cd_knn$fase <- as.factor(cd_knn$fase)

csi_knn$score <- as.numeric(csi_knn$score)
cd_knn$score <- as.numeric(cd_knn$score)

# plot
pl <-  ggplot(data = cd_knn, aes(x = fase, y = score, fill = fase,  label = (score)))+
  geom_col(position = "dodge")+
  geom_text(size = 3, nudge_y = 0.05)+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  labs( y = 'train and test accuracies', title = 'Change blindness task', fill = 'Fase:Train/Test')
  
  
pl + facet_wrap(.~image, nrow = 3)


pl2 <-  ggplot(data = csi_knn, aes(x = fase, y = score, fill = fase,  label = (score)))+
  geom_col(position = "dodge")+
  geom_text(size = 3, nudge_y = 0.05)+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  labs(y = 'train and test accuracies', title = 'Crime scene inspection task', fill = 'Fase: Train/Test')


pl2 + facet_wrap(.~image, nrow = 3)



