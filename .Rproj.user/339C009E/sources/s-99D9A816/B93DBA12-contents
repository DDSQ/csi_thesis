library( tidyr )
library( dplyr )
library( caret )
library( lattice )
library( ggplot2 ) 
install.packages('gridExtra')
library(gridExtra)
#---------------------------------------
df <- read.csv("one.csv")

# ------ image 1 change blind
df$experience <- ifelse(df$experience == 1, 'Novice', 'CSI')

p1 = ggplot(data = df, aes(x = experience, y = relevant_fixdur, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()

p2 = ggplot(data = df, aes(x = experience, y = redundant_fixdur, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()

p3 = ggplot(data = df, aes(x = experience, y = relevant_fixnum, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()

p4 = ggplot(data = df, aes(x = experience, y = redundant_fixnum, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()


grid.arrange(p1,p2,p3,p4, nrow = 2)

#----------image 5 changeblind
df5$experience <- ifelse(df5$experience == 1, 'Novice', 'CSI')

p51 = ggplot(data = df5, aes(x = experience, y = relevant_fixdur, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()

p52 = ggplot(data = df5, aes(x = experience, y = redundant_fixdur, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()

p53 = ggplot(data = df5, aes(x = experience, y = relevant_fixnum, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()

p54 = ggplot(data = df5, aes(x = experience, y = redundant_fixnum, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()

grid.arrange(p51,p52,p53,p54, nrow = 2)

df5 <- read.csv("five.csv")

# ----------eye22
dfeye22 <- read.csv("eye_22.csv")
#----------image 5 changeblind
dfeye22$experience <- ifelse(dfeye22$experience == 1, 'Novice', 'CSI')

e221 = ggplot(data = dfeye22, aes(x = experience, y = relevant_fixdur, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()

e222 = ggplot(data = dfeye22, aes(x = experience, y = redundant_fixdur, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()

e223 = ggplot(data = dfeye22, aes(x = experience, y = relevant_fixnum, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()

e224 = ggplot(data = dfeye22, aes(x = experience, y = redundant_fixnum, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()

grid.arrange(e221,e222,e223,e224, nrow = 2)


# --------------
library('dplyr')

img <- read.csv('localcb.csv')

img$X <- NULL
img$image <- as.factor(img$image)
img$measure <- as.factor(img$measure)
img<- img[img$image != 4,]

img$importance = format(img$importance, digits = 1)
img$importance <- as.numeric(img$importance)

img$measure <- mapvalues(img$measure, 
                         from = c("redundant_fixdur", 
                                  "redundant_fixnum",
                                  "relevant_fixdur",
                                  "relevant_fixnum"),
                         to = c( "fixation duration (non evidence)", 
                                 'number of fixations (non evidence)',
                                 'fixation duration (evidence)',
                                 'number of fixations (evidence)'))

t = ggplot(data = img, aes(x = reorder(measure, importance) , y = importance, fill = measure, label =(importance)))+
  geom_bar(stat = 'identity')+
  geom_text(size = 3, nudge_y = 0.05)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(y = 'Importance', title = "Relative feature importance per image on Change blindness task", fill = 'Eye Movements')

t + facet_wrap(.~image)

# -------------eye local importance


eye = read.csv('eye_imp.csv')

eye$X <- NULL
eye$image <- as.factor(eye$image)
eye$measure <- as.factor(eye$measure)

eye$importance = format(eye$importance, digits = 1)
eye$importance <- as.numeric(eye$importance)

eye$measure <- mapvalues(eye$measure, 
                         from = c("redundant_fixdur", 
                                  "redundant_fixnum",
                                  "relevant_fixdur",
                                  "relevant_fixnum"),
                         to = c( "Redundant fixation duration", 
                                 'Redundant number of fixations',
                                 'Relevant  fixation duration',
                                 'Relevant  number of fixations'))

t = ggplot(data = eye, aes(x = measure , y = importance, fill = measure) )+
  geom_col()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(y = 'Importance', title = "Relative feature importance per image on crime scene inspection task", fill = 'Eye Movements')

t + facet_wrap(.~image, nrow = 2)


# eye local analys is for relfixnum      -----------------
eye11 = read.csv('eye11.csv')
eye21 = read.csv('eye21.csv')
eye32 = read.csv('eye32.csv')

eye32

eye11$experience <- ifelse(eye11$experience == 1, 'Novice', 'CSI')
eye21$experience <- ifelse(eye21$experience == 1, 'Novice', 'CSI')
eye32$experience <- ifelse(eye32$experience == 1, 'Novice', 'CSI')

eye11$experience <- as.factor(eye11$experience)
eye21$experience <- as.factor(eye21$experience)
eye32$experience <- as.factor(eye32$experience)

eye1 <- ggplot(data = eye11, aes(x = experience, y = relevant_fixdur, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()+
  labs( title = "Image 1.1", fill = 'Experience')

eye2 <- ggplot(data = eye21, aes(x = experience, y = relevant_fixdur, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()+
  labs( title = "Image 2.1", fill = 'Experience')
  
eye3 <- ggplot(data = eye32, aes(x = experience, y = relevant_fixdur, fill = experience))+
  geom_boxplot(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()+
  labs( title = "Image 3.2", fill = 'Experience')

grid.arrange(eye1, eye2, eye3, nrow = 2, top = 'Relevant number of fixations' )



### -----------------------------zooming in on cb image 1

relfixbone = read.csv('relfixcbone.csv')

relfixbone$ROI <- as.factor(relfixbone$ROI)

ggplot(data = relfixbone, aes(x = ROI, y = FixDur, fill = Group))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()+
  labs(x = 'Region of Interest' , y= 'Number of relevant fixations', title = "Image 1", fill = 'Experience')

# for looked at evidence

cbive = read.csv('cbive.csv')


cbive$ROI <- as.factor(cbive$ROI)

ggplot(data = cbive, aes(x = ROI, y = FixDur, fill = Group))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()+
  labs(x = 'Region of Interest' , y= 'Number of relevant fixations', title = "Image 5", fill = 'Experience')

#For skipping evidence

cbive_no = read.csv('cbive_no.csv')
cbive_no$ROI <- as.factor(cbive_no$ROI)

ggplot(data = cbive_no, aes(x = ROI, y = FixDur, fill = Group))+
  geom_col(position = 'dodge')+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))+
  theme_classic()+
  labs(x = 'Region of Interest' , y= 'Number of relevant fixations', title = "Image 5", fill = 'Experience')


# global prediction scores----------
library("RColorBrewer")
# Box plot


#local prediction scores

cblocal = read.csv('cblocal.csv')
cblocal$image <- as.factor(cblocal$image)

train_ex <- ggplot(data = cblocal, aes(x = image, y = extratrees_model_best, fill = image , label =(extratrees_model_best) ))+
  geom_col(position = 'dodge')+
  geom_text( size = 3, nudge_y = 0.05)+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10), legend.position = 'top')+
  theme_classic()+
  labs(x = 'Image', y = 'Extra Trees Model', title = 'Extra Trees: Train scores')+
  scale_fill_discrete(guide = FALSE)

test_ex <- ggplot(data = cblocal, aes(x = image, y = f1_score_extra, fill = image , label =(f1_score_extra) ))+
  geom_col(position = 'dodge')+
  geom_text( size = 3, nudge_y = 0.05)+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10), legend.position = 'top')+
  theme_classic()+
  labs(x = 'Image', y = 'Extra Trees Model', title = 'Extra Trees: Test scores')

train_for <- ggplot(data = cblocal, aes(x = image, y = forest_model_best, fill = image , label =(forest_model_best) ))+
  geom_col(position = 'dodge')+
  geom_text( size = 3, nudge_y = 0.05)+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10), legend.position = 'None')+
  theme_classic()+
  labs(x = 'Image', y = 'Random Forest model', title = 'Random Forest: Train scores ')+
  scale_fill_discrete(guide = FALSE)

test_for <- ggplot(data = cblocal, aes(x = image, y = f1_score_forest, fill = image , label =(f1_score_forest) ))+
  geom_col(position = 'dodge')+
  geom_text( size = 3, nudge_y = 0.05)+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10), legend.position = 'None')+
  theme_classic()+
  labs(x = 'Image', y = 'Random Forest model', title = 'Random Forest: Test scores')+
  scale_fill_discrete(guide = FALSE)



grid.arrange(train_ex,test_ex,train_for,test_for, nrow = 2, top = 'Performance of Ensemble methods on Change blindness Data' )
#### ---------- Loca analysis eye dataset
loce = read.csv('loc_eye.csv')
loce$image = as.factor(loce$image)
loce$X <- NULL

View(loce)

train_ext = ggplot(data = loce, aes(x = image, y = extratrees_model_best, fill = image , label =(extratrees_model_best) ))+
  geom_col(position = 'dodge')+
  geom_text( size = 4, nudge_y = 0.05)+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10), legend.position = 'None')+
  theme_classic()+
  labs(x = 'Image', y = 'Extra Trees model', title = 'Extra Trees: Train scores ')+
  scale_fill_discrete(guide = FALSE)

test_ext = ggplot(data = loce, aes(x = image, y = f1_score_extra, fill = image , label =(f1_score_extra) ))+
  geom_col(position = 'dodge')+
  geom_text( size = 4, nudge_y = 0.05)+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10), legend.position = 'None')+
  theme_classic()+
  labs(x = 'Image', y = 'Extra Trees model', title = 'Extra Trees: Test scores ')+
  scale_fill_discrete(guide = FALSE)

train_rf = ggplot(data = loce, aes(x = image, y = forest_model_best, fill = image , label =(forest_model_best) ))+
  geom_col(position = 'dodge')+
  geom_text( size = 4, nudge_y = 0.05)+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10), legend.position = 'None')+
  theme_classic()+
  labs(x = 'Image', y = 'Random Forest model', title = 'Random Forest: Train scores ')+
  scale_fill_discrete(guide = FALSE)

test_rf = ggplot(data = loce, aes(x = image, y = f1_score_forest, fill = image , label =(f1_score_forest) ))+
  geom_col(position = 'dodge')+
  geom_text( size = 4, nudge_y = 0.05)+
  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10), legend.position = 'None')+
  theme_classic()+
  labs(x = 'Image', y = 'Random Forest model', title = 'Random Forest: Test scores ')

grid.arrange(train_ext, test_ext, train_rf, test_rf, nrow = 2, top = 'Performance of Ensemble methods on Crime scene inspection Data')



#feat importance

#change blindness task
cbf <- read.csv('cb_feat.csv')

#Crime scene invspection task
csi <- read.csv('csi_feat.csv')



#cb
cbf$X <- as.factor(cbf$X)
cbf$importance = format(cbf$importance, digits = 2)
cbf$importance <- as.numeric(cbf$importance)

#csi
csi$X <- as.factor(csi$X)
csi$importance = format(csi$importance, digits = 2)
csi$importance <- as.numeric(csi$importance)

cbf$X <- mapvalues(cbf$X, 
                         from = c("redundant_fixdur", 
                                  "redundant_fixnum",
                                  "relevant_fixdur",
                                  "relevant_fixnum",
                                  
                                  "perc_evidence",
                                  "perc_moveable",
                                  "pixel",
                                  "perc_exitentry",
                                  "FixDur"),
                         to = c( "fixation duration (non evidence)", 
                                 'number of fixations (non evidence)',
                                 'fixation duration (evidence)',
                                 'number of fixations (evidence)',
                                 
                                 "% dwell time evidence",
                                 "% dwell time moveable objects",
                                 "Mean saccade amplitudes",
                                 "% dwell time exit/entry points",
                                 "Mean fixation duration"))

summary(cbf)

cbf <- ggplot(data = cbf, aes(x = reorder(X, -importance),y = importance, fill = X, label = (importance) ))+
  geom_bar( stat = 'identity')+
 geom_text( size = 4, nudge_y = 0.05)+
#  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10), legend.position = 'None')+
  theme_classic()+
  coord_flip()+
  scale_fill_discrete(guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  labs(x = 'Features', y = 'Change blindness task')

csi <- ggplot(data = csi, aes(x = reorder(X, -importance),y = importance, fill = X, label = (importance) ))+
  geom_bar( stat = 'identity')+
  geom_text( size = 4, nudge_y = 0.05)+
  #  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10), legend.position = 'None')+
  theme_classic()+
  coord_flip()+
  scale_fill_discrete(guide = FALSE)+
  scale_y_continuous(limits = c(0,1))+
  labs(x = 'Features', y = 'Crime scene inspection task')

grid.arrange(csi,cbf, top = 'Relative feature importance')

#labs( x = " Anchoring condition", y = "Number of observations", 
      #fill = "Responses consistent?")

#<- ggplot(data = ROIFix, aes(y = EVB, x = Type, fill = Type)) +
 # geom_boxplot(outlier.colour="black", outlier.shape = 1, outlier.size = 2)+
#  stat_summary(fun.y=mean, geom="point", shape=23, size=5)+
 # labs(x = "Visual Presentation", y = "Expected Value", title = "Mean expected value bottom choice")+
#  scale_fill_discrete(guide=FALSE)+
#  theme_bw()+
#  theme(plot.title = element_text(size=11),axis.title = element_text(size = 10))