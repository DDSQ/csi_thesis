
# coding: utf-8

# In[ ]:


import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import pickle
import glob
from sklearn.utils import resample

import seaborn as sns
import matplotlib.image as mpimg
from PIL import Image
from sklearn.decomposition import PCA

from functools import reduce

import warnings



from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier, ExtraTreesClassifier
from sklearn.tree import DecisionTreeClassifier

from sklearn.model_selection import cross_val_score
from sklearn.pipeline import Pipeline
from sklearn.model_selection import GridSearchCV
from sklearn.preprocessing import StandardScaler

from sklearn.neighbors import KNeighborsClassifier
from sklearn import metrics



def create_dict(df, col):
    """takes a dataframe with a Participant column
    and splits it into a dictionary object containing a dataframe per participant
    in order for this to work the df columns has to have a participant column"""
    
    columns = list(df[col].unique()) 
    
    dict_df = {}
    for x in columns:
        dict_df[x] = df[df[col] == x]
    return(dict_df)

def dict_to_df(x, colname):
    
    #
    x = pd.Series(x).to_frame()
    x = x.rename(index = str, columns = {0: colname})
    x = x.reset_index()
    x = x.rename(index = str, columns = {'index': 'Participant'})
    x.loc[:,'Participant'] = x.loc[:,'Participant'].astype(int)
    return x

def return_perc(x):
    perc = x[x.Evidence == 'Yes']['FixDur'] / x.FixDur.sum() * 100
    return float(perc)


def numbers_pp(df, pp, col):
    """takes a dataframe and the name of a column with discrete variables and returns
    a dictionary with key value pairs being participant and a value of interest 
    
    df = df with columns
    col = column of interest (Note that this column should be included in df with columns).
    """
    perc_yes = {}
    
    value = create_dict(df, pp)
    
    for k in value.keys():
        value[k] = value[k].drop(['Participant'], axis = 1)
        
        a = pd.DataFrame(value[k].groupby([col]).sum()).reset_index()
        b = col
        
        perc_yes[k] = return_perc2(a,b)
        
    return perc_yes

def return_perc2(x, col):
    
    
    perc = x[x[col] == 'Yes'].loc[:,'FixDur'] / x.FixDur.sum() * 100
    return perc

def get_unique (df, col):
    new_df = {}
    
    dict_of_df = create_dict(df)
    
    for i in dict_of_df.keys():
        #dict_of_df[i] = dict_of_df[i].drop(['Participant'], axis = 1)
        new_df[i] = len(list(dict_of_df[i][col].unique())) 
        
    return new_df


def deal_with_imbalance(df,k):

    #df.Group = df.Group.map(dict(Novice = int(1), CSI = int(2))) #turn classes into 0 and 1 
    
    #Here we chose a knn strategy with k=3 in order to resample the minority class.
    smote = SMOTE(sampling_strategy = 'auto', k_neighbors = 3 ,random_state = 2018)
    X, y = smote.fit_resample(np.array(df.iloc[:,0:3]), np.array(df.Group)) # resample classes with a KNN

    #MAKE COLUMNS
    cols = ['ROI', 'FixDur', 'Pixel']
    cols2 = ['Group']

    #Concatenate the arrays into a new dataframe
    df = pd.concat([pd.DataFrame(X, columns = cols), pd.DataFrame(y, columns = cols2)],axis = 1  )
    df['Image'] = k
    df[['ROI','FixDur']] = df[['ROI','FixDur']].astype('int32') #= int(df['ROI'])
    
    
    return df

def tree_imp(df,cols):
    from sklearn.model_selection import train_test_split
    from sklearn.ensemble import RandomForestClassifier, ExtraTreesClassifier
    import pandas as pd
    
    """Tree variable importance estimator"""
    #Divide in predictors and variables
    X = df.iloc[:,:cols] #all features except for Participantnumber
    y = df.loc[:,'experience']
    
    #assign the forrest
    clf_ens = RandomForestClassifier(random_state = 2018)
    
    clf_ens.fit(X,y)
    importances = clf_ens.feature_importances_
    
    
    impo = {}
    
    for i,j in zip(clf_ens.feature_importances_, df.iloc[:,:cols].columns):
        impo[j] = i
        
    impo_rf = pd.DataFrame.from_dict(impo, orient = 'index')
    impo_rf.rename(columns = {0:'importance'}, inplace = True)
    
    return impo_rf


def feat_plot(df,cols, title):
    """ takes df, cols and title and returns feature_df and shows plot"""
    
    import matplotlib.pyplot as plt
    import seaborn as sns
    
    feat_df = tree_imp(df,cols)
    
    fig = plt.figure(figsize = (8,8), dpi = 80)
    plt.style.use('classic')
    plt.xticks(rotation = 45, size = 16)
    sns.barplot(x = feat_df.index, y = 'importance', data = feat_df)
    
    return feat_df, plt.show()


#Score_knn that returns only list of scores and value of best k



#Score_knn including classificationreport


def score_knn_cv(df,cols):
    
    from sklearn.metrics import classification_report
    
    labels = [1,2]
    target_names =['Novice', 'Expert']
    
    rs = 2018
    score_list = []
    report = {}
    
    X_train, X_test, y_train, y_test = train_test_split(df.iloc[:,:cols],df.experience,test_size = 0.33, random_state = rs)
    
    n_n = np.linspace(2,10,9)
    n_n = [int(i) for i in n_n if i % 2 == 1]
     
    
    #K moet ongelijk zijn.
    for k in n_n:
        knn = KNeighborsClassifier(n_neighbors = k)
        knn.fit(X_train, y_train)
        
        #y_pred = knn.predict(X_test)
        
        score = cross_val_score(knn, X_train, y_train, cv = 5, scoring = 'f1')
        score_list.append(score.mean())
        
    
    #Compute best K

    
    score_list_k = pd.DataFrame({'scores': [i for i in score_list ], 'K': [int(i) for i in n_n if i%2 == 1]})
    #Use best K for predictions
    
    
    #scorelist = crossvalidationscore per K
    # y = Best performing K over all crossvalidations
    # Prediction with that K
    
    value_of_best_k = int(score_list_k.nlargest(1,'scores').K)
    value_of_best_k = int(score_list_k.nlargest(1,'scores').K)
    
    
    # Fit KNN with best K for prediction
    k_pred = KNeighborsClassifier(n_neighbors = value_of_best_k)
    k_pred.fit(X_train, y_train)
    
    #Now predict
    y_pred = k_pred.predict(X_test)
    
    clf_rep = classification_report(y_test, y_pred, labels = labels,target_names = target_names, output_dict = True)
        
    return score_list_k, value_of_best_k, clf_rep



def ensemble_classifier(df,cols):
    
    """
    This function takes a dataframe and columns and performs a double stratified gridsearch cross validation with a
    RandomForest and an ExtraTree classifier. And uses the best performing estimator setting to predict and output
    two classification reports and two gridsearch objects for each classifier.    
    """
    
    from sklearn.model_selection import train_test_split
    from sklearn.ensemble import RandomForestClassifier, ExtraTreesClassifier
    from sklearn.metrics import classification_report
    from sklearn.model_selection import StratifiedKFold
    from sklearn.model_selection import GridSearchCV
    from sklearn import metrics
    import pandas as pd
    import numpy as np
    
    rs = 2018 #Randomstate    
    
    #Divide in predictors and variables
    X = df.iloc[:,:cols] #all features except for Participantnumber
    y = df.loc[:,'experience']
    
    
    #Feature importance
    clf_imp = RandomForestClassifier(random_state = 2018)
    clf_imp.fit(X,y)
    
    importances = clf_imp.feature_importances_
    
    
    impo = {}
    for i,j in zip(clf_imp.feature_importances_, df.iloc[:,:cols].columns):
        impo[j] = i
        
    impo_rf = pd.DataFrame.from_dict(impo, orient = 'index')
    impo_rf.rename(columns = {0:'importance'}, inplace = True)
    
    
    
    
    #Divide in train and test split
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.33, random_state = rs)
    
    
    #Defining our gridsearch parameters
    vife_ints = np.linspace(2,6,5).astype(int) # Create an array with 2 to 6 as grids to be searched
    
    
    param_grid_extra = {
    'max_depth':(vife_ints),
    'min_samples_split': (vife_ints),
    'min_samples_leaf' : (vife_ints),
    'max_features': ['auto', 'sqrt', 'log2']}
    
    #Feature importance of the dataframe
    
    
    
    #assign the tree_model and train
    clf_ens = ExtraTreesClassifier(bootstrap = True, oob_score = True, random_state = rs, verbose =1)
    grid_clf_ens = GridSearchCV(clf_ens, param_grid = param_grid_extra , cv = StratifiedKFold(n_splits = 3, shuffle = True, random_state = rs)).fit(X_train, y_train)
    
    #assign the forrest_model and train
    clf_for = RandomForestClassifier(bootstrap = True, oob_score = True, random_state = rs, verbose = 1)
    grid_clf_for = GridSearchCV(clf_for, param_grid = param_grid_extra , cv = StratifiedKFold(n_splits = 3, shuffle = True, random_state = rs)).fit(X_train, y_train)
        
    
    #Feature importance for tree model
    #importances = clf_ens.feature_importances_
    #oob_score = clf_ens.oob_score_
    
    #Feature importance for rf model
    #importances_for = clf_for.feature_importances_
    #oob_score_for = clf_for.oob_score_
    
    #predictions and classification_report
    y_pred = grid_clf_ens.best_estimator_.predict(X_test)
    y_hat_forest = grid_clf_for.best_estimator_.predict(X_test)
    
    
    #classification report
    class_rep_ens = classification_report(y_test, y_pred,
                          labels = [1,2], target_names = ['Novice', 'Expert'],
                         output_dict = True)
    
    class_rep_for = classification_report(y_test, y_hat_forest,
                          labels = [1,2], target_names = ['Novice', 'Expert'],
                         output_dict = True)    
    

    return grid_clf_ens, grid_clf_for, class_rep_ens, class_rep_for, impo_rf






#---------------



def compute_rel_red_cols(df):
    
    
    parts = pd.DataFrame({ 'Participant':list(set(df.Participant))})
    
    # Mean of relevant Fixdur
    relevant_FixDur = df[df.Evidence == 'Yes'].groupby(['Participant']).mean().rename( columns = {'FixDur': 'relevant_fixdur'})

    #Mean of irrelevant FixDUr
    redundant_FixDur = df[df.Evidence != 'Yes'].groupby(['Participant']).mean().rename(columns = {'FixDur': 'redundant_fixdur'})
    
    #number of relevant fixations, Here we drop evidence because for some reason counting stays in our dataframe
    relevant_nr_fix = df[df.Evidence == 'Yes'].groupby(['Participant']).count().drop(['Evidence'],axis =1).rename(columns = {'FixDur': 'relevant_fixnum' })
    
    
    #number of irrelevant fix|ations
    redundant_nr_fix = df[df.Evidence != 'Yes'].groupby(['Participant']).count().drop(['Evidence'],axis =1).rename( columns = {'FixDur': 'redundant_fixnum'})


    dfs = [relevant_FixDur,redundant_FixDur, relevant_nr_fix,redundant_nr_fix] # everything into a list of dfs to speed things up including participants
    
    for j in dfs:
        j = j.reset_index(inplace = True)  #reset index so we get 'Participant as a column'
    
    dfs.append(parts)
    
    df2 = reduce(lambda left,right: pd.merge(left,right,on = 'Participant'), dfs).drop(['index'], axis =1) # concatenate all the DF's
    
    df2 = df2.T.drop_duplicates().T #.drop(['image_scene_x'],axis = 1)
    df2.experience_x = df2[['experience']].astype(int)
    #df2.rename( columns = {'experience': 'experience'})
    #df2.experience = df2.experience.replace(['FirstYear','Control','ThirdYear'],['Novice','Novice', 'CSI'])
    
    df2 = df2.drop(['Participant'], axis= 1)
                
    return df2
                
                
                
                
                
    
    
    
    




