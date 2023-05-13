import numpy as np
import pandas as pd
from sklearn import linear_model
import seaborn as sns
import pickle
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.model_selection import TimeSeriesSplit
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import cross_val_score,KFold
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_absolute_error
# Find the best parameter combination
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import RandomizedSearchCV
from scipy.stats import randint
from scipy.stats import randint as sp_randint # helps to initialise random values
from xgboost import XGBRegressor
from xgboost import plot_importance
from sklearn import linear_model
from sklearn.model_selection import cross_val_score,KFold,cross_validate
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_absolute_error,mean_squared_error
from sklearn.model_selection import PredefinedSplit




def RF(X_train, Y_train, X_test, Y_test, method):
    
    if (method == 'GridSearch'):
        
        param_grid = [
        # try 12 (3×4) combinations of hyperparameters
        {'max_depth': [2, 3, 4, 5, 6, 8, 12, 14, 16]},
        # then try 6 (2×3) combinations with bootstrap set as False
        {'max_features': [2, 4, 6, 8, 10, 12, 14, 16]},
        ]

        # Create a base model
        rf = RandomForestRegressor(random_state = 42)

        # Instantiate the grid search model
        grid_search = GridSearchCV(estimator = rf, param_grid = param_grid, scoring='neg_mean_squared_error',
                                  cv = 10, n_jobs = -1, verbose = 2, return_train_score=True)

        grid_search.fit(X_train, Y_train)
        cvres = grid_search.cv_results_
        for mean_score, params in zip(cvres["mean_test_score"], cvres["params"]):
            print(np.sqrt(-mean_score), params)
            
        pd.DataFrame(grid_search.cv_results_)
        final_model = grid_search.best_estimator_
        features_importance = grid_search.best_estimator_.feature_importances_
        
        print('Score train: %.2f' % final_model.score(X_train, Y_train, sample_weight=None))
        print('Score test: %.2f' % final_model.score(X_test, Y_test, sample_weight=None))
        
    if (method == 'RandomizedSearch'):
        
        #param_distribs = {
        #'n_estimators': randint(low=1, high=200),
        #'max_features': randint(low=1, high=8),
        #}
        param_distribs = {"max_depth": [2, None],
              "max_features": sp_randint(1, X_train.shape[1]),
              "min_samples_split": sp_randint(2, 11),
              "bootstrap": [False],
              "n_estimators": sp_randint(10, 100)}

        forest_reg = RandomForestRegressor(random_state=42)
        rnd_search = RandomizedSearchCV(forest_reg, param_distributions=param_distribs,
                                n_iter=20, cv=10, scoring='neg_mean_squared_error', random_state=42)
        rnd_search.fit(X_train, Y_train)
        cvres = rnd_search.cv_results_
        for mean_score, params in zip(cvres["mean_test_score"], cvres["params"]):
            print(np.sqrt(-mean_score), params)
    
        final_model = rnd_search.best_estimator_
        features_importance = rnd_search.best_estimator_.feature_importances_
        
        
        print('Score train: %.2f' % final_model.score(X_train, Y_train, sample_weight=None))
        print('Score test: %.2f' % final_model.score(X_test, Y_test, sample_weight=None))
        
    if (method == None):
        
        forest_reg = RandomForestRegressor(random_state=42)
        forest_reg.fit(X_train, Y_train)
        forest_scores = cross_val_score(forest_reg, X_train, Y_train,
                                scoring="neg_mean_squared_error", cv=10)
        forest_rmse_scores = np.sqrt(-forest_scores)
        pd.Series(forest_rmse_scores).describe()
        final_model = forest_reg
        features_importance = forest_reg.feature_importances_
        
        print('Score train: %.2f' % final_model.score(X_train, Y_train, sample_weight=None))
        print('Score test: %.2f' % final_model.score(X_test, Y_test, sample_weight=None))
    
    return(final_model)
    
    
    
class Classicalmodels():
    """
      args: type of model
    """
    
    def __init__(self, model, X_train, y_train, X_test, y_test, yy, features, s_method, csv = True):
        
        self.X_train = X_train
        self.X_test = X_test
        self.y_train = y_train
        self.y_test = y_test
        self.yy = yy 
        self.features = features
        self.csv = csv
        self.method = s_method
        
        if model == "linear":
            self.model = self.LR()
        elif model == "randomforest":
            self.model = self.RF()
        elif model == "rf_Predefinesplit":
            self.model = self.RF_PredSplit()
        else:
            raise('The model is not defined')
            self.model = self.LR()
            
    def LR(self):
        
     #def LR(X_train, y_train, X_test, y_test, yy, features, csv = True):
        tscv = TimeSeriesSplit(n_splits=len(self.yy)-1)
        # if TimeSeriesSplit is used
        X_all = np.append(self.X_train, self.X_test, axis=0)
        y_all = np.append(self.y_train, self.y_test, axis=0)
        ## linear regression
        ml = LinearRegression()

        if self.csv == True: 
            scores = cross_validate(ml, X_all, y_all, cv=tscv, scoring= ['neg_root_mean_squared_error','r2'])
            neg_rmse = np.abs(scores['test_neg_root_mean_squared_error'])
            mean_neg_rmse = np.mean(neg_rmse)
            r2 =  np.mean(scores['test_r2'])
            print('Model r-sq mean:',r2)
            model = ml.fit(X_all, y_all)
            train_predicted = pd.DataFrame(model.predict(self.X_train), columns = ["predicted"])
            test_predicted = pd.DataFrame(model.predict(self.X_test), columns = ["predicted"])

        else:
            model = ml.fit(self.X_train, self.y_train)
            score_train = model.score(self.X_train, self.y_train)
            score_test = model.score(self.X_test, self.y_test)
            print('Model r-squared score from test data: {:0.4f}'.format(score_train))
            print('Model r-squared score from train data: {:0.4f}'.format(score_test))
        
       

        return model    
    
    
    def RF_PredSplit(self):
        
        X_all = np.append(self.X_train, self.X_test, axis=0)
        y_all = np.append(self.y_train, self.y_test, axis=0)
        n_test_obs = len(X_all)
        n_valid_obs = 365 # define the period for the test (validation in my case)
        test_fold_encoding = list(np.concatenate([np.ones(n_test_obs - n_valid_obs), np.zeros(n_valid_obs)]))
        cv = [[c for c in PredefinedSplit(test_fold=test_fold_encoding).split()][0]]
        
        model = RandomForestRegressor()
        param_grid = {'n_estimators':  [1,5],
                               'max_features': [2,4,6],
                               'max_depth': [10,12,14,16],
                              #'max_depth': [6,10,12,14,16],
                              # 'min_samples_split': min_samples_split,
                              # 'min_samples_leaf': min_samples_leaf,
                                }
        
        grid_search = GridSearchCV(estimator=model, param_grid=param_grid, refit=True,
                           scoring='neg_mean_absolute_error', cv=cv, n_jobs=-1).fit(X_all, y_all)
        final_model = grid_search.best_estimator_
        
        y_pred_rf = final_model.predict(self.X_test)

        # Compute and print the metrics
        print(f"Tuned RF params: {final_model.set_params}")
        print(f"Tuned RF Score:  {final_model.score(self.X_train, self.y_train)}")
        print(f"Tuned RF Score:  {final_model.score(self.X_test, self.y_test)}")
        
        
        return final_model
    
    def RF(self):
     #def RF(X_train, y_train, X_test, y_test, yy, features, method):
        tscv = TimeSeriesSplit(n_splits=len(self.yy)-1)
        # if TimeSeriesSplit is used
        X_all = np.append(self.X_train, self.X_test, axis=0)
        y_all = np.append(self.y_train, self.y_test, axis=0)

        # Create a base model
        rf = RandomForestRegressor(random_state = 42)

        n_estimators =  [1,5] #[10,20,50,100]
        max_features = [2, 4, 6, 8, 10]# Number of features to consider at every split
       # max_depth = [int(x) for x in np.linspace(1, 30, num = 10)] # Maximum number of levels in tree
       # max_depth.append(None)
        max_depth = [10,12,14,16]
        min_samples_split = [5, 10] # Minimum number of samples required to split a node
        min_samples_leaf = [1, 2, 4] # Minimum number of samples required at each leaf node
        # Create the random grid
        param_grid = {'n_estimators': n_estimators,
                       'max_features': max_features,
                       'max_depth': max_depth,
                      # 'min_samples_split': min_samples_split,
                      # 'min_samples_leaf': min_samples_leaf,
                        }
       

        if (self.method == 'GridSearch'):

            # Instantiate the grid search model
            grid_search = GridSearchCV(estimator = rf, param_grid = param_grid, scoring='neg_mean_squared_error',verbose=0,
                                      cv = tscv, n_jobs = -1, return_train_score=True)

            grid_search.fit(X_all, y_all)
            cvres = grid_search.cv_results_
            for mean_score, params in zip(cvres["mean_test_score"], cvres["params"]):
                print(np.sqrt(-mean_score), params)

            pd.DataFrame(grid_search.cv_results_)
            final_model = grid_search.best_estimator_
            #features_importance = grid_search.best_estimator_.feature_importances_

            print('Score train: %.4f' % final_model.score(self.X_train, self.y_train, sample_weight=None))
            print('Score test: %.4f' % final_model.score(self.X_test, self.y_test, sample_weight=None))

        if (self.method == 'RandomizedSearch'):

            rnd_search = RandomizedSearchCV(rf, param_distributions=param_grid,verbose=0,
                                    cv = tscv, n_jobs = -1,  return_train_score=True)
            rnd_search.fit(X_all, y_all)
            cvres = rnd_search.cv_results_
            for mean_score, params in zip(cvres["mean_test_score"], cvres["params"]):
                print(np.sqrt(-mean_score), params)

            final_model = rnd_search.best_estimator_
            #features_importance = rnd_search.best_estimator_.feature_importances_

            print('Score train: %.4f' % final_model.score(self.X_train, self.y_train, sample_weight=None))
            print('Score test: %.4f' % final_model.score(self.X_test, self.y_test, sample_weight=None))

        if (self.method == None):

            forest_reg = rf
            forest_reg.fit(self.X_train, self.y_train)
            forest_scores = cross_val_score(forest_reg, self.X_train, self.y_train,
                                    scoring="neg_mean_squared_error", cv=10)
            forest_rmse_scores = np.sqrt(-forest_scores)
            pd.Series(forest_rmse_scores).describe()
            final_model = forest_reg
           
            print('Score train: %.4f' % final_model.score(self.X_train, self.y_train, sample_weight=None))
            print('Score test: %.4f' % final_model.score(self.X_test, self.y_test, sample_weight=None))

        
        
        return final_model



