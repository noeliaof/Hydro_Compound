from sklearn.impute import SimpleImputer
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.compose import ColumnTransformer
import pandas as pd
import seaborn as sns
import numpy as np
import matplotlib.pyplot as plt

from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score
from sklearn.tree import DecisionTreeRegressor
from sklearn.model_selection import cross_val_score
from sklearn.metrics import mean_absolute_error,mean_squared_error
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report

def readcsv(csvfiles):
    dataframes = []  # a list to hold all the individual pandas DataFrames
    for csvfile in csvfiles:
        df = pd.read_csv(csvfile)
        dataframes.append(df)
        
    return(dataframes)

def expl_data(df_hp, nvars):
    # from pandas.tools.plotting import scatter_matrix # For older versions of Pandas
    from pandas.plotting import scatter_matrix
    #categorical = ['T90','HD']
    scatter_matrix(df_hp[nvars], figsize=(12, 8))
    #save_fig("scatter_matrix_plot")
    corr_matrix = df_hp[nvars].corr()
    print(corr_matrix)
    
    
def split_data(df, yy_train, yy_test, yy_reconstructed, attributes, ylabel):
    """"Split the data into train and test
         df is the data\n",
         attributes are the covariates,
         ylabel is the target variable"""
    if ( yy_reconstructed!=None):
        train_dataset = df[(df.date.dt.year >= yy_train[0]) & (df.date.dt.year <= yy_train[1])]
        test_dataset = df[(df.date.dt.year >= yy_test[0]) & (df.date.dt.year <= yy_test[1])]
        reconstructed_dataset = df[(df.date.dt.year >= yy_reconstructed[0]) & (df.date.dt.year <= yy_reconstructed[1])]
          # extract the dates for each datasets
        train_dates = train_dataset['date']
        test_dates = test_dataset['date']
        reconstructed_dates = reconstructed_dataset['date']
        # extract labels
        train_labels = train_dataset[ylabel].copy()
        test_labels = test_dataset[ylabel].copy()
        # extract predictors\n",
        train_dataset = train_dataset[attributes]
        test_dataset = test_dataset[attributes]
        reconstructed_dataset = reconstructed_dataset[attributes]
        return(train_dataset, train_labels, test_dataset, test_labels, train_dates, test_dates, reconstructed_dataset, reconstructed_dates)

    else:
        
        train_dataset = df[(df.date.dt.year >= yy_train[0]) & (df.date.dt.year <= yy_train[1])]
        test_dataset = df[(df.date.dt.year >= yy_test[0]) & (df.date.dt.year <= yy_test[1])]
        train_dates = train_dataset['date']
        test_dates = test_dataset['date']
        # extract labels
        train_labels = train_dataset[ylabel].copy()
        test_labels = test_dataset[ylabel].copy()
        train_dataset = train_dataset[attributes]
        test_dataset = test_dataset[attributes]
        return(train_dataset, train_labels, test_dataset, test_labels, train_dates, test_dates)


        
def prepareData(dd):
    
    num_attribs = list(dd)
    num_pipeline = Pipeline([
        ('imputer', SimpleImputer(strategy="median")),
        ('std_scaler', StandardScaler()),
    ])

    full_pipeline = ColumnTransformer([
        ("num", num_pipeline, num_attribs),
    ])

    df_prepared = full_pipeline.fit_transform(dd)
    
    return(full_pipeline)



def plotprediction_TS(test_dates, train_dates,train_labels, final_predictions, test_labels):
    import seaborn as sns
    df_to_compare = pd.DataFrame({'date': test_dates, 'Actual': test_labels, 'Predicted': final_predictions})
    dfm = pd.melt(df_to_compare, id_vars=['date'], value_vars=['Actual', 'Predicted'], var_name='data', value_name='precip')
    
    df_1 = pd.DataFrame({'date': test_dates, 'data': 'Predicted', 'generation':final_predictions})
    df_2 = pd.DataFrame({'date':train_dates, 'data': 'Actual', 'generation': train_labels})
    df_all = pd.concat([df_1, df_2])

    f, axs = plt.subplots(1,2,
                      figsize=(12,5),
                      sharey=True)

    sns.regplot(data= df_to_compare,
                x="Actual",
                y="Predicted",
                ax=axs[0],
                )
   # sns.lineplot(x='date', y='precip', hue = 'data', data=dfm, ax=axs[1])
    sns.lineplot(x='date', y='generation', hue = 'data', data=df_all, ax=axs[1])
    
    
def model_lr(X_train, Y_train, X_test, train_dates, test_dates):
    lin_reg = LinearRegression()
    lin_reg.fit(X_train, Y_train)
    print('Intercept: \n', lin_reg.intercept_)
    print('Coefficients: \n', lin_reg.coef_)
    final_predictions = lin_reg.predict(X_test)
    train_mse = mean_squared_error(train_labels, lin_reg.predict(X_train))
    train_rmse = np.sqrt(train_mse)
    len(np.array(final_predictions))
    #r2_train = r2_score(train_labels, lin_reg.predict(X_train))
    #r2_test = r2_score(test_labels, lin_reg.predict(X_test))
    #print('r2 train: \n', r2_train)
    #print('r2 test: \n', r2_test)
    return(lin_reg, final_predictions)



def plot_reconstructed(df, attributes, mvar, type_model, fit):
    import seaborn as sns
    
    # For reconstruction
    all_dates = df['date']
    X_all = fpipeline.fit_transform(df[attributes])
    Y_label = df[mvar]
    if (type_model == 'keras'):
        Reconstructed = list(fit.predict(X_all).flatten())
    else:
        Reconstructed = fit.predict(X_all)
    
    df_all = pd.DataFrame({'date': all_dates, 'Actual': df_hp[mvar], 'Reconstructed': Reconstructed})
    dfm = pd.melt(df_all, id_vars=['date'], value_vars=['Actual', 'Reconstructed'], var_name='data', value_name=mvar)
    plt.figure(figsize=(20,5))

    sns.lineplot(x='date', y=mvar, hue = 'data',linestyle="dashed", data=dfm)
    
    
    
def DecisionTr_model(X_train, Y_train, X_test, train_dates, test_labels, test_dates):
    tree_reg = DecisionTreeRegressor(random_state=42)
    tree_reg.fit(X_train, Y_train)
    tree_predictions = tree_reg.predict(X_test)
    plotprediction_TS(test_dates, train_dates,train_labels, tree_predictions, test_labels)
    # Model scores 
    tree_scores = cross_val_score(tree_reg, X_train, Y_train,
                         scoring="neg_mean_squared_error", cv=10)
    tree_rmse_scores = np.sqrt(-tree_scores)
    print("mean cross validation score: {}".format(np.mean(tree_rmse_scores)))
    print("score without cv: {}".format(tree_reg.score(X_train, Y_train)))

    pd.Series(tree_rmse_scores).describe()
    return(tree_rmse_scores)


def evaluate_model(truth, pred):
    from math import sqrt
    rmse = sqrt(mean_squared_error(truth, pred))
    print(' RMSE: %f' %  rmse)
    return rmse



def splitdata(df,YY_TRAIN, YY_TEST, YY_RECONSTRUCTION):
    # data preparation
    df["date"] = pd.to_datetime(df["date"])
    dat_train = df[(df.date.dt.year >= YY_TRAIN[0]) & (df.date.dt.year <= YY_TRAIN[1])]
    dat_test = df[(df.date.dt.year >= YY_TEST[0]) & (df.date.dt.year <= YY_TEST[1])]
    dat_reconstruction = df[(df.date.dt.year >= YY_RECONSTRUCTION[0]) & (df.date.dt.year <= YY_RECONSTRUCTION[1])]
    
    #Look at some correlations
    #dat_train[['t2mmax','prec','discharge','generation','hp','prec7D','prec15D', 'prec30D', 't2max7D', 't2max15D','spei_1','spei_3']].corr()
    
    dates_train = dat_train['date']
    dates_test = dat_test['date']
    dates_recons = dat_reconstruction['date']
    
    return dat_train, dat_test, dat_reconstruction, dates_train, dates_test, dates_recons



def preparedata_tomodel(dat_train, dat_test, dat_reconstruction, dat_all, colnames, label):
    
    # select columns
    x_train = dat_train[colnames]
    y_train = dat_train[label]
    # test
    x_test = dat_test[colnames]
    y_test = dat_test[label]
    # reconstruction
    x_recons = dat_reconstruction[colnames]
    x_all  = dat_all[colnames]
    
    # remove date
    x_train.drop('date', axis=1, inplace=True)
    x_test.drop('date', axis=1, inplace=True)
    x_recons.drop('date',axis =1, inplace = True)
    x_all.drop('date',axis =1, inplace = True)
    # Pipeline to normalise data

    
    # Normalise the data
    fpipeline = prepareData(x_train)
    X_train_in = fpipeline.fit_transform(x_train)
    X_test_in = fpipeline.transform(x_test)
    X_reconstruction_in = fpipeline.transform(x_recons)
    X_all_in = fpipeline.transform(x_all)
    
    # Check and fill NA if needed
    y_train = y_train.copy()
    train_median = y_train.median()
    y_train.fillna(train_median, inplace=True)
    
    y_test = y_test.copy()
    test_median = y_test.median()
    y_test.fillna(test_median, inplace=True)
    
    return X_train_in, y_train, X_test_in, y_test, X_reconstruction_in, X_all_in


def make_dataset(data, label, window_size=3):
    
    feature_list = []
    label_list = []
  
    for i in range(len(data) - window_size):
        feature_list.append(np.array(data.iloc[i:i+window_size]))
        label_list.append(np.array(label.iloc[i+window_size]))
    return np.array(feature_list), np.array(label_list)

def preparedata_toLSTM(dat_train, dat_test, dat_reconstruction, dat_all, Features, label, W):
    
    train_feature = dat_train[Features]
    train_label = dat_train[label]
    # test
    test_feature = dat_test[Features]
    test_label = dat_test[label]
    # reconstruction
    recons_feature = dat_reconstruction[Features]
    recons_label = dat_reconstruction[label]

    # all
    all_feature = dat_all[Features]
    all_label = dat_all[label]
    
     # Normalise the data returning dataframe
    fpipeline = prepareData(train_feature)
    X_train_in = pd.DataFrame(fpipeline.transform(train_feature), columns=Features)
    X_test_in = pd.DataFrame(fpipeline.transform(test_feature), columns=Features)
    X_reconstruction_in = pd.DataFrame(fpipeline.transform(recons_feature), columns=Features)
    X_all_in = pd.DataFrame(fpipeline.transform(all_feature), columns=Features)
    # Check and fill NA if needed
    y_train = train_label.copy()
    train_median = y_train.median()
    y_train.fillna(train_median, inplace=True)

    y_test = test_label.copy()
    test_median = y_test.median()
    y_test.fillna(test_median, inplace=True)
    
    # train dataset
    train_feature_array, train_label_array = make_dataset(X_train_in, y_train, W)
    test_feature_array, test_label_array = make_dataset(X_test_in, y_test, W)
    # same to do predictions afterwards
    recons_feature_array, recons_label_array = make_dataset(X_reconstruction_in, recons_label, W)
    all_feature_array, all_label_array = make_dataset(X_all_in, all_label, W)

    
    return train_feature_array, train_label_array, test_feature_array, test_label_array, recons_feature_array, all_feature_array




def get_scores(true, pred, scores):

        """function to get different socres
         Args: true , observations
               pred, predictions
        It is adapted from pySTEPS/pysteps/blob/ba2c81195dd36d1bede5370e0c5c1f4420657d6e/pysteps/verification/detcatscores.py
               
        +------------+--------------------------------------------------------+
        | Name       | Description                                            |
        +============+========================================================+
        |  ACC       | accuracy (proportion correct)                          |
        +------------+--------------------------------------------------------+
        |  BIAS      | frequency bias                                         |
        +------------+--------------------------------------------------------+
        |  CSI       | critical success index (threat score)                  |
        +------------+--------------------------------------------------------+
        |  ETS       | equitable threat score                                 |
        +------------+--------------------------------------------------------+
        |  F1        | the harmonic mean of precision and sensitivity         |
        +------------+--------------------------------------------------------+
        |  FA        | false alarm rate (prob. of false detection, fall-out,  |
        |            | false positive rate)                                   |
        +------------+--------------------------------------------------------+
        |  FAR       | false alarm ratio (false discovery rate)               |
        +------------+--------------------------------------------------------+
        |  GSS       | Gilbert skill score (equitable threat score)           |
        +------------+--------------------------------------------------------+
        |  HK        | Hanssen-Kuipers discriminant (Pierce skill score)      |
        +------------+--------------------------------------------------------+
        |  HSS       | Heidke skill score                                     |
        +------------+--------------------------------------------------------+
        |  MCC       | Matthews correlation coefficient                       |
        +------------+--------------------------------------------------------+
        |  POD       | probability of detection (hit rate, sensitivity,       |
        |            | recall, true positive rate)                            |
        +------------+--------------------------------------------------------+
        |  SEDI      | symmetric extremal dependency index                    |
        +------------+--------------------------------------------------------+
        """


        tn, fp, fn, tp = confusion_matrix(true, pred).ravel()

        H = tp  # true positives
        M = fn # false negatives
        F = fp  # false positives
        R = tn # true negatives

        result = {}
        for score in scores:
            # catch None passed as score
            if score is None:
                continue
            score_ = score.lower()

            # simple scores
            POD = H / (H + M)
            FAR = F / (H + F)
            FA = F / (F + R)
            s = (H + M) / (H + M + F + R)

            if score_ in ["pod", ""]:
                # probability of detection
                result["POD"] = POD
            if score_ in ["far", ""]:
                # false alarm ratio
                result["FAR"] = FAR
            if score_ in ["fa", ""]:
                # false alarm rate (prob of false detection)
                result["FA"] = FA
            if score_ in ["acc", ""]:
                # accuracy (fraction correct)
                ACC = (H + R) / (H + M + F + R)
                result["ACC"] = ACC
            if score_ in ["csi", ""]:
                # critical success index
                CSI = H / (H + M + F)
                result["CSI"] = CSI
            if score_ in ["bias", ""]:
                # frequency bias
                B = (H + F) / (H + M)
                result["BIAS"] = B

            # skill scores
            if score_ in ["hss", ""]:
                # Heidke Skill Score (-1 < HSS < 1) < 0 implies no skill
                HSS = 2 * (H * R - F * M) / ((H + M) * (M + R) + (H + F) * (F + R))
                result["HSS"] = HSS
            if score_ in ["hk", ""]:
                # Hanssen-Kuipers Discriminant
                HK = POD - FA
                result["HK"] = HK
            if score_ in ["gss", "ets", ""]:
                # Gilbert Skill Score
                GSS = (POD - FA) / ((1 - s * POD) / (1 - s) + FA * (1 - s) / s)
                if score_ == "ets":
                    result["ETS"] = GSS
                else:
                        result["GSS"] = GSS
            if score_ in ["sedi", ""]:
                # Symmetric extremal dependence index
                SEDI = (np.log(FA) - np.log(POD) + np.log(1 - POD) - np.log(1 - FA)) / (
                    np.log(FA) + np.log(POD) + np.log(1 - POD) + np.log(1 - FA)
                )
                result["SEDI"] = SEDI
            if score_ in ["mcc", ""]:
                # Matthews correlation coefficient
                MCC = (H * R - F * M) / np.sqrt((H + F) * (H + M) * (R + F) * (R + M))
                result["MCC"] = MCC
            if score_ in ["f1", ""]:
                # F1 score
                F1 = 2 * H / (2 * H + F + M)
                result["F1"] = F1

        return result