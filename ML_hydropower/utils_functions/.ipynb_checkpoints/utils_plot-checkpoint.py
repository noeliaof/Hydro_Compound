import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
import pandas as pd
import os
from tqdm import tqdm
from numpy import nan
import matplotlib.ticker as ticker
from utils_functions.utils_data import *

def save_fig(fig_id, IMAGES_PATH, tight_layout=True, fig_extension="png", resolution=300):
    path = os.path.join(IMAGES_PATH, fig_id + "." + fig_extension)
    print("Saving figure", fig_id)
    if tight_layout:
        plt.tight_layout()
    plt.savefig(path, format=fig_extension, dpi=resolution)


def plot_prediction_scatter(y_test, y_pred):
    n_cols = 3
    n_plots = y_pred.shape[1]
    nrows = -(-n_plots // n_cols)

    fig, axs = plt.subplots(nrows=nrows, ncols=n_cols,
                            sharex=False, figsize=(17, 11))
    for i in range(n_plots):
        ax = plt.subplot(nrows, n_cols, i + 1)
        x = y_test.iloc[:, i]
        y = y_pred[:, i]
        ax.scatter(x, y, facecolors='none', edgecolors='k', alpha=0.5)
        v_max = max(x.max(), y.max())
        ax.plot([0, v_max], [0, v_max], 'r--')
        ax.set_xlabel('Observed prec. [mm]')
        ax.set_ylabel('Predicted prec. [mm]')
        ax.set_title(y_test.iloc[:, i].name)
        ax.set_xlim([x.min(), 1.05 * v_max])
        ax.set_ylim([y.min(), 1.05 * v_max])


def plot_prediction_ts(test_dates, final_predictions, test_labels):
    df_to_compare = pd.DataFrame(
        {'date': test_dates, 'Actual': test_labels, 'Predicted': final_predictions})
    dfm = pd.melt(df_to_compare, id_vars=['date'], value_vars=[
                  'Actual', 'Predicted'], var_name='data', value_name='precip')
    f, axs = plt.subplots(1, 2, figsize=(12, 5), sharey=True)

    sns.regplot(data=df_to_compare, x="Actual", y="Predicted", ax=axs[0], )
    sns.lineplot(x='date', y='precip', hue='data', data=dfm, ax=axs[1])


def plot_importance(features_importance, attributes, IMAGES_PATH):
    indices = np.argsort(features_importance)
    plt.barh(range(len(attributes)),
             features_importance[indices], color='b', align='center')
    plt.yticks(range(len(indices)), [attributes[i] for i in indices])
    plt.xlabel('Relative Importance')
    save_fig("Rela_Importance", IMAGES_PATH)
    plt.show()


def save_fig(fig_id, IMAGES_PATH, tight_layout=True, fig_extension="png", resolution=300):
    path = os.path.join(IMAGES_PATH, fig_id + "." + fig_extension)
    print("Saving figure", fig_id)
    if tight_layout:
        plt.tight_layout()
    plt.savefig(path, format=fig_extension, dpi=resolution)


def plot_hist(history):
    
    # plot the train and validation losses
    N = np.arange(len(history.history['loss']))
    plt.figure()
    plt.plot(N, history.history['loss'], label='train_loss')
    plt.plot(N, history.history['val_loss'], label='val_loss')
    plt.title('Training Loss and Accuracy')
    plt.xlabel('Epochs')
    plt.ylabel('Loss/Accuracy')
    plt.legend(loc='upper right')
    
    plt.show()
    
    
def plot_moutput(dates, truth, predictions, fig_id, IMAGES_PATH):
    import seaborn as sns
    df_to_compare = pd.DataFrame({'date': dates, 'Actual': truth, 'Predicted': predictions.flatten()})
    dfm = pd.melt(df_to_compare, id_vars=['date'], value_vars=['Actual', 'Predicted'], var_name='data', value_name='hp')
    f, axs = plt.subplots(1,2, figsize=(12,5), sharey=True)   
    sns.regplot(data= df_to_compare,
                    x="Actual",
                    y="Predicted",
                    ax=axs[0],
                    )
    sns.lineplot(x='date', y='hp', hue = 'data', palette =['b','g'], data=dfm, ax=axs[1])
    save_fig(fig_id, IMAGES_PATH, tight_layout=True, fig_extension="png", resolution=100)
    
    

    
    
    
def show_features(model, type_model, features, fig_id, IMAGES_PATH):
     # Features importance
    if type_model == 'linear':
        
        coefs = pd.DataFrame(
        model.coef_,
        columns=['Coefficients'], index=features)

        coefs.plot(kind='barh', figsize=(9, 7))
        plt.title('LR')
        plt.axvline(x=0, color='.5')
        plt.subplots_adjust(left=.3)
        
    elif type_model == 'randomforest':
        
        features_importance = model.feature_importances_
        forest_importances = pd.Series(features_importance, index=features)
        fig, ax = plt.subplots()
        forest_importances.plot.bar()
        ax.set_title("Feature importances")
        #ax.set_ylabel("")
        fig.tight_layout()
          
    save_fig(fig_id, IMAGES_PATH, tight_layout=True, fig_extension="png", resolution=100)
    
    
def LSTM_imp(mod, X_test, y_test, features, name_s, name_mod, fig_id, IMAGES_PATH):
    """estimate the features importances of the models
    https://www.kaggle.com/cdeotte/lstm-feature-importance"""
    
    results = []
    print(' Computing LSTM feature importance...')

    # COMPUTE BASELINE (NO SHUFFLE)
    oof_preds = mod.predict(X_test, verbose=0).squeeze() 
    baseline_mae = np.mean(np.abs( oof_preds-y_test ))
    results.append({'feature':'BASELINE','mae':baseline_mae})           

    for k in tqdm(range(len(features))):
        # SHUFFLE FEATURE K
        save_col = X_test[:,:,k].copy()
        np.random.shuffle(X_test[:,:,k])

        # COMPUTE OOF MAE WITH FEATURE K SHUFFLED
        oof_preds = mod.predict(X_test, verbose=0).squeeze() 
        mae = np.mean(np.abs( oof_preds-y_test ))
        results.append({'feature':features[k],'mae':mae})
        X_test[:,:,k] = save_col


    print()
    df = pd.DataFrame(results)
    df = df.sort_values('mae')
    plt.figure(figsize=(10,20))
    plt.barh(np.arange(len(features)+1),df.mae)
    plt.yticks(np.arange(len(features)+1),df.feature.values)
    plt.title(name_mod +'-'+'Feature Importance'+'-'+ name_s ,size=16)
    plt.ylim((-1,len(features)+1))
    plt.plot([baseline_mae,baseline_mae],[-1,len(features)+1], '--', color='orange',
                         label=f'Baseline OOF\nMAE={baseline_mae:.3f}')

    plt.ylabel('Feature',size=14)
    plt.legend()
    #plt.show()
    
    save_fig(fig_id, IMAGES_PATH, tight_layout=True, fig_extension="png", resolution=100)
    


def recons(model, X_all_in, df_i):
    """Plot reconstructed data
       Args: model
             X_reconstruction: reconstructed data set
             dates_recons: from the data to be reconstructed
             dates_train: from the train data"""
    pred_recons = model.predict(X_all_in)
    df_n=df_i[['date','generation']]
    df_n['pred'] = pred_recons.flatten()
    
   
    return(df_n)


def plot_recons(dfrecons, fig_id, IMAGES_PATH):
    
    dfrecons.columns = ['date','generation','pred']
    dfm = pd.melt(dfrecons, id_vars=['date'], value_vars=['generation', 'pred'], var_name='data', value_name='hp')
    plt.figure()
    sns.lineplot(x='date', y='hp', hue = 'data', palette =['b','g'], data=dfm)
    save_fig(fig_id, IMAGES_PATH, tight_layout=True, fig_extension="png", resolution=100)
    
    
    
    
def create_monthlydata(list_data, IMAGES_PATH,th = 0.2, mon_input=True):

    all_data = []
    names_station = []
    for istat in range(0, len(list_data)):
        
        df_case = list_data[istat]
        
        name_s = df_case.name_p.unique()

        name_s = [x for x in name_s if x is not nan]
        name_s = ''.join(name_s)
        # agreegated on monthly-basis
        
        if mon_input == False :
            df_mon_case = df_case[['date','pred_randomforest','discharge','dis7D','dis15D','dis30D','spei_1',
                                   'spei_3','spei_6','t2mmax','t2max7D','t2max15D','t2max30D','STI_1', 'STI_2', 'STI_3','ssi_1', 'ssi_3', 'ssi_6']]
            df_mon_case['month'] = pd.to_datetime(df_mon_case['date']).dt.month
            df_mon_case['year'] = pd.to_datetime(df_mon_case['date']).dt.year
            df_mon_case = df_mon_case.groupby(['year','month'],as_index=False).mean()
            # add also the extremes
        else:
            df_mon_case = df_case
          
            
        df_ex =  extremes(df_mon_case,'pred',th,'low')
        
        # plot 
        fig, axes = plt.subplots(2, 3, figsize=(18, 10))

        fig.suptitle('Monthly patterns' + name_s)

        sns.boxplot(ax=axes[0, 0], data=df_mon_case, x='mon', y='pred')
        sns.boxplot(ax=axes[0, 1], data=df_mon_case, x='mon', y='spei_1')
        sns.boxplot(ax=axes[0, 2], data=df_mon_case, x='mon', y='spei_3')
        sns.boxplot(ax=axes[1, 0], data=df_mon_case, x='mon', y='STI_1')
        sns.boxplot(ax=axes[1, 1], data=df_mon_case, x='mon', y='STI_2')
        sns.boxplot(ax=axes[1, 2], data=df_mon_case, x='mon', y='discharge')
        sns.boxplot(ax=axes[1, 2], data=df_mon_case, x='mon', y='dis30D')
        
        fig_id = 'Monthly_patterns' + name_s
        save_fig(fig_id, IMAGES_PATH, tight_layout=True, fig_extension="png", resolution=100)
        
        
        fig, axes = plt.subplots(2, 3, figsize=(18, 10))
        # --------------------------------------------------
        fig.suptitle('Yearly patterns' + name_s)


        sns.boxplot(ax=axes[0, 0], data=df_mon_case, x='year', y='pred')
        axes[0,0].xaxis.set_major_locator(ticker.MultipleLocator(5))
        sns.boxplot(ax=axes[0, 1], data=df_mon_case, x='year', y='spei_1')
        axes[0,1].xaxis.set_major_locator(ticker.MultipleLocator(5))
        sns.boxplot(ax=axes[0, 2], data=df_mon_case, x='year', y='spei_3')
        axes[0,2].xaxis.set_major_locator(ticker.MultipleLocator(5))
        sns.boxplot(ax=axes[1, 0], data=df_mon_case, x='year', y='STI_1')
        axes[1,0].xaxis.set_major_locator(ticker.MultipleLocator(5))
        sns.boxplot(ax=axes[1, 1], data=df_mon_case, x='year', y='STI_2')
        axes[1,1].xaxis.set_major_locator(ticker.MultipleLocator(5))
        sns.boxplot(ax=axes[1, 2], data=df_mon_case, x='year', y='discharge')
        axes[1,2].xaxis.set_major_locator(ticker.MultipleLocator(5))
        sns.boxplot(ax=axes[1, 2], data=df_mon_case, x='year', y='dis30D')
        axes[1,2].xaxis.set_major_locator(ticker.MultipleLocator(5))
        
        fig_yy = 'Yearly_patterns' + name_s
        save_fig(fig_yy, IMAGES_PATH, tight_layout=True, fig_extension="png", resolution=100)
        
        
        all_data.append(df_ex)
        names_station.append(name_s)
        
    return all_data,names_station