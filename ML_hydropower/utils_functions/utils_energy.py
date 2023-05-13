# %%
#from math   import sqrt
#from pandas import DataFrame as df
import pandas as pd
import geopandas as gpd
import numpy as np
import regionmask

def energy_top_events(ss, mvar, window=30):
    
    nr = len(ss)
    #mvar = 'WD'
    windown = 30
    max_values_per_dt = pd.DataFrame({}) # final data frame where to store maxs and times
    for h in np.arange(0, nr, windown):
        #print(h)
        df_sliced = ss[['date',mvar]][h:h+windown] # 
        tmp_max = pd.DataFrame({'date':[df_sliced['date'][df_sliced[mvar].idxmax()]], mvar:[df_sliced[mvar].max()]})
        max_values_per_dt = max_values_per_dt.append(tmp_max) # append the maxs and the times into your final dataframe

    max_values_per_dt=max_values_per_dt.sort_values(by=[mvar], ascending=False)
    max_values_per_dt['country']=ss['country'].unique()[0]
    # now return only 20-top events
    max_values_per_dt = max_values_per_dt[0:19]
    return(max_values_per_dt)


def energy_analysis(ss, mvar):
    
    # get the maxima production
    # mvar = 'WD'
    new_ss = ss.copy()
    new_ss.loc[:,'year']=pd.DatetimeIndex(new_ss.loc[:,'date'].values).year
    new_ss.loc[:,'month'] = pd.DatetimeIndex(new_ss.loc[:,'date'].values).month
    #if (new_ss[mvar].isnull().all()==False):
    if mvar == 'WD' or mvar == 'res_load_WS' or mvar == 'res_load_tot':
        idx = new_ss.groupby(['year'])[mvar].transform(max) == new_ss[mvar]
        out = new_ss[idx]
    else:
        idx = new_ss.groupby(['year'])[mvar].transform(min) == new_ss[mvar]
        out = new_ss[idx]  
    #else:
    #    print("there is no data")
    #    out = 'NaN'
        
    out['country']=new_ss['country'].unique()[0]
    return(out)
    
def country_events(df_ener, mvar, infoevent):
    
    n_country=df_ener['country'].unique()
    len(n_country)
    out = []
    for icountry in range(len(n_country)):
        #print(n_country[icountry])
        ss = df_ener[df_ener['country'] == n_country[icountry]]
        if (ss[mvar].isnull().all()==False):
            if ( infoevent == 'top' ):
                tmp_out = energy_top_events(ss, mvar, 30)
            else:
                tmp_out = energy_analysis(ss, mvar)
        
            out.append(tmp_out)
        
    return(out)

def calc_percentile(tave,nyears,method='NF13',nwindow=15):
  
    if method == 'NF13':
        pct_calc=np.ones(tave.shape[1:],np.float64)*missingval
        for i in range(tave.shape[1]):
            for j in range(tave.shape[2]):
                aux=tave[:,i,j]
                if len(aux[~aux.mask].data)!=0:
                    pct_calc[i,j]=np.percentile(aux[~aux.mask].data,95,axis=0)

    elif method == 'PA13':
        print(method)
        windowrange=np.zeros((365,),dtype=np.bool)
        windowrange[:np.int(np.ceil(nwindow/2))]=True
        windowrange[-np.int(np.floor(nwindow/2)):]=True
        if (np.sum(windowrange)!=nwindow):
            raise SystemExit(0)
            
        windowrange=np.tile(windowrange,nyears)
        pct_calc=np.ones((365,)+tave.shape[1:],np.float64)*const.missingval

        if not isinstance(tave,np.ma.core.MaskedArray):
            for d in range(365):
                pct_calc[d,:,:]=np.percentile(tave[windowrange==True,:,:],90,axis=0)
                windowrange=np.roll(windowrange,1)
        else:
            for i in range(tave.shape[1]):
                for j in range(tave.shape[2]):
                    for d in range(365):
                        aux=tave[windowrange==True,:,:]
                        if len(aux[~aux.mask].data)!=0:
                            pct_calc[d,:,:]=np.percentile(aux[~aux.mask].data,90,axis=0)
                            windowrange=np.roll(windowrange,1)
                
    else:
        raise ValueError("Method not supported: Choose between NF13 or PA13")
     
                
    return pct_calc


def extract_nuts_TS(nc,mypath, sh_file):
    import regionmask
    # Important: pay attention to incresing (or decreasin order of latitude)
    # read file
    nuts = gpd.read_file(mypath+sh_file)
    nn = len(nuts.NUTS_ID)
    # Important: pay attention to incresing (or decreasin order of latitude)
    nuts_mask_poly = regionmask.Regions(name = 'nuts_mask', numbers = list(range(0,nn)), names = list(nuts.NUTS_ID), abbrevs = list(nuts.NUTS_ID), outlines =                list(nuts.geometry.values[i] for i in range(0,nn)))

    mask = nuts_mask_poly.mask(nc.isel(time = 0).sel(lat = slice(32,75), lon  = slice(-30, 50)), lat_name='lat', lon_name='lon')
    lat = mask.lat.values
    lon = mask.lon.values
    var_country = list()
    meanvar_country = list()
    nam = list()
    for ID_REGION in range(0,37):
        if (ID_REGION!=28 and ID_REGION!=35):
            #print(ID_REGION)
            sel_mask = mask.where(mask == ID_REGION).values
            id_lon = lon[np.where(~np.all(np.isnan(sel_mask), axis=0))]
            id_lat = lat[np.where(~np.all(np.isnan(sel_mask), axis=1))]
            if (len(id_lat)>0):
                out_sel = nc.sel(lat = slice(id_lat[0], id_lat[-1]), lon = slice(id_lon[0], id_lon[-1])).compute().where(mask == ID_REGION)
                var_country.append(out_sel)
                tmp_nam = nuts.NUTS_ID[ID_REGION]
                nam.append(tmp_nam)
                # For doing country average
                x = out_sel.groupby('time').mean(...)
                meanvar_country.append(x)
                
    return var_country, nam