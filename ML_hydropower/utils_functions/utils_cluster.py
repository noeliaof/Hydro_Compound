#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import os
import glob
import seaborn
import numpy as np
import pandas as pd
import xarray as xr
from datetime import datetime, timedelta


from eofs.xarray import Eof
from eofs.examples import example_data_path
from eofs.xarray import Eof as xEof
# To make this notebook's output stable across runs
np.random.seed(42)

# Config matplotlib
get_ipython().run_line_magic('matplotlib', 'inline')
import cartopy.crs as ccrs
import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
from utils_data import *

from sklearn.cluster import KMeans


def pc_eof(dat,mvar,N):
    """Create an EOF solver to do the EOF analysis. Square-root of cosine of
    latitude weights are applied before the computation of EOFs""" 
    
    coslat = np.cos(np.deg2rad(dat.coords['lat'].values)).clip(0., 1.)
    wgts = np.sqrt(coslat)[..., np.newaxis]
    #dat_stacked = dat[mvar].stack(latlon=('lat', 'lon'))
    solver = Eof(dat[mvar], weights=wgts)
    # Retrieve the 14 first leading PCs
    pcs = solver.pcs(npcs=14, pcscaling=1)
    # Get the variance fraction accounted for each EOF
    variances = solver.varianceFraction()
    s_evfs = solver.varianceFraction(neigs=14)
    s_evfs.sum()
    eofs = solver.eofs(neofs=N)
    pc1  = solver.pcs(npcs=N, pcscaling=0)
    PCs_n = pcs[:, :N]
    PCs_n = np.array(PCs_n)
    # Data frame format for the selected components
    PCdf = pd.DataFrame(PCs_n, index=dat['time'],
                        columns=["PC%s" % (x) for x in range(1, PCs_n.shape[1] + 1)])
    
    eof1 = solver.eofsAsCovariance(neofs=1)
    
    return(eofs, eof1, PCdf)


def KCluster(cldat, data, N):
    """Perform K-cluster analysis using the PCds obtained before"""
    kmeans = KMeans(init='k-means++', n_clusters=N, n_init=10)
    kmeans.fit(cldat.values)
    y_pred = kmeans.fit_predict(cldat.values)

    # Each day belongs to a cluster, labelled by kmeands.labels_
    np.unique(kmeans.labels_)
    labels = pd.DataFrame(kmeans.labels_, index=np.array(data['time']), columns=['cluster'])
    # Now, we can look at the number of days belong to cluster 0
    #index = labels.query('cluster == {}'.format(0))
    #len(index)
    return(labels)

def getclus(labels, data, N):
    """Process the cluster data"""
    num_tot = len(labels.cluster)
    clusters = []
    day_clusters = []
    nbdays = []

    for i_clus in range(N):
        index = labels.query('cluster == {}'.format(i_clus))
        freq = (len(index)/num_tot)*100
        freq = round(freq, 2)
        nbdays.append(freq)
        cluster = data.sel(time=index.index).mean('time')
        d_cluster = data.sel(time=index.index)
        clusters.append(cluster)
        day_clusters.append(d_cluster)

    clusters = xr.concat(clusters, dim='cluster')
    clusters.assign_coords(cluster=nbdays)
    
    
    return(day_clusters, clusters, nbdays)


def prepare_cluster_data(day_clusters, mvar):
    """Prepare the date based on the Kmeans results"""
    t_list = []
    for i_clus in range(len(day_clusters)):
        tmp = day_clusters[i_clus].mean(dim=['lon', 'lat'])
        tmp_df = pd.DataFrame({'date': tmp['time'], mvar: tmp[mvar], 'Cluster': i_clus})
        t_list.append(tmp_df)

    # Merge by date
    df = pd.concat(t_list)
    df = df.sort_values(by="date")
    return(df)



def KCluster_v2(data, var, N):
    """Perform K-cluster analysis using geopotential 500"""
    dat_stacked = data.stack(latlon=('lat', 'lon'))
    dat_stacked.load()
    X = dat_stacked[var]
    kmeans = KMeans(init='k-means++', n_clusters=N, n_init=10)
    kmeans.fit(X)
    y_pred = kmeans.fit_predict(X)

    # Each day belongs to a cluster, labelled by kmeands.labels_
    np.unique(kmeans.labels_)
    labels = pd.DataFrame(kmeans.labels_, index=np.array(data['time']), columns=['cluster'])
    # Now, we can look at the number of days belong to cluster 0
    #index = labels.query('cluster == {}'.format(0))
    #len(index)
    return(labels)

def getclus_v2(labels, data, N):
    """Updated version of getclus. 
       This function just gets the clusters and the frequency of each cluster
       Also the mean of each cluster is calculated"""
    num_tot = len(labels.cluster)
    clusters = []
    day_clusters = []
    nbdays = []
    n_clusters = []
    for i_clus in range(N):
        index = labels.query('cluster == {}'.format(i_clus))
        freq = (len(index)/num_tot)*100
        freq = round(freq, 2)
        nbdays.append(freq)
        cluster = data.sel(time=index.index).mean('time')
        d_cluster = data.sel(time=index.index)
        clusters.append(cluster)
        day_clusters.append(d_cluster)
        
    clusters = xr.concat(clusters, dim='cluster')
    clusters.assign_coords(cluster=nbdays)
    
    
    return(day_clusters, clusters, nbdays)


def getclus_cat(labels, d_copy):
    """Based on the Kmeans results, this function replace the geopotential or your var
        into the categories of the clusters"""
    
    lat = d_copy.lat
    lon = d_copy.lon
    dims = d_copy.z.shape
    new_data = np.zeros(shape=[dims[0],dims[1],dims[2]])
    for ila in range(len(lat)):
        for ilo in range(len(lon)):
            new_data[:,ila,ilo] = d_copy.z[:,ila,ilo].copy(data=labels.cluster)
            
    return(new_data)



def visCluster(myclusters,nfreq, N):
    """Visualize the clusters obtained from Kmeans"""
    fig, axs = plt.subplots(nrows=2,ncols=4,
                            subplot_kw={'projection': ccrs.PlateCarree()},
                            figsize=(20,8))

    axs=axs.flatten()

    for i in range(0,N):
        cs=axs[i].contourf(myclusters.lon,myclusters.lat,myclusters['z'][i,:,:],cmap=plt.cm.RdBu_r,
                              transform = ccrs.PlateCarree(),extend='both')
        axs[i].set_title("%.2f" %nfreq[i])
        axs[i].coastlines('50m',edgecolor='black', linewidth=0.75)

    fig.subplots_adjust(bottom=0.2, top=0.9, left=0.1, right=0.9,
                        wspace=0.02, hspace=0.02)
    # Add a colorbar axis at the bottom of the graph
    cbar_ax = fig.add_axes([0.2, 0.2, 0.6, 0.02])

    # Draw the colorbar
    cbar=fig.colorbar(cs, cax=cbar_ax,orientation='horizontal')
    #cbar.set_ticks(np.arange(-200, 200, 20))
    plt.show()