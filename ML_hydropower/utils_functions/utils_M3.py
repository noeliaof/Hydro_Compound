from tensorflow import keras
import xarray as xr
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap

class DataGenerator(keras.utils.Sequence):
    def __init__(self, ds, var_dict, batch_size=32, shuffle=True, load=True, mean=None, std=None):
    #def __init__(self, ds, var_dict, lead_time, batch_size=32, shuffle=True, load=True, mean=None, std=None):
        """
        Data generator for WeatherBench data.
        Template from https://stanford.edu/~shervine/blog/keras-how-to-generate-data-on-the-fly
        Args:
            ds: Dataset containing all variables
            var_dict: Dictionary of the form {'var': level}. Use None for level if data is of single level
            lead_time: Lead time in hours # I am skipping it for now
            batch_size: Batch size
            shuffle: bool. If True, data is shuffled.
            load: bool. If True, datadet is loaded into RAM.
            mean: If None, compute mean from data.
            std: If None, compute standard deviation from data.
        """
        self.ds = ds
        self.var_dict = var_dict
        self.batch_size = batch_size
        self.shuffle = shuffle
        #self.lead_time = lead_time

        data = []
        generic_level = xr.DataArray([1], coords={'level': [1]}, dims=['level'])
        for var, levels in var_dict.items():
            #try:
            #    data.append(ds[var].sel(level=levels))
            #except ValueError:
                data.append(ds[var].expand_dims({'level': generic_level}, 1))

        # I removed self.data.mean(('time', 'lat', 'lon')).compute()
        self.data = xr.concat(data, 'level').transpose('time', 'lat', 'lon', 'level')
        self.mean = self.data.mean(('time', 'lat', 'lon')) if mean is None else mean
        self.std = self.data.std('time').mean(('lat', 'lon')) if std is None else std
        # Normalize
        self.data = (self.data - self.mean) / self.std
        #self.n_samples = self.data.isel(time=slice(0, -lead_time)).shape[0]
        #self.init_time = self.data.isel(time=slice(None, -lead_time)).time
        #self.valid_time = self.data.isel(time=slice(lead_time, None)).time

        #self.on_epoch_end()
        # For some weird reason calling .load() earlier messes up the mean and std computations
        if load: print('Loading data into RAM'); self.data.load()

            
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
    
    
    
    
def visReconstruction(ds_newtest):

    # Get a handle on the figure and the axes
    fig, axs = plt.subplots(nrows=1,ncols=2,
                            subplot_kw={'projection': ccrs.PlateCarree()},
                            figsize=(12,8))

    # axs is a 2 dimensional array of `GeoAxes`.  We will flatten it into a 1-D array
    axs=axs.flatten()

    cax = ds_newtest.isel(time=0)[lp[0]].plot(ax=axs[0], transform=ccrs.PlateCarree(),cmap=plt.cm.RdBu_r,add_colorbar=False)
    axs[0].coastlines('50m',edgecolor='black', linewidth=0.75)

    cax1 = ds_newtest.isel(time=0)[lp[1]].plot(ax=axs[1], transform=ccrs.PlateCarree(),cmap=plt.cm.RdBu_r,add_colorbar=False)
    axs[1].coastlines('50m',edgecolor='black', linewidth=0.75)
    

    
def vae_loss(true, pred):
    # this might not work for non-binary dataset
    # Reconstruction loss 
    reconstruction_loss = binary_crossentropy(K.flatten(true), K.flatten(pred)) * image_width * image_height
    # KL divergence loss
    kl_loss = 1 + sigma - K.square(mu) - K.exp(sigma)
    kl_loss = K.sum(kl_loss, axis=-1)
    kl_loss *= -0.5
    # Total loss = 50% rec + 50% KL divergence loss
    return K.mean(reconstruction_loss + kl_loss)


    
def sample_z(args):
    """Reparameterization trick by sampling from an isotropic unit Gaussian.

    # Arguments
        args (tensor): mean and log of variance of Q(z|X)

    # Returns
        z (tensor): sampled latent vector
    """
    mu, sigma = args
    batch     = K.shape(mu)[0]
    dim       = K.int_shape(mu)[1]
    # by default, random_normal has mean = 0 and std = 1.0
    eps       = K.random_normal(shape=(batch, dim))
    return mu + K.exp(sigma / 2) * eps