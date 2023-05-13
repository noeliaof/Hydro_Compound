import keras
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers import Dropout, BatchNormalization, Bidirectional, Flatten, Activation
from keras.callbacks import EarlyStopping


class HPmodels():
    """
      args: inputs shape and type of model
    """
    
    def __init__(self, input_shape, model):
        
        self.input_shape = input_shape
        
        if model == "LSTM_v1":
            self.model = self.LSTM_v1()
        elif model == "LSTM_v2":
            self.model = self.LSTM_v2()
        elif model == "LSTM_v3":
            self.model = self.LSTM_v2()
        elif model == "ann1":
            self.model = self.ann1()
        else:
            raise('The model is not defined')
            self.model = self.LSTM_v1()
        
    def LSTM_v1(self):
        #create model
        model = Sequential()
        model.add(LSTM(8, input_shape=self.input_shape, activation = 'relu',return_sequences=False))
        model.add(Dropout(0.4))
        model.add(Dense(1,activation = 'relu'))
      
        return model

    def LSTM_v2(self):
        #create model
        model = Sequential()
        model.add(LSTM(units=16, input_shape=self.input_shape,activation = 'relu',return_sequences=True)) 
        #model.add(Dropout(0.4))
        model.add(LSTM(units=8,return_sequences=False))
        model.add(Dropout(0.4))
        model.add(Dense(1,activation = 'relu'))
    
        return model
        
    def LSTM_v3(self):
        #create model
        model = Sequential()
        model.add(LSTM(32, input_shape=inputs, activation = 'relu', return_sequences=True))
        model.add(Dropout(0.2))
        model.add(LSTM(16))
        model.add(Dense(1, activation = 'relu'))
        
        return model
    
    
    def LSTM_bi(self):
        model = Sequential()
        model.add(Bidirectional(LSTM(units=16, return_sequences=True, activation='relu', input_shape=self.input_shape)))
        model.add(Dropout(0.2))
        model.add(Bidirectional(LSTM(units=8, return_sequences=True, activation='relu')))
        model.add(BatchNormalization())
        model.add(Dropout(0.2))
        model.add(LSTM(units=1, return_sequences=False))
        model.add(Dense(1, activation = 'linear'))
        
        return model
    
    def ann1(self):
        
        model = Sequential()
        model.add(Dense(64, input_shape=self.input_shape, activation='relu'))
        model.add(Dense(32))
        model.add(Activation('relu'))
        #model.add(Dropout(0.2))
        model.add(Dense(16, activation='relu'))
        model.add(Dense(1, activation='relu'))
        
        return model