################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 oct 2023                            #
################################################

# The purpose of this module is to provide methods for fitting, saving and
# loading gas consumption and electricity co-generation models for heat generators

import os
import pickle
import numpy as np
import pandas as pd
import urllib.parse
from pathlib import Path

import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression

from py4jps import agentlogging

from dhoptimisation.datamodel.iris import *
from dhoptimisation.kgutils.kgclient import KGClient
from dhoptimisation.kgutils.tsclient import TSClient


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


# Specify relative file paths to store/load model files
CONSUMPTION_MODEL_REPO = '/app/dhoptimisation/resources/generator_models/consumption'
COGEN_MODEL_REPO =       '/app/dhoptimisation/resources/generator_models/cogeneration'
MODEL_PLOTS_REPO =       '/app/dhoptimisation/resources/generator_models/plots'


def get_generator_models(kg_client:KGClient, ts_client:TSClient, tmax:str,
                         modelpath=CONSUMPTION_MODEL_REPO):
    """
    
    
    Arguments:
        kg_client {KGClient} -- pre-initialised SPARQL client
        ts_client {TSClient} -- pre-initialised TimeSeries client
        tmax {str} -- datetime timestep until when to retrieve data for model
                      fitting, i.e., only retrieve historical data before 
                      optimisation time interval to fit models
    Returns:
        gas_consumption_models {dict} -- gas consumption as function of generated heat
        electricity_cogen_models {dict} -- electricity co-generation as function 
                                           of generated heat
        Both dictionaries have generator IRIs as keys and model objects as values
    """

    # Initialise return dicts
    gas_consumption_models = {}
    electricity_cogen_models = {}
    
    # Retrieve IRIs of gas boilers and gas turbine
    generators = kg_client.get_heat_providers()
    # Create/retrieve models for gas boilers
    for iri in generators['boilers']:
        # Create encoded IRI string (to use as file names)
        iri_encoded = urllib.parse.quote(iri, safe='')
        # Only create new model if not already saved
        fp = os.path.join(modelpath, iri_encoded +'.sav')
        if os.path.exists(fp):
            logger.info(f'Gas consumption model for generator "{iri}" already exists.')
            pass
        else:
            # Fit and save new model
            logger.info(f'Create new gas consumption model for generator "{iri}".')
            # Retrieve historical gas consumption and heat generation
            res = kg_client.get_heatgenerator_output_iris(iri)
            dataIRI, _ = kg_client.get_associated_dataIRI(res[OHN_GENERATED_HEAT_AMOUNT], 
                                unit=OM_MEGAWATTHOUR, forecast=False)
            df1 = ts_client.retrieve_timeseries_as_dataframe(dataIRI, 'heat_generation',
                                                             upperbound=tmax)
            dataIRI, _ = kg_client.get_associated_dataIRI(res[OHN_CONSUMED_GAS_AMOUNT], 
                                unit=OM_MEGAWATTHOUR, forecast=False)
            df2 = ts_client.retrieve_timeseries_as_dataframe(dataIRI, 'gas_consumption',
                                                             upperbound=tmax)
            # Join DataFrames (on time index); ensure both data available for each timestep
            df = df1.join(df2)
            df.dropna(inplace=True)
            # Create new model
            create_boiler_consumption_model(data=df, name=iri_encoded)
        # Load existing model
        gas_consumption_models[iri] = pickle.load(open(fp, 'rb'))
        
    
    return gas_consumption_models, electricity_cogen_models
        
        
def create_boiler_consumption_model(data:pd.DataFrame, name:str, efficiency=0.9,
                                    modelpath=CONSUMPTION_MODEL_REPO,
                                    plotpath=MODEL_PLOTS_REPO):
    """
    Creates and fits linear gas consumption model to historic heat boiler data 
    (i.e., gas consumption = f (heat generation))
    Plots and saves/serializes the fitted model

    :param pd.DataFrame data: DataFrame containing historic data [heat_generation, gas_consumption]
    :param str name: file name for heat generator, i.e., encoded IRI string
    :param float efficiency: efficiency (Nutzungsgrad) of heat boiler
    :param str modelpath/plotpath: paths to output directories
    """

    # Derive most representative subset of historical data to exclude outliers from linear regression
    # exclude historical data with gas consumption > consumption according to efficiency + 2 standard deviations
    data = data[~(data.iloc[:, 1] > (data.iloc[:, 0] / efficiency) + 2 * data.iloc[:, 1].std())]
    # exclude historical data with gas consumption < consumption according to efficiency - 2 standard deviations
    data = data[~(data.iloc[:, 1] < (data.iloc[:, 0] / efficiency) - 2 * data.iloc[:, 1].std())]

    # Fit linear regression model: gas consumption = f( heat generation )
    X = data.iloc[:, 0].values
    X = np.array(X).reshape(-1, 1)              # reshape as required by sklearn (vertical vector)
    y = np.array(data.iloc[:, 1].values)
    model = LinearRegression().fit(X, y)

    # Save the fitted model by serializing with Pickle
    f = os.path.join(modelpath, name +'.sav')
    pickle.dump(model, open(f, 'wb'))

    # Plot gas consumption vs. heat generation
    plt.rcParams['font.size'] = 14
    plt.figure(figsize=(16, 9))
    plt.scatter(data.iloc[:, 0], data.iloc[:, 1], alpha=0.5, marker='.', s=100, color='dimgrey')
    # Add simple efficiency model: gas consumption = heat generation / efficiency
    plt.plot(data.iloc[:, 0], data.iloc[:, 0] / efficiency, color='blue')
    # Add linear regression model
    plt.plot(X, model.predict(X), color='green')
    plt.grid('both')
    plt.legend(['Efficiency model', 'Linear regression model', 'Fitting data'])
    plt.xlabel('Heat generation (MWh)')
    plt.ylabel('Gas consumption (MWh)')
    plt.title(urllib.parse.unquote(name))
    plt.savefig(os.path.join(plotpath, name +'.png'))
