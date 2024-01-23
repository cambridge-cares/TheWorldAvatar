################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2023                            #
################################################

# The purpose of this module is to provide methods for fitting, saving and
# loading gas consumption and electricity co-generation models for heat generators

import os
import pickle
import numpy as np
import pandas as pd
import urllib.parse

import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression

from dhoptimisation.utils import *
from dhoptimisation.datamodel.iris import *
from dhoptimisation.kgutils.kgclient import KGClient
from dhoptimisation.kgutils.tsclient import TSClient


# Specify relative file paths to store/load model files
CONSUMPTION_MODEL_REPO = '/app/dhoptimisation/resources/generator_models/consumption'
COGEN_MODEL_REPO =       '/app/dhoptimisation/resources/generator_models/cogeneration'
MODEL_PLOTS_REPO =       '/app/dhoptimisation/resources/generator_models/plots'


def get_generator_models(kg_client:KGClient, ts_client:TSClient, tmax:str,
                         gas_modelpath=CONSUMPTION_MODEL_REPO,
                         cogen_modelpath=COGEN_MODEL_REPO):
    """
    Obtain gas consumption and electricity co-generation models for gas boilers
    and gas turbine; loads pre-existing models if available, and fits new ones if not
    
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
    
    print('Loading/fitting heat generator gas consumption (and electricity '
          'co-generation) models ...')

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
        fp_gas = os.path.join(gas_modelpath, iri_encoded +'.sav')
        if os.path.exists(fp_gas):
            logger.info(f'Gas consumption model for generator "{iri}" already exists.')
        else:
            # Fit and save new model
            logger.info(f'Create new gas consumption model for generator "{iri}".')
            # Retrieve historical gas consumption and heat generation
            res = kg_client.get_heatgenerator_output_iris(iri, gt=False)
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
            # Create new representation model
            create_boiler_consumption_model(data=df, name=iri_encoded)
            
        # Load existing/created model
        gas_consumption_models[iri] = pickle.load(open(fp_gas, 'rb'))
    
    # Create/retrieve models for gas turbine
    for iri in generators['gt']:
        # Create encoded IRI string (to use as file names)
        iri_encoded = urllib.parse.quote(iri, safe='')
        # Only create new model if not already saved
        fp_gas = os.path.join(gas_modelpath, iri_encoded +'.sav')
        fp_cogen = os.path.join(cogen_modelpath, iri_encoded +'.sav')
        create_model=True
        if os.path.exists(fp_gas):
            logger.info(f'Gas consumption model for gas turbine "{iri}" already exists.')
            if os.path.exists(fp_cogen):
                logger.info(f'Electricity co-gen model for gas turbine "{iri}" already exists.')
                create_model=False
        if create_model:
            # Fit and save new model
            logger.info(f'Create new gas consumption model for generator "{iri}".')
            # Retrieve historical gas consumption and heat generation
            res = kg_client.get_heatgenerator_output_iris(iri, gt=True)
            dataIRI, _ = kg_client.get_associated_dataIRI(res[OHN_GENERATED_HEAT_AMOUNT], 
                                unit=OM_MEGAWATTHOUR, forecast=False)
            df1 = ts_client.retrieve_timeseries_as_dataframe(dataIRI, 'heat_generation',
                                                             upperbound=tmax)
            dataIRI, _ = kg_client.get_associated_dataIRI(res[OHN_CONSUMED_GAS_AMOUNT], 
                                unit=OM_MEGAWATTHOUR, forecast=False)
            df2 = ts_client.retrieve_timeseries_as_dataframe(dataIRI, 'gas_consumption',
                                                             upperbound=tmax)
            dataIRI, _ = kg_client.get_associated_dataIRI(res[OHN_COGEN_ELECTRICITY_AMOUNT], 
                                unit=OM_MEGAWATTHOUR, forecast=False)
            df3 = ts_client.retrieve_timeseries_as_dataframe(dataIRI, 'cogen_electricity',
                                                             upperbound=tmax)
            # Join DataFrames (on time index); ensure both data available for each timestep
            df = df1.join(df2).join(df3)
            df.dropna(inplace=True)
            # Create new representation models
            if len(df) < 1000:
                raise_error(ValueError, f'Insufficient data points provided to fit '\
                            + f'representative model: {len(df)} data points')
            create_gt_consumption_model(data=df, name=iri_encoded)
            
        # Load existing/created models
        gas_consumption_models[iri] = pickle.load(open(fp_gas, 'rb'))
        electricity_cogen_models[iri] = pickle.load(open(fp_cogen, 'rb'))
        
    print('Heat generator models successfully loaded.')
    
    return gas_consumption_models, electricity_cogen_models
        
        
def create_boiler_consumption_model(data:pd.DataFrame, name:str, efficiency=0.9,
                                    modelpath=CONSUMPTION_MODEL_REPO,
                                    plotpath=MODEL_PLOTS_REPO):
    """
    Creates and fits linear gas consumption model to historic heat boiler data 
    (i.e., gas consumption = f (heat generation))
    Plots and saves/serializes the fitted model
    
    NOTE: Due to different data cleaning, these models are slightly different
          than the ones used for the initial SWPS project, but still comparable!

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
    
    # Ensure enough data is available to fit representative model
    if len(data) < 1000:
        raise_error(ValueError, f'Insufficient data points provided to fit representative '\
                    + f'gas consumption model: {len(data)} data points')

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
    plt.savefig(os.path.join(plotpath, name +'_gas.png'))


def create_gt_consumption_model(data:pd.DataFrame, name:str,
                                gas_modelpath=CONSUMPTION_MODEL_REPO,
                                cogen_modelpath=COGEN_MODEL_REPO,
                                plotpath=MODEL_PLOTS_REPO):
    """
    Creates and fits linear gas consumption and electricity co-generation models to historic gas turbine data
        1) total gas consumption = f (heat generation)
        2) electricity generation = f (heat generation)

    Plots initial efficiency model, OEM data sheet derived model, and fitted data-driven
    model for gas consumption and electricity generation
    
    NOTE: Due to different data cleaning, these models are slightly different
          than the ones used for the initial SWPS project, but still comparable!

    :param pd.DataFrame data: DataFrame containing historic data [heat_generation, gas_consumption, cogen_electricity]
    :param str name: file name for gas turbine, i.e., encoded IRI string
    :param str modelpath/plotpath: paths to output directories
    """

    # Initial gas turbine gas consumption model
    # NOTE: for visual comparison only; hence, hardcoded values are not critical
    def initial_gas_demand(power_q):
        p_el = 6.5      # MW
        p_q = 11.7      # MW
        eff_ges = 0.88
        return (power_q/eff_ges) * ((p_el+p_q)/p_q)

    # Initial gas turbine electrical power model
    # NOTE: for visual comparison only; hence, hardcoded values are not critical
    def initial_el_generation(power_q):
        p_el = 6.5      # MW
        p_q = 11.7      # MW
        return power_q * (p_el/p_q)

    # OEM data sheet derived gas turbine model (for T = 5°C or 10°C as average historic temperature)
    # NOTE: for visual comparison only; hence, hardcoded values are not critical
    def gt_oem_performance(power_q):
        # OEM data sheet information for T=5°C
        data_sheet = pd.DataFrame(columns=['P_th', 'P_el', 'load', 'etha'])
        # load (fraction of max. electrical power)
        data_sheet['load'] = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
        # electrical power, MW
        data_sheet['P_el'] = [0.0, 0.676, 1.351, 2.026, 2.702, 3.378, 4.024, 4.695, 5.365, 6.036, 6.756]
        # thermal power, kW
        data_sheet['P_th'] = [0.0, 5.272, 5.927, 6.624, 7.329, 8.067, 8.807, 9.602, 10.437, 11.304, 12.199]
        # total energy efficiency (etha for 0% load arbitrarily set to avoid division by 0)
        data_sheet['etha'] = [0.6897, 0.6897, 0.7267, 0.7553, 0.7754, 0.7937, 0.8091, 0.8222, 0.8332, 0.8423, 0.8517]

        # approximate electrical power output corresponding to thermal output 'power_q'
        p_el = np.interp(power_q, data_sheet['P_th'], data_sheet['P_el'])

        # approximate gas demand corresponding to thermal (and electrical) output 'power_q'
        eff = np.interp(power_q, data_sheet['P_th'], data_sheet['etha'])
        gas = (power_q + p_el) / eff

        return p_el, gas

    # Condition historical data to exclude outliers from linear regression for both models
    # replace non-physical negative values zero
    data[data < 0] = 0  
    # exclude historical data with non-meaningful gas consumption
    data = data[~(data.iloc[:, 1] < data.iloc[:, 0])]
    data_gas = data[['heat_generation', 'gas_consumption']].copy()
    data_gas.dropna(inplace=True)
    data_el = data[['heat_generation', 'cogen_electricity']].copy()
    data_el.dropna(inplace=True)
    # consider only data points in valid operational range; temperature dependent
    # range based on OEM data sheet [7.2, 12.7] -> [8, 14] to only remove 'real' outliers
    min_load = 8.0
    max_load = 14.0
    data_gas_upper = data_gas[(data_gas['heat_generation'] >= min_load) &
                              (data_gas['heat_generation'] <= max_load)].copy()
    data_el_upper = data_el[(data_el['heat_generation'] >= min_load) &
                            (data_el['heat_generation'] <= max_load)].copy()
    
    # Ensure enough data is available to fit representative models
    if len(data_gas_upper) < 1000:
        raise_error(ValueError, f'Insufficient data points provided to fit representative '\
                    + f'gas consumption model: {len(data_gas_upper)} data points')
    if len(data_el_upper) < 1000:
        raise_error(ValueError, f'Insufficient data points provided to fit representative '\
                    + f'co-generation model: {len(data_el_upper)} data points')

    # Fit linear gas consumption model: gas consumption = f( heat generation )
    Xg = data_gas_upper.iloc[:, 0].values
    # reshape as required by sklearn (vertical vector)
    Xg = np.array(Xg).reshape(-1, 1)
    yg = np.array(data_gas_upper.iloc[:, 1].values)
    model_gas = LinearRegression().fit(Xg, yg)
    # Save the fitted model by serializing with Pickle
    f = os.path.join(gas_modelpath, name +'.sav')
    pickle.dump(model_gas, open(f, 'wb'))

    # Fit linear electrical power model: electricity generation = f( heat generation )
    Xe = data_el_upper.iloc[:, 0].values
    # reshape as required by sklearn (vertical vector)
    Xe = np.array(Xe).reshape(-1, 1)
    ye = np.array(data_el_upper.iloc[:, 1].values)
    model_el = LinearRegression().fit(Xe, ye)
    # save the fitted model by serializing with Pickle
    f = os.path.join(cogen_modelpath, name +'.sav')
    pickle.dump(model_el, open(f, 'wb'))

    # Evaluate initial gas turbine operating models (for visual comparison only!)
    q = np.linspace(data['heat_generation'].min(), data['heat_generation'].max(), num=100)
    initial_gas = initial_gas_demand(q)
    initial_el = initial_el_generation(q)
    oem_el, oem_gas = gt_oem_performance(q)

    ###   plot gas consumption vs. heat generation   ###
    # set global plotting parameters
    plt.rcParams['font.size'] = 14
    # plot gas consumption vs. heat generation
    plt.figure(figsize=(16, 9))
    plt.grid('both')
    plt.scatter(data_gas.iloc[:, 0], data_gas.iloc[:, 1], marker='.', s=100, facecolors='none', color='dimgrey')
    plt.scatter(data_gas_upper.iloc[:, 0], data_gas_upper.iloc[:, 1], alpha=0.5, marker='.', s=100, color='dimgrey')
    plt.xlabel('Heat generation (MWh)')
    plt.ylabel('Gas consumption (MWh)')
    # add initial gas consumption model
    plt.plot(q, initial_gas, color='blue')
    # add gas consumption according to oem model
    plt.plot(q, oem_gas, color='orange')
    # add fitted linear gas consumption model
    plt.plot(Xg, model_gas.predict(Xg), color='green')
    plt.legend(['Efficiency model', 'OEM data sheet model', 'Linear regression model',
                'Historical data', 'Historical data > 50% load'])
    # extend model plot as dashed line below minimal load
    xg = np.arange(start=data['heat_generation'].min(), stop=Xg.min(), step=0.1)
    plt.plot(xg, model_gas.predict(np.array(xg).reshape(-1, 1)), '--', color='green')
    plt.title('Gas consumption GT')
    plt.savefig(os.path.join(plotpath, name+ '_gas.png'))

    ###   plot electrical output vs. heat generation   ###
    plt.figure(figsize=(16, 9))
    plt.grid('both')
    plt.scatter(data_el.iloc[:, 0], data_el.iloc[:, 1], marker='.', s=100, facecolors='none', color='dimgrey')
    plt.scatter(data_el_upper.iloc[:, 0], data_el_upper.iloc[:, 1], alpha=0.5, marker='.', s=100, color='dimgrey')
    plt.xlabel('Heat generation (MWh)')
    plt.ylabel('Electricity co-generation (MWh)')
    # add initial electricity generation model
    plt.plot(q, initial_el, color='blue')
    # add electricity generation according to oem model
    plt.plot(q, oem_el, color='orange')
    # add fitted linear electrical power model
    plt.plot(Xe, model_el.predict(Xe), color='green')
    plt.legend(['Efficiency model', 'OEM data sheet model', 'Linear regression model',
                'Historical data', 'Historical data > 50% load'])
    # extend model plot as dashed line below minimal load
    xe = np.arange(start=data['heat_generation'].min(), stop=Xe.min(), step=0.1)
    plt.plot(xe, model_el.predict(np.array(xe).reshape(-1, 1)), '--', color='green')
    plt.title('Electricity co-generation GT')
    plt.savefig(os.path.join(plotpath, name+ '_cogen.png'))

    ###   plot minimum load and minimum heat demand   ###
    min_load = data_el_upper['cogen_electricity'].quantile(0.05)     # MW_el
    min_heat = (min_load - model_el.intercept_) / model_el.coef_     # MW_q
    max_load = data_el_upper['cogen_electricity'].max()              # MW_el
    max_heat = (max_load - model_el.intercept_) / model_el.coef_     # MW_q

    plt.figure(figsize=(16, 9))
    plt.grid('both')
    plt.scatter(data_el.iloc[:, 0], data_el.iloc[:, 1], marker='.', s=100, facecolors='none', color='dimgrey')
    plt.scatter(data_el_upper.iloc[:, 0], data_el_upper.iloc[:, 1], alpha=0.5, marker='.', s=100, color='dimgrey')
    plt.xlabel('Heat generation (MWh)')
    plt.ylabel('Electricity co-generation (MWh)')
    plt.axhline(min_load, color='tab:blue')
    plt.axhline(max_load, color='tab:blue', linestyle=':')
    plt.axvline(min_heat, color='tab:cyan')
    plt.axvline(max_heat, color='tab:cyan', linestyle=':')
    # add fitted linear electrical power model
    plt.plot(Xe, model_el.predict(Xe), color='green')
    plt.legend(['Minimum load: %.1f MW' % min_load, 'Maximum load: %.1f MW' % max_load,
                'Minimum heat load: %.1f MW' % min_heat, 'Maximum heat load: %.1f MW' % max_heat,
                'Linear regression model', 'Historical data', 'Historical data > 50% load'], loc='lower left')
    plt.title('Electricity co-generation GT')
    plt.savefig(os.path.join(plotpath, name+ '_cogen_zoomed.png'))
