"""
Create and save gas consumption models for heat boilers and gas turbine based on historical data

@author: Markus Hofmeister
"""

import os
import pickle
import numpy as np
import pandas as pd
from pathlib import Path

import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression


####################     FUNCTIONS     ####################


def create_boiler_consumption_model(data, name, efficiency, outdir):
    """
    Creates and fits linear gas consumption model to historic heat boiler data (gas consumption = f (heat generation))
    Plots and saves/serializes the fitted model

    :param pd.DataFrame data: DataFrame containing historic data [heat generation, gas consumption]
    :param str name: name of gas boiler
    :param float efficiency: efficiency (Nutzungsgrad) of heat boiler
    :param str outdir: path to output directory
    """

    # derive most representative subset of historical data to exclude outliers from linear regression
    fit_data = data.copy()
    # exclude historical data with gas consumption > consumption according to efficiency + 2 standard deviations
    fit_data = fit_data[~(fit_data.iloc[:, 1] > (fit_data.iloc[:, 0] / efficiency) + 2 * data.iloc[:, 1].std())]
    # exclude historical data with gas consumption < consumption according to efficiency - 2 standard deviations
    fit_data = fit_data[~(fit_data.iloc[:, 1] < (fit_data.iloc[:, 0] / efficiency) - 2 * data.iloc[:, 1].std())]

    # fit linear regression model: gas consumption = f( heat generation )
    X = fit_data.iloc[:, 0].values
    X = np.array(X).reshape(-1, 1)              # reshape as required by sklearn (vertical vector)
    y = np.array(fit_data.iloc[:, 1].values)
    model = LinearRegression().fit(X, y)

    # save the fitted model by serializing with Pickle
    f = os.path.join(outdir, name+'.sav')
    pickle.dump(model, open(f, 'wb'))

    # plot gas consumption vs. heat generation
    plt.rcParams['font.size'] = 14
    plt.figure(figsize=(16, 9))
    plt.scatter(data.iloc[:, 0], data.iloc[:, 1], marker='.', s=100, facecolors='none', color='dimgrey')
    plt.scatter(fit_data.iloc[:, 0], fit_data.iloc[:, 1], alpha=0.5, marker='.', s=100, color='dimgrey')
    # add simple efficiency model: gas consumption = heat generation / efficiency
    plt.plot(fit_data.iloc[:, 0], fit_data.iloc[:, 0] / efficiency, color='blue')
    # add linear regression model
    plt.plot(X, model.predict(X), color='green')
    plt.grid('both')
    plt.legend(['Efficiency model', 'Linear regression model', 'Historical data', 'Fitting data'])
    plt.xlabel('Waermeleistung (MWh)')
    plt.ylabel('Gasbezug (MWh)')
    plt.title(name)
    plt.savefig(os.path.join(outdir, name+'.png'))
    plt.show()


def create_gt_consumption_model(data, outdir):
    """
    Creates and fits linear gas consumption and electrical power models to historic gas turbine data
        1) total gas consumption = f (heat generation)
        2) electricity generation = f (heat generation)

    Plots initial model, OEM data sheet derived model, and fitted data-driven model for gas consumption and electricity
    generation

    :param pd.DataFrame data: DataFrame containing historic data [heat generation, gas consumption, electrical power]
    :param str outdir: path to output directory
    """

    # initial gas turbine gas consumption model
    def initial_gas_demand(power_q):
        p_el = 6.5      # MW (based on SWPS Excel)
        p_q = 11.7      # MW (based on SWPS Excel)
        eff_ges = 0.88
        return (power_q/eff_ges) * ((p_el+p_q)/p_q)

    # initial gas turbine electrical power model
    def initial_el_generation(power_q):
        p_el = 6.5      # MW (based on SWPS Excel)
        p_q = 11.7      # MW (based on SWPS Excel)
        return power_q * (p_el/p_q)

    # OEM data sheet derived gas turbine model (for T = 5°C or 10°C as average historic temperature)
    def gt_oem_performance(power_q):
        # OEM data sheet information for T=5°C and T=10°C
        data_sheet = pd.DataFrame(columns=['P_th', 'P_el', 'load', 'etha'])
        # load (fraction of max. electrical power)
        data_sheet['load'] = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
        # electrical power, MW
        data_sheet['P_el'] = [0.0, 0.676, 1.351, 2.026, 2.702, 3.378, 4.024, 4.695, 5.365, 6.036, 6.756]    # T=5°C
        #data_sheet['P_el'] = [0.0, 0.658, 1.316, 1.974, 2.632, 3.29, 3.948, 4.606, 5.264, 5.922, 6.58]     # T=10°C
        # thermal power, kW
        data_sheet['P_th'] = [0.0, 5.272, 5.927, 6.624, 7.329, 8.067, 8.807, 9.602, 10.437, 11.304, 12.199] # T=5°C
        #data_sheet['P_th'] = [0.0, 5.36, 5.982, 6.67, 7.365, 8.089, 8.827, 9.612, 10.43, 11.29, 12.047]    # T=10°C
        # total energy efficiency (etha for 0% load arbitrarily set to avoid division by 0)
        data_sheet['etha'] = [0.6897, 0.6897, 0.7267, 0.7553, 0.7754, 0.7937, 0.8091, 0.8222, 0.8332, 0.8423,
                              0.8517]   # T=5°C
        #data_sheet['etha'] = [0.7054, 0.7054, 0.7389, 0.7672, 0.7897, 0.8046, 0.8175, 0.831, 0.842, 0.8514,
        #                      0.8584]   # T=10°C

        # approximate electrical power output corresponding to thermal output 'power_q'
        p_el = np.interp(power_q, data_sheet['P_th'], data_sheet['P_el'])

        # approximate gas demand corresponding to thermal (and electrical) output 'power_q'
        eff = np.interp(power_q, data_sheet['P_th'], data_sheet['etha'])
        gas = (power_q + p_el) / eff

        return p_el, gas

    # condition data sets for both models
    data[data < 0] = 0
    data_gas = data[['GT Waermeleistung (MW)', 'Gasverbrauch GT (MWh)']].copy()
    data_gas.dropna(inplace=True)
    data_el = data[['GT Waermeleistung (MW)', 'GT Wirkleistung (MW)']].copy()
    data_el.dropna(inplace=True)
    # consider only data points above 50% load (wrt electrical output, for 10°C)
    min_load = 8.0
    max_load = 12.0
    data_gas_upper = data_gas[(data_gas['GT Waermeleistung (MW)'] >= min_load) &
                              (data_gas['GT Waermeleistung (MW)'] <= max_load)].copy()
    data_el_upper = data_el[(data_el['GT Waermeleistung (MW)'] >= min_load) &
                            (data_el['GT Waermeleistung (MW)'] <= max_load)].copy()

    # fit linear gas consumption model: gas consumption = f( heat generation )
    Xg = data_gas_upper.iloc[:, 0].values
    # reshape as required by sklearn (vertical vector)
    Xg = np.array(Xg).reshape(-1, 1)
    yg = np.array(data_gas_upper.iloc[:, 1].values)
    model_gas = LinearRegression().fit(Xg, yg)
    # save the fitted model by serializing with Pickle
    f = outdir + '_gas_demand.sav'
    pickle.dump(model_gas, open(f, 'wb'))

    # fit linear electrical power model: electricity generation = f( heat generation )
    Xe = data_el_upper.iloc[:, 0].values
    # reshape as required by sklearn (vertical vector)
    Xe = np.array(Xe).reshape(-1, 1)
    ye = np.array(data_el_upper.iloc[:, 1].values)
    model_el = LinearRegression().fit(Xe, ye)
    # save the fitted model by serializing with Pickle
    f = outdir + '_el_power.sav'
    pickle.dump(model_el, open(f, 'wb'))

    # evaluate initial gas turbine operating models
    q = np.linspace(data['GT Waermeleistung (MW)'].min(), data['GT Waermeleistung (MW)'].max(), num=100)
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
    plt.xlabel('Waermeleistung (MW)')
    plt.ylabel('Gasverbrauch (MWh)')
    # add initial gas consumption model
    plt.plot(q, initial_gas, color='blue')
    # add gas consumption according to oem model
    plt.plot(q, oem_gas, color='orange')
    # add fitted linear gas consumption model
    plt.plot(Xg, model_gas.predict(Xg), color='green')
    plt.legend(['Efficiency model', 'OEM data sheet model', 'Linear regression model',
                'Historical data', 'Historical data > 50% load'])
    # extend model plot as dashed line below minimal load
    xg = np.arange(start=0, stop=Xg.min(), step=0.1)
    plt.plot(xg, model_gas.predict(np.array(xg).reshape(-1, 1)), '--', color='green')
    plt.title('Gasverbrauch SWPS GT')
    plt.savefig(os.path.join(outdir + '_gas_consumption.png'))
    plt.show()

    ###   plot electrical output vs. heat generation   ###
    plt.figure(figsize=(16, 9))
    plt.grid('both')
    plt.scatter(data_el.iloc[:, 0], data_el.iloc[:, 1], marker='.', s=100, facecolors='none', color='dimgrey')
    plt.scatter(data_el_upper.iloc[:, 0], data_el_upper.iloc[:, 1], alpha=0.5, marker='.', s=100, color='dimgrey')
    plt.xlabel('Waermeleistung (MW)')
    plt.ylabel('Elektrische Wirkleistung (MW)')
    # add initial electricity generation model
    plt.plot(q, initial_el, color='blue')
    # add electricity generation according to oem model
    plt.plot(q, oem_el, color='orange')
    # add fitted linear electrical power model
    plt.plot(Xe, model_el.predict(Xe), color='green')
    plt.legend(['Efficiency model', 'OEM data sheet model', 'Linear regression model',
                'Historical data', 'Historical data > 50% load'])
    # extend model plot as dashed line below minimal load
    xe = np.arange(start=0, stop=Xe.min(), step=0.1)
    plt.plot(xe, model_el.predict(np.array(xe).reshape(-1, 1)), '--', color='green')
    plt.title('Elektrische Leistung SWPS GT')
    plt.savefig(os.path.join(outdir + '_electrical_power.png'))
    plt.show()

    ###   plot minimum load and minimum heat demand   ###
    min_load = data_el_upper['GT Wirkleistung (MW)'].quantile(0.05)     # MW_el
    min_heat = (min_load - model_el.intercept_) / model_el.coef_        # MW_q
    max_load = data_el_upper['GT Wirkleistung (MW)'].max()              # MW_el
    max_heat = (max_load - model_el.intercept_) / model_el.coef_        # MW_q

    plt.figure(figsize=(16, 9))
    plt.grid('both')
    plt.scatter(data_el.iloc[:, 0], data_el.iloc[:, 1], marker='.', s=100, facecolors='none', color='dimgrey')
    plt.scatter(data_el_upper.iloc[:, 0], data_el_upper.iloc[:, 1], alpha=0.5, marker='.', s=100, color='dimgrey')
    plt.xlabel('Waermeleistung (MW)')
    plt.ylabel('Elektrische Wirkleistung (MW)')
    plt.axhline(min_load, color='tab:blue')
    plt.axhline(max_load, color='tab:blue', linestyle=':')
    plt.axvline(min_heat, color='tab:cyan')
    plt.axvline(max_heat, color='tab:cyan', linestyle=':')
    # add fitted linear electrical power model
    plt.plot(Xe, model_el.predict(Xe), color='green')
    plt.legend(['Minimum load: %.1f MW' % min_load, 'Maximum load: %.1f MW' % max_load,
                'Minimum heat load: %.1f MW' % min_heat, 'Maximum heat load: %.1f MW' % max_heat,
                'Linear regression model', 'Historical data', 'Historical data > 50% load'], loc='lower left')
    plt.title('Elektrische Leistung SWPS GT')
    plt.savefig(os.path.join(outdir + '_minimum_load.png'))
    plt.show()


####################     MAIN     ####################

if __name__ == '__main__':

    # set directory (extract directory of current module)
    root = Path(__file__).parent
    # relative path to consolidated pre-cleaned input file
    path_data = '..\\data\\input\\processed\\data_consolidated_clean.csv'
    # read pre-cleaned input data (full historic length)
    file = os.path.join(root, path_data)
    all_data = pd.read_csv(file, index_col=0, parse_dates=True, dayfirst=True)

    # temperature analysis
    temp = all_data[['GT Waermeleistung (MW)', 'Aussentemperatur (degC)', 'Temperatur Metromedia (degC)',
                    'Temp gemessen (degC)']].copy()
    temp = temp[temp['GT Waermeleistung (MW)'] > 0]
    print('\nAverage temperature of days with active gas turbine: ')
    print(temp.iloc[:,1:].mean())
    print('\nMedian temperature of days with active gas turbine: ')
    print(temp.iloc[:, 1:].median())

    # define which heat generators to fit
    fit_boilers = False
    fit_gt = True

    # create boiler consumption/gas demand models
    if fit_boilers:

        path_models = '..\\data\\models\\gas_consumption_boilers'

        # specify boiler names and efficiencies
        boilers = {'Kessel4': 0.9, 'Kessel5': 0.9, 'Kessel6': 0.95}

        # loop over all boilers
        for boiler in boilers:
            # only create model if not already saved
            if os.path.exists(os.path.join(root, path_models, boiler+'.sav')):
                pass
            else:
                # extract boiler specific data
                data = all_data[['Waermeleistung ' + boiler + ' (MW)', 'Gasbezug ' + boiler + ' (MWh)']].copy()
                data.dropna(inplace=True)

                # create, save, and plot linear consumption models
                create_boiler_consumption_model(data, boiler, boilers[boiler], os.path.join(root, path_models))

    # create GT consumption/gas demand model
    if fit_gt:

        path_model = '..\\data\\models\\gas_consumption_gts\\SWPS_GT'

        # only create model if not already saved
        if os.path.exists(os.path.join(root, path_model+'_gas_demand.sav')) and \
                os.path.exists(os.path.join(root, path_model+'_el_power.sav')):
            pass
        else:
            # extract gas turbine specific data
            data = all_data[['GT Waermeleistung (MW)', 'Gasverbrauch GT (MWh)', 'GT Wirkleistung (MW)']].copy()

            # create, save, and plot gt consumption model
            create_gt_consumption_model(data, os.path.join(root, path_model))

