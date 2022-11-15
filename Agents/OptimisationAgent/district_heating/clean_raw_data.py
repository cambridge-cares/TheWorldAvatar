"""
Reads in consolidated .csv file of all SWPS raw data (as created by 'load_raw_data.py'), cleans it, and creates
overarching (date-sorted) .csv file of cleaned data

Considered cleaning steps
    - removal of non-physical / implausible values
    - partial outlier removal

Plots raw vs. cleaned data for each time series when called as main script

@author: Markus Hofmeister
"""

import os
import numpy as np
import pandas as pd
from pathlib import Path

import matplotlib.pyplot as plt
import matplotlib.dates as mdates

# ensures availability of consolidated raw data csv
import district_heating.load_raw_data


####################     FUNCTIONS     ####################


def clean_data(data, limits):
    """
    CLeans raw data DataFrame according to provided limits per variable and replaces (obvious) outliers

    :param pd.DataFrame data: DataFrame with raw data series
    :param dict limits: Dictionary of value limits for each data series to be cleaned
    :returns pd.DataFrame: DataFrame containing CLEANED data series for all relevant optimization inputs
    """

    first = True

    # loop over all columns/variables
    for var in data.columns:

        # clean only data for variables with provided limits
        if var in limits.keys():

            # extract variable series and limits
            series = data[[var]].copy()     # extract series as DataFrame
            lim = limits[var]

            # 1) replace erroneous NEGATIVE values for non-negative variables
            if isinstance(lim[0], (int, float)) and lim[0] >= 0:
                # extract all negative entries
                neg = series[series.values < 0]
                # replace negative values with preceding positive value if overall amount of negative entries
                # is rather "insignificant", i.e. < 5%
                if len(neg) / len(series) < 0.05:
                    # 'pad': use preceding value for replacement
                    series.replace(neg[neg.columns[-1]].unique(), method='ffill', inplace=True)

            # 2) remove UPPER outliers (values outside the 99.8th percentile)
            cutoff = np.percentile(series, 99.8)
            # reindex with integers in ascending order (moves DateTime index into column 'Date' to the right)
            series = series.reset_index()
            # extract all rows with data exceeding the cutoff value
            out = series[series[series.columns[-1]] > cutoff]
            # derive median of n adjacent entries on the left and right of the potential outlier
            n = 10
            std = series[series.columns[-1]].std()
            for i in out.index:
                # calculate median left and right of potential outlier
                med1 = series.loc[i - n:i - 1, series.columns[-1]].median()
                med2 = series.loc[i + 1:i + n, series.columns[-1]].median()
                # if medians are within one std --> replace outlier; otherwise, there seems to be a step-change in values
                if (med2 - std) <= med1 <= (med2 + std):
                    series.loc[i, series.columns[-1]] = series.loc[i - 1, series.columns[-1]]
            # restore DateTime index
            series = series.set_index('Date')

            # 3) set ALL VALUES outside upper bounds to upper limit
            if isinstance(lim[1], (int, float)):
                out = series[series.values > lim[1]]
                series = series.replace(out[out.columns[-1]].unique(), lim[1])

            # 4) remove 'white noise' (small 'random' spikes) around 0
            # define cutoff magnitude for all spikes to be neglected, e.g. < 5%
            cutoff = 0.05 * series.values.max()
            # reindex with integers in ascending order (moves DateTime index into column 'Date' to the right)
            series = series.reset_index()
            # extract all rows with near-zero data below the cutoff value
            noise = series[(series[series.columns[-1]] > 0) & (series[series.columns[-1]] < cutoff)]
            # derive median of n adjacent entries on the left and right of the potential outlier
            n = 10
            std = series[series.columns[-1]].std()
            for i in noise.index:
                # calculate median left and right of potential noise spike
                med1 = series.loc[i - n:i - 1, series.columns[-1]].median()
                med2 = series.loc[i + 1:i + n, series.columns[-1]].median()
                # if medians are within one std --> replace spike with 0; otherwise, there seems to be a step-change in values
                if (med2 - std) <= med1 <= (med2 + std):
                    series.loc[i, series.columns[-1]] = 0
            # restore DateTime index
            series = series.set_index('Date')

        # construct cleaned output DataFrame
        if first:
            cleaned = pd.DataFrame(series)
            first = False
        else:
            cleaned = pd.concat([cleaned, series], axis=1)

    return cleaned


####################     BODY     ####################

# set root directory (extract directory of current module)
rootdir = Path(__file__).parent
# relative paths to consolidated raw data input file and target output file
path_in = '..\\data\\input\\processed\\data_consolidated_raw.csv'
path_out = '..\\data\\input\\processed\\data_consolidated_clean.csv'

# only clean raw data if it has not already been cleaned
if os.path.exists(os.path.join(rootdir, path_out)):
    pass

else:
    # read consolidated raw data csv file
    raw_data = pd.read_csv(os.path.join(rootdir, path_in), index_col=0, parse_dates=True, dayfirst=True)

    # define limits for input variables [min, max]
    limits = {'Waermemenge Innenstadt (MW)': [0.0, 23.0],       # Spitzenlast im Fernwärmenetz 21-23 MW
              'Waermemenge MHKW (MW)': [0.0, 12.0],             # technical limit ~11-11.5MW
              'Temp Vorlauf (degC)': [0, 125.0],                # Vorlauftemperatur maximal 125°C im Fernwärmenetz
              'Temp Ruecklauf (degC)': [0, 125.0],
              'GT Wirkleistung (MW)': [0.0, 8.0],               # max values temperature dependent ; max values based on OEM
              'GT Waermeleistung (MW)': [0.0, 14.0],            # data sheet [7.2, 12.7] -> [8, 14] to only remove 'real' outliers
              'Waermeleistung Kessel4 (MW)': [0.0, 7.5],
              'Waermeleistung Kessel5 (MW)': [0.0, 7.5],
              'Waermeleistung Kessel6 (MW)': [0.0, 4.5],
              'Gasbezug Kessel4 (MWh)': [0.0, None],
              'Gasbezug Kessel5 (MWh)': [0.0, None],
              'Gasbezug Kessel6 (MWh)': [0.0, None],
              'Gasverbrauch GT (MWh)': [0.0, None],
              'WSP Temp1 (degC)': [0.0, 125.0],                 # Vorlauftemperatur maximal 125°C im Fernwärmenetz
              'WSP Temp2 (degC)': [0.0, 125.0],
              'WSP Temp3 (degC)': [0.0, 125.0],
              'WSP Temp4 (degC)': [0.0, 125.0],
              'WSP Temp5 (degC)': [0.0, 125.0],
              'WSP Temp6 (degC)': [0.0, 125.0],
              'WSP Temp7 (degC)': [0.0, 125.0],
              'WSP Temp8 (degC)': [0.0, 125.0],
              'WSP Temp9 (degC)': [0.0, 125.0],
              'WSP Temp10 (degC)': [0.0, 125.0],
              'WSP Temp11 (degC)': [0.0, 125.0],
              'WSP Temp12 (degC)': [0.0, 125.0],
              'Aussentemperatur (degC)': [None, None],
              'Temperatur Metromedia (degC)': [None, None],
              'Globalstrahlung Metromedia': [0.0, None],
              'Temp gemessen (degC)': [None, None],
              # MHKW Einspeisung
              'MHKW Temp Vorlauf (degC)': [0.0, 125.0],         # same as HKW limits
              'MHKW Temp Ruecklauf (degC)': [0.0, 125.0],
              'MHKW Durchfluss (m3/h)': [0.0, None],
              'MHKW Druck Vorlauf (bar)': [0.0, None],
              'MHKW Waermemenge (MW)': [0.0, 12.0],
              # do not clean or 'despike'
              'Gaspreis GT (EUR/MWh)': [None, None],
              'Gaspreis Kessel (EUR/MWh)': [None, None],
              'Spotpreis (EUR/MWh)': [None, None],
              'CO2 Preis (EUR/t)': [None, None]
              }

    # clean raw data
    cleaned_data = clean_data(raw_data, limits)

    # write overall DataFrames to csv
    cleaned_data.to_csv(os.path.join(rootdir, path_out), index_label='Date')

####################     MAIN     ####################

if __name__ == '__main__':

    # read raw and cleaned data
    raw_data = pd.read_csv(os.path.join(rootdir, path_in), index_col=0, parse_dates=True, dayfirst=True)
    clean_data = pd.read_csv(os.path.join(rootdir, path_out), index_col=0, parse_dates=True, dayfirst=True)

    # set output path for plots
    path_plots = '..\\data\\output\\timeseries\\'

    # plots of raw vs. cleaned data
    for i in clean_data.columns:
        # plot full data series and data series without zeros
        for j in ['_entire_data_set', '_only_data_larger_zero']:
            if j == '_entire_data_set':
                # drop NaNs to ensure proper (box-)plotting
                raw = raw_data[i].copy()
                raw.dropna(inplace=True)
                clean = clean_data[i].copy()
                clean.dropna(inplace=True)
            else:
                clean = clean[clean > 0].copy()

            # create figure
            f, ax = plt.subplots(3, 2, figsize=(16, 9))
            plt.suptitle(i)
            # raw data plots
            ax[0][0].plot(raw, '.-', ms=2.0, lw=1.0)
            ax[0][0].xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
            ax[0][0].set_title('Raw data')
            ax[1][0].hist(raw, bins=100, density=True)
            ax[2][0].boxplot(raw, vert=False)
            # cleaned data plots
            ax[0][1].plot(clean, '.-', ms=2.0, lw=1.0)
            ax[0][1].xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
            ax[0][1].set_title('Cleaned data')
            ax[1][1].hist(clean, bins=100, density=True)
            ax[2][1].boxplot(clean, vert=False)
            plt.show()
            # adjust problematic file names for saving
            if '/' in i:
                i = i.replace('/','_')
            f.savefig(os.path.join(rootdir, path_plots, i+j))
            plt.close()