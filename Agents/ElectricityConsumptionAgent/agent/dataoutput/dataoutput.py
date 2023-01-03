################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to perform data output to generate figures
# in various format
# NOTE: pd.DataFrame is used frequently in this module, where for all the dataframe, the column[0]
# should all be LSOA code used as identifier

import agentlogging
from agent.kgutils.kgclient import KGClient
from agent.kgutils.querytemplates import *
from agent.errorhandling.exceptions import *
from agent.datamodel.spec import *

import matplotlib.pyplot as plt
import shapely.wkt
from geomet import wkt
import geopandas as gpd
from mpl_toolkits.axes_grid1 import make_axes_locatable
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
from mpl_toolkits.axes_grid1.inset_locator import zoomed_inset_axes, mark_inset
import scipy.stats as st
import matplotlib.colors as cl 
import matplotlib.cm as cm
import seaborn as sb 

#from agent.kgutils.tsclient import jpsBaseLibView

# Initialise logger
logger = agentlogging.get_logger("prod")

# ------------------------ Some 'shortcut' functions ----------------------- #
def convert_to_int(val):
    '''
    get rid of all Nan data, and convert the data format to int
    '''
    if val == 'NaN':
        return np.nan
    else:
        return int(val)

def convert_to_float(val):
    '''
    get rid of all Nan data, and convert the data format to Float
    '''
    if val == 'NaN':
        return np.nan
    else:
        val = float(val)
        val = round(val,3)
        return val

def save_figures(arg_name):
    '''
    Save the figure under ./'figure_output folder, both png and pdf format
    Argument:
    arg_name: the name of this figure file
    '''
    plt.savefig('figure_output/'+f"{arg_name}".lower()+'.png',dpi=200) 
    plt.savefig('figure_output/'+f"{arg_name}".lower()+'.pdf')

def data_treatment(df,arg_name,arg_value):
        '''
    This function is created to append a new column in the existing dataframe (at last position)
    and creat a np.darray contain all the new data, excluded nan data, to be used
    to set the scale of colorbar

    Arguments:
                  *** Please use this function with plot_geodistribution***
    df: dataframe to be appended
    arg_name: the name you want to give to this new column
    arg_value: two column pd.DataFrame, which MUST have a column with name 's' which contain LSOA code for data identification
               and another column for data
    '''
        # Appended the variable data to the df_geo
        df = df.assign(**{f"{arg_name}": np.nan})
        
        # Create a dict, which will make the data matching soooooo fast 
        dictionary = {row[arg_value.columns[0]]: row[arg_value.columns[1]] for _, row in arg_value.iterrows()}
        df[f"{arg_name}"] = df[df.columns[0]].apply(lambda x: dictionary.get(x, np.nan))
        
        # ------------ This val_values is normally used for colorbar plot ------------------- #
        # Specify the value to plot
        val_values = df[f"{arg_name}"].values
        # Create a boolean mask indicating which elements are NaN values
        mask = np.isnan(val_values)
        # Select only the non-NaN values from the array
        val_values = val_values[~mask]

        return df, val_values

def normalization(val_values, cb_scale):
    '''
    Provide normalization for color bar
    Arguments:
                  *** Please use this function with plot_geodistribution***
    val_values: NDArray which have NO nan data
    cb_scale: to adjust the scale of colorbar, sometimes the figure is ugly because the max and min value are too close
                 in a nutshell, the bigger the cb_scale, the larger the gap between max and min for colorbar
                 I suggest start with 0, and then try 1, 1.5 maybe. Need a few try and error.
    '''
    iqr = st.iqr(val_values)
    q1,q3 = st.mstats.idealfourths(val_values)
    bottom = q1 - cb_scale * iqr
    top = q3 + cb_scale * iqr
    divnorm = cl.Normalize(vmin=bottom, vmax=top)

    return divnorm

def create_color_bar(color_theme: str, divnorm, label: str, axs, cax1, val_df: pd.DataFrame):
    '''
    Provide color bar
    Arguments:
                  *** Please use this function with plot_geodistribution***
    color_theme: color theme of the map
    divnorm: normalization result returned from function normalization(val_values, cb_scale)
    label: the label of the color bar (or say legend)
    axs: define the axs, should be consistent with the previous plt
    cax1: define the cax1, should be consistent with the previous plt
    val_df: a pd.Dataframe (should have one column of data ONLY so which can be set as array)
    '''
    # Create a colorbar for the plot
    scalar_mappable = cm.ScalarMappable(cmap=color_theme, norm=divnorm)
    scalar_mappable.set_array(val_df)
    colorbar = plt.colorbar(scalar_mappable, ax=axs, cax=cax1)
    # Set the label for the colorbar
    colorbar.set_label(label)

def remove_NaN(df:pd.DataFrame):
    '''
    Remove all the row which contain the value 'NaN' 
    iteration start from second column and above, as the first column is always LSOA code for identification
    i.e., can't be NaN
    
    Note: This function is only used for data processing as this will delete the row
    '''
    df = df[df.iloc[:, 1:].notnull().all(axis=1)]
    
    return df

# ------------------------ Data retrieval ---------------------------------- #
def retrieve_elec_data_from_KG(query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT, year: str = '2020', per_household: bool = False) -> pd.DataFrame:
    '''
        perform SPARQL query to get the data from Blazegraph, return a DataFrame looks like:
              's'  'usage'  'meter' ('elec_consump_perhousehold')
        0
        1
        2
        ...

        Arguments:
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
        year: the number of year of which the data you may want to read
        per_household: bool, default as False, if it is yes, will return the value for per household
    '''
    # Get query string
    query = output_query_template('Electricity', year)

    # Construct kg client
    kg_client = KGClient(query_endpoint, update_endpoint)
    result = kg_client.performQuery(query)

    # Parse the result into DataFrame
    df = pd.DataFrame(columns=['s','usage','meter'])
    df = df.append(result)

    # Adjust the format, get rid of 'NaN' variables
    df["usage"] = df["usage"].apply(convert_to_float)
    df["meter"] = df["meter"].apply(convert_to_int)

    if per_household == True:
        df['elec_consump_perhousehold'] = df["usage"].to_numpy() / df["meter"].to_numpy() 
    
    return df 

def retrieve_gas_data_from_KG(query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT, year: str = '2020', per_household: bool = False) -> pd.DataFrame:
    '''
    perform SPARQL query to get the data from Blazegraph, return a DataFrame looks like:
        's'  'usage'  'meter'  'nonmeter' ('gas_consump_perhousehold')
    0
    1
    2
    ...

    Arguments:
    query_endpoint: str = QUERY_ENDPOINT,
    update_endpoint: str = UPDATE_ENDPOINT
    year: the number of year of which the data you may want to read
    per_household: bool, default as False, if it is yes, will return the value for per household
    '''
    # Get query string
    query = output_query_template('Gas', year)

    # Construct kg client
    kg_client = KGClient(query_endpoint, update_endpoint)
    result = kg_client.performQuery(query)

    # Parse the result into DataFrame
    df = pd.DataFrame(columns=['s','usage','meter','nonmeter'])
    df = df.append(result)

    # Adjust the format, get rid of 'NaN' variables
    df["usage"] = df["usage"].apply(convert_to_float)
    df["meter"] = df["meter"].apply(convert_to_int)
    df["nonmeter"] = df["nonmeter"].apply(convert_to_int)

    if per_household == True:
        df['gas_consump_perhousehold'] = df["usage"].to_numpy() / df["meter"].to_numpy() 
    return df

def retrieve_fuel_poor_from_KG(query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT, year: str = '2020') -> pd.DataFrame:
    '''
    perform SPARQL query to get the data from Blazegraph, return a DataFrame looks like:
        's'  'result' 'num' 
    0
    1
    2
    ...

    Arguments:
    year: the number of year of which the data you may want to read
    query_endpoint: str = QUERY_ENDPOINT,
    update_endpoint: str = UPDATE_ENDPOINT
'''
    # Get query string
    query = output_query_template('Fuel poverty', year)

    # Construct kg client
    kg_client = KGClient(query_endpoint, update_endpoint)
    result = kg_client.performQuery(query)

    # Parse the result into DataFrame
    df = pd.DataFrame(columns=['s', 'result', 'num'])
    df = df.append(result)

    # Adjust the format, get rid of 'NaN' variables
    df["result"] = df["result"].apply(convert_to_float)
    df["num"] = df["num"].apply(convert_to_int)

    return df

def retrieve_ONS_shape_from_KG(query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT, year: str = '2020') -> pd.DataFrame:
    '''
    perform SPARQL query to get the data from Blazegraph, return a DataFrame looks like:
    Note: NO NEED to specify year or iris
        's'  'geometry' 
    0
    1
    2
    ...

    Arguments:
    query_endpoint: str = QUERY_ENDPOINT,
    update_endpoint: str = UPDATE_ENDPOINT
'''

    # Get query string
    query = output_query_template('ONS output area', year, iris= False)

    # Construct kg client
    kg_client = KGClient(query_endpoint, update_endpoint)
    result = kg_client.performQuery(query)

    # check if WKT is valid and 
    # uploading polygons to Shapely to reduce precision to 5 DP (1m)
    for i in range(len(result[:,1])):
        shape = result[i,1]
        try:
            P = shapely.wkt.loads(shape)
            result[i,1] = shapely.wkt.dumps(P,rounding_precision=5)
        # if shape is invalid do chuff all 
        except TypeError:
            pass
        # if the shape is just a number (basically meaningless)
        # add to index of shapes to be deleted
        if type(result[i,1]) == int:
            del_ind = i 

    # get rid of invalid shapes
    result = np.delete(result,del_ind,axis=0)
    # Parse the result into DataFrame
    df = pd.DataFrame(columns=['s', 'geom'])
    for i in range(len(result)):
        df.loc[i,'s']=result[i,0]
        df.loc[i,'geom']=result[i,1]
    
    # specifying geodata frame
    df['geometry'] = gpd.GeoSeries.from_wkt(df['geom'])
    df = df.drop(columns=['geom'])

    df = gpd.GeoDataFrame(df, geometry='geometry')
    df = df.set_crs("EPSG:4326")
    print('Converting to Mercator projection (better than WGS84 for UK)')
    df = df.to_crs("EPSG:3395")
    # Enable it to save the df to pickle file, save the time for querying and processing :)
    #save_pickle_variable(df_geo = df)

    return df

def retrieve_temp_from_KG(query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT, year: str = '2020') -> pd.DataFrame:
    '''
    perform SPARQL query to get the data from Blazegraph, return a DataFrame looks like:
        s start var t  
    0
    1
    2
    ...

    Arguments:
    year: the number of year of which the data you may want to read
    iris: True to return data iri, False to not
    query_endpoint: str = QUERY_ENDPOINT,
    update_endpoint: str = UPDATE_ENDPOINT
'''
    # Get query string
    query = output_query_template('Temperature', year)

    # Construct kg client
    kg_client = KGClient(query_endpoint, update_endpoint)
    result = kg_client.performQuery(query)

    # Parse the result into DataFrame
    df = pd.DataFrame(columns=['s', 'start', 'var', 't' ])
    df = df.append(result)

    return df

# ------------------------- Calculation ------------------------------------ #
def monthly_disaggregation(df: pd.DataFrame, monthly_ref: list, annual: bool = False):
    '''
    To calculate the monthly distribution, based on the whole year data from df, and reference monthly distribution
    from monthly_ref
    Arguments:
    df: two-column data frame which MUST have the data to disaggregate placed at the second column
    (i.e. at position [1])
    monthly_ref: reference monthly distribution.
    annual: if True, the second column will include the annual value
            if False, only monthly value will be returned
    '''
    global months
    months = ['January','February','March','April','May','June','July','August','September','October','November','December']
    total = sum(monthly_ref)
    for i in range(12):
        df[f'{months[i]}'] = df[df.columns[1]] * monthly_ref[i] / total
    if annual == False:
        df = df.drop(columns=df.columns[1])
    return df

def fuel_cost(df_elec:pd.DataFrame, df_gas:pd.DataFrame, price_elec: float, price_gas:float, monthly_ref_elec:list, monthly_ref_gas:list, annual: bool = False):
    '''
    To calculate the fuel cost per LSOA, normally on per household basis
    Returns three dataframe which have first column as LSOA code, and the following 12
    columns for each month of df_cost_total, df_cost_elec, df_cost_gas
    Arguments:
    df_elec: two-column data frame which MUST have the electricity data placed at the second column
    (i.e. at position [1])
    df_gas: two-column data frame which MUST have the gas data placed at the second column
    (i.e. at position [1])
    price_elec: price of electricity
    price_gas: price of gas
    monthly_ref_elec: monthly consumption of electricity consumption, to be used in monthly_disaggregation 
    monthly_ref_gas: monthly consumption of gas consumption, to be used in monthly_disaggregation 
    annual: if True, the second column will include the annual value
            if False, only monthly value will be returned
    '''
    
    # Replace the first column from consumption into cost
    df_elec[df_elec.columns[1]] *= price_elec
    df_elec = df_elec.rename(columns={df_elec.columns[1]: 'Annual cost'}, inplace=False)
    df_cost_elec = df_elec.copy()
    
    df_gas[df_gas.columns[1]] *= price_gas
    df_gas = df_gas.rename(columns={df_gas.columns[1]: 'Annual cost'}, inplace=False)
    df_cost_gas = df_gas.copy()

    # Disaggrate into monthly dataframe
    df_cost_elec = monthly_disaggregation(df_cost_elec, monthly_ref_elec,annual)
    df_cost_gas = monthly_disaggregation(df_cost_gas,monthly_ref_gas,annual)
    
    # Merge to total cost
    df_cost_total = df_cost_elec.merge(df_cost_gas, left_on=df_cost_elec.columns[0], right_on=df_cost_gas.columns[0], how='outer')
    # Iterate through the columns of the merged dataframe and add the values
    for col in df_cost_gas.columns[1:]:
        df_cost_total[col] = df_cost_total[col + '_x'] + df_cost_total[col + '_y']

    # Drop the original columns from the merged dataframe
    df_cost_total.drop(columns=[col + '_x' for col in df_cost_elec.columns[1:]], inplace=True)
    df_cost_total.drop(columns=[col + '_y' for col in df_cost_gas.columns[1:]], inplace=True)
    
    return df_cost_total, df_cost_elec, df_cost_gas

def fuel_emission(df_elec:pd.DataFrame, df_gas:pd.DataFrame, carbon_intensity_elec: float, carbon_intensity_gas:float, monthly_ref_elec:list, monthly_ref_gas:list, annual: bool = False):
    '''
    To calculate the fuel emission per LSOA, normally on per household basis
    Returns three dataframe which have first column as LSOA code, and the following 12
    columns for each month of df_emission_total, df_emission_elec, df_emission_gas
    Arguments:
    df_elec: two-column data frame which MUST have the electricity data placed at the second column
    (i.e. at position [1])
    df_gas: two-column data frame which MUST have the gas data placed at the second column
    (i.e. at position [1])
    carbon_intensity_elec: carbon intensity of electricity
    carbon_intensity_gas: carbon intensity of gas
    monthly_ref_elec: monthly consumption of electricity consumption, to be used in monthly_disaggregation 
    monthly_ref_gas: monthly consumption of gas consumption, to be used in monthly_disaggregation 
    annual: if True, the second column will include the annual value
            if False, only monthly value will be returned
    '''
    df_elec[df_elec.columns[1]] *= carbon_intensity_elec
    df_elec = df_elec.rename(columns={df_elec.columns[1]: 'Annual emission'}, inplace=False)
    df_emission_elec = df_elec.copy()
    
    df_gas[df_gas.columns[1]] *= carbon_intensity_gas
    df_gas = df_gas.rename(columns={df_gas.columns[1]: 'Annual emission'}, inplace=False)
    df_emission_gas = df_gas.copy()

    # Disaggrate into monthly dataframe
    df_emission_elec = monthly_disaggregation(df_emission_elec, monthly_ref_elec,annual)
    df_emission_gas = monthly_disaggregation(df_emission_gas,monthly_ref_gas,annual)
    
    # Merge to total cost
    df_emission_total = df_emission_elec.merge(df_emission_gas, left_on=df_emission_elec.columns[0], right_on=df_emission_gas.columns[0], how='outer')
    # Iterate through the columns of the merged dataframe and add the values
    for col in df_emission_gas.columns[1:]:
        df_emission_total[col] = df_emission_total[col + '_x'] + df_emission_total[col + '_y']

    # Drop the original columns from the merged dataframe
    df_emission_total.drop(columns=[col + '_x' for col in df_emission_elec.columns[1:]], inplace=True)
    df_emission_total.drop(columns=[col + '_y' for col in df_emission_gas.columns[1:]], inplace=True)
    
    return df_emission_total, df_emission_elec, df_emission_gas

def get_median(df:pd.DataFrame, row_name: str = '0'):
    '''
    for given dataframes, calculate the median value of each column, and return one dataframe
    which contain only one row data of the median value result. The row name can be customised 
    and the column name remain unchanged.
    Arguments:
    df: dataframe to be calculated
    row_name: row name you may want to customise, default as 'o' 
    '''
    medians = df.median()
    median_df = medians.to_frame().transpose()
    median_df.index = [row_name]

    return median_df

def COP(temp, hp_efficiency:float = 0.35, T_H: float = 45 +273.15):
    '''
    Based on a given temperature to calculate the COP
    NOTE: COP = hp_efficiency * T_H / (T_H - T_C), where the input temperature is represented as T_C
    T_H, hp_efficiency are hypothesd as constant, which have default value as 318.15 and 0.35
    respectfully. I suggest to check if that default value is up to date or if that hypothesis is 
    valid in your case
    '''
    COP = hp_efficiency * T_H / (T_H -273.15 - temp)
    COP = round(COP,3)
    return COP

# ------------------------ GeoSpatial Plot --------------------------------- #
def plot_geodistribution(label: str, title:str, df: pd.DataFrame, cb_scale: float = 0):
    '''
    This module is aim to plot the input variable as geospatial scale (and do NOT have specifiy city view)
    Note: As the LSOA code is the unique identifier, this module accept the DataFrame which have one 
          column of LSOA code, and another column for the variable to plot

    Arguments:
       label: the legend label for colorbar
       title: title of the figure (be stored as file name as well)
       cb_scale: to adjust the scale of colorbar, sometimes the figure is ugly because the max and min value are too close
       df: df should be two column pd.DataFrame which have one column of LSOA code, and another column for the variable to plot/ can accept multiple
                 in a nutshell, the bigger the cb_scale, the larger the gap between max and min for colorbar
                 I suggest start with 0, and then try 1, 1.5 maybe. Need a few try and error.
    '''
    def basic_settings(df_geo):
        # Set the plot
        color_theme = 'coolwarm'
        fig = plt.figure(figsize=(3.5,4.5))
        plt.subplots_adjust(left=0.032,right=0.799)
        plt.tight_layout()
        axs = plt.axes()

        # Get the UK shape
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        UK_gdf.boundary.plot(ax=axs,color='k',linewidth=0.5)
        boundary = df_geo.bounds
        boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
        
        # y-axis limits of the axis are set to the minimum and maximum latitude values
        axs.set_ylim([boundary[1],boundary[3]])
        # Specify the color bar
        divider = make_axes_locatable(axs)
        cax1    = divider.append_axes("right", size="5%", pad=0.05)

        return axs, cax1, color_theme

    # Get Geospatial shapes:
    #df_geo = retrieve_ONS_shape_from_KG()
    ################### TBD
    df_geo = call_pickle('./Data/temp_Repo/df_geo in function retrieve_ONS_shape_from_KG')
    ###########################

    print(f'Beginning plot for geodistribution of {title}...')
    # Initilize the graph
    axs, cax1, color_theme = basic_settings(df_geo)

    # Revising the data
    df_geo, val_values =data_treatment(df_geo, title, df)

    # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
    divnorm = normalization(val_values, cb_scale)

    tl = df_geo.plot(column=f"{title}",
                cmap=color_theme,
                antialiased=False,
                ax=axs,
                legend=True,
                norm=divnorm,
                cax=cax1)

    # Create a colorbar for the plot
    create_color_bar(color_theme, divnorm, label, axs, cax1, df_geo[f"{title}"])

    axs.set_xticks([])
    axs.set_yticks([])
    axs.spines["top"].set_visible(False)
    axs.spines["right"].set_visible(False)
    axs.spines["left"].set_visible(False)
    axs.spines["bottom"].set_visible(False)
    axs.set_title(f"{title}",y=1.08)
    cax1.ticklabel_format(axis="y", style="sci", scilimits=(0,0))  
    
    # Store the figures
    save_figures(title)

def plot_geodistribution_with_cities(label: str, title:str, df: pd.DataFrame, cb_scale: float = 0):
    '''
    This module is aim to plot the input variable as geospatial scale (and DO have specifiy city view)
    Note: As the LSOA code is the unique identifier, this module accept the DataFrame which have one 
          column of LSOA code, and another column for the variable to plot

    Arguments:
       label: the legend label for colorbar
       title: title of the figure (be stored as file name as well)
       cb_scale: to adjust the scale of colorbar, sometimes the figure is ugly because the max and min value are too close
       df: df should be two column pd.DataFrame which have one column of LSOA code, and another column for the variable to plot/ can accept multiple
                 in a nutshell, the bigger the cb_scale, the larger the gap between max and min for colorbar
                 I suggest start with 0, and then try 1, 1.5 maybe. Need a few try and error.
    '''
    def basic_settings(df_geo):
        # Set the plot
        color_theme = 'coolwarm'
        mosaic = '''
        A
        A
        '''
        fig = plt.figure(figsize=(11,7))
        axs = fig.subplot_mosaic(mosaic)    
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        UK_gdf.boundary.plot(ax=axs['A'],color='k',linewidth=0.5)
        boundary = df_geo.bounds
        boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
        axs['A'].set_ylim([boundary[1]-5E4,boundary[3]+5E4])
        axs['A'].set_xlim(([boundary[0]-5E4,boundary[2]]))
        #plt.subplots_adjust(left=0)
        cax = fig.add_axes([0.9, 0.1, 0.02, 0.8])

        return axs, cax, color_theme, UK_gdf

    # Get Geospatial shapes:
    #df_geo = retrieve_ONS_shape_from_KG()
    ################### TBD
    df_geo = call_pickle('./Data/temp_Repo/df_geo in function retrieve_ONS_shape_from_KG')
    ###########################
    print(f'Beginning plot for geodistribution (with city view) of {title}...')
    
    # Initilize the graph
    axs, cax, color_theme, UK_gdf = basic_settings(df_geo)
    
    # Revising the data
    df_geo, val_values =data_treatment(df_geo, title,df)
    
    # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
    divnorm = normalization(val_values, cb_scale)

    axs_xbounds = [np.array([-2.815E5,-2E5]),np.array([-2.838E5,-1.05E5]),np.array([-3.35E4,9.4E3]),np.array([-6.5E5,-1.957E5])]
    axs_ybounds = [np.array([7.007E6,7.0652E6]),np.array([7.206E6,7.41E6]),np.array([6.656E6,6.6969E6]),np.array([6.39E6,6.78E6])]
    
    tl = df_geo.plot(column=f"{title}",
                cmap=color_theme,
                antialiased=False,
                ax=axs['A'],
                legend=True,
                norm=divnorm,
                cax=cax)

    # Create a colorbar for the plot
    create_color_bar(color_theme, divnorm, label, axs, cax, df_geo[f"{title}"])

    axs['A'].set_xticks([])
    axs['A'].set_yticks([])
    axs['A'].spines["top"].set_visible(False)
    axs['A'].spines["right"].set_visible(False)
    axs['A'].spines["left"].set_visible(False)
    axs['A'].spines["bottom"].set_visible(False)

    order = [4,3,2,1]
    loc1 = [1,2,2,1]
    loc2 = [3,3,3,4]
    names = ['Greater Manchester','North East','London','South West']

    for f in range(4):
        if f == 0 or f == 1:
            axins2 = inset_axes(axs['A'], width=4, height=2.5,
                    bbox_to_anchor=(0.5, 0.3),
                    bbox_transform=axs['A'].transAxes, loc=order[f], borderpad=6)
        else:
            axins2 = inset_axes(axs['A'], width=4, height=2.5,
                    bbox_to_anchor=(0.5, 0.5),
                    bbox_transform=axs['A'].transAxes, loc=order[f], borderpad=6)
        plt.subplots_adjust(bottom = 0.225,left=0.07)
        plt.setp(axins2.get_xticklabels(), visible=False)
        plt.setp(axins2.get_yticklabels(), visible=False)
        UK_gdf.boundary.plot(ax=axins2,color='k',linewidth=0.5)
        axins2.set_xticks([])
        axins2.set_title(str(names[f]))
        axins2.set_yticks([])
        axins2.set_ylim(axs_ybounds[f])
        axins2.set_xlim(axs_xbounds[f])
        df_geo.plot(column=f"{title}",cmap=color_theme,\
                antialiased=False,\
                ax = axins2,\
                norm = divnorm)
        mark_inset(axs['A'],axins2,loc1=loc1[f],loc2=loc2[f],fc='none',ec='0')
    
    # Store the figures
    save_figures(title)

# ------------------------ Line chart Plot --------------------------------- #
def plot_temproal_line_chart(filename: str, y_label: str, df: pd.DataFrame):
    '''
    To create a line chart to represent the monthly data
    Note: the input dataframe should be looks like:
# -------------------------------------------------------------#
#                Jan Feb Mar .... Nov Dec                      #
# [index_name]                                                 #
# [index_name]                                                 #
#    ...                                                       #
# -------------------------------------------------------------#
    so for how many rows were in this df, how many line will be plotted. noted that the column names 
    don't matter as it default the sequence as Jan to Dec. However, the [index_name] will be used as 
    the label of this line

    If in some case you may want fill_between feature to represent a range, please provide several rows with same 
    [index_name], for those rows have same [index_name], the function will automatically fill between the row have
    max average value and the row have min average value, while for rows in the middle will still be plotted.
    i.e., sequence don't matter as long as they have the same [index_name]

    Arguments:
    filename: str, name of the figure file you may want to store
    y_label: str, name of the y-axis legend
    df: pd.DataFrame, dataframe to be processd
    '''
    # Group the rows by index name
    groups = df.groupby(df.index)

    # Initialize the plot
    fig, axs = plt.subplots(1,1,figsize=(5,3))
    plt.tight_layout()
    
    i = 0
    for index_name, group in groups:
        if len(group) == 1:
            # Plot the line for the single-data row
            sb.lineplot(x=group.columns, y=group.values[0], label=index_name, color='black', linewidth=2, linestyle=['solid', 'dashed', 'dotted', 'dashdot'][i])
            i+=1

        else:
            # Sort the group by the mean value of the rows
            group = group.sort_values(by=group.mean(axis=1))
            
            # Get the minimum and maximum rows
            y1 = group.iloc[0]
            y2 = group.iloc[-1]
            # Plot the fill between the minimum and maximum rows
            sb.fill_between(x=df.columns, y1=y1, y2=y2, color='black',linestyle=['solid', 'dashed', 'dotted', 'dashdot'][i],alpha=0.1)
            # Plot the remaining rows
            for _, row in group.iloc[1:-1].iterrows():
                sb.lineplot(x=df.columns, y=row, label=index_name, color='black', linewidth=2, linestyle=['solid', 'dashed', 'dotted', 'dashdot'][i])
                i+=1
    axs.legend(frameon=False)

    plt.subplots_adjust(left=0.175)
    axs.set_xticklabels(labels = ['J','F','M','A','M','J','J','A','S','O','N','D'])
    axs.set_xlabel('')
    axs.set_ylabel(y_label)

    save_figures(filename)
    print(f'{i} number of lines have been plotted')



df_full = call_pickle('./Data/temp_Repo/df in function get_all_data')
'''
     LSOA_code	ons_shape	Electricity_consump	Electricity_meter	Electricty_cosumption_per_household	Gas_consump	Gas_meter	Gas_nonmeter	Gas_consumption_per_household	FuelPoor_%	Household_num	temp
'''
df_elec = df_full[['LSOA_code', 'Electricty_cosumption_per_household']]
df_gas = df_full[['LSOA_code', 'Gas_consumption_per_household']]
df_temp = df_full[['LSOA_code', 'temp']]

############### Test for plot_geodistribution ############
'''
df = retrieve_elec_data_from_KG()
df['Electricty cosumption per household'] = df['usage'].to_numpy() /df['meter'].to_numpy() 
df = df.drop(columns=['meter'])
df = df.drop(columns=['usage'])
plot_geodistribution_with_cities(label = 'kWh/year/household', title = 'Electricity Consumption', df =df, cb_scale = 1.5)
'''
##########################################################

############### Test for temproal_line_chart #############
# Test for fuel cost ---------------------------------------------
'''
# Calculate fuel cost
df_cost_total, df_cost_elec, df_cost_gas = fuel_cost(df_elec,df_gas,cost_elec_2020,cost_gas_2020,monthly_electricity_consumption_2020,monthly_gas_consumption_2020, annual=False)

# Calculate median value
df_cost_total = get_median(df_cost_total, 'Total cost')
df_cost_elec = get_median(df_cost_elec, 'Electricity cost')
df_cost_gas = get_median(df_cost_gas, 'Gas cost')

# Construct df to plot
df_to_plot = pd.concat([df_cost_total, df_cost_elec, df_cost_gas], axis=0)
plot_temproal_line_chart(filename='household_cost', y_label = 'Fuel Cost \n (£/month/household)',df = df_to_plot)
'''
# ----------------------------------------------------------------

# Test for emission ----------------------------------------------
'''
# Calculate fuel emission
df_emission_total, df_emission_elec, df_emission_gas = fuel_emission(df_elec,df_gas,carbon_intensity_CO2e_elec_2020,carbon_intensity_CO2e_gas_2020,monthly_electricity_consumption_2020,monthly_gas_consumption_2020, annual=False)
# Calculate median value
df_emission_total = get_median(df_emission_total, 'Total emissions')
df_emission_elec = get_median(df_emission_elec, 'Electricity emissions')
df_emission_gas = get_median(df_emission_gas, 'Gas emissions')

# Construct df to plot
df_to_plot = pd.concat([df_emission_total, df_emission_elec, df_emission_gas], axis=0)
plot_temproal_line_chart(filename='household_emissions', y_label = 'Emissions \n (kgCO$_2$eq/month/household)',df = df_to_plot)
'''
# ----------------------------------------------------------------
##########################################################

# Exclude NaN data
df_temp = remove_NaN(df_temp)
convert_df(df_temp)