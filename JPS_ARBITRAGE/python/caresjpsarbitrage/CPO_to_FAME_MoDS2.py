##
# @file
# File documentation
# This file is a set of functions designed to work as an arbitrage spotter for a chemical plant converting a reagent into a product. The spotter looks through the chemical futures markets for profitable opportunities to purchase the reagent, convert it into the product and then sell the product. It factors in the costs of transport, storage and conversion and looks for the cheapest schedule. The conversion costs are based on an Aspen simulation of a relevant conversion process.
# Important assumptions include instantenaous conversion and transport, pricing other than futures is based on the literature, only marginal profit is calculated (this is liable to changes soon). 
# Additionally, it i assumed that the plant is already functioning on a long-term contract. For that reason the changes to plant's function cannot be too great and capital costs may be ignored in the calculations.

import sys
from math import inf
#import matplotlib.pyplot as plt
from csv_funcs import RCSV, ACSV
import json

from caresjpsutil import returnExceptionToJava, returnResultsToJava
from caresjpsutil import PythonLogger

def preprocessing(miscCosts, cpo, fame):
    dates = {}
    prices = {}
    u_prices = {}
    t_prices = {}
    ex_rates = {}
    s_prices = {}


    dates[cpo['arrayHeader'][0]] = cpo['arrayHeader'][1:] + cpo['arrayMonths']
    dates[fame['arrayHeader'][0]] = fame['arrayHeader'][1:] + fame['arrayMonths']

    prices[cpo['arrayHeader'][0]] = [cpo['arrayDatetime'][0][5:]] + cpo['arrayDatetime'][1:] + cpo['arrayPrices']
    prices[fame['arrayHeader'][0]] = [fame['arrayDatetime'][0][5:]] + fame['arrayDatetime'][1:] + fame['arrayPrices']


    for key in prices:
        for j in range(2, len(prices[key])):
            try:
                prices[key][j] = float(prices[key][j])
                if float(prices[key][j]) == 0.0: 
                    prices[key][j] = None
                    continue                
            except:
                prices[key][j] = None

    for key in miscCosts:
        if key.rfind('USD') != -1:
            ex_rates[key] = float(miscCosts[key])
        elif key.rfind('Storage') != -1:
            s_prices[key] = float(miscCosts[key])
        elif key.rfind('Transport') != -1:
            t_prices[key] = float(miscCosts[key])
        else:
            u_prices[key] = float(miscCosts[key])
    
    
    return dates, prices, u_prices, t_prices, ex_rates, s_prices
 
def transport_costs(prices, t_prices):
    # This function adds transportation cost to the prices of the reagent and subtracts the transportation cost from the prices of the product. 
    for key in prices:
        if key == 'FAME':
            # The code adds the cost of transporting biodiesel from south-eastern Asia to south China.
            # Those markups are calculatd based on https://www.icis.com/resources/news/2013/11/08/9723077/se-asia-to-china-palm-oil-freight-rates-may-fall-on-weak-demand/

            tmp = prices[key][3:]
            # This loop calculates the converted prices.
            for i in range(len(tmp)):
                if type(tmp[i]) == float: tmp[i] = tmp[i] - t_prices['V_Price_Transport_SEA-SC_Biodiesel_001']
            prices[key][3:] = tmp

        else:
            # The code adds the cost of transporting crude palm oil from Malaysia to Singapore.
            # Those markups are very vaguely based on https://www.icis.com/resources/news/2013/11/08/9723077/se-asia-to-china-palm-oil-freight-rates-may-fall-on-weak-demand/
            tmp = prices[key][3:]
            # This loop calculates the converted prices.
            for i in range(len(tmp)):
                if type(tmp[i]) == float: tmp[i] = tmp[i] + t_prices['V_Price_Transport_Malaysia-SG_CrudePalmOil_001']
            prices[key][3:] = tmp

    return prices

def chemical_conversion(complexity = 'simple', MoDS_data = 1):
    # This function calculates the number of tonnes of crude palm oil required to produce a metric tonne of biodiesel.
    # Two modes are available: 'simple' (based on literature) and complex (based on an Aspen simulation).
    # The former assumes a constant conversion factor between biodiesel production and crude palm oil consumption.
    # The latter is a marginal analysis around the steady state within the model.

    if complexity == 'simple':
        # This analysis assumes that on average production of 1 tonne of biodiesel requires 1 tonne of crude palm oil.
        CPO_FAME = 1
        return CPO_FAME

    else:
        # This analysis calculates the ratio of tonnes of biodiesel to tonnes of crude palm oil based on the provided Aspen simulation.
        # Both biodiesel and palm oil are provided in kilogrames per hour.
        try:
            CPO = MoDS_data[0]
            FAME = MoDS_data[1]
        except:
            print('Invalid Aspen Plus address. Simple analysis will be performed.')
            return chemical_conversion()
        
        return CPO/FAME

def conversion_cost(MoDS_data, u_prices):
    
    # This function calculates the cost of producing an additional metric tonne of biodiesel based on the average utility consumption.
    # The pricing data has been taken from Martin's spreadsheets of the biodiesel plant and govermental data.
    # Historical conversion rates were taken from https://www.oanda.com/currency/converter/.
    # Adjustment for inflation was done using http://www.usinflationcalculator.com/.
    # Electricity costs (daily average): (0.1727+0.1051)/2 SGD per kWh (HIGH TENSION SMALL (HTS) SUPPLIES https://www.ema.gov.sg/Non_Residential_Programmes_Electricity_Tariffs.aspx)
    # HP Steam (2800kPa, 230C in HYSYS) 2.97 USD per tonne in 2007 (adjusting for inflation 3.49 USD on 09.05.2017)
    # MP Steam (600kPa, 160C in HYSYS) 10.75 USD per tonne in 2007 (adjusting for inflation 12.64 USD per tonne on 09.05.2017) 
    # PROCESS WATER in spreadhsheet 'utilities cost v19 (SCU) excl CPO refining.xls' in 'unit prices' see 'Industrial Water' 0.430 SGD per tonne in 2007 (converting to USD in 2007 and adjusting for inflation 0.33 USD until 09.05.2017)
    # COOLING WATER in spreadhsheet 'utilities cost v19 (SCU) excl CPO refining.xls' in 'total cost' see 'Cooling water makeup' 1.48 USD per tonne  in 2007 (adjusting for inflation 1.74 USD on 09.05.2017)
    # FUEL GAS in spreadhsheet 'utilities cost v19 (SCU) excl CPO refining.xls' in 'unit prices' see 'Fuel gas' 14.85 SGD per mmBTU in 2007 (converting to USD in 2007 and adjusting for inflation 9.8 USD until 09.05.2017)
    
    # Realistic utility costs (in order of significance: COOLING WATER, MP STEAM, HP STEAM, FUEL GAS, PROCESS WATER, Elec)
    
    
    #cost = {'MP STEAM':{'value': 12.64/1000, 'unit':'USD per kg'}, 'PROCESS WATER': {'value':0.33/1000, 'unit':'USD per kg'}, 'COOLING WATER':{'value': 1.74/1000, 'unit':'USD per kg'}, 'Elec': {'value':(0.1727+0.1051)/2/3600, 'unit':'SGD per kJ'}, 'FUEL GAS': {'value':9.8, 'unit':'USD per mmBTU'}, 'HP STEAM':{'value': 3.49/1000, 'unit':'USD per kg'}}

    
    # Read consumption rates from the provided HYSYS simulation
    consumption = {}
    consumption['FAME'] = {'value':MoDS_data[1], 'unit':'kg/hr'}
    # kW are converted to kJ/hr
    consumption['V_Price_Electricity_001'] = {'value':MoDS_data[2]*3600, 'unit':'kJ/hr'}
    consumption['V_Price_CoolingWater_001'] = {'value':MoDS_data[7], 'unit':'kg/hr'}
    # kmol/hr are converted into l/hr (molar volume of methane at STD was taken from http://chemistry.tutorvista.com/inorganic-chemistry/molar-volume.html)
    # l/hr into Mcft/hr (google.com unit converter; 1 Mcft = 1000 cft)
    # Mcft/hr into mmBTU/hr (https://business.directenergy.com/understanding-energy/energy-tools/conversion-factors)
    consumption['V_Price_FuelGas_001'] = {'value':float(MoDS_data[3])*22.4*0.0353147*0.9756, 'unit':'mmBTU/hr'}
    consumption['V_Price_HighPressureSteam_001'] = {'value':MoDS_data[4], 'unit':'kg/hr'}
    consumption['V_Price_MediumPressureSteam_001'] = {'value':MoDS_data[6], 'unit':'kg/hr'}
    consumption['V_Price_ProcessWater_001'] = {'value':MoDS_data[8], 'unit':'kg/hr'}
    
    # Calculate cost per tonne of biodiesel
    total = 0
    for key in u_prices:
        total += consumption[key]['value']*u_prices[key]
    
    return total/consumption['FAME']['value']

def look_for_munnies(MoDS_data, prices, dates,s_prices,u_prices):
    
    # This function performs puts together all the data and looks for the most profitable arbitrage opportunity using a brute force approach (i.e. checking all possible combintations subject to chronological order constraint).

    # Natural gas to menthanol conversion ratio (mmBTU to tonnes) and the average conversion cost per tonne of methanol are retrieved with the code below.
    # In case this cannot be done a simple analysis will be performed.

    # Retrieve the average conversion cost per tonne of methanol
    conv = conversion_cost(MoDS_data,u_prices)
    #how many NG contracts are needed to fulfill 1 MeoH contract
    CPO_FAME = chemical_conversion('complex', MoDS_data)
    # The storage cost per month-tonne are retrieved
    storage = {'CPO':s_prices['V_Price_Storage_CrudePalmOil_001'], 'FAME':s_prices['V_Price_Storage_Biodiesel_001']}

    # This loop searches through opportunities given that all ncrude palm oil is stored and that conversion is instantaneous
    lowest_diff = {'price difference': -inf, 'month_CPO':None, 'month_FAME':None}
    for j in range(3,len(prices['FAME'])):
        if prices['FAME'][j] == None: continue
        for i in range(3,len(prices['CPO'])):
            if prices['CPO'][i] == None: continue
            if i+1 >= j: continue
            diff = prices['FAME'][j] - CPO_FAME*(prices['CPO'][i] + storage['CPO']*(j-i)) - conv
            #print(diff)
            if diff > lowest_diff['price difference']: lowest_diff = {'price difference': diff, 'month_CPO':dates['CPO'][i], 'month_FAME':dates['CPO'][j-1], 'note':'Note that all crude palm oil was stored and instantaneously converted on delivery date.' }

    # This loop searches through opportunities given that all biodiesel is stored and that conversion is instantaneous
    for j in range(3,len(prices['FAME'])):
        if prices['FAME'][j] == None: continue
        for i in range(3,len(prices['CPO'])):
            if prices['CPO'][i] == None: continue
            if i+1 >= j: continue
            diff = prices['FAME'][j] - CPO_FAME*prices['CPO'][i] - storage['FAME']*(j-i) - conv
            #print(diff)
            if diff > lowest_diff['price difference']: lowest_diff = {'price difference': diff, 'month_CPO':dates['CPO'][i], 'month_FAME':dates['CPO'][j-1], 'note':'Note that all crude palm oil was instantaneously converted on arrival and biodiesel FAME was stored until delivery date.'}
    
    resultsDict = {
        "marginal profit per tonne of biodiesel FAME (in USD)": str(round(lowest_diff['price difference'],2)),
        "ratio of reagent to product": str(CPO_FAME/prices['CPO'][2]*prices['FAME'][2]),
        "month to buy crude palm oil futures contracts": lowest_diff['month_CPO'],
        "month to sell biodiesel FAME futures contract": lowest_diff['month_FAME'],
        "note": lowest_diff['note']
    }
    
    return json.dumps(resultsDict)
#     print('The highest marginal profit per tonne of  biodiesel FAME is', round(lowest_diff['price difference'],2), 
#           'USD. The futures contracts need to be accepted at the following ratio of reagent to product:', CPO_FAME/prices['CPO'][2]*prices['FAME'][2], 
#           '. Buy crude palm oil futures contracts with delivery in', lowest_diff['month_CPO'], 
#           'and sell biodiesel FAME futures contracts with delivery in', lowest_diff['month_FAME'],'.', lowest_diff['note'])

def plotting_prices(dates, prices, labels):

    # This function plots the futures prices for two commodities. It assumes that there may be None values between the headers and the prices. It searches for them and they are excluded from the plot.

    # The loop and if statements below segregate the keys based on the length of the dates array.
    keys = []
    for key in dates:
        keys.append(key)

    if len(dates[keys[0]]) > len(dates[keys[1]]):
        key_1 = keys[1]
        key_2 = keys[0]
    else:
        key_1 = keys[0]
        key_2 = keys[1]

    # The lower and upper bounds are calculated based on the number of None values within the prices array.
    lower_bound = 3
    upper_bound = len(dates[key_1])

    # Loop counting None values between the headers and prices
    for item in prices[key_1][3:]:
        if item == None: lower_bound += 1
        else: break

    # Loop counting None values at the back. The prices array needs to be reversed for the loop and then reversed back to the original
    prices[key_1].reverse()
    for item in prices[key_1]:
        if item == None: upper_bound -= 1
        else: break
    prices[key_1].reverse()

    # x-axis labels are defined from a user array. Here it depends on the outcome of the above calculations.
    x_labels = labels['x']['label'][lower_bound:upper_bound]

    # Changing font size
    #plt.rcParams.update({'font.size': 22})
    
    # The lines below put together a figure containing two plots, one for each commodity. Labels and titles come from the user-defined dictionary.
    plt.figure(figsize=(20.0, 12.5))
    plt.subplot(212)
    plt.xlabel(labels['x']['title'])
    plt.ylabel(labels[key_1]['label'])
    plt.title(labels[key_1]['title'])
    plt.xticks(range(len(x_labels)),x_labels)
    # The line below defines the style. For more information see Python manual.
    plt.plot(prices[key_1][lower_bound:upper_bound], 'k', prices[key_1][lower_bound:upper_bound], 'bo')

    plt.subplot(211)
    plt.ylabel(labels[key_2]['label'])
    plt.title(labels[key_2]['title'])
    plt.xticks(range(len(x_labels)),[])
    # The line below defines the style. For more information see Python manual.
    plt.plot(prices[key_2][lower_bound:upper_bound], 'k', prices[key_2][lower_bound:upper_bound], 'ro')
    plt.show()
    #plt.savefig(r'C:\Users\Janusz\Desktop\Commodity_prices\Market_data\arbitrage_CPO.png')
    #plt.savefig(plot_address)

#def run(plot_address, data):
def run(MoDS_data, miscCosts, cpo, fame):
    MoDS_data = MoDS_data.split(',')
    for i in range(len(MoDS_data)):
        MoDS_data[i] = float(MoDS_data[i])

    # Read in and process data into an appropriate format
    dates, prices, u_prices, t_prices, ex_rates, s_prices = preprocessing(miscCosts, cpo, fame)
    u_prices['V_Price_Electricity_001'] /= ex_rates['V_USD_to_SGD']

    # Adjust prices to include the transport cost
    prices = transport_costs(prices, t_prices)

    # Search through the arbitrage opportunities
    return look_for_munnies(MoDS_data, prices, dates, s_prices, u_prices)

    # Define titles and labels to plot the futures prices data and plot the data
    #labels = {'FAME':{'title':'Biodiesel FAME futures prices from Chicago Mercantile Exchange', 'label':'Price (USD per tonne)'},'CPO':{'title':'Crude palm oil futures prices from Chicago Mercantile Exchange', 'label':'Price (USD per tonne)'}, 'x':{'title':'Delivery date (-)', 'label':dates['FAME']}}
    #plotting_prices(dates, prices, labels)

if __name__ == "__main__":

        pythonLogger = PythonLogger('CPO_to_FAME_MoDS2.py')
        pythonLogger.postInfoToLogServer('start of CPO_to_FAME_MoDS2.py')

        MoDS_data = str(sys.argv[1])
        miscCosts = sys.argv[2]
        cpo = json.loads(sys.argv[3])
        fame = json.loads(sys.argv[4])
    try:
        returnResultsToJava(run(MoDS_data, miscCosts, cpo, fame))
    except Exception as e:
        import traceback
        pythonLogger.postInfoToLogServer(traceback.print_exc())
        returnExceptionToJava(e)
    finally:
        pythonLogger.postInfoToLogServer('end of CPO_to_FAME_MoDS2.py')