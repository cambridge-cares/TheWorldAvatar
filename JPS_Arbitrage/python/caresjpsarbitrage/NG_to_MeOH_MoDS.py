##
# @file
# File documentation
# This file is a set of functions designed to work as an arbitrage spotter for a chemical plant converting a reagent into a product. The spotter looks through the chemical futures markets for profitable opportunities to purchase the reagent, convert it into the product and then sell the product. It factors in the costs of transport, storage and conversion and looks for the cheapest schedule. The conversion costs are based on an Aspen simulation of a relevant conversion process.
# Important assumptions include instantenaous conversion and transport, pricing other than futures is based on the literature, only marginal profit is calculated (this is liable to changes soon). 
# Additionally, it i assumed that the plant is already functioning on a long-term contract. For that reason the changes to plant's function cannot be too great and capital costs may be ignored in the calculations.

import requests, sys
from math import inf
#import matplotlib.pyplot as plt
from csv_funcs import RCSV, ACSV

def preprocessing(data):

    data = data.split('&')

    for i in range(len(data)):
        if data[i][-1] == ",": data[i] = data[i][:-1]
        
    dates = {}
    prices = {}
    u_prices = {}
    t_prices = {}
    ex_rates = {}
    s_prices = {}

    data[1] = data[1].split(",")
    dates[data[1][0]] = data[1][1:]

    data[3] = data[3].split(",")
    dates[data[3][0]] = data[3][1:]

    data[2] = data[2].split(",")
    prices[data[1][0]] = data[2][1:]

    data[4] = data[4].split(",")
    prices[data[3][0]] = data[4][1:]


    for key in prices:
        for j in range(2,len(prices[key])):
            try:
                prices[key][j] = float(prices[key][j])
                if float(prices[key][j]) == 0.0: 
                    prices[key][j] = None
                    continue                
            except:
                prices[key][j] = None

                
    data[0] = data[0].split(",")
    for i in range(len(data[0])):
        if data[0][i].rfind('USD') != -1:
            ex_rates[data[0][i]] = float(data[0][i+1])
            continue
        if data[0][i].rfind('Storage') != -1:
            s_prices[data[0][i]] = float(data[0][i+1])
            continue
        if data[0][i].rfind('Transport') != -1 and data[0][i].rfind('NaturalGas') != -1:
            t_prices[data[0][i]] = data[0][i+1]
            data[0][i+1] = '1.0'
            continue
        if data[0][i].rfind('Transport') != -1:
            t_prices[data[0][i]] = float(data[0][i+1])
            continue
        try: float(data[0][i])
        except: u_prices[data[0][i]] = float(data[0][i+1])
    
    
    return dates, prices, u_prices, t_prices, ex_rates, s_prices
 
def transport_costs(prices, t_prices):
    # This function adds transportation cost to the prices of the reagent and subtracts the transportation cost from the prices of the product. 
    for key in prices:
        if key == 'MeOH':
            # The code adds the cost of transporting methanol from the Singapore to North-eastern Asia.
            # Those markups are calculatd based on the the transportation from the US to Asia (https://www.methanex.com/sites/default/files/investor/MEOH%20Presentation%20-%20June%202016_0.pdf)

            tmp = prices[key][3:]
            # This loop calculates the converted prices.
            for i in range(len(tmp)):
                if type(tmp[i]) == float: tmp[i] = tmp[i] - t_prices['V_Price_Transport_SG-SC_Methanol_001']
            prices[key][3:] = tmp

        else:
            
            function = t_prices['V_Price_Transport_USGC-NEA_NaturalGas_001'].split('_')
            # The code adds the cost of transporting natural gas from the US Gulf Coast (or US North-western coast) to North-eastern Asia.
            # Those markups are calculatd based on the spot price at Henry Hub in the US. This is based on Competitive gas pricing: oil-linked versus spot delivered to NE Asia https://webforms.ey.com/Publication/vwLUAssets/Competing-for-LNG-demand/$FILE/Competing-for-LNG-demand-pricing-structure-debate.pdf)
            # For transport from USGC
            #a = 1.142857143
            #b = 6.571428571
            # For USNW b = 4.571428571
            
            
            tmp = prices[key][3:]
            # This loop calculates the converted prices.
            for i in range(len(tmp)):
                if type(tmp[i]) == float: tmp[i] = tmp[i]*float(function[1]) + float(function[0])
            prices[key][3:] = tmp

    return prices



def chemical_conversion(complexity = 'simple',MoDS_data=1):
    # This function calculates the number of mmBTU of natural gas required to produce a metric tonne of methanol.
    # Two modes are available: 'simple' (based on literature) and complex (based on an Aspen simulation).
    # The former assumes a constant conversion factor between methanol production and natural gas consumption.
    # The latter is a marginal analysis around the steady state within the model.

    if complexity == 'simple':
        # This analysis assumes that on average 29.5 mmBTU of natural gas are required to produce a tonne of methanol.
        # This is based on http://petrowiki.org/Gas_to_methanol#cite_note-r1-1, where it is stated that the typical gas consumption for a world-scale methanol plant ranges from 28 to 31  mmBTU per metric tonne of methanol based on LHV of the natural gas.
        NG_MeOH = 29.5
        return NG_MeOH

    else:
        # This analysis calculates the ratio of mmBTU of natural gas to tonnes of methanol based on the provided Aspen simulation.
        # The simulation provides natural gas in kmol per second (this is converted to cubic meters, then cubic feet and then to mmBTU based on google.com's unt converter and https://business.directenergy.com/understanding-energy/energy-tools/conversion-factors.
        # Methanol is provided in kilogrames per second (this is converted to tonnes per second).
        try:
            NG = MoDS_data[0]*23.64443643412583*35.3147*0.9756/1000
            MeOH = MoDS_data[1]/1000
        except:
            print('Invalid Hysys address. Simple analysis will be performed.')
            return chemical_conversion()
        
        return NG/MeOH

        
def conversion_cost(MoDS_data, u_prices):
    
    # This function calculates the cost of producing an additional metric tonne of methanol based on the average utility consumption.
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

    
    # Read consumption rates from the provided HYSYS simulation
    consumption = {}
    consumption['MeOH'] = {'value':MoDS_data[1], 'unit':'kg/hr'}
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
    
    return total/consumption['MeOH']['value']
    
 
def look_for_munnies(MoDS_data, prices, dates,s_prices,u_prices):
    
    # This function performs puts together all the data and looks for the most profitable arbitrage opportunity using a brute force approach (i.e. checking all possible combintations subject to chronological order constraint).

    # Natural gas to menthanol conversion ratio (mmBTU to tonnes) and the average conversion cost per tonne of methanol are retrieved with the code below.
    # In case this cannot be done a simple analysis will be performed.

    # Retrieve the average conversion cost per tonne of methanol
    conv = conversion_cost(MoDS_data,u_prices)
    #how many NG contracts are needed to fulfill 1 MeoH contract
    NG_MeOH = chemical_conversion('complex', MoDS_data)
    # The storage cost per month-mmBTU/tonne are retrieved
    storage = {'NG':s_prices['V_Price_Storage_NaturalGas_001'], 'MeOH':s_prices['V_Price_Storage_Methanol_001']}
    
    
    
    # This loop searches through opportunities given that all natural gas is stored and that conversion is instantaneous
    lowest_diff = {'price difference': -inf, 'month_NG':None, 'month_MeOH':None}
    for j in range(3,len(prices['MeOH'])):
        if prices['MeOH'][j] == None: continue
        for i in range(3,len(prices['NG'])):
            if prices['NG'][i] == None: continue
            if i+1 >= j: continue
            diff = prices['MeOH'][j] - NG_MeOH*(prices['NG'][i] + storage['NG']*(j-i)) - conv
            if diff > lowest_diff['price difference']: lowest_diff = {'price difference': diff, 'month_NG':dates['NG'][i], 'month_MeOH':dates['NG'][j-1], 'note':'Note that all natural gas was stored and instantaneously converted on delivery date.' }

    # This loop searches through opportunities given that all methanol is stored and that conversion is instantaneous
    for j in range(3,len(prices['MeOH'])):
        if prices['MeOH'][j] == None: continue
        for i in range(3,len(prices['NG'])):
            if prices['NG'][i] == None: continue
            if i+1 >= j: continue
            diff = prices['MeOH'][j] - NG_MeOH*prices['NG'][i] - storage['MeOH']*(j-i) - conv
            if diff > lowest_diff['price difference']: lowest_diff = {'price difference': diff, 'month_NG':dates['NG'][i], 'month_MeOH':dates['NG'][j-1], 'note':'Note that all natural gas was instantaneously converted on arrival and methanol was stored until delivery date.'}

    print('The highest marginal profit per tonne of methanol is', round(lowest_diff['price difference'],2), 'USD. The futures contracts need to be accepted at the following ratio of reagent to product:',NG_MeOH/prices['NG'][2]*prices['MeOH'][2], '. Buy natural gas futures contracts with delivery in', lowest_diff['month_NG'], 'and sell methanol futures contracts with delivery in', lowest_diff['month_MeOH'],'.', lowest_diff['note'])

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
    plt.figure(1)
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

def run(MoDS_data, data):
    
    MoDS_data = MoDS_data.split(',')
    for i in range(len(MoDS_data)):
        MoDS_data[i] = float(MoDS_data[i])
        
    # Read in and process data into an appripriate format
    dates, prices, u_prices, t_prices, ex_rates, s_prices = preprocessing(data)
    u_prices['V_Price_Electricity_001'] /= ex_rates['V_USD_to_SGD']
    
    for i in range(3,len(prices["MeOH"])):
        prices["MeOH"][i] /= ex_rates['V_USD_to_CNY']
    prices["MeOH"][1] = 'Prior Settlement (USD per tonne)'
    
    # Adjust prices to include the transport cost
    prices = transport_costs(prices, t_prices)

    # Search through the arbitrage opportunities
    look_for_munnies(MoDS_data, prices, dates, s_prices, u_prices)    


    # Define titles and labels to plot the futures prices data and plot the data
    #labels = {'NG':{'title':'Natural gas futures prices from Chicago Mercantile Exchange', 'label':'Price (USD per mmBTU)'},'MeOH':{'title':'Methanol futures prices from Zhengzhou Commodity Exchange', 'label':'Price (USD per tonne)'}, 'x':{'title':'Delivery date (-)', 'label':dates['NG']}}
    #plotting_prices(dates, prices, labels)

if __name__ == "__main__":
    run(str(sys.argv[1]),str(sys.argv[2]))


