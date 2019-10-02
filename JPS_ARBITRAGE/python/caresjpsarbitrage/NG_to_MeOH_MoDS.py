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
import json
 
from caresjpsutil import returnExceptionToJava, returnResultsToJava
from caresjpsutil import PythonLogger

def preprocessing(miscCosts, hng, zce):
    dates = {}
    prices = {}
    u_prices = {}
    t_prices = {}
    ex_rates = {}
    s_prices = {}
 
    dates[hng['arrayHeader'][0]] = hng['arrayHeader'][1:] + hng['arrayMonths']
    dates[zce['arrayHeader'][0]] = zce['arrayHeader'][1:] + zce['arrayMonths']
 
    prices[hng['arrayHeader'][0]] = [hng['arrayDatetime'][0][5:]] + hng['arrayDatetime'][1:] + hng['arrayPrices']
    prices[zce['arrayHeader'][0]] = [zce['arrayDatetime'][0][5:]] + zce['arrayDatetime'][1:] + zce['arrayPrices']
 
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
        elif key.rfind('Transport') != -1 and key.rfind('NaturalGas') != -1:
            t_prices[key] = miscCosts[key]
            miscCosts[key] = '1.0'
        elif key.rfind('Transport') != -1:
            t_prices[key] = float(miscCosts[key])
        else:
            u_prices[key] = float(miscCosts[key])
 
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
     
  
def look_for_munnies(MoDS_data, prices, dates, s_prices, u_prices):
     
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
 
    resultsDict = {
        "marginal profit per tonne of methanol (in USD)": str(round(lowest_diff['price difference'], 2)),
        "ratio of reagent to product": str(NG_MeOH/prices['NG'][2]*prices['MeOH'][2]),
        "month to buy natural gas futures contracts": lowest_diff['month_NG'],
        "month to sell methanol futures contract": lowest_diff['month_MeOH'],
        "note": lowest_diff['note']
    }
 
    return json.dumps(resultsDict)
 
    # print('The highest marginal profit per tonne of methanol is',
    #       round(lowest_diff['price difference'],2),
    #       'USD. The futures contracts need to be accepted at the following ratio of reagent to product:',
    #       NG_MeOH/prices['NG'][2]*prices['MeOH'][2],
    #       '. Buy natural gas futures contracts with delivery in',
    #       lowest_diff['month_NG'],
    #       'and sell methanol futures contracts with delivery in',
    #       lowest_diff['month_MeOH'],
    #       '.',
    #       lowest_diff['note'])
 
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
 
def run(MoDS_data, miscCosts, hng, zce):
    MoDS_data = MoDS_data.split(',')
    for i in range(len(MoDS_data)):
        MoDS_data[i] = float(MoDS_data[i])
         
    # Read in and process data into an appripriate format
    dates, prices, u_prices, t_prices, ex_rates, s_prices = preprocessing(miscCosts, hng, zce)
    u_prices['V_Price_Electricity_001'] /= ex_rates['V_USD_to_SGD']
     
    for i in range(3, len(prices["MeOH"])):
        prices["MeOH"][i] /= ex_rates['V_USD_to_CNY']
    prices["MeOH"][1] = 'Prior Settlement (USD per tonne)'
     
    # Adjust prices to include the transport cost
    prices = transport_costs(prices, t_prices)
 
    # Search through the arbitrage opportunities
    return look_for_munnies(MoDS_data, prices, dates, s_prices, u_prices)    
 
    # Define titles and labels to plot the futures prices data and plot the data
    #labels = {'NG':{'title':'Natural gas futures prices from Chicago Mercantile Exchange', 'label':'Price (USD per mmBTU)'},'MeOH':{'title':'Methanol futures prices from Zhengzhou Commodity Exchange', 'label':'Price (USD per tonne)'}, 'x':{'title':'Delivery date (-)', 'label':dates['NG']}}
    #plotting_prices(dates, prices, labels)
 
if __name__ == "__main__":
    pythonLogger = PythonLogger('NG_to_MeOH_MoDS.py')
    pythonLogger.postInfoToLogServer('start of NG_to_MeOH_MoDS.py')
    
    MoDS_data = str(sys.argv[1])
    miscCosts = json.loads(sys.argv[2])
    hng = json.loads(sys.argv[3])
    zce = json.loads(sys.argv[4])
    
    try:
        returnResultsToJava(run(MoDS_data, miscCosts, hng, zce))
    except Exception as e:
        returnExceptionToJava(e)
    finally:
        pythonLogger.postInfoToLogServer('end of NG_to_MeOH_MoDS.py')
 
#     run("0.5527777778,-1.9678704734013681E43,-1.947340493583137E67,-1.3064212763820435E47,3063955.568812896,0.0,-1.312550425729447E46,-1.8126031951762418E54,-1.052184254037493E43",
#         json.loads('{"V_Price_Electricity_001":"0.0000385833","V_Price_Storage_NaturalGas_001":"0.11104","V_Price_FuelGas_001":"9.8","V_USD_to_CNY":"6.4757816642","V_USD_to_SGD":"1.3582620112","V_Price_Transport_USGC-NEA_NaturalGas_001":"6.571428571_1.142857143","V_Price_Transport_SG-SC_Methanol_001":"22.9","V_Price_CoolingWater_001":"0.00174","V_Price_ProcessWater_001":"0.00033","V_Price_MediumPressureSteam_001":"0.01264","V_Price_HighPressureSteam_001":"0.00349","V_Price_Storage_Methanol_001":"0.2075"}'),
#         json.loads('{"arrayHeader": ["NG", "Date", "Price type", "Size (mmBTU)"], "arrayMonths": ["JUL 2018", "AUG 2018", "SEP 2018", "OCT 2018", "NOV 2018", "DEC 2018", "JAN 2019", "FEB 2019", "MAR 2019", "APR 2019", "MAY 2019", "JUN 2019", "JUL 2019", "AUG 2019", "SEP 2019", "OCT 2019", "NOV 2019", "DEC 2019", "JAN 2020", "FEB 2020", "MAR 2020", "APR 2020", "MAY 2020", "JUN 2020", "JUL 2020", "AUG 2020", "SEP 2020", "OCT 2020", "NOV 2020", "DEC 2020", "JAN 2021", "FEB 2021", "MAR 2021", "APR 2021", "MAY 2021", "JUN 2021", "JUL 2021", "AUG 2021", "SEP 2021", "OCT 2021", "NOV 2021", "DEC 2021", "JAN 2022", "FEB 2022", "MAR 2022", "APR 2022", "MAY 2022", "JUN 2022", "JUL 2022", "AUG 2022", "SEP 2022", "OCT 2022", "NOV 2022", "DEC 2022", "JAN 2023", "FEB 2023", "MAR 2023", "APR 2023", "MAY 2023", "JUN 2023", "JUL 2023", "AUG 2023", "SEP 2023", "OCT 2023", "NOV 2023", "DEC 2023", "JAN 2024", "FEB 2024", "MAR 2024", "APR 2024", "MAY 2024", "JUN 2024", "JUL 2024", "AUG 2024", "SEP 2024", "OCT 2024", "NOV 2024", "DEC 2024", "JAN 2025", "FEB 2025", "MAR 2025", "APR 2025", "MAY 2025", "JUN 2025", "JUL 2025", "AUG 2025", "SEP 2025", "OCT 2025", "NOV 2025", "DEC 2025", "JAN 2026", "FEB 2026", "MAR 2026", "APR 2026", "MAY 2026", "JUN 2026", "JUL 2026", "AUG 2026", "SEP 2026", "OCT 2026", "NOV 2026", "DEC 2026", "JAN 2027", "FEB 2027", "MAR 2027", "APR 2027", "MAY 2027", "JUN 2027", "JUL 2027", "AUG 2027", "SEP 2027", "OCT 2027", "NOV 2027", "DEC 2027", "JAN 2028", "FEB 2028", "MAR 2028", "APR 2028", "MAY 2028", "JUN 2028"], "arrayDatetime": ["Tue, 19 Jun 2018 10:48:18 GMT", "Prior Settlement (USD per mmBTU)", "10.0"], "arrayPrices": ["2.951", "2.951", "2.932", "2.944", "2.986", "3.095", "3.183", "3.154", "3.056", "2.677", "2.639", "2.665", "2.694", "2.701", "2.687", "2.702", "2.755", "2.880", "2.979", "2.945", "2.854", "2.547", "2.520", "2.548", "2.579", "2.587", "2.573", "2.595", "2.650", "2.780", "2.879", "2.845", "2.757", "2.512", "2.487", "2.515", "2.545", "2.556", "2.554", "2.580", "2.641", "2.781", "2.889", "2.862", "2.793", "2.556", "2.535", "2.561", "2.589", "2.602", "2.603", "2.631", "2.692", "2.837", "2.953", "2.928", "2.863", "2.633", "2.615", "2.640", "2.667", "2.684", "2.686", "2.714", "2.775", "2.921", "3.037", "3.012", "2.947", "2.707", "2.689", "2.714", "2.743", "2.760", "2.762", "2.790", "2.851", "2.997", "3.113", "3.088", "3.023", "2.783", "2.764", "2.789", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "2.691", "2.821", "2.875"]}'),
#         json.loads('{"arrayHeader": ["MeOH", "Date", "Price type", "Size (tonne)"], "arrayMonths": ["JUL 2018", "AUG 2018", "SEP 2018", "OCT 2018", "NOV 2018", "DEC 2018", "JAN 2019", "FEB 2019", "MAR 2019", "APR 2019", "MAY 2019", "JUN 2019"], "arrayDatetime": ["Tue, 19 Jun 2018 10:48:08 GMT", "Prior Settlement (CNY per tonne)", "1.0"], "arrayPrices": ["2755.0", "2816.0", "2818.0", "2795.0", "2869.0", "2909.0", "2903.0", "2871.0", "2864.0", "2827.0", "2734.0", "2749.0"]}'))