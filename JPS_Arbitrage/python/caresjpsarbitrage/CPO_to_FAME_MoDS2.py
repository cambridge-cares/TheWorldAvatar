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


def preprocessing(data):

    data = data.split('&')

    for i in range(len(data)):
        if data[i][-1] == ",":
            data[i] = data[i][:-1]
        
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
        for j in range(2, len(prices[key])):
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
        if data[0][i].rfind('Transport') != -1:
            t_prices[data[0][i]] = float(data[0][i+1])
            continue
        try:
            float(data[0][i])
        except:
            u_prices[data[0][i]] = float(data[0][i+1])
    
    
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
    
    print('The highest marginal profit per tonne of  biodiesel FAME is', round(lowest_diff['price difference'],2), 'USD. The futures contracts need to be accepted at the following ratio of reagent to product:',CPO_FAME/prices['CPO'][2]*prices['FAME'][2], '. Buy crude palm oil futures contracts with delivery in', lowest_diff['month_CPO'], 'and sell biodiesel FAME futures contracts with delivery in', lowest_diff['month_FAME'],'.', lowest_diff['note'])

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
def run(MoDS_data, data):

    MoDS_data = MoDS_data.split(',')
    for i in range(len(MoDS_data)):
        MoDS_data[i] = float(MoDS_data[i])
        
    # Read in and process data into an appropriate format
    dates, prices, u_prices, t_prices, ex_rates, s_prices = preprocessing(data)
    u_prices['V_Price_Electricity_001'] /= ex_rates['V_USD_to_SGD']
    
    # Adjust prices to include the transport cost
    prices = transport_costs(prices, t_prices)

    # Search through the arbitrage opportunities
    look_for_munnies(MoDS_data, prices, dates, s_prices, u_prices)

    # Define titles and labels to plot the futures prices data and plot the data
    #labels = {'FAME':{'title':'Biodiesel FAME futures prices from Chicago Mercantile Exchange', 'label':'Price (USD per tonne)'},'CPO':{'title':'Crude palm oil futures prices from Chicago Mercantile Exchange', 'label':'Price (USD per tonne)'}, 'x':{'title':'Delivery date (-)', 'label':dates['FAME']}}
    #plotting_prices(dates, prices, labels)



if __name__ == "__main__":
    # run(str(sys.argv[1]), str(sys.argv[2]))
    run("24220.0656,24334.440322240484,4.458421599656575,22.296794453326125,0.0,0.0,10034.322882179931,29191.613776154845,3062.5976",
        "V_Price_CoolingWater_001,0.00174,V_Price_Storage_Biodiesel_001,0.2075,V_Price_Storage_CrudePalmOil_001,0.2075,V_Price_Transport_Malaysia-SG_CrudePalmOil_001,5.0,V_Price_Electricity_001,0.0000385833,V_USD_to_SGD,0.74934896187518,V_Price_ProcessWater_001,0.00033,V_Price_HighPressureSteam_001,0.00349,V_Price_MediumPressureSteam_001,0.01264,V_Price_Transport_SEA-SC_Biodiesel_001,40.0,V_Price_FuelGas_001,9.8,&CPO,Date,Price type,Size (tonne),JUN 2018,JUL 2018,AUG 2018,SEP 2018,OCT 2018,NOV 2018,DEC 2018,JAN 2019,FEB 2019,MAR 2019,APR 2019,MAY 2019,JUN 2019,JUL 2019,AUG 2019,SEP 2019,OCT 2019,NOV 2019,DEC 2019,JAN 2020,FEB 2020,MAR 2020,APR 2020,MAY 2020,JUN 2020,JUL 2020,AUG 2020,SEP 2020,OCT 2020,NOV 2020,DEC 2020,JAN 2021,FEB 2021,MAR 2021,APR 2021,MAY 2021,JUN 2021,JUL 2021,AUG 2021,SEP 2021,OCT 2021,NOV 2021,DEC 2021,JAN 2022,FEB 2022,MAR 2022,APR 2022,MAY 2022,JUN 2022,JUL 2022,AUG 2022,SEP 2022,OCT 2022,NOV 2022,DEC 2022,JAN 2023,FEB 2023,MAR 2023,APR 2023,MAY 2023,JUN 2023&Thu, 14 Jun 2018 08:27:43 GMT,Prior Settlement (USD per tonne),25.0,589.50,583.75,584.00,587.25,592.75,600.25,605.25,608.00,613.00,617.25,617.00,617.00,617.25,617.50,617.25,617.00,618.25,619.75,619.00,618.50,618.00,617.25,616.75,616.25,615.50,615.00,614.25,613.75,613.00,612.50,612.00,611.25,610.75,610.25,609.50,608.75,608.00,607.25,606.50,605.75,605.00,604.25,603.50,602.75,602.00,601.25,600.50,599.75,599.00,598.25,597.50,596.75,596.00,595.25,594.50,593.75,593.00,592.25,591.50,590.75,-,&FAME,Date,Price type,Size (tonne),JUN 2018,JUL 2018,AUG 2018,SEP 2018,OCT 2018,NOV 2018,DEC 2018,JAN 2019,FEB 2019,MAR 2019,APR 2019,MAY 2019,JUN 2019,JUL 2019,AUG 2019,SEP 2019,OCT 2019,NOV 2019,DEC 2019,JAN 2020,FEB 2020,MAR 2020,APR 2020,MAY 2020,JUN 2020&Thu, 14 Jun 2018 08:27:52 GMT,Prior Settlement (USD per tonne),100.0,850.500,854.932,854.402,854.088,832.859,831.057,830.013,843.068,842.188,840.417,854.286,853.826,853.750,842.263,842.263,842.263,842.263,842.263,0.000,0.000,0.000,0.000,0.000,0.000,-,")

