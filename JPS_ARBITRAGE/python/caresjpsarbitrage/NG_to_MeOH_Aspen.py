##
# @file
# File documentation
# This file is a set of functions designed to work as an arbitrage spotter for a chemical plant converting a reagent into a product. The spotter looks through the chemical futures markets for profitable opportunities to purchase the reagent, convert it into the product and then sell the product. It factors in the costs of transport, storage and conversion and looks for the cheapest schedule. The conversion costs are based on an Aspen simulation of a relevant conversion process.
# Important assumptions include instantenaous conversion and transport, pricing other than futures is based on the literature, only marginal profit is calculated (this is liable to changes soon). 
# Additionally, it i assumed that the plant is already functioning on a long-term contract. For that reason the changes to plant's function cannot be too great and capital costs may be ignored in the calculations.

import win32api, win32com.client as win32, requests
from lxml import html
from math import inf
import matplotlib.pyplot as plt
from csv_funcs import RCSV, ACSV

def exchange_rates(prices, currencies):
    # This function converts an array of prices from one currency to another. It accepts an array with floats (but skips over non-floats) and an array with two strings. The latter need to be the correct currency codenames as per www.xe.com as this function will download the rates from there.
    # The first entry in the array with codenames corresponds to the present currency, while the second to the target currency. 

    # Url is being formulated
    url = 'http://www.xe.com/currencyconverter/convert/?Amount=1&From='+currencies[0]+'&To='+currencies[1]
    
    # requests library is used to download the source code of the page with exchange rates. The code is then parsed as html.
    page = requests.get(url)
    tree = html.fromstring(page.content)
    page.close()

    # lxml library is used to search through the html file using its structure and attributes
    exchange_rate = float(tree.xpath('//span[@class="uccResultUnit"]')[0].attrib['data-amount'])

    # This loop calculates the converted prices if they are in an array.
    if type(prices) == list:
        for i in range(len(prices)):
               if type(prices[i]) == float: prices[i] *= exchange_rate
    elif type(prices) == dict:
        for key in prices:
            if  prices[key]['unit'][:3] == currencies[1]:
                continue
            else:
                prices[key]['value'] *= exchange_rate
                prices[key]['unit'] = currencies[1]+prices[key]['unit'][3:]
    return prices

def preprocessing(file_addresses):
    
    # This function reads pricing data from .csv files in 'file_addresses', converts numbers from strings to floats and CNY to USD and sorts prices and their timestamps into separate dictionaries
    # It accepts a dictionary where keys are names of the chemicals and values are the file addresses.

    # Create variables to hold the read data
    prices, dates = {}, {}
    for key in file_addresses:
        prices[key] = []
        dates[key] = []

    # Read and store data in arrays in the dictionaries
    for key in file_addresses:
        prices[key], dates[key] = RCSV(file_addresses[key])
        # This line discards historical data  by trimming off just the last line
        prices[key] = prices[key][-len(dates[key]):] 

    
    # This loops convert the contract size and the prices from strings to floats. It is assumed that the first two entries are not numbers, but headers.
    # If a string within the range is not a number or a 0, then it is store as a None.
    for key in file_addresses:
        for i in range(2,len(prices[key])):
            try:
                prices[key][i] = float(prices[key][i])
                if  prices[key][i] == 0:  prices[key][i] = None
            except ValueError:
                prices[key][i] = None

    return dates, prices
 
def transport_costs(prices):
    # This function adds transportation cost to the prices of the reagent and subtracts the transportation cost from the prices of the product. 
    for key in prices:
        if key == 'NG':
            # The code adds the cost of transporting natural gas from the US Gulf Coast (or US North-western coast) to North-eastern Asia.
            # Those markups are calculatd based on the spot price at Henry Hub in the US. This is based on Competitive gas pricing: oil-linked versus spot delivered to NE Asia https://webforms.ey.com/Publication/vwLUAssets/Competing-for-LNG-demand/$FILE/Competing-for-LNG-demand-pricing-structure-debate.pdf)
    
            # For transport from USGC
            a = 1.142857143
            b = 6.571428571

            tmp = prices[key][3:]
            # This loop calculates the converted prices.
            for i in range(len(tmp)):
                if type(tmp[i]) == float: tmp[i] = tmp[i]*a+b
            prices[key][3:] = tmp
            # For USNW b = 4.571428571

        else:
            # The code adds the cost of transporting methanol from the Singapore to North-eastern Asia.
            # Those markups are calculatd based on the the transportation from the US to Asia (https://www.methanex.com/sites/default/files/investor/MEOH%20Presentation%20-%20June%202016_0.pdf)
            tmp = prices[key][3:]
            # This loop calculates the converted prices.
            for i in range(len(tmp)):
                if type(tmp[i]) == float: tmp[i] = tmp[i] - 80/3.5
            prices[key][3:] = tmp

    return prices
        
def storage_cost(prices = None):
    # This function desribes two option for pricing storage of natural gas and methanol.
    # Option 1 is entirely based on literature and data from the biodiesel plant on Jurong Island provided by Martin (for details see below).
    # Option 2 is based on the futures prices where it is assumed that the differences in chronologically subsequent prices approximates storage prices.
    
    
    # Option 1 - Literature
    # Storage and associated costs per year per tonne of methanol is 2.70 USD in 2017 (extrapolated from Martin's documents by adjusting for inflation using http://www.usinflationcalculator.com/ on 09.05.2017; ~3.5SGD or ~2.3USD in 2007  (price range 3-4 SGD in 2007); conversion from SGD to USD based on historical rates from http://www.x-rates.com/historical/?from=SGD&amount=1&date=2007-05-01)
    # Storage (and associated) costs per half-year per MCF (or 0.9756 mmBTU) of NG is 0.65 USD in 2017(extrapolated from https://www.ferc.gov/EventCalendar/Files/20041020081349-final-gs-report.pdf; ~0.545USD in 2004; price range 0.47-0.62 USD in 2004; adjusted for inflation using http://www.usinflationcalculator.com/ on 09.05.2017)
    # 1 MCF = 0.9756 mmBTU (note that MCF = 1000 ft^3) taken from https://business.directenergy.com/understanding-energy/energy-tools/conversion-factors)
    
    # Prices are converted to USD per unit (mmBTU for natural gas and tonnes for methanol) per month
    storage = {'NG':0.65/6/0.9756, 'MeOH':2.49/12}
    
    # Option 2 - infer from futres prices
    #for key in prices:
    #    counter = 0
    #    while prices[key][4+counter] == None or prices[key][3+counter] == None:
    #        counter += 1
    #    if prices[key][4+counter]-prices[key][3+counter] > 0: storage[key] = prices[key][4+counter]-prices[key][3+counter]
    #    print(prices[key][4+counter]-prices[key][3+counter])

    return storage

def chemical_conversion(complexity = 'simple', hysys = None):
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
        # The simulation provides natural gas in cubic meters per second (this is converted to cubic feet and then to mmBTU based on google.com's unt converter and https://business.directenergy.com/understanding-energy/energy-tools/conversion-factors.
        # Methanol is provided in kilogrames per second (this is converted to tonnes per second).
        try:
            NG = hysys.Flowsheet.Operations.Item(75).Cell('B2').CellValue*35.3147*0.9756/1000
            MeOH = hysys.Flowsheet.Operations.Item(75).Cell('B3').CellValue/1000
        except:
            print('Invalid Hysys address. Simple analysis will be performed.')
            return chemical_conversion()
        
        return NG/MeOH

def conversion_cost(hysys):
    
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
    cost = {'MP STEAM':{'value': 12.64/1000, 'unit':'USD per kg'}, 'PROCESS WATER': {'value':0.33/1000, 'unit':'USD per kg'}, 'COOLING WATER':{'value': 1.74/1000, 'unit':'USD per kg'}, 'Elec': {'value':(0.1727+0.1051)/2/3600, 'unit':'SGD per kJ'}, 'FUEL GAS': {'value':9.8/1.05506/1E6, 'unit':'USD per kJ'}, 'HP STEAM':{'value': 3.49/1000, 'unit':'USD per kg'}}

    cost = exchange_rates(cost, ['SGD', 'USD'])

    # Read consumption rates from the provided HYSYS simulation
    consumption = {}
    
    consumption['MeOH'] = {'value':hysys.Flowsheet.Operations.Item(75).Cell('B3').CellValue/1000, 'unit':'tonne/s'}
    consumption['Elec'] = {'value':hysys.Flowsheet.Operations.Item(75).Cell('K14').CellValue, 'unit':'kW'}
    consumption['COOLING WATER'] = {'value':hysys.Flowsheet.Operations.Item(75).Cell('H14').CellValue, 'unit':'kg/s'}
    consumption['FUEL GAS'] = {'value':abs(hysys.Flowsheet.Operations.Item(75).Cell('E14').CellValue), 'unit':'kJ/s'}
    consumption['HP STEAM'] = {'value':hysys.Flowsheet.Operations.Item(75).Cell('N14').CellValue, 'unit':'kg/s'}
    consumption['MP STEAM'] = {'value':hysys.Flowsheet.Operations.Item(75).Cell('Q14').CellValue, 'unit':'kg/s'}
    consumption['PROCESS WATER'] = {'value':hysys.Flowsheet.Operations.Item(75).Cell('T14').CellValue, 'unit':'kg/s'}

    # Calculate cost per tonne of methanol
    total = 0
    for key in cost:
        total += consumption[key]['value']*cost[key]['value']
    
    return total/consumption['MeOH']['value']
  
def look_for_munnies(SimAddress, prices, dates):
    
    # This function performs puts together all the data and looks for the most profitable arbitrage opportunity using a brute force approach (i.e. checking all possible combintations subject to chronological order constraint).

    # Natural gas to menthanol conversion ratio (mmBTU to tonnes) and the average conversion cost per tonne of methanol are retrieved with the code below.
    # In case this cannot be done a simple analysis will be performed.
    try:
        # Connect to an existing simulation or launch a new instance
        hysys = win32.GetObject (SimAddress)
        # Retrieve the average conversion cost per tonne of methanol
        conv = conversion_cost(hysys)
        #how many NG contracts are needed to fulfill 1 MeoH contract
        NG_MeOH = chemical_conversion('complex', hysys)
    except:
        print('Incorrect model address or wrong model item address. Simple analysis will be executed.')
        NG_MeOH = chemical_conversion()
    
    # The storage cost per month-mmBTU/tonne are retrieved
    storage = storage_cost()


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
    plt.rcParams.update({'font.size': 22})
    
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


# Define address of a relevant Aspen HYSYS model
SimAddress = win32api.GetLongPathName(r"C:\Users\Janusz\Desktop\Commodity_prices\METHANOL PRODUCTION SIMULATION\Methanol Production Plant Rev 1.hsc")
    
# Define the addresses of the futures prices data and chemical codenames
file_addresses = {
'NG':r'C:\Users\Janusz\Desktop\Commodity_prices\Market_data\HNG_data.csv',
'MeOH':r'C:\Users\Janusz\Desktop\Commodity_prices\Market_data\ZCE_data.csv'
}

# Read in and process data into an appripriate format
dates, prices = preprocessing(file_addresses)

# Convert prices into USD
prices['MeOH'][3:] = exchange_rates(prices['MeOH'][3:], ['CNY','USD'])
prices['MeOH'][1] = 'Prior Settlement (USD per tonne)'

# Adjust prices to include the transport cost
prices = transport_costs(prices)

# Search through the arbitrage opportunities
look_for_munnies(SimAddress,prices, dates)

# Define titles and labels to plot the futures prices data and plot the data
labels = {'NG':{'title':'Natural gas futures prices from Chicago Mercantile Exchange', 'label':'Price (USD per mmBTU)'},'MeOH':{'title':'Methanol futures prices from Zhengzhou Commodity Exchange', 'label':'Price (USD per tonne)'}, 'x':{'title':'Delivery date (-)', 'label':dates['NG']}}
plotting_prices(dates, prices, labels)