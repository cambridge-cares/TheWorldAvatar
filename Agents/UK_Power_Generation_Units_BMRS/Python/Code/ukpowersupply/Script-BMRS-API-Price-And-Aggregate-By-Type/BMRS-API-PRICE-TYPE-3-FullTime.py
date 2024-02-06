#TEST from page 193: https://www.elexon.co.uk/documents/training-guidance/bsc-guidance-notes/bmrs-api-and-data-push-user-guide-2/

###Libraries###
import httplib2
from pprint import pformat

import re
import pandas as pd
import sys
from datetime import datetime, timedelta



####Functions###
def str0_2(value):
    #Converts the day or month int to a string of length 2. Thus, 12 -> "12", and 1 -> "01", the leading 0 is important.
    #This function thus performs a similar role as str(), but also can add the 0 at the start, and is applied to length 2 instances.
    #Must use for day or month, but use for period is optional, as length 1 and 2 is accepted by the API format the period, but not the month or day. 
    value = str(value)
    if len(value) == 1:
        value = "0"+value
    return value


def next_day(Year, Month, Day):
    #Convert to timestamp.
    day = datetime(int(Year), int(Month), int(Day), 0, 0, 0, 0)
    
    #Get a week ago (returns str for year, month, day: 
    nextDay = day + timedelta(days=1)
    print(nextDay)
    return str(nextDay.year), str0_2(nextDay.month), str0_2(nextDay.day)


def post_elexon(url):
    http_obj = httplib2.Http()
    resp, content = http_obj.request(
        uri=url,
        method='GET',
        headers={'Content-Type': 'application/xml; charset=UTF-8'},
    )
    printbool = False
    if printbool:
        print('===Response===')
        print(pformat(resp))
        
        print('===Content===')
        print(pformat(content))
        print(type(pformat(content)))

        print('===Finished===')
    return pformat(content)


def run_query(YYYY, MM, DD, PP):
    #Query All
    makeurl = 'https://api.bmreports.com/BMRS/B1770/v1?APIKey=<PROVIDE_BMRS_API_KEY>&SettlementDate='+YYYY+'-'+MM+'-'+DD+'&Period='+PP+'&ServiceType=xml'
    return post_elexon(url=makeurl,)
    
    #Specific
    #WHILW #48WSTN1000WHILWQ
    #post_elexon(url='https://api.bmreports.com/BMRS/B1610/v2?APIKey=<PROVIDE_BMRS_API_KEY>&SettlementDate=2021-01-01&Period=1&NGCBMUnitID=WHILW&ServiceType=xml',) #HEYM2


def run_type_query(YYYY, MM, DD, PP):
    #Query All
    makeurl = 'https://api.bmreports.com/BMRS/B1620/v1?APIKey=<PROVIDE_BMRS_API_KEY>&SettlementDate='+YYYY+'-'+MM+'-'+DD+'&Period='+PP+'&ServiceType=xml'
    queryoutput = post_elexon(url=makeurl,)
    queryoutput = queryoutput.replace("'\n b'","")
    queryoutput = queryoutput.replace("<quantity>", "&quot;") #Cannot split multiple times, so as we have 3 terms we want to split on, we will replace the first 2 with the third, then split. 
    queryoutput = queryoutput.replace("</quantity>", "&quot;")
    queryoutput = queryoutput.replace("<settlementPeriod>", "&quot;")
    queryoutput = queryoutput.replace("</settlementPeriod>", "&quot;")
    queryoutput = queryoutput.split("&quot;")
    return queryoutput


def get_price(YYYY, MM, DD, PP):
    #NOTE: Maybe check the preceeding 0 script function is used here. 
    queryoutput = run_query(YYYY, MM, DD, PP)
    queryoutput = queryoutput.replace("'\n b'","")
    #print(queryoutput)
    queryoutput = queryoutput.split("<imbalancePriceAmountGBP>")
    if (type(queryoutput) == list) and (len(queryoutput) > 1):
        queryoutput = queryoutput[1]
        queryoutput = queryoutput.split("</imbalancePriceAmountGBP>")
        if (type(queryoutput) == list) and (len(queryoutput) > 1):
            queryoutput = queryoutput[0]
            if queryoutput.strip("0987654321.-") == "":
                #Valid output
                queryoutput = float(queryoutput)
                #print(queryoutput)
                return queryoutput, True
    
    return 0, False #FAIL
    

def get_prices(YYYY, MM, DD):
    #For a given day, extracts prices for all values.
    #Has 2 outputs.
    #First output is an array of the 48 prices (for the 48 half-hourly periods throughout the given day).
    #Second output is the success, so if it is 'True', then it worked, and if it is 'False', then it failed. 
    checked = 0
    price_array = [0]*48
    for i in range (1,49):
        price, exists = get_price(YYYY, MM, DD, str(i))
        if exists:
            #Worked
            price_array[i-1] = price
            #print(i, price)
        else:
            #Failed
            checked = 1
    if checked == 0:
        return price_array, True
    else:
        return price_array, False


def get_type(YYYY, MM, DD):
    #For a day, get the generation by aggregated type. 
    type_array = [[0 for x in range(49)] for y in range(11)] 
    #X = Periods (48) Index 0 is the types, the other 48 are the period quantities.
    #Y = Types (11): #between &quot;
    type_array[0][0] = "Biomass"
    type_array[1][0] = "Hydro Pumped Storage"
    type_array[2][0] = "Hydro Run-of-river and poundage"
    type_array[3][0] = "Fossil Hard coal"
    type_array[4][0] = "Fossil Gas"
    type_array[5][0] = "Fossil Oil"
    type_array[6][0] = "Nuclear"
    type_array[7][0] = "Other"
    type_array[8][0] = "Wind Onshore"
    type_array[9][0] = "Wind Offshore"
    type_array[10][0] = "Solar"
    
    queryoutput = run_type_query(YYYY, MM, DD, "*")

    i = 0
    lim = len(queryoutput) - 5
    while(i < lim):
        #print(queryoutput[i+1]) #Quantity
        #print(queryoutput[i+3]) #Period
        #print(queryoutput[i+5]) #Type
        for j in range(0,11):
            if (type_array[j][0] == queryoutput[i+5]) and (int(queryoutput[i+3]) < 49):
                #Same Type
                type_array[j][int(queryoutput[i+3])] = queryoutput[i+1]
                continue
        i += 6 #Next
    return type_array



###Primary Function###
def get_data(excelName, startYear, startMonth, startDay, endYear, endMonth, endDay, index):
    #Open CSV
    data = pd.read_excel(excelName)

    loop = True
    #NOTE: index = 0 by default, only pick a different value if you are starting part way in (eg. if there was an error)
    
    while(loop):
        #Get Data
        
        #Get Type Data.
        types = get_type(startYear, startMonth, startDay)

        #Get Price Data. 
        prices, priceCheck = get_prices(startYear, startMonth, startDay)
        if priceCheck == False:
            print("PriceError")

        
        #Add Day's Data.
        for i in range(0, len(prices)):
            #Add Price
            data.iloc[index, data.columns.get_loc('Price')] = prices[i]

            #Add TIme
            data.iloc[index, data.columns.get_loc('Year')] = str(int(startYear))
            data.iloc[index, data.columns.get_loc('Month')] = str(int(startMonth))
            data.iloc[index, data.columns.get_loc('Day')] = str(int(startDay))
            data.iloc[index, data.columns.get_loc('Period')] = str(int((i+1)))
            
            #Add Type
            for j in range(0,11):
                #For each type add the value.
                data.iloc[index, data.columns.get_loc(types[j][0])] = types[j][i+1]

            index += 1
        
        #Time Incriment
        if (startYear == endYear) and (startMonth == endMonth) and (startDay == endDay):
            loop = False
        else:
            oldMonth = startMonth
            startYear, startMonth, startDay = next_day(startYear, startMonth, startDay)

            if oldMonth != startMonth:
                #Every month save in case there's an error, write index and save. 
                data.to_excel(excelName, index = False)
                print("Index (at last monthly autosave): " + str(index))
                data = pd.read_excel(excelName)

    #Close Excel
    data.to_excel(excelName, index = False)



###Main Function###
if __name__ == "__main__":
    #Current print output is when it changes to a new day (prints date) - note that this means the first day's date is not printed, though it is processed.
    #Index is printed monthly, and the excel also saves monthly (when a new month is arrived at) as it's possible for this to have a connection error that stops it mid-way...
    #... so saving every month is a backup. Also the index is printed so you can use it to restart the process (with the printed index and date as your new index and startdate.
    #By default index should be left at 0 though.
    #So again, to resume the process part way through, have the first day of the next month (the one just before the autosave the autosave message) be the start day, and have the index printed with the last autosave be the starting index. 
    get_data("ExcelTemplatePriceVSType.xlsx", "2017", "01", "01", "2022", "04", "01", 0)

