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
    #print('===Response===')
    #print(pformat(resp))
    
    #print('===Content===')
    #print(pformat(content))
    #print(type(pformat(content)))

    #print('===Finished===')
    
    return pformat(content)


def run_price_query(YYYY, MM, DD, PP):
    #Query All
    makeurl = 'https://api.bmreports.com/BMRS/B1770/v1?APIKey=<PROVIDE_BMRS_API_KEY>&SettlementDate='+YYYY+'-'+MM+'-'+DD+'&Period='+PP+'&ServiceType=xml'
    return post_elexon(url=makeurl,)
    
    #Specific
    #WHILW #48WSTN1000WHILWQ
    #post_elexon(url='https://api.bmreports.com/BMRS/B1610/v2?APIKey=<PROVIDE_BMRS_API_KEY>&SettlementDate=2021-01-01&Period=1&NGCBMUnitID=WHILW&ServiceType=xml',) #HEYM2


def get_price(YYYY, MM, DD, PP):
    #NOTE: Maybe check the preceeding 0 script function is used here. 
    queryoutput = run_price_query(YYYY, MM, DD, PP)
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


def query_bidOffers(YYYY, MM, DD, PP):
    #NOTE: IF YOU WANT PRICES IN THE FUTURE, then I reccomend duplicating this function, but going by bidPrice (or something else like that once you've checked what is needed) instead of bidVolume.
    #Keep in mind, however, that there's a but and sell side to keep track of (think the buy side might use something like offerPrice and offerVolume, rather than bidPrice and bidVolume), so you'd need to consider this change too.
    #Currently the bidding is not used here, but rather, as this is for curtailment, sell side volumes are considered.
    #This process is also specifically used for renewables. 
    queryoutput = post_elexon(url='https://api.bmreports.com/BMRS/DETSYSPRICES/v1?APIKey=<PROVIDE_BMRS_API_KEY>&SettlementDate='+YYYY+'-'+MM+'-'+DD+'&SettlementPeriod='+PP+'&ServiceType=xml',) #All
    queryoutput = queryoutput.replace("'\n b'","")
    queryoutput = queryoutput.replace("</","<")
    queryoutput = queryoutput.replace("<bidPrice>","<id>")
    queryoutput = queryoutput.replace("<offerPrice>","<id>")
    #queryoutput = queryoutput.replace("<nivAdjustedVolume>","<id>")
    queryoutput = queryoutput.split("<id>")
    
    v = 0
    while v < len(queryoutput):
        if ("<" in queryoutput[v]) or (">" in queryoutput[v]):
            queryoutput.remove(queryoutput[v])
        elif queryoutput[v] == "":
                queryoutput.remove(queryoutput[v])
        else:
            v += 1
    #print(queryoutput)
    return queryoutput


def get_data(excelName, genmapName, startYear, startMonth, startDay, endYear, endMonth, endDay, index):
    #Open CSV
    data = pd.read_excel(excelName) #Can comment out for early testing, but needed now. 
    genmap = pd.read_csv(genmapName) #Can comment out for early testing, but needed now. 
    
    loop = True
    #NOTE: index = 0 by default, only pick a different value if you are starting part way in (eg. if there was an error)

    #Define stations (of interest) columns from excelName. 
    #stations = ["48WSTN0000ABRBON", "48WSTN0000ABRTWR", "48WSTN0000ACHRWV", "48WSTN0000ANSUWY", "48WSTN0000ARCHW6", "48WSTN0000ASHWWA", "48WSTN1000BABAWQ", "48WSTN0000BEATOG", "48WSTN0000BEINWN", "48WSTN0000BETHWY", "48WSTN0000BHLAWZ", "48WSTN0000BLKWWR", "48WSTN00000BLLAV", "48WSTN00000BLLXM", "48WSTN0000BNAKWJ", "48WSTN0000BNWKW5", "48WSTN0000BOWLWY", "48WSTN0000BRBEOT", "48WSTN0000BRDUWV", "48WSTN0000BRYBW4", "48WSTN0000BTUIWQ", "48WSTN0000BURBWH", "48WSTN0000CAUSWB", "48WSTN0000CGTHWI", "48WSTN0000CLDCWZ", "48WSTN0000CLDNW2", "48WSTN0000CLDRWR", "48WSTN0000CLDSWO", "48WSTN0000COUWW3", "48WSTN0000CRMLWG", "48WSTN0000CRYRBT", "48WSTN0000CRYRWO", "48WSTN0000DALSW4", "48WSTN0000DDGNO3", "48WSTN0000DEUCWX", "PLACEHOLDER1", "48WSTN0000DRDGWO", "48WSTN0000DRSLWN", "48WSTN0000DUNGW6", "48WSTN00000EAAOS", "48WSTN0000EARBWP", "48WSTN0000EDINWA", "48WSTN1000EWHLWQ", "48WSTN0000FAARW2", "48WSTN0000FALGWS", "48WSTN1000FDUNTQ", "48WSTN0000FSDLWT", "48WSTN0000GAOFOT", "48WSTN0000GDSTWE", "48WSTN0000GFLDW6", "48WSTN0000GLOFWW", "48WSTN0000GLWSWZ", "48WSTN0000GNFSWJ", "48WSTN0000GRGBW9", "48WSTN0000GRIFWQ", "PLACEHOLDER2", "48WSTN0000HADHW8", "48WSTN0000HLTWWT", "48WSTN0000HMGTOR", "48WSTN0000HOWAOA", "48WSTN0000HRSTWC", "48WSTN0000HYWDW9", "48WSTN0000KILBWA", "48WSTN0000KLGLWM", "48WSTN0000LARYWP", "48WSTN0000LCLTWH", "48WSTN0000MDHLW9", "48WSTN0000MILWW9", "48WSTN0000MINSWD", "48WSTN0000MKHLWB", "48WSTN0000MOWEO5", "48WSTN0000NHOYWR", "48WSTN1000NOVAWQ", "48WSTN0000OMNDWQ", "48WSTN0000PAUHW3", "48WSTN0000RCBKOV", "48WSTN0000RHYFWK", "48WSTN0000RMPNON", "48WSTN00000RREWF", "48WSTN00000RRWWZ", "48WSTN0000RSHLWF", "48WSTN0000SHRSW3", "48WSTN0000STLGW3", "48WSTN0000STRNWW", "48WSTN0000TDBNWM", "48WSTN0000THNTWA", "48WSTN0000TULWBN", "48WSTN0000TULWWI", "48WSTN0000WDNSWF", "48WSTN1000WHILWQ", "48WSTN0000WLNYWV", "48WSTN0000WLNY3F", "48WSTN0000WLNY4D", "48WSTN0000WTMSOT"]
    
    while(loop):
        #For each day...
        
        for i in range(1, 48 + 1):
            #Get Period's Data
            #Get Price for Period
            price, price_e = get_price(startYear, startMonth, startDay, str(i))
            if price_e == False:
                print("Price Error For: " + startYear +'-'+ startMonth +'-'+ startDay +' Period: ' + str(i))
            #Get bidOffers for Period
            bidOffers = query_bidOffers(startYear, startMonth, startDay, str(i))

            #Add Period's Data            
            #Add Time
            data.iloc[index, data.columns.get_loc('Year')] = str(int(startYear))
            data.iloc[index, data.columns.get_loc('Month')] = str(int(startMonth))
            data.iloc[index, data.columns.get_loc('Day')] = str(int(startDay))
            data.iloc[index, data.columns.get_loc('Period')] = str(int(i))
            
            #Clear Values
            data.iloc[index, data.columns.get_loc('PlantRRN')] = ''
            data.iloc[index, data.columns.get_loc('PlantEIC')] = ''
            data.iloc[index, data.columns.get_loc('PlantDUKESName')] = ''
            data.iloc[index, data.columns.get_loc('PlantTypeBMRS')] = ''
            data.iloc[index, data.columns.get_loc('PlantTypeDUKES')] = ''
            
            #Find Closest
            if len(bidOffers) > 2:
                ID = bidOffers[0]
                bidOffer = float(bidOffers[1])
                b = 2
                while b < len(bidOffers):
                    if (abs(price - bidOffer) > abs(price - float(bidOffers[b+1]))):
                        ID = bidOffers[b]
                        bidOffer = float(bidOffers[b+1])
                    b += 2
                #print(ID)
                #print(bidOffer)
                #sys.exit()
                
                #Find Closest PowerGenerator's PowerPlant (using the auto file as the map)
                for i in range(0,len(genmap['Type (powerplant(station) or generator(unit))'])):
                    #Loop for generators
                    if (genmap['Type (powerplant(station) or generator(unit))'][i] == 'generator') and (genmap['Registered Resource Name'][i] in ID) and (genmap['Connected (if generator(unit))'][i] != 'None') and (genmap['Connected (if generator(unit))'][i] != '') and (genmap['Connected (if generator(unit))'][i] != '0'):
                        #Generator Found, and it is connected to a powerplant. So set the EIC, and we'll loop over everything again (at once). 
                        data.iloc[index, data.columns.get_loc('PlantEIC')] = genmap['Connected (if generator(unit))'][i]
                        for i in range(0,len(genmap['Type (powerplant(station) or generator(unit))'])):
                            #Loop for BMRS plants
                            if (genmap['Type (powerplant(station) or generator(unit))'][i] == 'powerplant') and (data['PlantEIC'][index] == genmap['Registered Resource EIC code'][i]):
                                #Plant Found
                                data.iloc[index, data.columns.get_loc('PlantRRN')] = genmap['Registered Resource Name'][i]
                                data.iloc[index, data.columns.get_loc('PlantTypeBMRS')] = genmap['PSR Type'][i]
                                data.iloc[index, data.columns.get_loc('PlantDUKESName')] = genmap['outputpowerplant =IF(ISNA(VLOOKUP(C2,CP$2:CR$1184,3,FALSE)),"",VLOOKUP(C2,CP$2:CR$1184,3,FALSE))'][i]
                                for i in range(0,len(genmap['plantname'])):
                                    #DUKES Loop
                                    if (data['PlantDUKESName'][index] != "") and (genmap['plantname'][i] == data['PlantDUKESName'][index]):
                                        #DUKES Found
                                        data.iloc[index, data.columns.get_loc('PlantTypeDUKES')] = genmap['Type'][i]
                                        break
                                break
                        break
            
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
    get_data("Template-Powerplant-MarginalSeller.xlsx", "Input-Template-Auto.csv", "2017", "01", "01", "2022", "04", "01", 0) #Inclusive of start / end date (as with other similar scripts). 

