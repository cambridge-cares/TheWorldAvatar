###Libraries###
import pandas as pd
import sys
from datetime import datetime, timedelta
import re
from collections import OrderedDict
import logging



###Logging###
#Setup the logging with a logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
fileHandler = logging.FileHandler('csvMatchOutputLog.log', mode='w') #remove the "mode='w'" to have it only append with each run to the log file, rather than deleting previous contents. 
#formatter = logging.Formatter()
#fileHandler.setFormatter(formatter)
logger.addHandler(fileHandler)



###Functions###
def MultipleBMRSEIC (data):
    #Check for a repeat EIC in the BMRS data. 
    for i in range(0,len(data['Registered Resource EIC code'])):
        for j in range(0,len(data['Registered Resource EIC code'])):
            if i != j:
                if data['Registered Resource EIC code'][i] == data['Registered Resource EIC code'][j]:
                    #print("Data Error: Multiple instances of EIC in BMRS Data: " + str(data['Registered Resource EIC code'][i]))
                    #logger.info('Data Error: Multiple instances of EIC in BMRS Data: {}'.format(str(data['Registered Resource EIC code'][i]))) #Alternate way to give this Input-Template. 
                    logger.warning("Data Error: Multiple instances of EIC in BMRS Data: " + str(data['Registered Resource EIC code'][i]))
                    #Note, the multiple instance error above occurs when the same EIC code occurs multiple times in the BMRS data. These are handled individually, but will obtain the same result. 


def WriteRoman(num):
    #Used to re-write a string with roman numeral replacements for numbers. EG. Let's say we want to compare II and 2, they could match.
    #You may want to use write_roman(int(match.group(0))) to call with a given string. Where match is the string. 
    roman = OrderedDict()
    roman[1000] = "M"
    roman[900] = "CM"
    roman[500] = "D"
    roman[400] = "CD"
    roman[100] = "C"
    roman[90] = "XC"
    roman[50] = "L"
    roman[40] = "XL"
    roman[10] = "X"
    roman[9] = "IX"
    roman[5] = "V"
    roman[4] = "IV"
    roman[1] = "I"

    def RomanNum(num):
        for r in roman.keys():
            x, y = divmod(num, r)
            yield roman[r] * x
            num -= (r * x)
            if num <= 0:
                break

    return "".join([a for a in RomanNum(num)])


def RomeReplace(match):
    #Used with the above function, allowing for a whole string. Only words that are numbers are used, not cases where there's a number in a word for some reason. 
    return WriteRoman(int(match.group(0)))


def ExemptFind(data, ExemptWords, strictness):
    #Find repeat words, and add them to ExemptWords.
    words = []
    for i in range(0,len(data['Station Name'])):
        words.extend(data['Station Name'][i].split(" "))
    for word in words:
        count = 0
        for check in words:
            if word == check:
                count += 1
        if count > strictness: 
            #If the word occurs more than once then add it to the list.
            if (word not in ExemptWords):
                ExemptWords.append(word)
            #print(word)
    return ExemptWords


def BMRSEICDUKESStationNameCheck(data, j, k, ExemptWords):
    #data is the csv data.
    #j is the index in the EIC station data that we will try to find a name match for in DUKES.
    #k is the index of the EIC in the BMRS data (used to extract BMRS capacity). 
    regex = re.compile(r"\b\d+\b")
    
    name = data['EIC Name - Asset Name - Stations'][j]
    
    PickName = ""
    PickCapacity = -10
    
    #First, just check if there is a direct name match. 
    for i in range(0,len(data['Station Name'])):
        if (name == data['Station Name'][i]):
            if (PickCapacity == -10):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
            elif (abs(float(data['Installed Capacity (MW)'][i]) - float(data['Capacity MW'][k])) < abs(float(PickCapacity) - float(data['Capacity MW'][k]))):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
    if(PickName != ""):
        return PickName, -10

    #Extension of the first, but using the Roman Numeral Match. 
    romeName = regex.sub(RomeReplace, name)
    for i in range(0,len(data['Station Name'])):
        if (romeName == regex.sub(RomeReplace, data['Station Name'][i])):
            if (PickCapacity == -10):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
            elif (abs(float(data['Installed Capacity (MW)'][i]) - float(data['Capacity MW'][k])) < abs(float(PickCapacity) - float(data['Capacity MW'][k]))):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
    if(PickName != ""):
        return PickName, -9

    #Second, check if a simplified version of both names creates a match.
    name2 = name.translate(str.maketrans('','','* -_&’\''))
    for i in range(0,len(data['Station Name'])):
        if (name2 == data['Station Name'][i].translate(str.maketrans('','','* -_&’\''))):
            if (PickCapacity == -10):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
            elif (abs(float(data['Installed Capacity (MW)'][i]) - float(data['Capacity MW'][k])) < abs(float(PickCapacity) - float(data['Capacity MW'][k]))):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
    if(PickName != ""):
        return PickName, -8

    #Extension of the second, but using the Roman Numeral Match. 
    romeName = regex.sub(RomeReplace, name2)
    for i in range(0,len(data['Station Name'])):
        if (romeName == regex.sub(RomeReplace, data['Station Name'][i].translate(str.maketrans('','','* -_&’\'')))):
            if (PickCapacity == -10):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
            elif (abs(float(data['Installed Capacity (MW)'][i]) - float(data['Capacity MW'][k])) < abs(float(PickCapacity) - float(data['Capacity MW'][k]))):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
    if(PickName != ""):
        return PickName, -7
    
    #Third, try with letter last letter or GT from 'Display Name - Stations'. 
    name3 = name2.translate(str.maketrans('','','0123456789'))
    name6 = name3 #For later
    if (data['Display Name - Stations'][j][-2] == 'G' and data['Display Name - Stations'][j][-1] == 'T'):
        name3 += "GT"
    else:
        name3 += data['Display Name - Stations'][j][-1]
    for i in range(0,len(data['Station Name'])):
        if (name3 == data['Station Name'][i].translate(str.maketrans('','','* -_&’\''))): 
            if (PickCapacity == -10):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
            elif (abs(float(data['Installed Capacity (MW)'][i]) - float(data['Capacity MW'][k])) < abs(float(PickCapacity) - float(data['Capacity MW'][k]))):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
    if(PickName != ""):
        return PickName, -7

    #Extension of the third, but using the Roman Numeral Match. 
    romeName = regex.sub(RomeReplace, name3)
    for i in range(0,len(data['Station Name'])):
        if (romeName == regex.sub(RomeReplace, data['Station Name'][i].translate(str.maketrans('','','* -_&’\'')))): 
            if (PickCapacity == -10):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
            elif (abs(float(data['Installed Capacity (MW)'][i]) - float(data['Capacity MW'][k])) < abs(float(PickCapacity) - float(data['Capacity MW'][k]))):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
    if(PickName != ""):
        return PickName, -6
    
    #Fourth, try just the first word to the whole DUKES name. 
    name7 = name.split(" ")
    name4 = name7[0]
    if ' ' in name:
        for i in range(0,len(data['Station Name'])):
            if (name4 == data['Station Name'][i]): 
                if (PickCapacity == -10):
                    PickCapacity = data['Installed Capacity (MW)'][i]
                    PickName = data['Station Name'][i]
                    
                elif (abs(float(data['Installed Capacity (MW)'][i]) - float(data['Capacity MW'][k])) < abs(float(PickCapacity) - float(data['Capacity MW'][k]))):
                    PickCapacity = data['Installed Capacity (MW)'][i]
                    PickName = data['Station Name'][i]
                    
    if(PickName != ""):
        return PickName, -4

    #Fifth, try the first word to the first word of the DUKES name (such that if there would have been multiple in the above split, then specific is tried first before this). 
    for i in range(0,len(data['Station Name'])):
        if (' ' in data['Station Name'][i] and name4 == data['Station Name'][i].split(" ")[0] and name not in ExemptWords): 
            if (PickCapacity == -10):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
            elif (abs(float(data['Installed Capacity (MW)'][i]) - float(data['Capacity MW'][k])) < abs(float(PickCapacity) - float(data['Capacity MW'][k]))):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
    if(PickName != ""):
        return PickName, -3

    #Sixth, just try the first word, also removing all characters that might be removable to this point.
    #name6 defined earlier. 
    for i in range(0,len(data['Station Name'])):
        if (name6 == data['Station Name'][i].translate(str.maketrans('','','* -_&’\'0123456789'))): 
            if (PickCapacity == -10):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
            elif (abs(float(data['Installed Capacity (MW)'][i]) - float(data['Capacity MW'][k])) < abs(float(PickCapacity) - float(data['Capacity MW'][k]))):
                PickCapacity = data['Installed Capacity (MW)'][i]
                PickName = data['Station Name'][i]
                
    if(PickName != ""):
        return PickName, -2

    #Seventh, try the each word against eachother (such that if there would have been multiple in the above split, then specific is tried first before this). 
    if ' ' in name:
        for i in range(0,len(data['Station Name'])):
            if ' ' in data['Station Name'][i]: 
                DataSplitName = data['Station Name'][i].split(" ")
                for d in DataSplitName:
                    for l in name7:
                        #print(d + ' ' + l)
                        if (l.translate(str.maketrans('','','* -_&’\'')) == d.translate(str.maketrans('','','* -_&’\'')) and l.translate(str.maketrans('','','* -_&’\'')) not in ExemptWords): 
                            if (PickCapacity == -10):
                                PickCapacity = data['Installed Capacity (MW)'][i]
                                PickName = data['Station Name'][i]
                                
                            elif (abs(float(data['Installed Capacity (MW)'][i]) - float(data['Capacity MW'][k])) < abs(float(PickCapacity) - float(data['Capacity MW'][k]))):
                                PickCapacity = data['Installed Capacity (MW)'][i]
                                PickName = data['Station Name'][i]
                                
            else:
                d = data['Station Name'][i]
                if (l.translate(str.maketrans('','','* -_&’\'')) == d.translate(str.maketrans('','','* -_&’\'')) and l.translate(str.maketrans('','','* -_&’\'')) not in ExemptWords): 
                    if (PickCapacity == -10):
                        PickCapacity = data['Installed Capacity (MW)'][i]
                        PickName = data['Station Name'][i]
                        
                    elif (abs(float(data['Installed Capacity (MW)'][i]) - float(data['Capacity MW'][k])) < abs(float(PickCapacity) - float(data['Capacity MW'][k]))):
                        PickCapacity = data['Installed Capacity (MW)'][i]
                        PickName = data['Station Name'][i]
                        
    else:
        for i in range(0,len(data['Station Name'])):
            l = name
            if ' ' in data['Station Name'][i]: 
                DataSplitName = data['Station Name'][i].split(" ")
                for d in DataSplitName:
                    #print(d + ' ' + l)
                    if (l.translate(str.maketrans('','','* -_&’\'')) == d.translate(str.maketrans('','','* -_&’\'')) and l.translate(str.maketrans('','','* -_&’\'')) not in ExemptWords): 
                        if (PickCapacity == -10):
                            PickCapacity = data['Installed Capacity (MW)'][i]
                            PickName = data['Station Name'][i]
                            
                        elif (abs(float(data['Installed Capacity (MW)'][i]) - float(data['Capacity MW'][k])) < abs(float(PickCapacity) - float(data['Capacity MW'][k]))):
                            PickCapacity = data['Installed Capacity (MW)'][i]
                            PickName = data['Station Name'][i]
                            
    if(PickName != ""):
        return PickName, -1
    
    return "None", 999


def SmallOnLargeDiffFraction(num1, num2):
    #The difference between the two provided numbers, as calculated of the smaller number / the larger number (and then 1 - this fraction). EG. 3 and 4 as inputs (either way) would give 0.25 as an output. 
    if (num1 == 0) or (num2 == 0):
        return 0.18 #If there's no capacity listed, then it gives an 18% difference, which is right on the edge, and it has to earn its way back. 
    if num1 > num2:
        return (1.0 - (num2/num1))
    else:
        return (1.0 - (num1/num2))


def IDPick(a, b):
    #There are two ID options, the first isn't always filled in, but is better if it exists. So return this if it exists, otherwise, return the more common backup.
    if ((len(a) > 0) and (a != "nan")):
        return a
    return b



#Error Text Functions, these take output data from the Primary Function, and give text translations for the logger file.
#Name Match Type
def ErrorNameText(e):
    e = int(e)
    if e == -10:
        return "Exact Match" + " (" + str(e) + ")"
    elif e == -9:
        return "Roman Numeral / Decimal Writing Difference Only" + " (" + str(e) + ")"
    elif e == -8:
        return "Simplified Names Match" + " (" + str(e) + ")"
    elif e == -7:
        return "Simplified Name with Roman Numeral / Decimal Writing Difference or Name Extension Convention Difference" + " (" + str(e) + ")"
    elif e == -6:
        return "Simplified Name with Roman Numeral / Decimal Writing Difference and Name Extension Convention Difference" + " (" + str(e) + ")"
    elif e == -4: #No -5
        return "First EIC Word Matches Whole DUKES Name" + " (" + str(e) + ")"
    elif e == -3:
        return "First Words of EIC and DUKES Match (if unique)" + " (" + str(e) + ")"
    elif e == -2:
        return "First Simplified EIC and DUKES Names Match" + " (" + str(e) + ")"
    elif e == -1:
        return "Any Simplified Unique Word Appears in both EIC and DUKES Names" + " (" + str(e) + ")"
    return "None" #incl. e == 999


#Capacity Difference (%) Note that the x value (sensitivity limit) is given if it is not possible. 
def ErrorCapacityDifferenceText(e):
    e = int(e)
    if e == 999:
        return "None"
    elif (e < 999) and (e >= 0):
        return str(e)
    return "None" #incl. e == 999


#Error score description from year match (very low quality test, so minimal effect on final score). 
def ErrorDateText(e):
    e = int(e)
    if e == -5:
        return "Same Year" + " (" + str(e) + ")"
    elif e == -1:
        return "Within a Year" + " (" + str(e) + ")"
    elif e == 0:
        return "Unclear" + " (" + str(e) + ")"
    elif e == 1: 
        return "Startup Differences" + " (" + str(e) + ")"
    return "None" #incl. e == 999


#Gen type match test output description. 
def ErrorGenTypeText(x, e): 
    e = int(e)
    if e == (-int(x/5) -3):
        return "Same Generation Type Match" + " (" + str(e) + ")"
    elif e == -2:
        return "Ambiguous Generation Type" + " (" + str(e) + ")"
    elif e == 1:
        return "Unknown Generation Type" + " (" + str(e) + ")"
    elif e == x:
        return "Generation Type Differences" + " (" + str(e) + ")"
    return "None" #incl. e == 999


#The final verdict described. 
def ErrorMatch(e):
    if int(e) == 1:
        return "Matched" + " (" + str(e) + ")"
    return "Not Matched" 



###Primary Function###
def BMRSEICDUKESMap(csvName, x):
    #The function this functionality is run through. Reads the csv, processes the matches (with scores), then outputs back to csv.
    logger.info('\nBMRSEICDUKESMap Function Started: \'Data Error\'s below if found (not fatal): ')
    
    #Read csv
    data = pd.read_csv(csvName) #place "r" before the path string to address special character, such as '\'. Don't forget to put the file name at the end of the path + '.csv'

    #Initialise ExemptWords for the name comparison. Some of these are hardcoded (note that just because a word doesn't repeat, doesn't make it significant, imagine if some instances are missing in datasets for example. Thus hard coded ones, and found ones. 
    ExemptWords = ["VPI", "CHP", "West", "South", "East", "North", "Hill", "Ferry", "&", "Power", "Heat", "Great", "Farm", "Windfarm", "Lane", "Street", "A", "B", "C", "D", "E", "F", "G", "H", "I", "II", "III", "IV", "V", "VI", "VII", ""] #One place is just called 'FERRY FARM'. It's solar, so doesn't matter here, but if there's other similar issues, just have to go on the more exact match methods. 
    ExemptWords = ExemptFind(data, ExemptWords, len(str(x))) #scrictness was increased from > 1 to > 2 as, if there's just 2 instances, the capacity matching seems to be sufficient. 
    
    #Initialise the array for the acceptable error output (confidence of answer). 
    #error = [len(data['Installed Capacity (MW)'])][5]
    error = [[999]*5 for z in range(len(data['Installed Capacity (MW)']))] #At the time of writing this makes an array of lengths [1183][5].

    terror = [999]*len(data['Registered Resource EIC code'])
    for i in range(0,len(data['tMatchType'])):
        data.iloc[i, data.columns.get_loc('tMatchType')] = 999
    
    #print(data)
    #print(data['NGC BM Unit ID'][1])
    
    #BMRS -> EIC -> DUKES (output: outputDUKESToBMRS EIC or ID)
    
    #Check data (turn this off when testing if you want to speed things up a bit)
    MultipleBMRSEIC(data)
    
    #1: For items in BMRS EICs
    for i in range(0,len(data['Registered Resource EIC code'])):
        #For items in EIC EICs
        #a and b used for error checking. 
        a = 0
        b = 0
        match = "None"
        #For Units
        for k in range(0,len(data['Energy Identification Code - Units'])):
            if (data['Registered Resource EIC code'][i] == data['Energy Identification Code - Units'][k]): 
                #Match
                b += 1
                match = "None" #DUKES is just stations, so currently only using this check for error matching. 
        #For Stations
        for j in range(0,len(data['Energy Identification Code - Stations'])):
            #print (data['Energy Identification Code'][j])
            if (data['Registered Resource EIC code'][i] == data['Energy Identification Code - Stations'][j]): 
                #Match
                a += 1
                #Now that we know the EIC row in the csv, we want to look for a name match with DUKES.
                #This is not exact, so will try multiple versions of the name as manually investigated, so hopefully they work in multiple instances.
                match, tempError  = BMRSEICDUKESStationNameCheck(data, j, i, ExemptWords)
                match = str(match)
                terror[i] = tempError
                data.iloc[j, data.columns.get_loc('tMatchType')] = terror[i]
                #data['outputBMRSUnitToDUKESStation'][i] = Match
                ###if(i > -1 and b == 0): ###
                ###    print(str(i + 1) + ' ' + data['Registered Resource Name'][i] + ' ' + Match) ###
        data.iloc[i, data.columns.get_loc('outputBMRSUnitToDUKESStation')] = match #When assigning a value to a dataframe, need to do this unfortunately (no way to do it with a string and integer index, so need to go for the integer one). Also it's row, column, rather than column, row as we use in other cases. 
        if a > 1:
            #print("Data Error: Multiple instances of EIC in EIC Station Data: " + str(data['Registered Resource EIC code'][i]))
            logger.warning("Data Error: Multiple instances of EIC in EIC Station Data: " + str(data['Registered Resource EIC code'][i]))
        if b > 1:
            #print("Data Error: Multiple instances of EIC in EIC Unit Data: " + str(data['Registered Resource EIC code'][i]))
            logger.warning("Data Error: Multiple instances of EIC in EIC Unit Data: " + str(data['Registered Resource EIC code'][i]))
        if (((a + b) > 1) and (a < 2) and (b < 2)):
            #print("Data Error: Multiple instances of EIC in EIC Station and Unit Data: " + str(data['Registered Resource EIC code'][i]))
            logger.warning("Data Error: Multiple instances of EIC in EIC Station and Unit Data: " + str(data['Registered Resource EIC code'][i]))
            #Note, the multiple instance errors above occur when multiple EIC matches in the EIC data match the BMRS EIC code. So the same EIC code occurs multiple times in the EIC data. In this instance the last map is used.
            #The reason this doesn't have its own loop is that only EIC matches to BMRS are relevant, so only those are considered to save time. 

    #2: Now that the BMRS are linked to DUKES (via EIC data), we can now go through the DUKES and see which of them map back (go both ways as there may be repeats)
    for m in range(0,len(data['Station Name'])): #DUKES
        ClosestCapacity = -10
        ClosestEIC = "None"
        ClosestID = "None"
        #ClosestTime = datetime.strptime("01/01/01  12:00:00 AM", "%d/%m/%Y %I:%M:%S %p") #If there's a replacement it doesn't actually matter as they have the same EIC, so this comparrison idea is not required and this line is for debugging times. 
        for n in range(0,len(data['outputBMRSUnitToDUKESStation'])):
            #ClosestTime = datetime.strptime(str(data['Effective From (Date)'][n]), "%d/%m/%Y %I:%M:%S %p")
            #print(ClosestTime)
            if data['Station Name'][m] == data['outputBMRSUnitToDUKESStation'][n]: #BMRS
                #There's a match, so kemack of the name and capacity.
                if (ClosestCapacity == -10):
                    ClosestCapacity = data['Capacity MW'][n]
                    ClosestEIC = str(data['Registered Resource EIC code'][n])
                    ClosestID = IDPick(str(data['NGC BM Unit ID'][n]), str(data['Registered Resource Name'][n]))
                    error[m][0] = int(100 * SmallOnLargeDiffFraction(float(ClosestCapacity), float(data['Installed Capacity (MW)'][m])) + 0.5)
                    error[m][1] = terror[n]
                elif (abs(float(data['Installed Capacity (MW)'][m]) - float(data['Capacity MW'][n])) < abs(float(data['Installed Capacity (MW)'][m]) - float(ClosestCapacity))):
                    ClosestCapacity = data['Capacity MW'][n]
                    ClosestEIC = str(data['Registered Resource EIC code'][n])
                    ClosestID = IDPick(str(data['NGC BM Unit ID'][n]), str(data['Registered Resource Name'][n]))
                    error[m][0] = int(100 * SmallOnLargeDiffFraction(float(ClosestCapacity), float(data['Installed Capacity (MW)'][m])) + 0.5)
                    error[m][1] = terror[n]
        #Assign value
        data.iloc[m, data.columns.get_loc('outputDUKESToBMRSEIC')] = ClosestEIC #When assigning a value to a dataframe, need to do this unfortunately (no way to do it with a string and integer index, so need to go for the integer one). Also it's row, column, rather than column, row as we use in other cases.
        data.iloc[m, data.columns.get_loc('outputDUKESToBMRSID')] = ClosestID
        data.iloc[m, data.columns.get_loc('CapacityDiff')] = error[m][0]
        data.iloc[m, data.columns.get_loc('MatchType')] = error[m][1]
    
    #Data now has the output values, so need to export this back to the csv document.
    data.to_csv(csvName, index = False)

    #3: Check Integrity of Final Matches of DUKES Stations. 
    data = pd.read_csv(csvName) #Reopen to have everything (mainly dates) in the python format (as it changes, just be python reading it). 
    data['Effective From (Date)'] = pd.to_datetime(data['Effective From (Date)'])

    #Compare using these by type:
    solar = ["Solar", "PV"] #This is used to autofail as BMRS has no solar. So if DUKES gives this, it fails the test. 
    onshorewind = ["Wind Onshore", "Wind (onshore)", "Onshore"] #To compare BMRS and DUKES. 
    offshorewind = ["Wind Offshore", "Wind (offshore)", "Offshore"] #To compare BMRS and DUKES. 
    wind = ["Wind", "Wind Power"] #To compare BMRS and DUKES as a backup to the above.
    wind.extend(onshorewind)
    wind.extend(offshorewind)
    biomass = ["Biomass", "Bio", "Biomass (woodchip)", "Biomass (wood pellets, sunflower/oat husk pellets)", "Biomass (straw)", "Biomass (poultry litter, woodchip)", "Biomass (meat and bone meal)", "Biomass (wood pellets)", "Biomass (recycled wood, virgin wood)", "Biomass (recycled wood)", "Biomass (litter, woodchip)", "Biomass (poultry litter, waste wood)", "Biomass (virgin wood)"] #To compare BMRS and DUKES. 
    coal = ["Fossil Hard coal", "Coal"] #To compare BMRS and DUKES. 
    hydro = ["Hydro Water Reservoir", "Hydro Run-of-river and poundage", "Hydro / pumped storage", "Hydro", "Hydropower", "Pumped Storage", "Pumped"] #To compare BMRS and DUKES.
    gas = ["Fossil Gas", "Gas", "CCGT", "OCGT", "Natural gas", "Natural Gas", "Sour gas"] #To compare BMRS and DUKES. 
    nuclear = ["Nuclear", "AGR", "PWR", "Nuke"] #To compare BMRS and DUKES.
    oil = ["Oil", "Diesel", "Fossil Oil", "Diesel/gas oil"] #To compare BMRS and DUKES.
    #For those that are difficult to compare: 
    other = ["", "nan", "Generation", "Other", "Conventional Steam", "Waste (municipal solid waste)", "Waste (anaerobic digestion)", "Waste"] #This is used differently to the above. If an item is in here the test gives a neutral response (0), as these types cannot be clearly mapped. 
    allgen = ["Power"]
    allgen.extend(solar)
    allgen.extend(wind)
    allgen.extend(biomass)
    allgen.extend(coal)
    allgen.extend(hydro)
    allgen.extend(gas)
    allgen.extend(nuclear)
    allgen.extend(oil)
    allgen.extend(other)
    
    for i in range(0,len(data['MatchYear'])):
        data.iloc[i, data.columns.get_loc('MatchYear')] = 999
    for i in range(0,len(data['MatchGenType'])):
        data.iloc[i, data.columns.get_loc('MatchGenType')] = 999

    #Loop through the DUKES matches to BMRS
    for i in range(0,len(data['outputDUKESToBMRSEIC'])):
        #Loop through the BMRS
        for j in range(0,len(data['Registered Resource EIC code'])):
            if data['outputDUKESToBMRSEIC'][i] == data['Registered Resource EIC code'][j]:
                #Compare Installation Year, and Type
                #Installation Year
                if (str(data['Effective From (Date)'][j].year) == str(data['Year of commission or year generation began'][i])):
                    error[i][2] = -5
                elif abs(int(data['Effective From (Date)'][j].year) - int(data['Year of commission or year generation began'][i])) < 2:
                    error[i][2] = -1
                elif abs(int(data['Effective From (Date)'][j].year) - int(data['Year of commission or year generation began'][i])) < 3:
                    error[i][2] = 0
                elif (int(data['Effective From (Date)'][j].year) < int(data['Year of commission or year generation began'][i])):
                    error[i][2] = 0
                else:
                    error[i][2] = 1
                data.iloc[i, data.columns.get_loc('MatchYear')] = error[i][2]
                
                #Type (incl. auto solar fail)
                if (data['Type'][i] in solar) or (data['Fuel'][i] in solar): #No need for BMRS check, as it has no solar. If it does in the future, this rule has to be changed to be similar to those below. 
                    error[i][3] = 999
                elif ((data['Type'][i] in onshorewind) or (data['Fuel'][i] in onshorewind)) and ((data['PSR Type'][j] in onshorewind)):
                    error[i][3] = (-int(x/5) -3)
                elif ((data['Type'][i] in offshorewind) or (data['Fuel'][i] in offshorewind)) and ((data['PSR Type'][j] in offshorewind)):
                    error[i][3] = (-int(x/5) -3)
                elif ((data['Type'][i] in wind) or (data['Fuel'][i] in wind)) and ((data['PSR Type'][j] in wind)):
                    error[i][3] = -2
                elif ((data['Type'][i] in biomass) or (data['Fuel'][i] in biomass)) and ((data['PSR Type'][j] in biomass)):
                    error[i][3] = (-int(x/5) -3)
                elif ((data['Type'][i] in coal) or (data['Fuel'][i] in coal)) and ((data['PSR Type'][j] in coal)):
                    error[i][3] = (-int(x/5) -3)
                elif ((data['Type'][i] in hydro) or (data['Fuel'][i] in hydro)) and ((data['PSR Type'][j] in hydro)):
                    error[i][3] = (-int(x/5) -3)
                elif ((data['Type'][i] in gas) or (data['Fuel'][i] in gas)) and ((data['PSR Type'][j] in gas)):
                    error[i][3] = (-int(x/5) -3)
                elif ((data['Type'][i] in nuclear) or (data['Fuel'][i] in nuclear)) and ((data['PSR Type'][j] in nuclear)):
                    error[i][3] = (-int(x/5) -3)
                elif ((data['Type'][i] in oil) or (data['Fuel'][i] in oil)) and ((data['PSR Type'][j] in oil)):
                    error[i][3] = (-int(x/5) -3)
                elif (data['Type'][i] in other) or (data['Fuel'][i] in other) or (data['PSR Type'][j] in other):
                    error[i][3] = -2
                elif((data['Type'][i] not in allgen) or (data['Fuel'][i] not in allgen) or (data['PSR Type'][j] not in allgen)): #Can't find it in known types. 
                    error[i][3] = 1
                else: #Generator types were known, but just not matched. 
                    error[i][3] = x #10 by default
                data.iloc[i, data.columns.get_loc('MatchGenType')] = error[i][3]
    
    #Final Confidence
    for i in range(0,len(data['ConfidenceScore'])):
        data.iloc[i, data.columns.get_loc('ConfidenceScore')] = 999
    for i in range(0,len(data['ConfidenceResult'])):
        data.iloc[i, data.columns.get_loc('ConfidenceResult')] = 999

    #The scores should sum to a verdict. If it is less than or equal to 'x' then it is approved.
    logger.info('\nBMRSEICDUKESMap BMRS->EIC->DUKES mapping complete, results as follows: ')
    for i in range(0,len(data['outputDUKESToBMRSEIC'])):
        error[i][4] = error[i][0] + error[i][1] + error[i][2] + error[i][3]
        data.iloc[i, data.columns.get_loc('ConfidenceScore')] = error[i][4]
        if error[i][4] <= x:
            data.iloc[i, data.columns.get_loc('ConfidenceResult')] = 1
        else:
            data.iloc[i, data.columns.get_loc('ConfidenceResult')] = 0
        #Log final result for DUKES station. 
        logger.info('Match Result For, DUKES Station Name: {}, EIC: {}, Name Match: {}, Capacity Difference (if known, and at the error threshold if unknown) (%): {}, Start Year: {}, Generation Type: {}, Match: {}. '.format(str(data['Station Name'][i]), str(data['outputDUKESToBMRSEIC'][i]), ErrorNameText(data['MatchType'][i]), ErrorCapacityDifferenceText(data['CapacityDiff'][i]), ErrorDateText(data['MatchYear'][i]), ErrorGenTypeText(x, data['MatchGenType'][i]), ErrorMatch(data['ConfidenceResult'][i])))
        
    
    #Re-export to csv, with the confidence values. 
    data.to_csv(csvName, index = False)
    logger.info('BMRSEICDUKESMap Function Completed')



###Main Function###
if __name__ == "__main__":
    #You may add two args to this input: 
    #The first is the name of the csv sheet to open (type: string).'Input-Template.csv' is default. 
    #The second is the error threshold (type: int). Reccomended 5-50 range, where 5 is more sensitive and 50 is less sensitive. If in doubt use 10 (default). Be sure to give the file extension.
    #If you give two valid integers, or two non-integers (presumably file names then), the latter arg shall be used for its respective category. 
    args = sys.argv[1:]
    inputName = 'Input-Template.csv' #Default, will be changed if a valid input is given.
    errorThreshold = 10
    if len(args) > 0:
        if args[0].isdigit() == True:
            errorThreshold = int(args[0])
        else: 
            inputName = args[0]
    if len(args) > 1:
        if args[0].isdigit() == True:
            errorThreshold = int(args[1])
        else:
            inputName = args[1]
    BMRSEICDUKESMap(inputName, errorThreshold)
    #print("END")

#sys.exit()

