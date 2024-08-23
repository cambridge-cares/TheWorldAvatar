"""This module is developed for creating the figures visualise the generation output against a daily base"""

import sys, os
BASE = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, BASE) 
from UK_Digital_Twin_Package.OWLfileStorer import readFile
from pathlib import Path
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy, math
from matplotlib.ticker import MultipleLocator

officialRegionNameList = ["NE", "NW", "YH", "EM", "WM", "EE", "LDN", "SE", "SW", "WALES", "SCOT"]
labelFontSize = 14
legendFontSize = 12
dotLabel = 12
annotateSize = 12

localRootFilePath = str(Path(__file__).resolve().parent.parent.parent) + "/outputs/smr_replacement_fig"
generationPerFuelType = localRootFilePath + '/generationPerFuelType/'

"""This method is to create the file path for figs"""
def mkdirFilePath(path, addingPath):
    folder = os.path.exists(path + addingPath)
    if not folder:                
        os.makedirs(path + addingPath)           
        print("---  new folder %s...  ---" % path + addingPath)
    else:
        print("---  The folder exists! %s  ---" % path + addingPath)

"""Create the multiple lines diagram reflecting the weather condition impact under the same weight 0.5"""
def lineGraph_generationByTypePerYear_BMRS(sattlementPeriodPerDay:int, year):  
    ## Download the csv file from BMRS: https://www.bmreports.com/bmrs/?q=generation/fueltype/current

    ## read the data from resource
    generationByTypePath = str(Path(__file__).resolve().parent.parent.parent) + f"/resources/BMRS_GenerationDataByFuelType/{str(year)}/GenerationbyFuelType.csv"
    generationByType = readFile(generationByTypePath)
    header = generationByType[0]
    del generationByType[0]
    
    ## if leap year
    if int(year) % 4 == 0:
        febdays = 29
    else:
        febdays = 28

    monthList = [31, febdays, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    ## calculate generation per day
    genlist = []

    index_generationByTypeList = 0

    for index_m, monthDays in enumerate(monthList):
        for i in range(monthDays):
            dailyOutput = [f"{str(year)}/{str(index_m + 1)}/{str(i + 1)}",0,0,0,0,0,0,0,0,0]
            for j in range(int(sattlementPeriodPerDay)):
                for k, output in enumerate(generationByType[index_generationByTypeList]):
                    if k >= 2: 
                        dailyOutput[k-1] += float(output)/48/1E3 ## Unit: GW
                for i_dop, dop in enumerate(dailyOutput):
                    if i_dop >= 1:
                        dailyOutput[i_dop] = round(dop, 2)
                index_generationByTypeList += 1
            genlist.append(dailyOutput)

    ## set up the clourmap
    cmap = mpl.cm.get_cmap("viridis", len(header) - 2) # viridis RdYlGn
    colors = cmap(numpy.linspace(0, 1, len(header) - 2))
    
    ##-- Plot the output by fuel type vs time --##
    fig, ax1 = plt.subplots()
    ax1.set_ylabel("Generation output (GW)", fontsize = labelFontSize)

    ## Find the maximum
    maximum = 0
    # genlist = numpy.array(genlist)
    for col in range(len(header) - 2):
        row_ = [row[col + 1] for row in genlist]
        max_ = max(row_)
        if float(max_) > maximum:
            maximum = float(max_)
    maximum = (math.ceil(maximum/5)) * 5

    ## plot each weather condition
    del header[0]
    del header[0]
    dayList = [i + 1 for i in range(len(genlist))]

    ## set the axis
    y_major_locator = MultipleLocator(5)
    ax1.yaxis.set_major_locator(y_major_locator)
    plt.ylim(0, maximum)

    genlist_ = numpy.array(genlist)
    timeline = genlist_[:, 0]

    for g in genlist:
        del g[0] 

    genlist = numpy.array(genlist)
    for h, fuelType in enumerate(header):
        ax1.plot(dayList, genlist[:, h], label = fuelType, color = colors[h], linewidth = 0.5)

    ## set legend
    pos = ax1.get_position() 
    ax1.set_position([pos.x0, pos.y0, pos.width, pos.height * 0.85])
    ax1.legend(
        loc="upper center",
        fontsize = legendFontSize,
        ncol=5,
        bbox_to_anchor=(0.5, 0.065),
        frameon=False,
        bbox_transform=fig.transFigure 
        ) 
    plt.tight_layout()
    mkdirFilePath(generationPerFuelType, f"{str(year)}/")
    plt.savefig(generationPerFuelType + f"{str(year)}/"+ "generationPerType.pdf", dpi = 1200, bbox_inches='tight')
    plt.clf()
    plt.cla()          
    return

"""Create the multiple lines diagram reflecting the weather condition impact under the same weight 0.5"""
def lineGraph_annualGeneration_WindAndSolar_ESO(sattlementPeriodPerDay:int, year:int, 
                                                windUpperBound:float, windLowerBound:float, windAverage:float,
                                                solarUpperBound:float, solarLowerBound:float, solarAverage:float):  
    ## Download the csv file from ESO: https://data.nationalgrideso.com/carbon-intensity1/historic-generation-mix/r/historic_gb_generation_mix
    year = int(year)
    ## read the data from resource
    generationByTypePath = str(Path(__file__).resolve().parent.parent.parent) + "/resources/ESO_GenerationDataByFuelType/eso.csv"
    generationByType = readFile(generationByTypePath)

    ## Fuel type
    for i_f, fuel in enumerate(generationByType[0]): 
        if "WIND" == fuel:
            windCOL = i_f
        elif "SOLAR" == fuel:
            solarCOL = i_f
    ## Find the start and end line
    for i, line in enumerate(generationByType):
        if f"{str(year)}-01-01 00:00:00+00" in line:
            startLine = i
        elif f"{str(year + 1)}-01-01 00:00:00+00" in line:
            endLine = i
            break
    
    for line in generationByType:
        del line[0]

    generationByType = numpy.array(generationByType)
    generationByType = generationByType[startLine:endLine]

    ## if leap year
    if int(year) % 4 == 0:
        febdays = 29
        daysNum = 366
    else:
        febdays = 28
        daysNum = 365
    monthList = [31, febdays, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    ## calculate generation per day
    genlist = []
    index_generationByTypeList = 0
    for monthDays in monthList:
        for i in range(monthDays):
            dailyOutput = [0, 0]
            for j in range(int(sattlementPeriodPerDay)):
                dailyOutput[0] += float(generationByType[index_generationByTypeList][windCOL - 1])/int(sattlementPeriodPerDay)/1E3 ## Unit: GW
                dailyOutput[1] += float(generationByType[index_generationByTypeList][solarCOL - 1])/int(sattlementPeriodPerDay)/1E3 ## Unit: GW
                index_generationByTypeList += 1
            for i_dop, dop in enumerate(dailyOutput):
                dailyOutput[i_dop] = round(dop, 2)          
            genlist.append(dailyOutput)

    ##-- Plot the output by fuel type vs time --##
    fig, ax1 = plt.subplots()
    ax1.set_ylabel("Generation output (GW)", fontsize = labelFontSize)

    ## Find the maximum
    genlist = numpy.array(genlist)
    maximum = 0
    for col in range(2):
        max_ = max(genlist[:, col])
        if float(max_) > maximum:
            maximum = float(max_)
    maximum = (math.ceil(maximum/5)) * 5

    ## Day number list
    dayList = [i + 1 for i in range(daysNum)]

    ## set the axis
    y_major_locator = MultipleLocator(5)
    ax1.yaxis.set_major_locator(y_major_locator)
    plt.ylim(0, maximum)

    ax1.plot(dayList, genlist[:, 0], label = "Wind", color = "#1f4e79", linewidth = 1.3, alpha = 0.85)
    ax1.plot(dayList, genlist[:, 1], label = "Solar", color = "#548235" , linewidth = 1.3, alpha = 0.85) # "#F39530"

    ## New ticks showing the qtr of the year
    qtr_2 = monthList[0] + monthList[1] + monthList[2] + 1
    qtr_3 = monthList[3] + monthList[4] + monthList[5]
    qtr_4 = monthList[6] + monthList[7] + monthList[8]
    qtr_xticks = [1, qtr_2, qtr_2 + qtr_3, qtr_2 + qtr_3 + qtr_4, daysNum]  # Define the new tick locations
    qtr_xticklabels = [f'JAN {str(year)}', f'APR {str(year)}', f'JUL {str(year)}', f'OCT {str(year)}', f'DEC {str(year)}']  # Define the new tick labels
    ax1.set_xticks(qtr_xticks)
    ax1.set_xticklabels(qtr_xticklabels)

    ## horizontal line: label the wind and solar average 
    ax1.axhline(y=float(windUpperBound), color='#1a1a1a', linestyle='dashdot', alpha=1, lw = 1)
    ax1.axhline(y=float(windLowerBound), color='#1a1a1a', linestyle='dashdot', alpha=1, lw = 1)
    ax1.axhline(y=float(windAverage), color='#1a1a1a', linestyle='dashdot', alpha=1, lw = 1)

    ax1.axhline(y=float(solarUpperBound), color='#5D6D7E', linestyle='dashdot', alpha=1, lw = 1)
    ax1.axhline(y=float(solarLowerBound), color='#5D6D7E', linestyle='dashdot', alpha=1, lw = 1)
    ax1.axhline(y=float(solarAverage), color='#5D6D7E', linestyle='dashdot', alpha=1, lw = 1)

    ax1.text(5, float(windUpperBound) + 0.1, f'Wind max: {str(windUpperBound)} GW', color='#434c5e', fontsize = 9, alpha = 1, weight="bold")  
    ax1.text(5, float(windLowerBound) + 0.1, f'Wind min: {str(windLowerBound)} GW', color='#434c5e', fontsize = 9, alpha = 1, weight="bold")  
    ax1.text(5, float(windAverage) + 0.1, f'Wind average: {str(windAverage)} GW', color='#434c5e', fontsize = 9, alpha = 1, weight="bold")  
    
    ax1.text(240, float(solarUpperBound) - 0.6, f'Solar max: {str(solarUpperBound)} GW', color='#6E2C00', fontsize = 9, alpha = 0.9, weight="bold")  
    ax1.text(240, float(solarLowerBound) + 0.1, f'Solar min: {str(solarLowerBound)} GW', color='#6E2C00', fontsize = 9, alpha = 0.9, weight="bold")  
    ax1.text(240, float(solarAverage) + 0.1, f'Solar average: {str(solarAverage)} GW', color='#6E2C00', fontsize = 9, alpha = 0.9, weight="bold")   

    ## set legend
    pos = ax1.get_position() 
    ax1.set_position([pos.x0, pos.y0, pos.width, pos.height * 0.85])
    ax1.legend(
        loc="upper center",
        fontsize = legendFontSize,
        ncol=5,
        bbox_to_anchor=(0.5, 0.065),
        frameon=False,
        bbox_transform=fig.transFigure 
        ) 
    plt.tight_layout()
    mkdirFilePath(generationPerFuelType, f"{str(year)}/")
    plt.savefig(generationPerFuelType + f"{str(year)}/"+ "generationPerType.pdf", dpi = 1200, bbox_inches='tight')
    plt.clf()
    plt.cla()          
    return

if __name__ == '__main__':
    lineGraph_annualGeneration_WindAndSolar_ESO(48, 2022, 17, 2.89, 8.9, 2.9, 0.25, 1.4)