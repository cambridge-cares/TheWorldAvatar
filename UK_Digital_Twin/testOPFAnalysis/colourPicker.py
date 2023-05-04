##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 25 April 2023        #
##########################################

import math
import matplotlib.pyplot as plt
import matplotlib as mpl

colourHEXList_9class = [
    '#67000d',
    '#a50f15',
    '#cb181d',
    '#ef3b2c',
    '#fb6a4a',
    '#fc9272',
    '#fcbba1',
    '#fee0d2',
    '#fff5f0',
    '#ffffff', ## white, for the middle is the middle is given
    '#f7fcf5',
    '#e5f5e0',
    '#c7e9c0',
    '#a1d99b',
    '#74c476',
    '#41ab5d',
    '#238b45',
    '#006d2c',
    '#00441b']

colourHEXList_9ClasswithElimination = [## eliminate the first 2 colours
    '#67000d',
    '#a50f15',
    '#cb181d',
    '#ef3b2c',
    '#fb6a4a',
    '#fc9272',
    '#fcbba1',
    '#fee0d2',
    '#fff5f0',
    '#ffffff', ## white, for the middle is the middle is given
    '#f7fcf5',
    '#e5f5e0',
    '#c7e9c0',
    '#a1d99b',
    '#74c476',
    '#41ab5d',
    '#238b45',
    '#006d2c',
    '#00441b']

colourHEXList_11class = [ ## eliminate the first colour, 6class
    '#de2d26', # '#a50f15',
    '#fb6a4a',
    '#fc9272',
    '#fcbba1',
    '#FEEAE2',
    '#EEF8EC', # '#ffffff', ## white, for the middle is the middle is given
    '#c7e9c0',
    '#a1d99b',
    '#74c476',
    '#31a354',
    '#006d2c']

colourHEXListForBranch_8class = [ ## Yellow for grid 
    '#ffffcc',
    '#ffeda0',
    '#fed976',
    '#feb24c',
    '#fd8d3c',
    '#fc4e2a',
    '#e31a1c',
    '#b10026']

colourHEXListForBranch_7class = [ ## Green for output
    '#f7fcf5',
    '#e5f5e0',
    '#c7e9c0',
    '#a1d99b',
    '#74c476',
    '#41ab5d',
    '#238b45',
    '#005a32'
]

# colourHEXListForBranch_8class = [
#     '#fde0dd',
#     '#fcc5c0',
#     '#fa9fb5',
#     '#f768a1',
#     '#dd3497',
#     '#ae017e',
#     '#7a0177',
#     '#49006a']

def sequentialHEXColourCodePicker(dataValue, upperBound, lowerBound, middle = None, colourClassNumber:int = 11): 
    if colourClassNumber == 11:
        colourHEXList = colourHEXList_11class
        dividend = 5
        if middle is None:
            middle = round((upperBound - lowerBound) / 2, 2)
    elif colourClassNumber == 8:
        colourHEXList = colourHEXListForBranch_8class
        dividend = 8
    elif colourClassNumber == 7:
        colourHEXList = colourHEXListForBranch_7class
        dividend = 8
    else:
        raise ValueError('Invalid colourClassNumber. colourClassNumber should be either 11, 8 or 7, while picking 11, the middle should be specified.')

    if upperBound < lowerBound:
        raise ValueError('Invalid upper bound. Expected lower bound should be smaller than upper bound.')
    elif dataValue > upperBound or dataValue < lowerBound:
        raise ValueError('Invalid dataValue. Expected between upper and lower bound.') 

    ## round up new boundaries
    upperBound = math.ceil(upperBound)
    lowerBound = math.floor(lowerBound)

    if middle is not None: ## if the middle is given, it by default will be assumed to be '#ffffff'  
        if not (middle >= lowerBound and middle <= upperBound):
            raise ValueError('Invalid middle number is given. Middle should be between upper and lower bounds.')
        elif round(dataValue) == round(middle):
            return '#EEF8EC'
        else:
            if (upperBound - middle) >= (middle - lowerBound):
                interval = round((upperBound - middle) / dividend, 2)
            else:
                interval = round((middle - lowerBound) / dividend, 2)

            if dataValue > middle:
                index = colourHEXList.index('#EEF8EC') - math.ceil((dataValue - middle) / interval)
            else: 
                index = colourHEXList.index('#EEF8EC')  +  math.ceil((middle - dataValue) / interval) - 1
           
            if index == -1:
                index = 0
            elif index == len(colourHEXList):
                index = len(colourHEXList) - 1
 
            if index < -1 or index > len(colourHEXList):
                    raise ValueError('Invalid index value. Expected to be non-negative or exceed the lenth of th colour list.')
            return colourHEXList[index]
    else:
        interval =  round((upperBound - lowerBound) / dividend, 2)
        index = math.ceil((dataValue - lowerBound) / interval) - 1

        if index == -1:
            index = 0
        elif index == len(colourHEXList):
            index = len(colourHEXList) - 1
        if index < -1 or index > len(colourHEXList):
                    raise ValueError('Invalid index value. Expected to be non-negative or exceed the lenth of th colour list.')
        return colourHEXList[index]

def createColourBarLegend(filepath, upperBound, lowerBound, lebel:str, fileName:str, middle = None, colourClassNumber:int = 11):
    
    fig, ax = plt.subplots(figsize=(0.5, 12))
    fig.subplots_adjust(bottom=0.5)

    upperBound = math.ceil(upperBound)
    lowerBound = math.floor(lowerBound)

    if colourClassNumber == 11:
        colourHEXList = colourHEXList_11class
        dividend = 5
        if middle is None:
            middle = round((upperBound - lowerBound) / 2, 2)

        upperBoundHexColourCode = sequentialHEXColourCodePicker(upperBound, upperBound, lowerBound, middle)
        lowerBoundHexColourCode = sequentialHEXColourCodePicker(lowerBound, upperBound, lowerBound, middle)

        ub_index = colourHEXList.index(upperBoundHexColourCode)
        lb_index = colourHEXList.index(lowerBoundHexColourCode)

        cmap_list = colourHEXList[ub_index:lb_index + 1]
        cmap_list.reverse()
        cmap = mpl.colors.ListedColormap(cmap_list)

        if (upperBound - middle) >= (middle - lowerBound):
            interval = round((upperBound - middle) / dividend, 2)
        else:
            interval = round((middle - lowerBound) / dividend, 2)

        bounds = []
        upperBound = math.ceil((upperBound - middle) / interval) * interval
        for i in range(len(cmap_list) + 1):
            if i == 0:
                bounds.append(upperBound)
            else: 
                bounds.append(round(upperBound - (i * interval), 2))
        bounds.reverse()
    elif colourClassNumber == 8:
        colourHEXList = colourHEXListForBranch_8class
        dividend = 8
        interval =  round((upperBound - lowerBound) / dividend, 2)
        bounds = []
        for i in range(len(colourHEXList)):
            if i == 0:
                bounds.append(lowerBound)
            else: 
                bounds.append(round(lowerBound + (i * interval), 2))
        cmap = mpl.colors.ListedColormap(colourHEXList)
    elif colourClassNumber == 7:
        colourHEXList = colourHEXListForBranch_7class
        dividend = 8
        interval =  round((upperBound - lowerBound) / dividend, 2)
        bounds = []
        for i in range(len(colourHEXList)):
            if i == 0:
                bounds.append(lowerBound)
            else: 
                bounds.append(round(lowerBound + (i * interval), 2))
        cmap = mpl.colors.ListedColormap(colourHEXList)
    else:
        raise ValueError('Invalid colourClassNumber. colourClassNumber should be either 11, 8 or 7, while picking 11, the middle should be specified.')
    
    norm = mpl.colors.BoundaryNorm(bounds, cmap.N)
    cb2 = mpl.colorbar.ColorbarBase(ax, 
                                    cmap = cmap,
                                    norm = norm,
                                    boundaries = bounds,
                                    extend='max',
                                    ticks = bounds,
                                    spacing = 'proportional',
                                    orientation = 'vertical')
    cb2.set_label(str(lebel))
    ## fig.show()
    fig.set_size_inches(0.5, 12)
    plt.savefig(filepath + str(fileName) + '.png', dpi = 200, bbox_inches = "tight", transparent = True)
    return

if __name__ == '__main__': 
    ## print(sequentialHEXColourCodePicker(6.1, 6, 0, None, 7))
    createColourBarLegend('/mnt/d/wx243/FromTWA/', 5000, -40000, 'Net demanding (GWh/yr)', 'legend-netDemanding', 0, 11)