##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 19 Jan 2023          #
##########################################

import math
import matplotlib.pyplot as plt
import matplotlib as mpl

colourHEXList = [
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

def sequentialHEXColourCodePicker(dataValue, upperBound, lowerBound, middle = None):
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
        elif dataValue == middle:
            return '#ffffff'
        else:
            if (upperBound - middle) >= (middle - lowerBound):
                interval = round((upperBound - middle) / 10, 2)
            else:
                interval = round((middle - lowerBound) / 10, 2)

            if dataValue > middle:
                index = colourHEXList.index('#ffffff') - int((dataValue - middle) / interval)
            else: 
                index = colourHEXList.index('#ffffff')  +  math.ceil((middle - dataValue) / interval)
           
            if index == -1:
                index = 0
            elif index == len(colourHEXList):
                index = len(colourHEXList) - 1
 
            if index < -1 or index > len(colourHEXList):
                    raise ValueError('Invalid index value. Expected to be non-negative or exceed the lenth of th colour list.')
            return colourHEXList[index]
    else:
        interval =  round((upperBound - lowerBound)/20, 2)
        index = len(colourHEXList) - int((dataValue - lowerBound) / interval) - 1

        if index == -1:
            index = 0
        elif index == len(colourHEXList):
            index = len(colourHEXList) - 1
        if index < -1 or index > len(colourHEXList):
                    raise ValueError('Invalid index value. Expected to be non-negative or exceed the lenth of th colour list.')
        return colourHEXList[index]

def createColourBarLegend(filepath, upperBound, lowerBound, middle = None):
    fig, ax = plt.subplots(figsize=(0.5, 12))
    fig.subplots_adjust(bottom=0.5)

    upperBound = math.ceil(upperBound)
    lowerBound = math.floor(lowerBound)

    upperBoundHexColourCode = sequentialHEXColourCodePicker(upperBound, upperBound, lowerBound, middle)
    lowerBoundHexColourCode = sequentialHEXColourCodePicker(lowerBound, upperBound, lowerBound, middle)

    ub_index = colourHEXList.index(upperBoundHexColourCode)
    lb_index = colourHEXList.index(lowerBoundHexColourCode)

    cmap_list = colourHEXList[ub_index:lb_index + 1]
    cmap_list.reverse()
    cmap = mpl.colors.ListedColormap(cmap_list)

    if (upperBound - middle) >= (middle - lowerBound):
        interval = round((upperBound - middle) / 10, 2)
    else:
        interval = round((middle - lowerBound) / 10, 2)

    bounds = []
    for i in range(len(cmap_list) + 1):
        if i == 0:
            bounds.append(upperBound)
        else: 
            bounds.append(round(upperBound - (i * interval), 2))
    bounds.reverse()

    norm = mpl.colors.BoundaryNorm(bounds, cmap.N)
    cb2 = mpl.colorbar.ColorbarBase(ax, 
                                    cmap = cmap,
                                    norm = norm,
                                    boundaries = bounds,
                                    extend='max',
                                    ticks = bounds,
                                    spacing = 'proportional',
                                    orientation = 'vertical')
    cb2.set_label('Net demanding (GWh/yr)')
    ## fig.show()
    fig.set_size_inches(0.5, 12)
    plt.savefig( filepath + 'legend-netDemanding.png', dpi = 200, bbox_inches = "tight", transparent = True)
    return

if __name__ == '__main__': 
    #print(sequentialHEXColourCodePicker(-0.5, 1.15, -2.7, 0))
    createColourBarLegend('', 2.1, -1.3, 0)