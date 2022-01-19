import numpy as np
import csv

def writeThermoDatCsvFile(Species):
    ThData, ThDataHeaders = get_diagnostic_file_content(Species)

    with open(Species.DevOutFile, 'w', newline='') as csvfile:
        wrt = csv.writer(csvfile, delimiter=',')
        wrt.writerow(ThDataHeaders)
        for row in ThData:
            wrt.writerow(row)


def get_diagnostic_file_content(Spec):
    unit={'Energy':['[J/mol]',1.0,0.0],
          'Entropy':['[J/mol/K]',1.0,0.0],
          'HeatCap':['[J/mol/K]',1.0,0.0],
          'Temperature':['[K]',1.0,0.0]}
    dataitems = ['S', 'H', 'Cp', 'Cv', 'U', 'G']
    dataunits = ['Entropy', 'Energy', 'HeatCap', 'HeatCap', 'Energy', 'Energy']
    DataHeaders = ['T ' + unit['Temperature'][0]]
    T = Spec.RequestedTrange

    inclNasa = Spec.DevHighNasaCoeffs is not None \
               and Spec.DevLowNasaCoeffs is not None \
               and Spec.DevNasaTemps is not None
    inclSthdNasaDiff = inclNasa
    inclFitNasa = Spec.FitNasa
    inclFitNasaNasaDiff = inclFitNasa and inclNasa
    inclSthdNasaFitNasaDiff = inclFitNasa

    for i,item in enumerate(dataitems):
        DataHeaders.append(item + ' sthd ' + unit[dataunits[i]][0])

    if Spec.DevHighNasaCoeffs is not None and Spec.DevLowNasaCoeffs is not None \
       and Spec.DevNasaTemps is not None:
        for i,item in enumerate(dataitems):
            DataHeaders.append(item + ' nasa ' + unit[dataunits[i]][0])

    if Spec.FitNasa:
        for i,item in enumerate(dataitems):
            DataHeaders.append(item + ' fitnasa ' + unit[dataunits[i]][0])

    if inclSthdNasaDiff and inclNasa:
        for i,item in enumerate(dataitems):
            DataHeaders.append(item + ' abs sthd nasa diff ' + unit[dataunits[i]][0])

    if inclFitNasaNasaDiff and inclFitNasa:
        for i,item in enumerate(dataitems):
            DataHeaders.append(item + ' abs fitnasa nasa diff ' + unit[dataunits[i]][0])

    if inclSthdNasaFitNasaDiff and inclFitNasa:
        for i,item in enumerate(dataitems):
            DataHeaders.append(item + ' abs sthd fitnasa diff ' + unit[dataunits[i]][0])


    Data = np.zeros([len(T),len(DataHeaders)])
    for i,Ti in enumerate(T):
        nc = 6
        Data[i][0] = Ti*unit['Temperature'][1]+unit['Temperature'][2]
        Data[i][1] = Spec.getSpEntropySTHD(Ti)*unit['Entropy'][1]+unit['Entropy'][2]
        Data[i][2] = Spec.getSpEnthalpySTHD(Ti)*unit['Energy'][1]+unit['Energy'][2]
        Data[i][3] = Spec.getSpHeatCapacityCpSTHD(Ti)*unit['HeatCap'][1]+unit['HeatCap'][2]
        Data[i][4] = Spec.getSpHeatCapacityCvSTHD(Ti)*unit['HeatCap'][1]+unit['HeatCap'][2]
        Data[i][5] = Spec.getSpInternalEnergySTHD(Ti)*unit['Energy'][1]+unit['Energy'][2]
        Data[i][6] = Spec.getSpGibbsEnergySTHD(Ti)*unit['Energy'][1]+unit['Energy'][2]

        if inclNasa:
            Data[i][nc+1] = Spec.getSpEntropyNASA(Ti,useFittedNasa=False)*unit['Entropy'][1]+unit['Entropy'][2]
            Data[i][nc+2] = Spec.getSpEnthalpyNASA(Ti,useFittedNasa=False)*unit['Energy'][1]+unit['Energy'][2]
            Data[i][nc+3]  = Spec.getSpHeatCapacityCpNASA(Ti,useFittedNasa=False)*unit['HeatCap'][1]+unit['HeatCap'][2]
            Data[i][nc+4]  = Spec.getSpHeatCapacityCvNASA(Ti,useFittedNasa=False)*unit['HeatCap'][1]+unit['HeatCap'][2]
            Data[i][nc+5]  = Spec.getSpInternalEnergyNASA(Ti,useFittedNasa=False)*unit['Energy'][1]+unit['Energy'][2]
            Data[i][nc+6]  = Spec.getSpGibbsEnergyNASA(Ti,useFittedNasa=False)*unit['Energy'][1]+unit['Energy'][2]
            nc = nc + 6

        if inclFitNasa:
            Data[i][nc+1] = Spec.getSpEntropyNASA(Ti)*unit['Entropy'][1]+unit['Entropy'][2]
            Data[i][nc+2] = Spec.getSpEnthalpyNASA(Ti)*unit['Energy'][1]+unit['Energy'][2]
            Data[i][nc+3] = Spec.getSpHeatCapacityCpNASA(Ti)*unit['HeatCap'][1]+unit['HeatCap'][2]
            Data[i][nc+4] = Spec.getSpHeatCapacityCvNASA(Ti)*unit['HeatCap'][1]+unit['HeatCap'][2]
            Data[i][nc+5] = Spec.getSpInternalEnergyNASA(Ti)*unit['Energy'][1]+unit['Energy'][2]
            Data[i][nc+6] = Spec.getSpGibbsEnergyNASA(Ti)*unit['Energy'][1]+unit['Energy'][2]
            nc = nc + 6

        if inclSthdNasaDiff and inclNasa:
            Data[i][nc+1] = abs(Data[i][1]-Data[i][7])
            Data[i][nc+2] = abs(Data[i][2]-Data[i][8])
            Data[i][nc+3] = abs(Data[i][3]-Data[i][9])
            Data[i][nc+4] = abs(Data[i][4]-Data[i][10])
            Data[i][nc+5] = abs(Data[i][5]-Data[i][11])
            Data[i][nc+6] = abs(Data[i][6]-Data[i][12])
            nc = nc + 6

        if inclFitNasaNasaDiff and inclFitNasa and inclNasa:
            Data[i][nc+1] = abs(Data[i][13]-Data[i][7])
            Data[i][nc+2] = abs(Data[i][14]-Data[i][8])
            Data[i][nc+3] = abs(Data[i][15]-Data[i][9])
            Data[i][nc+4] = abs(Data[i][16]-Data[i][10])
            Data[i][nc+5] = abs(Data[i][17]-Data[i][11])
            Data[i][nc+6] = abs(Data[i][18]-Data[i][12])
            nc = nc + 6

        if inclSthdNasaFitNasaDiff and inclFitNasa:
            shift = 0
            if inclNasa: shift = 6
            Data[i][nc+1] = abs(Data[i][1]-Data[i][7+shift])
            Data[i][nc+2] = abs(Data[i][2]-Data[i][8+shift])
            Data[i][nc+3] = abs(Data[i][3]-Data[i][9+shift])
            Data[i][nc+4] = abs(Data[i][4]-Data[i][10+shift])
            Data[i][nc+5] = abs(Data[i][5]-Data[i][11+shift])
            Data[i][nc+6] = abs(Data[i][6]-Data[i][12+shift])

    return Data,DataHeaders