import csv
import chemspecies as chs

def writeThermoDatCsvFile(Sp,outfile,T=[298,300,400,600,800,1000,1500,2000,2500,3000],inclFitNasa=True,inclNasa=False):
    unit=1
    ThData, ThDataHeaders = chs.GetThermoData(T,Sp,unit,inclNasa,inclFitNasa)
    with open(outfile, 'w', newline='') as csvfile:
        wrt = csv.writer(csvfile, delimiter=',')
        wrt.writerow(ThDataHeaders)
        for row in ThData:
            wrt.writerow(row)
    csvfile.close()