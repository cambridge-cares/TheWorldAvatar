# Write nasa polynomials to dat file
def writeNasaDatFile(Sp,datfile):
    with open(datfile, 'w', newline='') as thdfile:
        thdfile.write("{:16}".format(Sp.Formula))
        thdfile.write("{:8}".format('STHD'))
        for i in range(0,8):
            if (i+1)%2==0:
                if i>len(Sp.Composition)-1:
                    thdfile.write("{:2}".format(' '))
                else:
                    thdfile.write("{0:3d}".format(int(Sp.Composition[i])))
            else:
                if i>len(Sp.Composition)-1:
                    thdfile.write("{:3}".format(' '))
                else:
                    thdfile.write("{:2}".format(Sp.Composition[i]))

        thdfile.write("{:1}".format('G'))
        thdfile.write("{0:10.2f}".format(Sp.FitTrangeNasa[0]))
        thdfile.write("{0:10.2f}".format(Sp.FitTrangeNasa[2]))
        thdfile.write("{0:8.2f}".format(Sp.FitTrangeNasa[1]))
        thdfile.write("{:6}".format(' '))
        thdfile.write("{:1}".format('1\n'))

        for i in range(0,5):
            thdfile.write("{0:15.8e}".format(Sp.FitHighNasa[i]))
        thdfile.write("{:4}".format(' '))
        thdfile.write("{:1}".format('2\n'))
        thdfile.write("{0:15.8e}".format(Sp.FitHighNasa[5]))
        thdfile.write("{0:15.8e}".format(Sp.FitHighNasa[6]))
        for i in range(0,3):
            thdfile.write("{0:15.8e}".format(Sp.FitLowNasa[i]))
        thdfile.write("{:4}".format(' '))
        thdfile.write("{:1}".format('3\n'))
        for i in range(3,7):
            thdfile.write("{0:15.8e}".format(Sp.FitLowNasa[i]))
        thdfile.write("{:19}".format(' '))
        thdfile.write("{:1}".format('4\n'))
    thdfile.close()