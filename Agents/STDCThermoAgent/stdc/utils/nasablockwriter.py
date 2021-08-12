import io

def writeNasaChemkinBlock(Formula,Composition,P,Tlow,Tmid,Thigh,
                     NasaHighTCoeffs,NasaLowTCoeffs):
    thdfile = io.StringIO(newline='')

    thdfile.write("{:16}".format(Formula))
    thdfile.write("{:8}".format('STHD'))
    for i in range(0,8):
        if (i+1)%2==0:
            if i>len(Composition)-1:
                thdfile.write("{:2}".format(' '))
            else:
                thdfile.write("{0:3d}".format(int(Composition[i])))
        else:
            if i>len(Composition)-1:
                thdfile.write("{:3}".format(' '))
            else:
                thdfile.write("{:2}".format(Composition[i]))

    thdfile.write("{:1}".format('G'))
    thdfile.write("{0:10.2f}".format(Tlow))
    thdfile.write("{0:10.2f}".format(Thigh))
    thdfile.write("{0:8.2f}".format(Tmid))
    thdfile.write("{:6}".format(' '))
    thdfile.write("{:1}".format('1\n'))

    for i in range(0,5):
        thdfile.write("{0:15.8e}".format(NasaHighTCoeffs[i]))
    thdfile.write("{:4}".format(' '))
    thdfile.write("{:1}".format('2\n'))
    thdfile.write("{0:15.8e}".format(NasaHighTCoeffs[5]))
    thdfile.write("{0:15.8e}".format(NasaHighTCoeffs[6]))
    for i in range(0,3):
        thdfile.write("{0:15.8e}".format(NasaLowTCoeffs[i]))
    thdfile.write("{:4}".format(' '))
    thdfile.write("{:1}".format('3\n'))
    for i in range(3,7):
        thdfile.write("{0:15.8e}".format(NasaLowTCoeffs[i]))
    thdfile.write("{:19}".format(' '))
    thdfile.write("{:1}".format('4\n'))

    thdfileContent = thdfile.getvalue()
    thdfile.close()
    return thdfileContent