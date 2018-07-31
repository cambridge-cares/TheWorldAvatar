import chemspecies as chs
import utilities as utl

# Parse dat file with molecular data
#--------------------------------------------------
def readChemSpeciesFile(spFile,spName='ALL'):
    #----------------------------------------------------
    # main body of the "readChemSpeciesFile" function
    #----------------------------------------------------
    parseVars={} 
    parseVars['data_delim']      = ','
    parseVars['start_bracket']  = '['
    parseVars['end_bracket']  = ']'
    parseVars['comment_char']    = '!'
    
    #set default values
    dfPs= chs.getDefaultProps()
    
    rSpecList= []
    stop_parsing = False
    with open(spFile, 'r') as f:
        while True:
            line = f.readline()
            if not line: break
            skip_line = False
            line = line.rstrip('\n')
            line = line.strip()
            if len(line) == 0:
                skip_line = True
            elif line[0] == parseVars['comment_char']:
                skip_line = True
            if skip_line == False:
                if parseVars['comment_char'] in line:
                    line = line.split(parseVars['comment_char'])
                    line = line[0]
                line = line.split()
                key = line[0]
                if len(line)>1:
                    value = line[1]
                if 'SPECIES' in key:
                    dfPs['Name'] = value
                    if spName.upper() !='ALL':
                        if spName.upper()!=dfPs['Name'].upper():
                            while True:
                                line = f.readline()
                                if not line: break
                                line = line.rstrip('\n')
                                line = line.strip()
                                if line =='END':
                                    break
                        else:
                            stop_parsing = True
                elif 'Formula' in key:
                    dfPs['Formula'] = value
                elif 'Composition' in key:
                    dfPs['Comp'] = value.split(parseVars['data_delim'])
                elif 'TfitNasa' in key:
                    value = value.split(parseVars['data_delim'])
                    dfPs['FitTrangeNasa'] = [float(x) for x in value]
                elif 'ToutCsv' in key:
                    value = value.split(parseVars['data_delim'])
                    dfPs['ToutCsv'] = [float(x) for x in value]
                elif 'Frequencies[' in key:
                    f,dfPs['VibFreq'] = readFrequencies(f,key,parseVars)
                elif 'FreqScalingFactor' in key:
                    dfPs['VibScale'] = float(value)
                elif 'SymmetryNr' in key:
                    dfPs['SymNr'] = float(value)
                elif 'GeomType' in key:
                    if value == 'atomic':
                        dfPs['GeomType'] = 0
                    elif value == 'linear':
                        dfPs['GeomType'] = 1
                    else:
                        dfPs['GeomType'] = 2
                elif 'Geometry[' in key:
                    f,dfPs['Geom'] = readGeometry(f,key,parseVars)
                elif 'SpinMultiplicity' in key:
                    dfPs['SpinMult'] = float(value)
                elif 'ElecEnergy' in key:
                    runit = getUnit(key,parseVars)
                    u_fact = utl.convertEnergyMoleUnitsToSI(runit)
                    dfPs['ElecEn'] = float(value)*u_fact
                elif 'InertiaMom' in key:
                    if len(dfPs['Imom']) != 0:
                        print('Warning: Multiple definition of moments of inertia.')
                    value = ''.join(line[1:])
                    runit = getUnit(key,parseVars)
                    u_fact = utl.convertInertiaUnitsToSI(runit)
                    value = value.split(parseVars['data_delim'])
                    dfPs['Imom'] = [float(value[0])*u_fact,float(value[1])*u_fact,float(value[2])*u_fact]
                    #get units right
                elif 'RotConstants' in key:
                    RotConst = []
                    if len(dfPs['Imom']) != 0:
                        print('Warning: Multiple definition of moments of inertia.')
                    value = ''.join(line[1:])
                    runit = getUnit(key,parseVars)
                    u_fact1 = utl.convertEnergyMoleculeUnitsToSI(runit,1.0) # 1/TIME,Hz,1/cm => J
                    u_fact2 = utl.convertEnergyMoleculeUnitsToSI('1/M',-1.0) # J => 1/m
                    u_fact = u_fact1*u_fact2
                    value = value.split(parseVars['data_delim'])
                    for v in value:
                        RotConst.append(float(v)*u_fact)
                    dfPs['Imom'] = chs.getImomFromRotConstant(RotConst)
                    #get units right
                elif 'ZeroPointEnergy' in key:
                    runit = getUnit(key,parseVars)
                    u_fact = utl.convertEnergyMoleculeUnitsToSI(runit)
                    dfPs['ZPE'] = float(value)*u_fact
                elif 'ElecLevels' in key:
                    f,dfPs['ElecLvL'] = readElecLevels(f,key,parseVars)
                elif 'NasaThermCoeffs' in key:
                    Nasa = []
                    f,dfPs['LowNasa'],dfPs['HighNasa'],dfPs['TrangeNasa'] = readNasaThermCoeffs(f,parseVars)
                elif 'EnhtalpyRef' in key:
                    f,dfPs['EnthRefSource'],dfPs['EnthRefTemp'],dfPs['EnthRef'] = readEnthalpyRef(f,value,parseVars)
                elif 'END' in key:

                    rSpec = chs.CreateChemSpecFromDict(dfPs)
                    #add species to a list
                    rSpecList.append(rSpec)
                    if stop_parsing:
                        break
                    #set keywords to defaults
                    dfPs= chs.getDefaultProps()
    return rSpecList


# Parse Frequency section in a file
#--------------------------
def readFrequencies(rf,key,parseVars):
    runit = getUnit(key,parseVars)
    u_fact = utl.convertFrequencyUnitsToSI(runit)
    freq = []
    while True:
        line = rf.readline()
        stop_parsing ,skip_line, line = checkLine(line,'EndFrequencies',
                                parseVars['comment_char'],['r','s'],0)
        if stop_parsing: break
        if skip_line == False:
            if line[-1] == parseVars['data_delim']:
                line = line[:-1]
            if line[0] == parseVars['data_delim']:
                line = line[1:]
            line = line.split(parseVars['data_delim'])
            for items in line:
                freq.append(float(items)*u_fact)
    return rf,freq

# Parse Geometry section in a file
#--------------------------
def readGeometry(rf,key,parseVars):
    runit = getUnit(key,parseVars)
    u_fact = utl.convertLengthUnitsToSI(runit)
    geom = []
    while True:
        line = rf.readline()
        stop_parsing,skip_line,line = checkLine(line,'EndGeometry',
                                parseVars['comment_char'],['r','s'],4)
        if stop_parsing: break
        if skip_line == False:
            geom.append([line[0], float(line[1])*u_fact,float(line[2])*u_fact,float(line[3])*u_fact])
    return rf,geom

# Parse Electronic levels section in a file
#--------------------------    
def readElecLevels(rf,key,parseVars):
    runit = getUnit(key,parseVars)
    u_fact = utl.convertEnergyMoleculeUnitsToSI(runit)
    ElecLvl = []
    while True:
        #skip_line = False
        line = rf.readline()

        stop_parsing,skip_line,line = checkLine(line,'EndElecLevels',
                                parseVars['comment_char'],['r','l','s'],2)
        if stop_parsing: break
        if skip_line == False:
            ElecLvl.append([float(line[0]),float(line[1])*u_fact])
    return rf,ElecLvl

# Parse NASA thermdata section in a file
#--------------------------    
def readNasaThermCoeffs(rf,parseVars):
    nc = 0
    alow= []
    ahigh= []
    Trange=[]
    while True:
        line = rf.readline()
        stop_parsing,skip_line,line = checkLine(line,'EndNasaThermCoeffs',
                                parseVars['comment_char'],['r','l','s'],0)
        if stop_parsing: break
        if skip_line == False:
            nc = nc + 1
            if nc == 1:
                Trange.append(float(line[45:55]))
                Trange.append(float(line[65:73]))
                Trange.append(float(line[55:65]))
            else:
                if line[0]!='-':
                    line = ' '+line
                if nc == 2:
                    ahigh.append(float(line[0:15]))
                    ahigh.append(float(line[15:30]))
                    ahigh.append(float(line[30:45]))
                    ahigh.append(float(line[45:60]))
                    ahigh.append(float(line[60:75]))
                elif nc == 3:
                    ahigh.append(float(line[0:15]))
                    ahigh.append(float(line[15:30]))
                    alow.append(float(line[30:45]))
                    alow.append(float(line[45:60]))
                    alow.append(float(line[60:75]))
                elif nc == 4:
                    alow.append(float(line[0:15]))
                    alow.append(float(line[15:30]))
                    alow.append(float(line[30:45]))
                    alow.append(float(line[45:60]))
    return rf,alow,ahigh,Trange

# Parse Enthalpy Source section in a file
#--------------------------    
def readEnthalpyRef(rf,val,parseVars):
    EnthRefTemp = 298.15
    EnthRef = 0.0
    EnthSource = val
    while True:
        line = rf.readline()
        stop_parsing,skip_line,line = checkLine(line,'EndEnhtalpyRef',
                                parseVars['comment_char'],['r','s'],2)
        if stop_parsing: break
        if skip_line == False:
            key = line[0]
            value = line[1]
            if key == 'EnthRefSource':
                EnthSource = value
            elif 'EnthRefTemp' in key:
                runit = getUnit(key,parseVars)
                u_fact = utl.convertTemperatureUnitsToSI(runit)
                EnthRefTemp = float(value)*u_fact
            elif 'EnthRef' in key and EnthSource!='#NASA':
                runit = getUnit(key,parseVars)
                u_fact = utl.convertEnergyMoleUnitsToSI(runit)
                EnthRef = float(value)*u_fact
    return rf,EnthSource,EnthRefTemp,EnthRef

# Get unit from a keyword
#--------------------------
def getUnit(key,parseVars):
    key = key.split(parseVars['start_bracket'])
    key = key[1].split(parseVars['end_bracket'])
    key = key[0]
    runit = key.upper()
    return runit

# check and prepare file line
#--------------------------
def checkLine(line,token,com_char,line_op,check_len):
    skip_line = False
    stop_parsing = False
    if not line: stop_parsing = True
    if token in line: stop_parsing = True

    if stop_parsing==False:
        if utl.compareList('s',line_op):
            line = line.strip()
        if utl.compareList('l',line_op):
            line = line.lstrip()
        if utl.compareList('r',line_op):
            line = line.rstrip('\n')

        if len(line)==0:
            skip_line = True
        elif line[0] == com_char:
            skip_line = True

        if check_len>0:
            line = line.split()
            if len(line)<check_len:
                skip_line = True
    return stop_parsing,skip_line,line
#--------------------------