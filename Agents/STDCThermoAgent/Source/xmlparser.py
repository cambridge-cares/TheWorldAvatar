import chemspecies as chs
from lxml import etree
import utilities as utl

# Parse xml file with molecular data
def readSpecXML(xmlfile):
    tree = etree.parse(xmlfile)
    root = tree.getroot()
    nsm = root.nsmap

    dfPs= chs.getDefaultProps()

    nsm['def'] = nsm[None]
    del nsm[None]

    # Get appropariate elements
    moduleInit = root.find(".//def:module[@dictRef='cc:initialization']",nsm)
    moleculeInit = moduleInit.find('.//def:molecule',nsm)
    paramListInit = moduleInit.find('.//def:parameterList',nsm)
    moduleEnv= root.find(".//def:module[@dictRef='cc:environment']",nsm)
    paramListEnv = moduleEnv.find('.//def:parameterList',nsm)

    # Read Name, Formula and Composition
    for property in moleculeInit.iterchildren():
        if 'atomArray' in property.tag:
            for elem in property.iterchildren():
                dfPs['Comp'].append(elem.attrib['elementType'] )
                dfPs['Comp'].append(str(int(float(elem.attrib['count']))))
        elif 'formula' in property.tag:
            dfPs['Name'] = ''.join(property.attrib['concise'].split())
            dfPs['Formula'] = dfPs['Name']

    # Read Molecular properties
    moduleFinit = root.find(".//def:module[@dictRef='cc:finalization']",nsm)
    for property in moduleFinit.iterchildren():
        if 'propertyList' in property.tag:
            for subprop in property.iterchildren():
                # Read vibrations
                if 'vibrations' in subprop.attrib['dictRef']:
                    propchild = subprop.getchildren()[0]
                    VibFreqUnit = propchild.attrib['units'].split(':')[1].upper()
                    if 'M-1' in VibFreqUnit:
                        VibFreqUnit = VibFreqUnit.replace('-1','^-1')
                    u_fact = utl.convertFrequencyUnitsToSI(VibFreqUnit)
                    dfPs['VibFreq'] = [float(f)*u_fact for f in propchild.text.split()]
                # Read SymNr
                elif 'rotational_symmetry' in subprop.attrib['dictRef']:
                    propchild = subprop.getchildren()[0]
                    dfPs['SymNr'] = float(propchild.text)
                # Read RotConst
                elif 'rotational_constants' in subprop.attrib['dictRef']:
                    propchild = subprop.getchildren()[0]
                    RotConstUnit = propchild.attrib['units'].split(':')[1].upper()
                    u_fact1 = utl.convertEnergyMoleculeUnitsToSI(RotConstUnit,1.0) # 1/TIME,Hz,1/cm => J
                    u_fact2 = utl.convertEnergyMoleculeUnitsToSI('1/M',-1.0) # J => 1/m
                    u_fact = u_fact1*u_fact2
                    RotConst = [float(f)*u_fact for f in propchild.text.split()]
                    dfPs['Imom']=chs.getImomFromRotConstant(RotConst)
                # Read GeomType
                elif 'geometry_type' in subprop.attrib['dictRef']:
                    propchild = subprop.getchildren()[0]
                    if propchild.text == 'nonlinear':
                        dfPs['GeomType'] = 2
                    elif propchild.text == 'linear':
                        dfPs['GeomType'] = 1
                    else:
                        dfPs['GeomType'] = 0
        elif 'molecule' in property.tag:
            # Read SpinMult
            dfPs['SpinMult']=float(property.attrib['spinMultiplicity'])
            for subprop in property.iterchildren():
                # Read Geom, ElMolwt
                if 'atomArray' in subprop.tag:
                    u_fact = utl.convertLengthUnitsToSI('A')
                    for subsubprop in subprop.iterchildren():
                        atom = subsubprop.attrib['elementType']
                        x = float(subsubprop.attrib['x3'])*u_fact
                        y = float(subsubprop.attrib['y3'])*u_fact
                        z = float(subsubprop.attrib['z3'])*u_fact
                        dfPs['ElMolWt'].append(float(subsubprop.attrib['atomicMass']))
                        dfPs['Geom'].append([atom,x,y,z])
    # Read MetaData
    for property in paramListEnv.iterchildren():
        dictRef = property.attrib['dictRef']
        if 'program' in dictRef and not 'programVersion' in dictRef:
            propchild = property.getchildren()[0]
            dfPs['MetaData']['programName'] = propchild.text
        elif 'programVersion' in dictRef:
            propchild = property.getchildren()[0]
            dfPs['MetaData']['programVersion'] = propchild.text
        elif 'runDate' in dictRef:
            propchild = property.getchildren()[0]
            dfPs['MetaData']['runDate'] = propchild.text
    for property in paramListInit.iterchildren():
        dictRef = property.attrib['dictRef']
        if 'method' in dictRef:
            propchild = property.getchildren()[0]
            dfPs['MetaData']['levelOfTheory'] = propchild.text
        elif 'basis' in dictRef:
            propchild = property.getchildren()[0]
            dfPs['MetaData']['basisSetValue'] = propchild.text

    # Create Species
    rSpec = chs.CreateChemSpecFromDict(dfPs)
    return rSpec