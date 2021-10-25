def processCEF(assemeblyModelSubstring):
    """This function parses the substrings of the assembly model string and provides back information on the modularity and planarity."""
    assemeblyModelSubstring = assemeblyModelSubstring[1:]
    nummod = assemeblyModelSubstring.split('X')
    num = int(nummod[0])
    mod = int(nummod[1])
    return num, mod

def parseAssemblyModelString(assemeblyModelString):
    """This function parses the assembly model strung and provides back information on the building block multiplicity and planarity."""
    assemblyModelDict = {}
    assemblyModelDict['numcorners'] = None
    assemblyModelDict['numedges'] = None
    assemblyModelDict['numfaces'] = None
    assemblyModelDict['modcorners'] = None
    assemblyModelDict['modedges'] = None
    assemblyModelDict['modfaces'] = None
    assemblyModelDict['plancorners'] = None
    assemblyModelDict['planedges'] = None
    assemblyModelDict['planfaces'] = None
    assemeblyModelString = assemeblyModelString.upper()
    ceflist = assemeblyModelString.split('-')
    for item in ceflist:
        mod, num = processCEF(item)
        if item[0] == 'C':
            assemblyModelDict['modcorners'] = mod
            assemblyModelDict['numcorners'] = num
            assemblyModelDict['plancorners'] = 'pyramidal'
        elif item[0] == 'F':
            assemblyModelDict['modfaces'] = mod
            assemblyModelDict['numfaces'] = num
            assemblyModelDict['planfaces'] = 'planar'
        elif item[0] == 'E':
            assemblyModelDict['modedges'] = mod
            assemblyModelDict['numedges'] = num
        else:
            print('Please put a corrent assembly string') #process corner
    if assemblyModelDict['numedges'] is not None:
        if assemblyModelDict['numcorners'] is not None:
            assemblyModelDict['planedges'] = 'linear'
        if assemblyModelDict['numfaces'] is not None:
            assemblyModelDict['planedges'] = 'bent'
    return assemblyModelDict

def nestingModelDict(assemeblyModelString):
    """This is the main parse function. It passes substrings to the subfunctions and returns back a dictionary of chemical building block units."""
    assemblyModelDict = parseAssemblyModelString(assemeblyModelString)
    unit1 = {"Modularity": None, "UnitNumber" : None, "Planarity": None}
    unit2 = {"Modularity": None, "UnitNumber" : None, "Planarity": None}
    unit3 = {"Modularity": None, "UnitNumber" : None, "Planarity": None}

    if assemblyModelDict['numedges'] is not None:
        unit1 = {"Modularity": assemblyModelDict['modedges'], "UnitNumber": assemblyModelDict['numedges'], "Planarity": assemblyModelDict['planedges']}
        if assemblyModelDict['numfaces'] is not None:
            unit2 = {"Modularity": assemblyModelDict['modfaces'],"UnitNumber": assemblyModelDict['numfaces'], "Planarity": assemblyModelDict['planfaces']}
            nestedAssemblyModelDict = [unit1, unit2]
            if assemblyModelDict['numcorners'] is not None:
                unit3 = {"Modularity": assemblyModelDict['modcorners'], "UnitNumber": assemblyModelDict['numcorners'],"Planarity": assemblyModelDict['plancorners']}
                nestedAssemblyModelDict = [unit1, unit2, unit3]
        elif assemblyModelDict['numcorners'] is not None:
            unit2 = {"Modularity": assemblyModelDict['modcorners'],"UnitNumber": assemblyModelDict['numcorners'], "Planarity": assemblyModelDict['plancorners']}
            nestedAssemblyModelDict = [unit1, unit2]
            if assemblyModelDict['numfaces'] is not None:
                unit3 = {"Modularity": assemblyModelDict['modfaces'], "UnitNumber": assemblyModelDict['numfaces'],"Planarity": assemblyModelDict['planfaces']}
                nestedAssemblyModelDict = [unit1, unit2, unit3]
    if assemblyModelDict['numedges'] is None:
        if assemblyModelDict['numfaces'] is not None:
            unit1 = {"Modularity": assemblyModelDict['modcorners'], "UnitNumber": assemblyModelDict['numcorners'],"Planarity": assemblyModelDict['plancorners']}
            if assemblyModelDict['numcorners'] is not None:
                unit2 = {"Modularity": assemblyModelDict['modfaces'],"UnitNumber": assemblyModelDict['numfaces'], "Planarity": assemblyModelDict['planfaces']}
                nestedAssemblyModelDict = [unit1, unit2]
    #print (nestedAssemblyModelDict)
    return nestedAssemblyModelDict