import re
import stdc.chemspecies.chemspecies as chs
import stdc.unitconverter.unitconverter as unitconv

def runThermoCalculator(args):
    args = _preprocessArgs(args)
    ChemSpeciesData = chs.ChemSpecies(**args)
    ChemSpeciesData.getThermoData()
    if ChemSpeciesData.DevOutFile is not None:
        ChemSpeciesData.outputDiagnosticFile()
    print('done')

def _preprocessArgs(args):
    args = _removeNoneArgs(args)
    args =_removeHypens(args)
    return args

def _removeNoneArgs(args):
    filtered = {k: v for k, v in args.items() if v is not None}
    args.clear()
    args.update(filtered)
    return args

def _removeHypens(args):
    argsWithNoHyphens = {}
    for key, value in args.items():
        new_key = re.sub('^-+', '', key)
        new_key = re.sub('-', '_', new_key)
        argsWithNoHyphens[new_key] = value
    return argsWithNoHyphens