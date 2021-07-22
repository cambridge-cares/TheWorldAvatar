from openbabel import openbabel
from chemutils.ioutils import readFile, \
                              writeFile, \
                              fileExists, \
                              verbosityBasedOutput
import re
optTypesRegex = re.compile('(^-x|^-a|^-([^-ax])|^--)')

def obConvert(inputMol, inputMolFormat, outputMolFormat, options=None):
    if fileExists(inputMol): inputMol= readFile(inputMol)
    obConversion = openbabel.OBConversion()    
    obConversion.SetInAndOutFormats(inputMolFormat, outputMolFormat)

    if options is not None:
        addObConversionOptions(obConversion, options)

    mol = openbabel.OBMol()
    obConversion.ReadString(mol, inputMol)
    mol = obConversion.WriteString(mol).rstrip("\n")
    return mol

def obConvertWrapper(inputMol, inputMolFormat, outputMolFormat, convOptions=None, outFile=None, silent=False):
    if convOptions is not None: convOptions = convOptions.split(' ')
    mol = obConvert(inputMol, inputMolFormat, outputMolFormat, convOptions)
    verbosityBasedOutput(mol, silent, outFile)
    return mol

def addObConversionOptions(obConvObject, options):
    def splitOptKeyValue(opt):
        key = opt
        value = ''
        if '=' in opt: key, value = opt.split('=')
        return key, value

    for opt in options:
        opt = opt.strip()
        parsedOpt = optTypesRegex.match(opt)
        if parsedOpt:
            optType = parsedOpt.group(1)
            if optType == '-a':
                opt = opt.replace(optType,'')
                for o in opt:
                    obConvObject.AddOption(o, obConvObject.INOPTIONS)
            elif optType == '-x':
                opt = opt.replace(optType,'')
                for o in opt:
                    obConvObject.AddOption(o, obConvObject.OUTOPTIONS)
            else:
                optKey, optValue = splitOptKeyValue(opt.replace('-',''))
                obConvObject.AddOption(optKey, obConvObject.GENOPTIONS, optValue)