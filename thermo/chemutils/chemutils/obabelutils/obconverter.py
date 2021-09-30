import openbabel.openbabel as ob
import chemutils.ioutils.ioutils as ioutils
import re
optTypesRegex = re.compile('(^-x|^-a|^-([^-ax])|^--)')

def obConvert(inputMol, inputMolFormat, outputMolFormat, options=None):
    if ioutils.fileExists(inputMol): inputMol= ioutils.readFile(inputMol)
    obConversion = ob.OBConversion()
    obConversion.SetInAndOutFormats(inputMolFormat, outputMolFormat)

    if options is not None:
        _addObConversionOptions(obConversion, options)

    mol = ob.OBMol()
    obConversion.ReadString(mol, inputMol)
    mol = obConversion.WriteString(mol).rstrip()
    return mol

def _addObConversionOptions(obConvObject, options):
    def _splitOptKeyValue(opt):
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
                optKey, optValue = _splitOptKeyValue(opt.replace('-',''))
                obConvObject.AddOption(optKey, obConvObject.GENOPTIONS, optValue)