from twa.resRegistry.resRegistry import resRegistry
import importlib

# import all installed resources
resReg = resRegistry()
for res in resReg.resReg['resources'].keys():
    locals()[res] = getattr(importlib.import_module('twa.resources.{0}.{0}'.format(res)), res)
del resReg
