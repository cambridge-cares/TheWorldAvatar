import cclib

GAUSSIAN = "gaussian"
CCPACKAGES = [GAUSSIAN]

def get_ccpackage(logFile):
    cclib_data = cclib.io.ccread(logFile)
    ccpackage = cclib_data.metadata.get('package', 'unknown').lower()
    return ccpackage
