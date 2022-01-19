import subprocess
import os

GAUSSIAN = "gaussian"
CCPACKAGES = [GAUSSIAN]

def get_ccattr(logFile,attr,subattr='',default='unknown'):
    ccattr = default
    out = subprocess.run(["ccget", attr, logFile], text=True, stdout=subprocess.PIPE, universal_newlines=True)
    stdout_lines = out.stdout.replace(",","").replace("'","").strip().splitlines()

    if subattr:
        attr_use = subattr
    else:
        attr_use = attr

    for line in stdout_lines:
        if attr_use+":" in line:
            ccattr = line.split(':')[1].strip().replace("'",'').lower()
            ccattr = ccattr.replace('}','')
            break
    return ccattr

def get_ccpackage(logFile,default='unknown'):
    cwd=os.getcwd()
    outDir=os.path.dirname(logFile)
    baseName=os.path.basename(logFile)
    os.chdir(outDir)
    ccpackage = default
    ccpackage = get_ccattr(baseName,"metadata","package")
    os.chdir(cwd)
    return ccpackage
