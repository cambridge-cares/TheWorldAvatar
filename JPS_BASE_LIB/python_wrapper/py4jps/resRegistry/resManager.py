from py4jps.resRegistry import resRegistry
from docopt import docopt, DocoptExit
import shutil
import sys
import os

resReg = resRegistry()

doc = """jpsrm.

Usage:
    jpsrm install <resource> <from> [--jar JARFILE]
    jpsrm uninstall <resource>
    jpsrm list
    jpsrm clean

    options:
    -j, --jar       Name of the main jar file. If not provided, the first
                    found jar file in the resource directory will be used.
"""

def start():
    devinstall = False
    try:
        args = docopt(doc)
    except DocoptExit:
        if len(sys.argv)==2:
            if sys.argv[1]=='devinstall':
                devinstall = True
        if not devinstall:
            raise DocoptExit('Error: jpsrm called with wrong arguments.')

    if devinstall:
        _doDevinstall()
    else:
        if args['install']:
            resReg.addResToReg(resName=args['<resource>'], resLoc=args['<from>'], resMainJarFile=args['JARFILE'])
        elif args['uninstall']:
            resReg.removeResFromReg(resName=args['<resource>'])
        elif args['list']:
            resReg.listRes()
        elif args['clean']:
            resReg.cleanReg()

# this function is intended only for jps developers
def _doDevinstall():
    thisPath = os.path.realpath(__file__)
    src = os.path.abspath(os.path.join(thisPath,'..','..','..','..','target'))
    dst = os.path.abspath(os.path.join(thisPath,'..','tmp'))
    if os.path.exists(dst):
        shutil.rmtree(dst)
    shutil.copytree(os.path.join(src,'lib'), os.path.join(dst,'lib'))
    shutil.copyfile(os.path.join(src,'jps-base-lib.jar'), os.path.join(dst,'jps-base-lib.jar'))
    resReg.addResToReg(resName='JpsBaseLib', resLoc=dst, resMainJarFile='jps-base-lib.jar')
    shutil.rmtree(dst)

if __name__ == '__main__':
    start()