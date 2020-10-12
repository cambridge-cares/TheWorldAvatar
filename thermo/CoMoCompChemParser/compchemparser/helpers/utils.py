import sys
import numpy as np
import compchemparser.helpers.params as p

def wait():
	input("Press Enter to continue...")

def codexit():
    wait()
    sys.exit()

def dienicely(errmsg):
    print(errmsg)
    codexit()