import sys

def wait():
	input("Press Enter to continue...")

def codexit():
    wait()
    sys.exit()

def dienicely(errmsg):
    print(errmsg)
    codexit()