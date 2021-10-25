from assembler.assembly_operations import mopIRIquery
import json


def start():
    x = mopIRIquery()
    print(x)

if __name__ == '__main__':
    start()