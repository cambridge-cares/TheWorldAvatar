from mopcsv_operations.omjson_operations import mopCSVOperations

doc = """This utility program creates om.json files for ontomops."""

def start():
    cbuIRICSVFilePath = input("Path to the CBU-IRI csv file:")
    mopCSVFilePath = input("Path to the MOP csv file:")
    mopCSVOperations(cbuIRICSVFilePath, mopCSVFilePath)

if __name__ == '__main__':
    start()