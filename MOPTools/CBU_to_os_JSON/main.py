from cbuCSVtoJSON.osJSONwrapper import cbuOperations

doc = """This program generates os_json files.
The input files of for this program are single CBU.csv files and XYZ cbu files.
The files are then converted to owl files with the help of A-box writers."""

def start():
    cbuCSVFilePath = input("Path to the csv file:")
    cbuOperations(cbuCSVFilePath)

if __name__ == '__main__':
    start()