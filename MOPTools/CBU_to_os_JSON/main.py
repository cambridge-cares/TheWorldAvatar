from cbuCSVtoJSON.osJSONwrapper import cbuOperations

doc = """oswriter
Usage: oswriter <path> 
"""

def start():
    cbuCSVFilePath = input("Path to the csv file:")
    cbuOperations(cbuCSVFilePath)

if __name__ == '__main__':
    start()