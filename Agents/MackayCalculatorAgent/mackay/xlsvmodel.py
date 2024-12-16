'''
# Basic wrapper includes read and write to the Excel Calculator model. This class is to be extended for more complex functions of the calculator.
'''


import xlwings as xw
import time
import numpy as np

class XLSVModel():
    def __init__(self, root):
        self.root = root  # connect to a file that is open or in the current working directory
        self.pageDict = {'control':'Control', 'output':'WebOutputs', 'result':'Results', 'model':'Model'}

    def readValue(self,ranges,pagename='output'):
        book = xw.Book(self.root)
        sheet = book.sheets[self.pageDict[pagename]]
        if type(ranges) == str:#a single range str
            values = sheet[ranges].value
            return values
        return [sheet[r].value for r in ranges]

    def updateValue(self, range, values, pagename='control', transpose=True):
        book = xw.Book(self.root)
        sheet = book.sheets[self.pageDict[pagename]]
        myrange = sheet.range(range)
        rrow, rcol = myrange.last_cell.row - myrange.row + 1, myrange.last_cell.column - myrange.column + 1
        values = np.reshape(values, (rrow, rcol))
        sheet.range(range).options().value = values