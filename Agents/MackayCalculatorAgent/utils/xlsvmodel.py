'''
# Basic wrapper includes read and write to the Excel Calculator model.
'''


import xlwings as xw
import time

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
        #book.app.screen_updating = 'False'
        sheet = book.sheets[self.pageDict[pagename]]
        sheet.range(range).options(transpose=transpose).value = values

