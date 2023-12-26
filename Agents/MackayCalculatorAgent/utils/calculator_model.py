# The CalculatorModel object provides utilities to perform specific tasks on the Calculator model like
# set lever configurations or read all weboutputs.
# ===============================================================================
from .xlsvmodel import XLSVModel
from .plot_formatter import translateValueIdList,translateControlIdList,translateSingleValueIdList
from .config import XLSMMODELPATH,KG_UPDATE_LIST,NAME_NETZERO

import time
from collections import deque

VALID_LEVELS = [1, 2, 3, 4]

class CalculatorModel(XLSVModel):
    def __init__(self):
        XLSVModel.__init__(self, XLSMMODELPATH)
        self.valueLists = translateValueIdList()
        self.levelLists = translateControlIdList()
        self.singleValueLists = translateSingleValueIdList()
        self.updateList = KG_UPDATE_LIST

    #set values of lever config
    def setControls(self, levels):
        start = time.time()
        valid_result = self.validateLevel(levels)
        if valid_result:
            self.updateValue(self.levelLists, levels)
        else:
            raise ValueError('control lever value error')
            #print('set control by {}'.format(levels))
            #print('run time: {}'.format(time.time()-start))

    #validate level config are in crrect range (1-4)
    def validateLevel(self, levels):
        #print('validated that post levels are integers')
        for l in levels:
            if l not in VALID_LEVELS:
                return False
        return True

    #twerk till netzero
    def twerkTillZero(self, action, pos_cate, current_config):
        all_cate = list(range(45))
        neg_cate = deque([ i for i in all_cate if i not in pos_cate])
        other_direction = 0
        if action == 'down':
            current_config[pos_cate[0]] = current_config[pos_cate[0]] -1
            other_direction = +1
        elif action =='up':
            current_config[pos_cate[0]] = current_config[pos_cate[0]] +1
            other_direction = -1
        self.setControls(current_config)
        net = prevnet = self.readSingles(NAME_NETZERO)
        idx = 0
        if action == 'down':#relax one cut, need to increase other cuts
            while idx<len(neg_cate):#Not enough cut
                print(net)
                if net <= -1.0:
                    break #great, done
                if current_config[neg_cate[idx]] < 4:
                    current_config[neg_cate[idx]] = current_config[neg_cate[idx]]+other_direction
                    print(current_config)
                    self.setControls(current_config)
                    net = self.readSingles(NAME_NETZERO)
                    idx = idx + 1
                    if idx == len(neg_cate):
                        idx = 0
                else:
                    neg_cate.popleft()

        elif action =='up': #increase one cuts, need to relax other cutes
            while net <= -1.0 and idx<len(neg_cate):#As long as enough cut
                if current_config[neg_cate[idx]] > 1: # can cut
                    prevnet = net
                    orivalue = current_config[neg_cate[idx]]
                    current_config[neg_cate[idx]] = orivalue+other_direction
                    self.setControls(current_config)
                    net = self.readSingles(NAME_NETZERO)
                    if net > -1.0:#oh no, relax too much
                        current_config[neg_cate[idx]] = orivalue #set back
                        net = prevnet#set back
                        neg_cate.popleft()
                    else:
                        idx = idx + 1
                        if idx == len(neg_cate):
                            idx = 0
                else:
                    neg_cate.popleft()

        return current_config, net



    # update all values in the updatelist by querying the KG
    def updateFromKG(self, new_values):
        for k in self.updateList:
            cell = self.updateList[k]
            new_value = new_values[k]
            self.updateValue(cell, [new_value],pagename='model',transpose=False)

    # read a particular value in the single value list by value name
    def readSingles(self, singlename):
        value = None
        for name, cell, page in self.singleValueLists:
            if singlename == name:
                value = self.readValue(cell,page)
        return value

    # get all chartdata and single values as specified by the value list and single value list
    def getData(self):
        start = time.time()
        ys = []
        #print(self.valueLists)
        for page in self.valueLists:
            pageList = []
            for chartrange in page:
                y = self.readValue(chartrange)
                if type(y)==list and type(y[0])!=list:#check, if single line graph, add extra []
                    y = [y]
                pageList.append(y)
            ys.append(pageList)
        #ys = [ [self.readValue(chartrange) for chartrange in page] for page in self.valueLists]
        singleValues = {}
        for name, cell, page in self.singleValueLists:
            singleValues[name] = self.readValue(cell,page)

        #print('read data run time: {}'.format(time.time()-start))
        return ys, singleValues