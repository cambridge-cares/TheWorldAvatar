'''
This module contains utility functions to construct mappings for values in the Excel model
'''
import json

from utils.config import XLSMMODELPATH,OUTPUTMAPPINGPATH,CONTROLMAPPINGPATH,SINGLEVALUEMAPPINGPATH
from mackay.xlsvmodel import XLSVModel
import csv
import string

ALPHABETIC_LIST = list(string.ascii_uppercase)


#Color Palattes of charts
palette = ["#f5e58f",
            "#d2b085",
            "#8061a2",
            "#aca6a8",
            "#abe9ab",
            "#dea1d2",
            "#a1a1e6",
            "#bad38f",
            "#b68a40",
            "#89c6ef",
            "#659a6e",
            "#4d75a6",
            "#666666",
            "#e95abe",
            "#f38f58"]


yranges =[
    [-35,300],#Over
    [0,1500],
    [0,10000],
    [0,1100],
    [-5,240],#Tran
    [0,900],
    [0,30],
    [-10,60],#Bu
    [0,170],
    [0,4],
    [0,4],
    [-10,20],#Indu
    [0,80],
    [0,50],#CO2
    [0,630],
    [0,65],
    [0,15],
    [-25,61],#E
    [0,200],
    [0,55],
    [0,16],
    [-1.5,1.2],#Bio
    [0,750],
    [0,2],
    [0,2000],
    [0,1500],#imports
    [0,105]
]




#construct a data object for one line in chart
def getLine(idxline, name,type,y, maxy=None):
    HOVERTEMPLATE = "year %{x}: %{y}"
    x = list(range(2015,2105,5))
    component = {}
    component["y"] = y
    component["x"] = x
    component["name"] = name
    component["hovertemplate"] = HOVERTEMPLATE
    if type=='stack':
        allzero = True
        for yv in y:
            if yv!=0:
                allzero = False
                break
        component['visible'] = "legendonly" if allzero else "true"
    if type == 'line':
        component['type'] = 'scatter'
    elif type == 'stack':
        component['mode']="text+lines"
        component['textposition'] = "bottom right"
        if maxy and  y[1]-0>maxy/60:
            component['text'] = ["", name]


        component['stackgroup'] = 'one'
    return component


# parse to the format to feed into Plotly to create a flow chart from edge list
def parseFlowEdges(edges):
    nodes = {}
    idx = 0
    labels = []
    sources = []
    targets = []
    for s, t in edges:
        if s not in nodes:
            nodes[s] = idx
            idx = idx+1
            labels.append(s)
        if t not in nodes:
            nodes[t] = idx
            idx = idx+1
            labels.append(t)
        sources.append(nodes[s])
        targets.append(nodes[t])
    return labels,sources,targets


# get the value range of a list of values in graph
def getGraphRange(ys):
    low, high = 0,0
    for ylist in ys:
        low = min(0, min(ylist))
        high = max(high, max(ylist))
    return [low, high]

# read from the control mapping csv to construct a value dictionary
def translateControlIdList(type="levervalue"):
    metas = readGraphsMeta(CONTROLMAPPINGPATH)
    reformed = {}
    for idx,row in enumerate(metas):
        content = row['content']
        values= row['values']
        reformed[content] = values

    return reformed[type] if type is not None else reformed

# construct a data object for one chart
def getGraph(idx,names, linetype, ys, title, unit=None):
    graph = []
    titleObj = {}
    titleObj['title'] = title
    titleObj['font'] = {"size": 16,"color": "#000000"}
    maxy = None
    if linetype == 'flow':
        gobj = {}
        ls, ss, ts = parseFlowEdges(names)
        gobj['type'] = 'sankey'
        gobj['orientation'] = 'h'
        nodeobj = {}
        nodeobj['label'] = ls
        gobj['node'] = nodeobj
        gobj['link'] = {'source':ss, 'target':ts, 'value':ys}
        graph.append(gobj)
        return graph, titleObj
    elif linetype == 'line':
        if type(ys[0])!=list:
            ys = [ys]
            names = [names]
        yobj,ytitleobj = {},{}
        yobj['text'] = unit
        ytitleobj['title'] = yobj
        ytitleobj["range"] = yranges[idx]
        maxy = yranges[idx][1]
        titleObj['yaxis'] = ytitleobj
        xtitleObj = {}
        xtitleObj["range"] = [2015,2100]
        titleObj['xaxis'] = xtitleObj

        types = ['line' for _ in names]
    elif linetype == 'stack_lastline':
        yobj,ytitleobj = {},{}
        yobj['text'] = unit
        ytitleobj['title'] = yobj
        ytitleobj["range"] = yranges[idx]
        maxy = yranges[idx][1]
        titleObj['yaxis'] = ytitleobj
        xtitleObj = {}
        xtitleObj["range"] = [2015,2100]
        titleObj['xaxis'] = xtitleObj
        titleObj['colorway'] = palette
        types =['stack' for _ in names]
        types[len(names)-1] = 'line'
    else:
        raise TypeError('Not a valid graph type')

    idxline = 0
    for name, ltype, y in zip(names,types,ys):
        graph.append(getLine(idxline,name, ltype,y,maxy))
        idxline = idxline+1
    return  graph,titleObj



# read meta infomration from a mappingcsv
def readGraphsMeta(folder, readAsDict = True):
    metas =[]
    with open(folder) as csvfile:
        if readAsDict:
            reader = csv.DictReader(csvfile)
            for row in reader:
                metas.append(row)
        else:
            next(csvfile)#skip header
            reader = csv.reader(csvfile)
            for row in reader:
                metas.append(row)

    return metas



# prase a Excel range str to a list of cell names
def parseCellTable(cellstr):
    tags = cellstr.split('|')
    colTags = tags[0].split('-')
    colStart = ALPHABETIC_LIST.index(colTags[0])
    colEnd = ALPHABETIC_LIST.index(colTags[1])
    colTags = ALPHABETIC_LIST[colStart:colEnd+1]
    rowTags = tags[1].split('-')
    rowTags = list(range(int(rowTags[0]),int(rowTags[1])+1))
    cells = [ [ col+str(rowId) for col in colTags]   for rowId in rowTags]
    return cells


levelLists = translateControlIdList("levervalue")


# parse for the full range of all the charts in all 4 level settings
def getYRange(calculator, valueCells):
    calculator.updateValue(levelLists, [4 for _ in range(45)])
    ys1 = calculator.readValue(valueCells)
    if type(ys1[0])!=list:
        ys1 = [ys1]
    min1,max1 = min(ys1[0][:8]),max(ys1[0][:8])
    for ys in ys1:
        ys = ys[:8]
        min1 = min(min(ys),min1)
        max1 = max(max(ys),max1)
    calculator.updateValue(levelLists, [1 for _ in range(45)])
    ys4 = calculator.readValue(valueCells)
    if type(ys4[0])!=list:
        ys4 = [ys4]
    for ys in ys4:
        ys = ys[:8]
        min1 = min(min(ys),min1)
        max1 = max(max(ys),max1)

    return (min1,max1)


# Main function to extract all plot data from the Excel model for a initial json file
#format of one line in output mapping csv
#{'page': '1A', 'chartno': '1', 'names': 'G272-G283', 'values': 'H-Y|272-283', 'type': "'stack_lastline'", 'title': 'G264', 'unit': 'G265'}
def translateMetas():
    model = XLSVModel(XLSMMODELPATH)
    metas = readGraphsMeta(OUTPUTMAPPINGPATH)
    layouts = []
    datas = []
    cates = []
    idxPage = -1
    for idx,chart in enumerate(metas):
        if chart['page'] not in cates:
            cates.append(chart['page'])
            datas.append([])
            layouts.append([])
            idxPage = idxPage+1
        lineNames = chart['names']
        names = model.readValue(lineNames)
        valueCells = chart['values']
        ys =  model.readValue(valueCells)
        type = chart['type']
        title = model.readValue(chart['title'])
        unit = model.readValue(chart['unit'])
        data,layout = getGraph(idx,names,type,ys,title,unit)
        datas[idxPage].append(data)
        layouts[idxPage].append(layout)

    charts = []
    for cate, data, layout in zip(cates,datas,layouts):
        chartObj = {"category":cate, "data":data,"layout":layout}
        charts.append(chartObj)
    plotdataObj = {"plotdata":charts}
    jsonStr = json.dumps(plotdataObj)
    return jsonStr


# Main function to extract all plot data from the Excel model for a initial json file
def translateValueIdList():
    metas = readGraphsMeta(OUTPUTMAPPINGPATH)
    nametoid = {}
    IdLists = []
    pageidx = 0
    for idx,chart in enumerate(metas):
        pagename = chart['page']
        if pagename not in nametoid:
            nametoid[pagename] = pageidx
            pageidx = pageidx + 1
            IdLists.append([])
        #valueCells = parseCellTable(chart['values'])
        valueCells =chart['values']
        IdLists[nametoid[pagename]].append(valueCells)
    return IdLists


# Main function to extract the dict for single values
def translateSingleValueIdList():
    metas = readGraphsMeta(SINGLEVALUEMAPPINGPATH,readAsDict=False)
    return metas


#  Main function to read all names of control levels
def getControlNamelist():
    controlIdlist = translateControlIdList(type=None)
    nameIds = controlIdlist['levername']
    model = XLSVModel(XLSMMODELPATH)
    names = model.readValue(nameIds,'control')

# Replace all possible typos of country in model texts
def checkexpression(s):
    return s.replace('UK', 'Singapore')

# Main function to extract control lever descriptions into a json
def readControlMetas():
    cellmap = translateControlIdList(type=None)
    d0range = cellmap['leveldescription0']
    d1range = cellmap['leveldescription1']
    d2range = cellmap['leveldescription2']
    d3range = cellmap['leveldescription3']
    d4range = cellmap['leveldescription4']
    namerange = cellmap['levername']
    model = XLSVModel(XLSMMODELPATH)
    d0 = model.readValue(d0range, 'control')
    d1 = model.readValue(d1range, 'control')
    d2 = model.readValue(d2range, 'control')
    d3 = model.readValue(d3range, 'control')
    d4 = model.readValue(d4range, 'control')
    names = model.readValue(namerange, 'control')
    alldObj = {}
    for idx,n in enumerate(names):
        dobj={}
        dobj['d0'] = checkexpression(d0[idx])
        dobj['d1'] = checkexpression(d1[idx])
        dobj['d2'] = checkexpression(d2[idx])
        dobj['d3'] = checkexpression(d3[idx])
        dobj['d4'] = checkexpression(d4[idx])
        alldObj[n] = dobj
    return  json.dumps(alldObj)











