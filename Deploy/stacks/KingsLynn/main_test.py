from SPARQLWrapper import SPARQLWrapper, JSON
import pyproj
import geojson_formatter
ServerIP = "192.168.1.7"
PortID   = "9995"
DataBaseName = "blazegraph"
NameSpaceName = "berlin"

OWLFile = "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl"

SparqlWrapperID = "http://"+ServerIP+':'+PortID+'/'+DataBaseName+'/namespace/'+NameSpaceName+"/sparql"

# Construt SPARSQL query
def ConstructSPARQL(owlfiledir):

    gap1Str = ' '
    prfStr = 'PREFIX OntoCityGML: '+'<'+owlfiledir+'#>'

    sbjStr = '?s'
    pdcStr = 'OntoCityGML:'+'GeometryType'
    objStr = '?o'

    Line1 = 'SELECT *'
    Line2 = 'WHERE {'+sbjStr+gap1Str+pdcStr+gap1Str+objStr+'}'

    OutLine = prfStr+gap1Str+Line1+gap1Str+Line2
    return OutLine

# Get SPARSQL results
def GetSarqlResults(OWLFile,SparqlWrapperID):
    sparqlline = ConstructSPARQL(OWLFile)
    sparql = SPARQLWrapper(SparqlWrapperID)
    sparql.setQuery(sparqlline)
    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()
    return results

def GetCoordinateData(InputDataStr):
    proj = pyproj.Transformer.from_crs(3857, 4326, always_xy=True)
    CoordinateStr = InputDataStr.split("#")
    if len(CoordinateStr) % 3 == 0:
        NoOfNode = len(CoordinateStr)/3
        BuildingData = []

        AvgH = 0.0
        HDataCount = 0
        for i in range(0,int(NoOfNode)):
            nodei = i*3
            h = float(CoordinateStr[nodei+2])
            AvgH = AvgH + h
            HDataCount = HDataCount + 1
            x, y = proj.transform(CoordinateStr[nodei], CoordinateStr[nodei+1])
            BuildingData.append([x,y])

        AvgHeight = round(AvgH/HDataCount)

        if AvgHeight == 0:
            AvgHeight = 1
    else:
        print('Error data with field ',CoordinateStr)
        BuildingData = 0
        AvgHeight = 0

    return BuildingData,AvgHeight

####################     FUNCTIONS     ####################
if __name__ == '__main__':
    SPARSQL_Result = GetSarqlResults(OWLFile,SparqlWrapperID)

    Output = []
    for result in SPARSQL_Result["results"]["bindings"]:
        Output.append(result["o"]["value"])

    NoOfBuilding = len(Output)

    LHead = geojson_formatter.WriteType()
    LEnd = geojson_formatter.WriteEnd()

    MainText = LHead
    #for i in range(0,NoOfBuilding):

    endloop = NoOfBuilding
    for i in range(0,endloop):
        BD,AvgH = GetCoordinateData(Output[i])
        GeometryInfo = BD
        if i == endloop -1:
            Body = geojson_formatter.WriteFeature(i, AvgH, GeometryInfo, 1)
        else:
            Body = geojson_formatter.WriteFeature(i, AvgH, GeometryInfo, 0)
        
        if GeometryInfo != 0:
            MainText = MainText + Body
        else:
            MainText = MainText

    MainText = MainText + LEnd
    FileName = 'Building'+str(0)+'.geojson'
    with open(FileName, 'w') as f:
        f.write(MainText)