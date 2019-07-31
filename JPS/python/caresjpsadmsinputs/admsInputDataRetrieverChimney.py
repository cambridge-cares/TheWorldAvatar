"""
module that retreives and pack adms input info
"""
import rdflib
import sys
import rdflib.plugins.sparql.results.jsonresults as jsresult
from collections import namedtuple
from admsSrcChimney import admsSrc
from admsPol import admsPol
from caresjpsutil import PythonLogger
import requests
from config import Constants, QueryStrings

pm10er = 10
pm25er = 0


class AdmsInputDataRetriever(object):
    OPT = namedtuple('OPT', ['OptNumOutputs', 'OptPolName', 'OptInclude', 'OptShortOrLong', 'OptSamplingTime',
                             'OptSamplingTimeUnits', 'OptCondition', 'OptNumPercentiles', 'OptNumExceedences',
                             'OptPercentiles', 'OptExceedences', 'OptUnits', 'OptGroupsOrSource', 'OptAllSources',
                             'OptNumGroups', 'OptIncludedGroups', 'OptIncludedSource', 'OptCreateComprehensiveFile'])

    def __init__(self, topnode, bdnnode=None, range=None, pollutants=['HC'], srcLimit=5, bdnLimit=5, filterSrc=False,
                 rawBdn=None, targetCRS=None):
        """constructor
        inputs:
        range - user input range {'xmin', 'xmax', 'ymin', 'ymax'}, actual range is the min(user range, region
        envelope(e.g. jurongisland))
        topnode - uri of topnode to begin search within tree struct,
        filtersrc - true if use all children under topnode as src, false to use topnode as src directly
        bdnnode - top/colleciton node of building
        pollutants: pollutant to test
        srcLimit: limit of src number, actual number might fewer
        bdnLimit: limit of bdn number, actual number might fewer
        """
        self.address = None
        self.pollutants = pollutants
        self.topnode = topnode
        self.bdnnode = bdnnode
        self.srcLimit = srcLimit
        self.bdnLimit = bdnLimit
        # TODO-AE remove filterSrc and topnode
        self.filterSrc = filterSrc
        self.rawBdn = rawBdn

        self.range = self.getRange(range)
        self.pythonLogger = PythonLogger('admsInputDataRetrieverChimney.py')

    def getRange(self, userrange):
        return (
            (float(userrange['xmin']), float(userrange['xmax'])), (float(userrange['ymin']), float(userrange['ymax'])))

    def getSrcData(self):
        """get all sourced data :
        returns: data object
        """
        if isinstance(self.topnode, list):
            filtered = (*self.topnode,)
        else:
            filtered = (self.topnode,)

        s = set()  # make a set of substance to query later
        result = []
        for uri in filtered:
            self.connectDB(uri, connectType='parse')
            qdata = self.query(QueryStrings.SPARQL_DIAMETER_TEMP_HEIGHT_MASSFLOW_HEATCAPA_DENSITY_MOLEWEIGHT)

            aresult, = [row.asdict() for row in qdata]

            qdataC = self.query(QueryStrings.SPARQL_CONTENT)

            contents = [row['content'].toPython() for row in qdataC]

            s = s.union(contents)
            aresult['content'] = contents

            qdataERate = self.query(QueryStrings.SPARQL_ERATE)

            emissionrates = {row['er'].toPython(): row['v'].toPython() for row in qdataERate}

            sorteder = []
            for content in contents:
                name = content.split('#')[1]
                for ername, v in emissionrates.items():
                    if name in ername:
                        print('name=' + name)
                        if "Particulate" in name:
                            print('name2=' + name)
                            sorteder.append(pm10er)
                            sorteder.append(pm25er)
                        else:
                            sorteder.append(v)

            aresult['emissionrates'] = sorteder

            result.append(aresult)

        packed = []
        for src in result:
            SrcNumPollutants = len(src['content'])
            pollutantnames = [self.polIRI2Name(content) for content in src['content']]
            # add the pm 2.5 as the particulat is mapped to 2 different category
            if 'http://www.theworldavatar.com/kb/ships/Chimney-1.owl#Particulate-001' in src['content']:
                SrcNumPollutants += 1
                pollutantnames.append('PM2.5')

            newSrc = admsSrc(SrcName=src['o'].toPython(), SrcHeight=src['height'].toPython(),
                             SrcDiameter=float(src['diameter'].toPython()), SrcPolEmissionRate=src['emissionrates'],
                             SrcPollutants=pollutantnames, SrcTemperature=src['temp'].toPython(),
                             SrcMolWeight=src['moleweight'].toPython(), SrcDensity=float(src['density'].toPython()),
                             SrcSpecHeatCap=src['heatcapa'].toPython(), SrcNumPollutants=SrcNumPollutants,
                             SrcMassFlux=src['massflow'].toPython())
            print("hello")
            print(newSrc)
            packed.append(newSrc)
        return packed

    def filterBdnEnvelope(self):
        '''
        Get all buildings within range by comparing range with envelope
        return list of building url
        '''
        xRange, yRange = self.range
        qb = self.query('''
            PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX citygml: <http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#>
            PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

              SELECT distinct ?bdn 
              WHERE{{
              ?cityM a citygml:CityModelType.
              ?cityM citygml:boundedBy ?envelope .
              ?envelope a citygml:EnvelopeType.            # get all envelopes
              ?envelope citygml:upperCornerPoint ?upoint.  # get bounds of envelope
              ?upoint space_and_time_extended:hasGISCoordinateSystem ?uco.  
              ?uco   space_and_time_extended:hasProjectedCoordinate_x ?uxe.    
              ?uxe sys:hasValue ?uxv.
              ?uxv sys:numericalValue ?ux.
              ?uco   space_and_time_extended:hasProjectedCoordinate_y ?uye.
              ?uye sys:hasValue ?uyv.
              ?uyv sys:numericalValue ?uy.
              ?envelope citygml:lowerCornerPoint ?lpoint.
              ?lpoint space_and_time_extended:hasGISCoordinateSystem ?lco.  
              ?lco   space_and_time_extended:hasProjectedCoordinate_x ?lxe.    
              ?lxe sys:hasValue ?lxv.
              ?lxv sys:numericalValue ?lx.
              ?lco   space_and_time_extended:hasProjectedCoordinate_y ?lye.
              ?lye sys:hasValue ?lyv.
              ?lyv sys:numericalValue ?ly.
              ?cityM citygml:cityObjectMember ?bdn .  #get bdn belongs to filterd envelope
                Filter(xsd:double(?ly) > "{1}"^^xsd:double  && xsd:double(?uy) < "{2}"^^xsd:double && xsd:double(?lx) > "{3}"^^xsd:double && xsd:double(?ux) < "{4}"^^xsd:double) #filter envelope within range
                        }} 
            LIMIT {0}   #limit of building num
            '''.format(self.bdnLimit, *yRange, *xRange))

        ########todo: in future delete stud data
        return tuple(row['bdn'] for row in qb)

    def coreBdn2Src(self):
        # compare src coords to each bdn
        y_midpoint = (self.range[1][0] + self.range[1][1]) / 2
        x_midpoint = (self.range[0][0] + self.range[0][1]) / 2
        for src in self.rawSrc:
            closed, dClosed, first = None, sys.maxsize, True
            for i in range(len(self.rawBdn.BldX)):
                dx, dy = self.rawBdn.BldX[i] - x_midpoint, self.rawBdn.BldY[i] - y_midpoint
                d = dx * dx + dy * dy
                if first:
                    dClosed = d
                    closed = self.rawBdn.BldName[i]
                    first = False
                if d - dClosed < 0:
                    closed = self.rawBdn.BldName[i]
                    dClosed = d
            if closed is not None:
                src.setMainBuilding(closed)

            else:  # err handling, something is wrong if no closed building is found, just throw it
                raise Exception('Dear lord, no closed buildinf found for src: ' + src.SrcName)

    def getOpt(self, PolNames, SrcNames):
        numPol = len(PolNames)
        return self.OPT(numPol, PolNames, [1] * numPol, [0] * numPol, [1] * numPol, [3] * numPol, [0] * numPol,
                        [0] * numPol, [0] * numPol, [0] * 80, [0] * 80, ['ug/m3'] * numPol, 0, 0, 1, "Grouptank001",
                        "SrcNames", 0)

    def polIRI2Name(self, polIRI):
        substances = {
            # 'http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#chlorine':'Cl2',
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Nitrogen__dioxide': 'NO2',
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Carbon__monoxide': 'CO',
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Carbon__dioxide': 'CO2',
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Sulfur__dioxide': 'SO2',
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Ozone': 'O3',
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/pseudocomponent.owl#Unburned_Hydrocarbon': 'HC',
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/pseudocomponent.owl#Nitrogen__oxides': 'NOx',
            # 'http://www.theworldavatar.com/kb/ships/Chimney-1.owl#Particulate-001':'Particulate001',
            'http://www.theworldavatar.com/kb/ships/Chimney-1.owl#Particulate-001': 'PM10'
            # 'http://www.theworldavatar.com/kb/ships/Chimney-1.owl#Particulate-001':'PM2.5'

        }

        if polIRI in substances.keys():
            print('Found: ' + substances[polIRI])
            return substances[polIRI]
        else:
            print(polIRI + 'Not found !!!!')
            # raise Exception('This substance is not defined!!!!')
            return None

    def getWeather(self):
        '''
        get weather data,
        for now we trigger a python script to write the .met file directly
        '''
        # finish writing met
        metpath = "C://JPS_DATA/workingdir/JPS/ADMS/test.met"  # later it will be replaced, just ignored
        self.pythonLogger.postInfoToLogServer('path for adms met file=' + metpath)
        return metpath

    def getBkg(self):
        '''
        get background data,
        for now we trigger a python script to write the .met file directly
        '''
        # finish writing met
        bkgpath = "C://JPS_DATA/workingdir/JPS/ADMS/testbackgrnd.bkg"  # later it will be replaced, just ignored
        self.pythonLogger.postInfoToLogServer('path for adms bkg file=' + bkgpath)
        return bkgpath

    def getPol(self):
        self.connectDB('http://www.theworldavatar.com/kb/ships/Chimney-1.owl', connectType = 'parse')
        qb = self.query(QueryStrings.SPARQL_DIAMETER_DENSITY_MASSFRACTION)
        massrate = self.query(QueryStrings.SPARQL_MASSRATE).__iter__().__next__()[Constants.KEY_MASS_RATE].toPython()

        pm10D = []
        pm25D = []
        pm10massf = []
        pm25massf = []
        pm10Density = []
        pm25Density = []
        pm10fraction = []
        pm25fraction = []

        for row in qb:
            diameter = row[Constants.KEY_DIAMETER].toPython()
            density = row[Constants.KEY_DENSITY].toPython()
            massFraction = float(row[Constants.KEY_MASS_FRACTION])
            indivrate = massFraction * massrate

            if diameter < 0.00001:
                pm10D.append(diameter)
                pm10Density.append(density)
                pm10massf.append(indivrate)
                if diameter < 0.0000025:
                    pm25D.append(diameter)
                    pm25Density.append(density)
                    pm25massf.append(indivrate)

        all_pm10_massf = sum(pm10massf)
        all_pm25_massf = sum(pm25massf)

        for p in pm10massf:
            pm10fraction.append(p / all_pm10_massf)

        for p in pm25massf:
            pm25fraction.append(p / all_pm25_massf)

        raw_solpm25 = admsPol(Constants.POL_PM25, len(pm25D), pm25D, pm25Density, pm25fraction)
        raw_solpm10 = admsPol(Constants.POL_PM10, len(pm10D), pm10D, pm10Density, pm10fraction)

        return raw_solpm10, raw_solpm25

    def get(self):
        '''main function, get all related info for adms
        returns: complete data for adms
        '''
        pm10, pm25 = self.getPol()

        # get all src data
        self.rawSrc = self.getSrcData()
        ##todo: think about this case: when nothing is found ,where should we handle it?
        ##then nothing should be written, and catch this exception and mute it in main function
        if self.rawSrc is None:
            raise Exception("No src in found to requiries")

        print('raw building: ')
        print(self.rawBdn)
        rawOpt = self.getOpt(self.pollutants, [s.SrcName for s in self.rawSrc])
        self.coreBdn2Src()

        # for debuging, in future,define this for data type, i dont think it auto rpint for objet
        for src in self.rawSrc:
            print(src)

        met = self.getWeather()
        xran, yran = self.range
        grd = xran[0], yran[0], xran[1], yran[1]

        pol = pm10, pm25
        bkg = self.getBkg()

        # return {'Src': self.rawSrc, 'Bdn': self.rawBdn, 'Opt': rawOpt, 'Met': met, 'Grd':grd}
        return {'Src': self.rawSrc, 'Opt': rawOpt, 'Met': met, 'Grd': grd, 'Pol': pol, 'Bkg': bkg}

    def queryEndpoint(self, str):
        def literal2TLit(sparqlres):

            if 'results' not in sparqlres:
                return
            for row in sparqlres['results']['bindings']:
                for name, value in row.items():
                    if value['type'] == 'literal' and 'datatype' in value:
                        value['type'] = 'typed-literal'

        print('requesting @ ' + self.address + " with query:")
        # print(str)
        resp = requests.get(self.address, params={'query': str}, timeout=1500, headers={'user-agent': 'my-app/0.0.1'})
        print('raw resp:')

        print(resp.json())
        result = resp.json()
        literal2TLit(result)
        print('literal to typed-literal')
        print(result)
        qres = jsresult.JSONResult(result)  # json decoded
        print('after parse:')
        print(qres)
        return qres

    def queryLocalGraph(self, str):
        qres = self.g.query(str)
        return qres

    def Uri2Local(uri):
        '''replace a uri to local address
        inputs:
        uri - uri to be changed
        returns: string - local address
        '''
        return uri.replace("http://www.jparksimulator.com", config.root).replace("http://www.theworldavatar.com",
                                                                                 config.root)

    def remote2local(self, func):
        '''decorator to change connection function to local connection by replacing given iri to local address
        '''

        def functionWrapper(self, address):
            address = self.Uri2Local(address)
            func(self, address)

        return functionWrapper

    def connectDB(self, address, connectType='endpoint'):
        '''connect to db anyhow (we use rdflib graph parse now)
        '''

        def connectDBActual(address):
            '''
            Actual method to connect to db
            '''
            # obsolete: use rdflib locally
            self.address = address
            if connectType is 'parse':
                self.g = rdflib.Graph()  # comment out in future
                self.g.parse(address)  # comment out in future

        self.qmethodMap = {'parse': self.queryLocalGraph, 'endpoint': self.queryEndpoint}

        if not sameGraph(address, self.address):
            print('parsing graph: ' + address)
            if connectType not in self.qmethodMap:
                raise Exception('db connection method not defined')
            self.query = self.qmethodMap[connectType]
            connectDBActual(address)


def sameGraph(uri1, uri2):
    def trimloc(uri):
        if uri is None:
            return None
        else:
            return uri.split('#')[0]

    return trimloc(uri1) == trimloc(uri2)


def uri2name(uri):
    base = 'http://www.theworldavatar.com/'
    return uri.split('#')[1]
