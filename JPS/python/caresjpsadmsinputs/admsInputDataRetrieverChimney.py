"""
module that retreives and pack adms input info
"""
import rdflib
import sys
import rdflib.plugins.sparql.results.jsonresults as jsresult
from rdflib.plugins.sparql import prepareQuery
from collections import namedtuple
from admsSrcChimney import admsSrc
from admsPol import admsPol
from caresjpsutil import PythonLogger
import requests
from config import Constants, QueryStrings


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
        if isinstance(topnode, list):
            self.topnode = (*topnode,)
        else:
            self.topnode = (topnode,)
        self.bdnnode = bdnnode
        self.srcLimit = srcLimit
        self.bdnLimit = bdnLimit
        # TODO-AE remove filterSrc and topnode
        self.filterSrc = filterSrc
        self.rawBdn = rawBdn
        self.em_rates = {}

        self.range = self.getRange(range)
        self.pythonLogger = PythonLogger('admsInputDataRetrieverChimney.py')

    def getRange(self, userrange):
        return (
            (float(userrange['xmin']), float(userrange['xmax'])), (float(userrange['ymin']), float(userrange['ymax'])))

    def get_src_data(self):
        """Gets all sourced data.
        returns: list of source data objects for the ADMS_SOURCE_DETAILS section of the APL.
        """
        sources = []
        q1 = prepareQuery(QueryStrings.SPARQL_DIAMETER_TEMP_HEIGHT_MASSFLOW_HEATCAPA_DENSITY_MOLEWEIGHT)
        q2 = prepareQuery(QueryStrings.SPARQL_CONTENT)
        q3 = prepareQuery(QueryStrings.SPARQL_ERATE)

        for src in self.topnode:
            iri = self.connectChimneyDB(src)
            qdata = self.query(q1)
            qdataC = self.query(q2)
            qdataERate = self.query(q3)

            aresult, sorteder, pollutantnames = self.get_new_src_data(iri, qdata, qdataC, qdataERate)

            new_src = admsSrc(SrcName=str(src[Constants.KEY_MMSI]),
                              SrcHeight=aresult[Constants.KEY_HEIGHT].toPython(),
                              SrcDiameter=float(aresult[Constants.KEY_DIAMETER].toPython()),
                              SrcPolEmissionRate=sorteder,
                              SrcPollutants=pollutantnames,
                              SrcTemperature=aresult[Constants.KEY_TEMP].toPython(),
                              SrcMolWeight=aresult[Constants.KEY_MOLE_WEIGHT].toPython(),
                              SrcDensity=float(aresult[Constants.KEY_DENSITY].toPython()),
                              SrcSpecHeatCap=aresult[Constants.KEY_HEAT_CAP].toPython(),
                              SrcNumPollutants=len(pollutantnames),
                              SrcMassFlux=aresult[Constants.KEY_MASS_FLOW].toPython())
            sources.append(new_src)

        return sources

    def get_new_src_data(self, iri, qdata, qdataC, qdataERate):
        aresult, = [row.asdict() for row in qdata]
        contents = [row[Constants.KEY_CONTENT].toPython() for row in qdataC]
        pollutantnames = [self.polIRI2Name(content) for content in contents]
        if None in pollutantnames:
            pollutantnames.remove(None)
            pollutantnames = pollutantnames + list(self.em_rates[iri].keys())

        emissionrates = {row[Constants.KEY_ER].toPython(): row[Constants.KEY_V].toPython() for row in qdataERate}

        sorteder = []
        for content in contents:
            name = content.split('#')[1]
            for ername, v in emissionrates.items():
                if name in ername:
                    if Constants.POL_PART_001 in name.replace('-', ''):
                        sorteder = sorteder + list (self.em_rates[iri].values())
                    else:
                        sorteder.append(v)

        return aresult, sorteder, pollutantnames

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
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Nitrogen__dioxide':
                Constants.POL_NO2,
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Carbon__monoxide':
                Constants.POL_CO,
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Carbon__dioxide':
                Constants.POL_CO2,
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Sulfur__dioxide':
                Constants.POL_PART_SO2,
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Ozone':
                Constants.POL_PART_O3,
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/pseudocomponent.owl#Unburned_Hydrocarbon':
                Constants.POL_HC,
            'http://www.theworldavatar.com/ontology/ontocape/material/substance/pseudocomponent.owl#Nitrogen__oxides':
                Constants.POL_NOX,
            'http://www.theworldavatar.com/kb/ships/Chimney-1.owl#Particulate-001': None
        }

        if polIRI in substances.keys():
            return substances[polIRI]
        else:
            raise Exception('This substance is not defined!!!!')

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

    def get_pol(self):
        """
        Prepares data for ADMS_POLLUTANT_DETAILS section of the APL.
        Separates particles to PM10 and PM2.5 categories. Stores emission rates in em_rates
        class member variable for every examined source for later use. @see: get_src_data()
        @return: list of Pol objects.
        """
        pols = []
        pol_data = []
        diam_dens = set()
        pol_names = {}
        q1 = prepareQuery(QueryStrings.SPARQL_DIAMETER_DENSITY_MASSFRACTION)
        q2 = prepareQuery(QueryStrings.SPARQL_MASSRATE)
        i = 0
        k = 0

        for src in self.topnode:
            iri = self.connectChimneyDB(src)
            qb = self.query(q1)
            massrate = self.query(q2).__iter__().__next__()[Constants.KEY_MASS_RATE].toPython()

            for row in qb:
                dd = (row[Constants.KEY_DIAMETER].toPython(), row[Constants.KEY_DENSITY].toPython())
                pol_data.append({Constants.KEY_DIAMETER + Constants.KEY_DENSITY: dd,
                                 Constants.KEY_MASS_FLOW: float(row[Constants.KEY_MASS_FRACTION]) * massrate,
                                 Constants.KEY_SRC: iri})
                diam_dens.add(dd)
            self.em_rates[iri] = {}

        for diam, dens in diam_dens:
            name = None
            if diam <= 0.00001:
                name = Constants.POL_PM10 + '-' + str(i)
                i = i + 1
                if diam <= 0.0000025:
                    name = Constants.POL_PM25 + '-' + str(k)
                    k = k + 1
            if name != None:
                pol_names[(diam, dens)] = name
                pols.append(admsPol(name, 1, [diam], [dens], [1.0e+0]))
                self.pollutants.append(name)

        for pd in pol_data:
            pol_key = pd[Constants.KEY_DIAMETER + Constants.KEY_DENSITY]
            if pol_key in pol_names:
                self.em_rates[pd[Constants.KEY_SRC]][pol_names[pol_key]] = pd[Constants.KEY_MASS_FLOW]

        return pols

    def get(self):
        '''main function, get all related info for adms
        returns: complete data for adms
        '''
        pol = self.get_pol()

        # get all src data
        self.rawSrc = self.get_src_data()
        ##todo: think about this case: when nothing is found ,where should we handle it?
        ##then nothing should be written, and catch this exception and mute it in main function
        if self.rawSrc is None:
            raise Exception("No src in found to requiries")

        rawOpt = self.getOpt(self.pollutants, [s.SrcName for s in self.rawSrc])
        self.coreBdn2Src()

        met = self.getWeather()
        xran, yran = self.range
        grd = xran[0], yran[0], xran[1], yran[1]

        bkg = self.getBkg()

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

    def connectChimneyDB(self,src):
        mmsi = src[Constants.KEY_MMSI]
        iri = Constants.IRI_KB_SHIPS + str(mmsi) + Constants.STR_CHIMNEY
        self.connectDB(iri, connectType=Constants.KEY_PARSE)

        return iri



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
