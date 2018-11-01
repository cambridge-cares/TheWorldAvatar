'''
module that retreives and pack adms input info
'''
import rdflib
from pyproj import Proj, transform
import requests

import math
import sys
import os
import rdflib.plugins.sparql.results.jsonresults as jsresult
from collections import namedtuple
from admsSrc import admsSrc
from admsPolygon import Polygon
import cobbling



class admsInputDataRetriever(object):
    BDN = namedtuple('BDN', ['BldNumBuildings','BldName','BldType','BldX','BldY','BldHeight', 'BldLength', 'BldWidth', 'BldAngle'])
    OPT = namedtuple('OPT', ['OptNumOutputs','OptPolName','OptInclude','OptShortOrLong', 'OptSamplingTime','OptSamplingTimeUnits','OptCondition','OptNumPercentiles','OptNumExceedences','OptPercentiles','OptExceedences','OptUnits','OptGroupsOrSource','OptAllSources','OptNumGroups','OptIncludedGroups','OptIncludedSource','OptCreateComprehensiveFile'])

    def __init__(self, topnode, bdnnode=None, range=None, pollutants =['HC'], srcLimit = 5, bdnLimit = 5, filterSrc = False):
        '''constructor
        inputs:
        range - user input range {'xmin', 'xmax', 'ymin', 'ymax'}, actual range is the min(user range, region envelope(e.g. jurongisland))
        topnode - uri of topnode to begin search within tree struct,
        filtersrc - true if use all children under topnode as src, false to use topnode as src directly
        bdnnode - top/colleciton node of building
        pollutants: pollutant to test
        srcLimit: limit of src number, actual number might fewer
        bdnLimit: limit of bdn number, actual number might fewer
        '''
        self.address  = None
        self.pollutants = pollutants
        self.topnode = topnode
        self.bdnnode = bdnnode
        self.srcLimit = srcLimit
        self.bdnLimit = bdnLimit
        self.filterSrc = False

        self.range = self.getRange(range)
        print(self.range)

    def getRange(self, userrange):
        '''
        Define range from topnode info and user give parameters
        returns (xRange, yRange)
        '''
        if not self.filterSrc:
            return ((userrange['xmin'], userrange['xmax']), (userrange['ymin'],userrange['ymax']))
        self.connectDB(self.topnode)#connect to db       
        qx = self.query(
            """
            PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
            
            SELECT ?value
            WHERE {
            ?co  space_and_time_extended:hasProjectedCoordinate_x ?upper.
            ?upper sys:hasValue ?v.
            ?v sys:numericalValue ?value .
            }
            """)
        qy = self.query(
            """
            PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
            SELECT ?value
            WHERE {
            ?co  space_and_time_extended:hasProjectedCoordinate_y ?upper.
            ?upper sys:hasValue ?v.
            ?v sys:numericalValue ?value .
            }
            """)

        #extract bounds data
        xs = tuple(row['value'] for row in qx)
        ys = tuple(row['value'] for row in qy)


        xRange =  (xs[0].toPython(),xs[1].toPython()) if xs[0]<xs[1] else (xs[1].toPython(),xs[0].toPython())            
        yRange =  (ys[0].toPython(),ys[1].toPython()) if ys[0]<ys[1] else (ys[1].toPython(),ys[0].toPython())


        #todo: provide gis speci number in future and do conversion if needed
        #if user specified range, compare
        if userrange is not None:
            xRange = (max(xRange[0], userrange['xmin']), min(xRange[1], userrange['xmax']))
            yRange = (max(yRange[0], userrange['ymin']), min(yRange[1], userrange['ymax']))

        print('xrange: {} - {}', *xRange)
        print('yrange: {} - {}', *yRange)

        return(xRange, yRange)

    def filterSource(self):
        '''filter the source from tree starting from topnode, within the range and with user set content
        returns: list of source uris
        '''
        xRange, yRange = self.range
 
        self.connectDB(self.topnode)#connect to db

        #query for children uris from topnode
        #todo: future kb: type check :emission type
        qChildren = self.query(
            """
            PREFIX Eco-industrialPark: <http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#> 
            SELECT ?child
            WHERE {{
            ?o  Eco-industrialPark:hasIRI ?child . 
            }}
            LIMIT {0}
            """.format(self.srcLimit)
            )

        ###query each children to get coordinates
        uris = list(row["child"].strip() for row in qChildren)
        ###todo: add this test file, delete in future
        #uris.append("http://www.theworldavatar.com/TankID_1574.owl#TankID_1574")
        filtered = []

        print(uris)
        for uri in uris:
            print("connecting: {:s}".format(uri))

            self.connectDB(uri)
            qstr ='''
            PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
            PREFIX material: <http://www.theworldavatar.com/ontology/ontocape/material/material.owl#>

            SELECT DISTINCT ?x ?y ?content
            WHERE {{
            <{0!s}> space_and_time_extended:hasGISCoordinateSystem ?o.
            ?o  space_and_time_extended:hasProjectedCoordinate_x ?xe.
            ?xe sys:hasValue ?vx.
            ?vx sys:numericalValue ?x .
            <{0!s}> sys:hasContent ?contentE .
            ?contentE material:intrinsicCharacteristics ?chemsp.
            ?chemsp sys:containsDirectly ?content.


            OPTIONAL{{
            ?o  space_and_time_extended:hasProjectedCoordinate_y ?ye.
            ?ye sys:hasValue ?vy.
            ?vy sys:numericalValue ?y .
            }}
            }}
            '''.format(uri)
            #print (qstr)
            coordQresults = self.query(qstr)

  

            ##filter children within range
            for row in coordQresults:
                x,y,content = float(row['x'].toPython()), float(row['y'].toPython()), row['content'].toPython()
                #print("{},{},{}".format(x, y, content))
                if  x - xRange[0]>0 and x - xRange[1] < 0 and y - yRange[0] > 0 and y - yRange[1]<0 and content  in self.pollutants:
                    filtered.append(uri)
                    print('add to filtered {}'.format(uri))
                break
    
        return filtered
    
    def getSrcData(self):
        '''get all sourced data : 
        returns: data object 
        '''        
        filtered = None
        if self.filterSrc:
            filtered =  self.filterSource() 

        else: 
            filtered = (self.topnode,)
        
        s = set()#make a set of substance to query later
        result = []
        for uri in filtered:
            print("connecting: {:s}".format(uri))
            self.connectDB(uri, connectType = 'parse')
            qdata = self.query(
                """
                PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
                PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
                PREFIX plant:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#>
                PREFIX topology:<http://www.theworldavatar.com/OntoCAPE/meta_model/topology/topology.owl#>
                PREFIX behavior: <http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>
                PREFIX chemical_process_system:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#>
                PREFIX phase_system:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>
                PREFIX material: <http://www.theworldavatar.com/ontology/ontocape/material/material.owl#>
                PREFIX substance:<http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#>

                SELECT ?o ?height ?diameter ?content ?x ?y ?velocity ?massflow ?temp ?moleweight ?heatcapa ?density
                WHERE {{
                ?o plant:hasHeight ?he.
                ?he sys:numericalValue ?height .

                 ?o sys:hasSubsystem ?chm.
                ?chm plant:hasInsideDiameter ?de . #?dev  sys:hasValue ?de.
                ?de sys:numericalValue ?diameter.

                ?phase phase_system:has_temperature  ?tempE.
                ?tempE sys:hasValue ?vte.
                ?vte sys:numericalValue ?temp .

                ?o space_and_time_extended:hasGISCoordinateSystem ?coe .
                ?coe space_and_time_extended:hasProjectedCoordinate_x ?xe.
                 ?xe sys:hasValue ?xv.
                  ?xv sys:numericalValue ?x.

                   ?coe space_and_time_extended:hasProjectedCoordinate_y ?ye.
                 ?ye sys:hasValue ?yv.
                  ?yv sys:numericalValue ?y.

                  ?stream topology:leaves ?o.
                  ?stream chemical_process_system:refersToGeneralizedAmount ?ga.
                  ?ga sys:hasSubsystem ?ma.
                  
                  ?ma sys:hasProperty ?ve.
                  ?ve a behavior:Velocity .
                  ?ve sys:hasValue ?vv.
                  ?vv sys:numericalValue ?velocity.
                  
                  ?ma sys:hasProperty ?me.
                  ?me a behavior:ConvectiveMassFlowrate .
                  ?me sys:hasValue ?mv.
                  ?mv sys:numericalValue ?massflow.
    
                 ?mw a substance:MolecularWeight.
                 ?mw sys:hasValue ?mwv.
                 ?mwv  sys:numericalValue ?moleweight.
    
                 ?cp a phase_system:ThermodynamicStateProperty.
                                     ?cp sys:hasValue ?cpv.
                 ?cpv  sys:numericalValue ?heatcapa.
    
                 ?den a phase_system:Density.
                  ?den sys:hasValue ?denv.
                 ?denv  sys:numericalValue ?density.
    
                }}
                LIMIT 1 
            """)


            

            aresult, = [row.asdict() for row in qdata]

            


            
            qdataC = self.query("""
                    PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
           PREFIX substance:<http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#>

            SELECT DISTINCT  ?content
            WHERE {{
            ?mix a substance:Mixture.
            ?mix sys:containsDirectly  ?content. 
            }}
            """)

            contents = [row['content'].toPython() for row in qdataC]

            s = s.union(contents)
            aresult['content'] = contents
            

            qdataERate = self.query("""
                    PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
           PREFIX substance:<http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#>
           PREFIX behavior:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>

            SELECT DISTINCT  ?er ?v
            WHERE {{
            ?mix a substance:Mixture.
            ?mix sys:hasProperty  ?er.
            ?er  a behavior:ConvectiveMassFlowrate.
                         ?er sys:hasValue ?erv.
             ?erv sys:numericalValue ?v
            }}
            """)

            emissionrates = {row['er'].toPython():row['v'].toPython() for row in qdataERate}
            #todo: sort emissionrates to order of substances

            sorteder =  []
            for content in contents:
                name = content.split('#')[1]
                for ername, v in emissionrates.items():
                    if name in ername:
                        sorteder.append(v)

            print(sorteder)
            aresult['emissionrates'] = sorteder

            result.append(aresult)





            

        
        print("FILTERED :")
        print(result)



        
        '''   
        #use this if need query substance separately
        #query substance for substance related data
        cMap = {}

        
        #hard coding for now before I get a better solusion
        self.connectDB("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl", connectType = 'parse')

        template = """
            PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
            PREFIX sub:<http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#>
            PREFIX behavior: <http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>
            PREFIX phase_system:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>
            SELECT DISTINCT  ?o ?hc ?d ?mw
            WHERE {{
            <{0}> behavior:hasHeatCapacityRatio  ?hce.   #heatcapacity ratio
            ?hce sys:hasValue ?hcv . 
            ?hcv sys:numericalValue ?hc.
           
            <{0}> phase_system:has_density ?de .        #density
           ?de sys:hasValue ?dv . 
            ?dv sys:numericalValue ?d.
           
             <{0}> sys:hasProperty ?mwe.                #molecular weight
             ?mwe a sub:MolecularWeight .
             ?mwe sys:hasValue ?mwv . 
            ?mwv sys:numericalValue ?mw.
            }}
            LIMIT 1
       """

        for sub in s:
            print (sub)
            print (template.format(sub))
            cMap[sub], = self.query(template.format(sub))

               
     
        
        #todo:construct an array of contents, and corresponding content data
        

            subData = {}
            subData['mw'] = [cMap[content]['mw'] for content in src['content'] ]
            subData['d'] = [cMap[content]['d'] for content in src['content'] ]
            subData['hc'] = [cMap[content]['hc'] for content in src['content'] ]
            pollutantnames = [self.polIRI2Name(content) for content in src['content'] ]
            print(subData)
            print('!!!!!!!!!!!')
            print(src['y'].datatype)
            print(src['y'].value)
            print(type(src['y'].value))

            print(src['y'].toPython)
            print(float(src['y']))

        '''
        packed = []
        for src in result:
            SrcNumPollutants = len(src['content'])
            pollutantnames = [self.polIRI2Name(content) for content in src['content'] ]

            newSrc = admsSrc(SrcName = src['o'].toPython(), SrcHeight = src['height'].toPython(), SrcDiameter = src['diameter'].toPython(),SrcVertVeloc = src['velocity'].toPython(), SrcPolEmissionRate = src['emissionrates'], SrcPollutants = pollutantnames,SrcTemperature = src['temp'].toPython(), SrcX1 = src['x'].toPython(), SrcY1 = src['y'].toPython(), SrcMolWeight = src['moleweight'].toPython(), SrcDensity = src['density'].toPython(), SrcSpecHeatCap = src['heatcapa'].toPython(), SrcNumPollutants=SrcNumPollutants, SrcMassFlux = src['massflow'].toPython())
            packed.append(newSrc)


             

        return packed

    def getBdnData(self):
        self.connectDB(self.bdnnode, connectType = 'endpoint')
        bdns = []#self.filterBdnEnvelope()
        
        if len(bdns) is 0: #range is smaller than any envelope, 
        #then we have to filter indi buildings
            #todo: in this case, should we filter by calculated cnetroid, or a crude one with ground x,y? i'd go with x, y first。。。
            bdns = self.filterBdns()
            if len(bdns) is 0:    
                raise Exception('no bdn within range')

        print ('Found {0} bdn within range , they are '.format(len(bdns)))
        result = list((zip(*[self.getMetrics(bld) for bld in bdns])))
        print (result)


        newBdn = self.BDN(len(bdns), *result)
        return newBdn

    
    def filterBdnEnvelope(self):
        '''
        Get all buildings within range by comparing range with envelope
        return list of building url
        '''  
        xRange, yRange = self.range
        qb =self.query('''
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

    #todo
    def filterBdns(self, topython = False):
        '''
        filter individual building to see if they are within range
        get all uris where every x and y in its ground is within range(maybe count ?)

        ''' 
        xRange, yRange = self.range
       
        qstr = '''
            PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
            PREFIX citygml:<http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#>
            #PREFIX citygml:<file:/D:/citygmllearn/citygmlhandmade.owl#>

            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

            SELECT  distinct ?bdn 
            WHERE {{
            {{                                                             #case1:building has no parts
            ?bdn a citygml:BuildingType.                              
            ?bdn citygml:boundedBy ?g.                                     #building  boundBy      surface
            ?g a citygml:GroundSurfaceType.                                 # surface            is a         ground 
            ?g citygml:lod2MultiSurface ?ms.                                #ground              has lod2multisurface  ms
            ?ms citygml:surfaceMember ?pol.                                 #ms        has member   polygon
            ?pol citygml:exterior ?lring.                                   # polygon            exterior is  linear ring
            ?lring sys:contains ?po.                                        # linear ring        consists of  points
            ?po space_and_time_extended:hasGISCoordinateSystem ?co.         #point               has coordinate system   cs     
            ?co   space_and_time_extended:hasProjectedCoordinate_x ?xe.     #[extract cs to get x,y,z value] 
            ?xe sys:hasValue ?xv.
            ?xv sys:numericalValue ?x.
            ?co   space_and_time_extended:hasProjectedCoordinate_y ?ye.
            ?ye sys:hasValue ?yv.
            ?yv sys:numericalValue ?y.
            }} UNION {{                                                     #case 2: 
                ?bdn a citygml:BuildingType.                                #bdns that consists of part 
                ?bdn citygml:consistsOfBuildingPart ?part.
                ?part a citygml:BuildingPartType.
                ?part citygml:boundedBy ?g.  
                ?g a citygml:GroundSurfaceType.                                 
            ?g citygml:lod2MultiSurface ?ms.                                
            ?ms citygml:surfaceMember ?pol.                                 
            ?pol citygml:exterior ?lring.                                   
            ?lring sys:contains ?po.                                        
            ?po space_and_time_extended:hasGISCoordinateSystem ?co.            
            ?co   space_and_time_extended:hasProjectedCoordinate_x ?xe.    
            ?xe sys:hasValue ?xv.
            ?xv sys:numericalValue ?x.
            ?co   space_and_time_extended:hasProjectedCoordinate_y ?ye.
            ?ye sys:hasValue ?yv.
            ?yv sys:numericalValue ?y.
            }}

               filter(xsd:double(?y) > "{1}"^^xsd:double  && xsd:double(?y) < "{2}"^^xsd:double && xsd:double(?x) > "{3}"^^xsd:double && xsd:double(?x) < "{4}"^^xsd:double)
            }} 
            LIMIT {0}   #limit of building num
        '''.format(self.bdnLimit, *yRange, *xRange)

        qre = self.query(qstr)

        bdnlist = list(row['bdn'].toPython() for row in qre)

        return bdnlist


    def getBdnVertices(self, nodeuri):
        '''get all buildings data
        returns: data object
        '''
        #todo: modify query to get raw data,then pass to converter
        qData= self.query('''
            PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
            PREFIX citygml:<http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#>

            SELECT   ?x ?y ?z
            WHERE {{
            <{0}> citygml:boundedBy ?g.                                     #building/part IRI   boundBy      surface
            ?g a citygml:GroundSurfaceType.                                 # surface            is a         ground 
            ?g citygml:lod2MultiSurface ?ms.                                #ground              has lod2multisurface  ms
            ?ms citygml:surfaceMember ?pol.                                 #ms        has member   polygon
            ?pol citygml:exterior ?lring.                                   # polygon            exterior is  linear ring
            ?lring sys:contains ?po.                                        # linear ring        consists of  points
            ?po space_and_time_extended:hasGISCoordinateSystem ?co.         #point               has coordinate system   cs     
            ?co   space_and_time_extended:hasProjectedCoordinate_x ?xe.     #[extract cs to get x,y,z value] 
            ?xe sys:hasValue ?xv.
            ?xv sys:numericalValue ?x.
            ?co   space_and_time_extended:hasProjectedCoordinate_y ?ye.
            ?ye sys:hasValue ?yv.
            ?yv sys:numericalValue ?y.
            ?co   space_and_time_extended:hasProjectedCoordinate_z ?ze.
            ?ze sys:hasValue ?zv.
            ?zv sys:numericalValue ?z.
            }}
            '''.format(nodeuri)) 
            

        #query for roof max and ground min

        qHeight = self.query("""
            PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
            PREFIX citygml:<http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#>

             SELECT (MIN(?z) AS ?min) (MAX(?zr) AS ?max)                                       #select min of ground z values,   max  of   roof z values
            WHERE {{
              <{0}> citygml:boundedBy ?g.                                                      #building/part IRI   boundBy      surface
                ?g a citygml:GroundSurfaceType.                                                # surface            is a         GROUND 
              ?g citygml:lod2MultiSurface ?ms.                                                  #[select all coordi z value for ground surface]
               ?ms citygml:surfaceMember ?pol.                                                
               ?pol citygml:exterior ?lring.                                                   
                ?lring sys:contains ?po.
                 ?po space_and_time_extended:hasGISCoordinateSystem ?co.
                    ?co   space_and_time_extended:hasProjectedCoordinate_z ?ze.
                   ?ze sys:hasValue ?zv.
                  ?zv sys:numericalValue ?z.
             <{0}> citygml:boundedBy ?gr.                                                    #building/part IRI   boundBy      surface
                  ?gr a citygml:RoofSurfaceType.                                              # surface            is a         ROOF  
              ?gr citygml:lod2MultiSurface ?msr.                                             #[select all coordi z value for roof surface]
               ?msr citygml:surfaceMember ?polr.
               ?polr citygml:exterior ?lringr.
                ?lringr sys:contains ?por.
                 ?por space_and_time_extended:hasGISCoordinateSystem ?cor.
                    ?cor   space_and_time_extended:hasProjectedCoordinate_z ?zer.
                   ?zer sys:hasValue ?zvr.
                  ?zvr sys:numericalValue ?zr.
  
  
            }} GROUP BY ?g                                                                  # group by  each ground IRI
        """.format(nodeuri))

        # define coordi convert function :  building kb   --->   adms
        #Bdn2ADMSCoordC = defineCoordConvert('epsg:28992','epsg:32648')
        #float(row['min'].toPython()), float(row['max'].toPython())


        zlimit = tuple( (float(row['min'].toPython()), float(row['max'].toPython()))   for row in qHeight  )[0]

        return (   list( (float(row['x'].toPython()), float(row['y'].toPython())) for row in qData),        zlimit)

    def getMetrics(self, nodeuri):
        base = None
        if self.hasBuildingPart(nodeuri):
            print('{0} has building part'.format(nodeuri))
            #get list of building part
            bparts = list(row['p'] for row in self.query(
            ''' PREFIX citygml:<http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#>
            SELECT  ?p 
            WHERE
             {
             ?b citygml:consistsOfBuildingPart ?p.
            }'''))

            #get metrics for each part
            polygons = tuple( Polygon(*self.getBdnVertices(uri))    for uri in bparts)
            #get centroid for pols
            base = Polygon.combineBaseMulti(polygons)


        else: # no building part
            print('{0} NOT has building part'.format(nodeuri))
            verticesNHeight = self.getBdnVertices(nodeuri)
            #print(verticesHeight)
            base = Polygon(*verticesNHeight)

            #todo pack return result
            #('BldName','BldType','BldX','BldY','BldHeight', 'BldLength', 'BldWidth', 'BldAngle')

        print ((nodeuri, base.type, base.centroid[0], base.centroid[1], base.height, base.length, base.width, base.angle)   )
        #todo: coordinate coversion for centroid!!!
        return (uri2name(nodeuri), base.type, base.centroid[0], base.centroid[1], base.height, base.length, base.width, base.angle)    






        #calulate centro
        #choose shape
        #calculate angle
        #probably for the best if we construct a type of polygon instead?

    def hasBuildingPart(self, nodeuri):
        #self.connectDB(nodeuri)
        print('checking if building part for :{0}'.format(nodeuri))
        qData = self.query('''
            PREFIX citygml:<http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#>
            ASK       
             {{
             <{0}> citygml:consistsOfBuildingPart ?p
            }}
            '''.format(nodeuri))

        qData, = tuple(qData)
        return qData

    def coreBdn2Src(self):
        '''calculate main building for each src
        '''
        #compare src coords to each bdn
        for src in self.rawSrc:
            closed, dClosed, first = None, sys.maxsize, True
            print('find closed bdn for src: '+src.SrcName+" with x: "+str(src.SrcX1) +" y: "+str(src.SrcY1))
            for i in range(len(self.rawBdn.BldX)):
                

                #print('bdn x: ' +str( self.rawBdn.BldX[i]))
                print(type(self.rawBdn.BldX[i]))
                print(type(src.SrcX1))

                dx, dy = self.rawBdn.BldX[i] - src.SrcX1, self.rawBdn.BldY[i] - src.SrcY1 

                d = dx * dx + dy * dy
                if first:
                    dClosed = d
                    closed =  self.rawBdn.BldName[i]
                    first = False
                #print('d:{0} dclosed:{1}'.format(d, dClosed))
                if d - dClosed  < 0:
                    closed = self.rawBdn.BldName[i]
                    dClosed = d
                    print('new smallest distance: '+str(dClosed))
            if closed is not None:
                src.setMainBuilding(closed)

            else: #err handling, something is wrong if no closed building is found, just throw it    
                raise Exception('Dear lord, no closed buildinf found for src: '+src.SrcName)        

    def getOpt(self, PolNames, SrcNames):
        numPol = len(PolNames)
        return self.OPT(numPol,PolNames, [1]*numPol,[0]*numPol,[1]*numPol,[3]*numPol,[0]*numPol,[0]*numPol,[0]*numPol,[0]*80,[0]*80,['ug/m3']*numPol,1,0,1,"Grouptank001",SrcNames,0)

    def polIRI2Name(self, polIRI):
        substances = {
		#'http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#chlorine':'Cl2',
        'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Nitrogen__dioxide':'NO2',
        'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Carbon__monoxide':'CO',
        'http://www.theworldavatar.com/ontology/ontocape/material/substance/chemical_species.owl#Carbon__dioxide':'CO2',
		'http://www.theworldavatar.com/ontology/ontocape/material/substance/pseudocomponent.owl#Unburned_Hydrocarbon':'HC',
        'http://www.theworldavatar.com/ontology/ontocape/material/substance/pseudocomponent.owl#Nitrogen__oxides':'NOx'

        }

        if polIRI in substances.keys():
            print('Found: '+ substances[polIRI])
            return substances[polIRI]
        else:
            print (polIRI + 'Not found !!!!')
            #raise Exception('This substance is not defined!!!!')
            return None
  
    def getWeather(self):
        '''
        get weather data,
        for now we trigger a python script to write the .met file directly
        '''
        #finish writing met
        metLoc= r"test.met"
        cobbling.run(meteo_data = metLoc)
        #pointing to met in apl
        return os.path.realpath(metLoc)





    def get(self):
        '''main function, get all related info for adms
        returns: complete data for adms
        '''       
        #get all src data
        self.rawSrc = self.getSrcData()
        ##todo: think about this case: when nothing is found ,where should we handle it?
        ##then nothing should be written, and catch this exception and mute it in main function
        if self.rawSrc is None:
            raise Exception("No src in found to requiries")

        #get all building data

        self.rawBdn = self.getBdnData()
        #print('raw building: ')
        #print(self.rawBdn)
        rawOpt = self.getOpt(self.pollutants, [s.SrcName for s in self.rawSrc])
        self.coreBdn2Src()

        #for debuging, in future,define this for data type, i dont think it auto rpint for objet
        for src in self.rawSrc:
            print(src)
        
        met = self.getWeather()
        xran,yran = self.range
        grd = xran[0], yran[0], xran[1], yran[1]

        return {'Src': self.rawSrc, 'Bdn': self.rawBdn, 'Opt': rawOpt, 'Met': met, 'Grd':grd}
        


    def queryEndpoint(self, str):
        def literal2TLit(sparqlres):

            if 'results' not in sparqlres:
                return
            for row in sparqlres['results']['bindings']:
                for name,value in row.items():
                    if value['type'] == 'literal' and 'datatype' in value:
                        value['type'] = 'typed-literal'

        print('requesting @ '+self.address+" with query:")
        #print(str)
        resp = requests.get(self.address, params = {'query':str}, timeout = 1500, headers = {'user-agent': 'my-app/0.0.1'})
        print('raw resp:')

        print(resp.json())
        result = resp.json()
        literal2TLit(result)
        print('literal to typed-literal')
        print(result)
        qres = jsresult.JSONResult(result)#json decoded
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
        return uri.replace("http://www.jparksimulator.com",config.root ).replace("http://www.theworldavatar.com",config.root)

    def remote2local(self, func):
        '''decorator to change connection function to local connection by replacing given iri to local address
        '''
        def functionWrapper(self, address):
            address = self.Uri2Local(address)
            func(self, address)
        return functionWrapper    
    
    def connectDB(self, address, connectType = 'endpoint'):
        '''connect to db anyhow (we use rdflib graph parse now)
        '''
        def connectDBActual( address):
            '''
            Actual method to connect to db
            '''
            #obsolete: use rdflib locally
            self.address = address
            if connectType is 'parse':
                self.g = rdflib.Graph()#comment out in future
                self.g.parse(address)#comment out in future


        self.qmethodMap = {'parse': self.queryLocalGraph, 'endpoint':self.queryEndpoint}

        if not sameGraph(address, self.address):
            print ('parsing graph: '+ address)
            if connectType not in self.qmethodMap:
                raise exception('db connection method not defined')
            #self.connectType = connectType
            self.query = self.qmethodMap[connectType]
            connectDBActual(address)                 
       

        
               


def sameGraph(uri1, uri2):
    def trimloc(uri):
        if uri is None:
            return None
        else:
            return uri.split('#')[0]
    return trimloc(uri1) == trimloc(uri2)

def defineCoordConvert(inCode, outCode):
    inProj = Proj(init=inCode)
    outProj = Proj(init=outCode)
    def coordConvert(x,y):
        return transform(inProj, outProj, x, y)
    return coordConvert

def uri2name(uri):
    base = 'http://www.theworldavatar.com/'
    return uri.split('#')[1]



