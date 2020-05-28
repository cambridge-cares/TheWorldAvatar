from collections import namedtuple


class Constants(object):
    BASE_IRI = 'http://www.theworldavatar.com/'
    IRI_ONTO = BASE_IRI + 'ontology/'
    IRI_ONTOCAPE = IRI_ONTO + 'ontocape/'
    IRI_KB = BASE_IRI + 'kb/'
    IRI_KB_SHIPS = IRI_KB + 'ships/'
    #IRI_KB_SHIPS ='http://localhost:8080/kb/ships/' #(only used for testing local)
    BLD_BDN = 'BDN'
    BLD_NUM = 'BldNumBuildings'
    BLD_NAME = 'BldName'
    BLD_TYPE = 'BldType'
    BLD_X = 'BldX'
    BLD_Y = 'BldY'
    BLD_HEIGHT = 'BldHeight'
    BLD_LRNGTH = 'BldLength'
    BLD_WIDTH = 'BldWidth'
    BLD_ANGLE = 'BldAngle'
    BLD_TOPNODE = BASE_IRI + 'damecoolquestion/buildingsLite/sparql'
    BLD_LIMIT = 25
    SRC_MAIN_BLD = 'SrcMainBuilding'
    SRC_VERT_VELOC = 'SrcVertVeloc'
    COORD_MAX_CORNER = 'uppercorner'
    COORD_MAX_X = 'upperx'
    COORD_MAX_Y = 'uppery'
    COORD_MIN_CORNER = 'lowercorner'
    COORD_MIN_X = 'lowerx'
    COORD_MIN_Y = 'lowery'
    KEY_MAX_X = 'xmax'
    KEY_MAX_Y = 'ymax'
    KEY_MIN_X = 'xmin'
    KEY_MIN_Y = 'ymin'
    KEY_BDN = 'Bdn'
    KEY_MET = 'Met'
    KEY_BKG = 'Bkg'
    KEY_COORD_SYS = 'CoordiSys'
    KEY_LAT = 'lat'
    KEY_LON = 'lon'
    KEY_MMSI = 'mmsi'
    KEY_SRC = 'Src'
    KEY_GRD = 'Grd'
    KEY_OPT = 'Opt'
    KEY_POL = 'Pol'
    KEY_INDICATOR = 'indicator'
    KEY_INDICATOR_TERR = 'terr' + KEY_INDICATOR
    KEY_INDICATOR_CHEM = 'chem' + KEY_INDICATOR
    KEY_INDICATOR_WET = 'wet' + KEY_INDICATOR
    KEY_WASHOUT = 'washout'
    KEY_WASHOUT_SO2 = 'so2' + KEY_WASHOUT
    KEY_WASHOUT_PM10 = 'pm10' + KEY_WASHOUT
    KEY_NIGHT = 'night'
    KEY_DIR = 'dir'
    KEY_DIR_NIGHT = KEY_DIR + KEY_NIGHT
    KEY_INDEX = 'index'
    KEY_COUNT = 'count'
    STR_CHIMNEY = "/Chimney-1.owl"
    POL_CO2 = 'CO2'
    POL_NOX = 'NOx'
    POL_NO2 = 'NO2'
    POL_NO = 'NO'
    POL_PART_O3 = 'O3'
    POL_VOC = 'VOC'
    POL_PART_SO2 = 'SO2'
    POL_PM10 = 'PM10'
    POL_PM25 = 'PM2.5'
    POL_CO = 'CO'
    POL_BENZENE = 'BENZENE'
    POL_BUTADIENE = 'BUTADIENE'
    POL_HCl = 'HCl'
    POL_Cl2 = 'Cl2'
    POL_CH3Cl = 'CH3Cl'
    POL_ISOBUTYLENE = 'ISOBUTYLENE'
    POL_NH3 = 'NH3'
    POL_HC = 'HC'
    POL_PART_001 = 'Particulate001'
    FILE_NAME_APL = '/test.apl'
    CRS_EPSG_4326 = 'epsg:4326'
    ENTITY_TYPE_PLANT = "plant"
    ENTITY_TYPE_SHIP = "ship"
    FILENAME_MET = '/test.met'
    FILENAME_BGD = '/testbackgrnd.bgd'
    FILEPATH_NIGHT = 'C:\\JPS_DATA\\workingdir\\JPS\\ADMS\\chemistrynight.AAI'
    FILEPATH_HIL_HK = 'C:\\JPS_DATA\\workingdir\\JPS\\ADMS\\hkterrainlatestupdated.ter'
    FILEPATH_HIL_SG = 'C:\\Users\\kevin\\Downloads\\A48\\terrain accurate\\singaporeterrain.ter'
    FILEPATH_HIL_BGD = 'D:\\ADMS 5.2\\Test files\\tank1574leakage\\background condition.bgd'
    FILEPATH_MET = 'C://JPS_DATA/workingdir/JPS/ADMS/test.met'
    FILEPATH_BKG = 'C://JPS_DATA/workingdir/JPS/ADMS/testbackgrnd.bkg'
    UNIT_PPB = 'ppb'
    UNIT_UGM3 = 'ug/m3'
    GRD_X = 'grid_x'
    GRD_Y = 'grid_y'
    OWL_BEHAVIOUR = IRI_ONTOCAPE + 'chemical_process_system/CPS_behavior/behavior.owl#'
    OWL_GEOMETRY = IRI_ONTOCAPE + 'supporting_concepts/geometry/geometry.owl#'
    OWL_SYSTEM = IRI_ONTOCAPE + 'upper_level/system.owl#'
    OWL_PHASE_SYSTEM = IRI_ONTOCAPE + 'material/phase_system/phase_system.owl#'
    OWL_TIMESPACE_EXT = IRI_ONTOCAPE + 'supporting_concepts/space_and_time/space_and_time_extended.owl#'
    OWL_PLANT = IRI_ONTOCAPE + 'chemical_process_system/CPS_realization/plant.owl#'
    OWL_TOPOLOGY = IRI_ONTO + 'meta_model/topology/topology.owl#'
    OWL_CHEM_PROC_SYS = IRI_ONTOCAPE + 'chemical_process_system/chemical_process_system.owl#'
    OWL_TECH_SYS = IRI_ONTOCAPE + 'upper_level/technical_system.owl#'
    OWL_MATERIAL = IRI_ONTOCAPE + 'material/material.owl#'
    OWL_SUBSTANCE = IRI_ONTOCAPE + 'material/substance/substance.owl#'
    OWL_NO2 = IRI_ONTOCAPE + 'material/substance/chemical_species.owl#Nitrogen__dioxide'
    OWL_CO = IRI_ONTOCAPE + 'material/substance/chemical_species.owl#Carbon__monoxide'
    OWL_CO2 = IRI_ONTOCAPE + 'material/substance/chemical_species.owl#Carbon__dioxide'
    OWL_SO2 = IRI_ONTOCAPE + 'material/substance/chemical_species.owl#Sulfur__dioxide'
    OWL_O3 = IRI_ONTOCAPE + 'material/substance/chemical_species.owl#Ozone'
    OWL_HC = IRI_ONTOCAPE + 'material/substance/pseudocomponent.owl#Unburned_Hydrocarbon'
    OWL_NOx = IRI_ONTOCAPE + 'material/substance/pseudocomponent.owl#Nitrogen__oxides'
    OWL_PART_001 = IRI_KB + 'ships/Chimney-1.owl#Particulate-001'
    KEY_DIAMETER = 'diameter'
    KEY_DENSITY = 'density'
    KEY_MASS_FRACTION = 'massFraction'
    KEY_MASS_RATE = 'massrate'
    KEY_CONTENT = 'content'
    KEY_HEIGHT = 'height'
    KEY_TEMP = 'temp'
    KEY_MOLE_WEIGHT = 'moleweight'
    KEY_HEAT_CAP = 'heatcapa'
    KEY_MASS_FLOW = 'massflow'
    KEY_VELOCITY = 'velocity'
    KEY_EM_RATES = 'emissionrates'
    KEY_ER = 'er'
    KEY_V = 'v'
    KEY_O = 'o'
    KEY_X = 'x'
    KEY_Y = 'y'
    KEY_PARSE = 'parse'
    KEY_CONN_PARSE = KEY_PARSE
    KEY_CONN_ENDPOINT = 'endpoint'
    KEY_RESULTS = 'results'
    KEY_BINDINGS = 'bindings'
    KEY_TYPE = 'type'
    KEY_LITERAL = 'literal'
    KEY_TLITERAL = 'typed-' + KEY_LITERAL
    KEY_DATA_TYPE = 'datatype'
    KEY_QUERY = 'query'
    KEY_UAGENT = 'user-agent'
    KEY_APP_CLIENT = 'my-app/0.0.1'
    TPL_OPT = namedtuple('OPT', ['OptNumOutputs', 'OptPolName', 'OptInclude', 'OptShortOrLong', 'OptSamplingTime',
                                 'OptSamplingTimeUnits', 'OptCondition', 'OptNumPercentiles', 'OptNumExceedences',
                                 'OptPercentiles', 'OptExceedences', 'OptUnits', 'OptGroupsOrSource',
                                 'OptAllSources', 'OptNumGroups', 'OptIncludedGroups', 'OptIncludedSource',
                                 'OptCreateComprehensiveFile'])
    KEY_GRPTANK = 'Grouptank001'
    KEY_SHIP_OPT_SRCNAME = 'SrcNames'


class QueryStrings(object):
    SPARQL_DIAMETER_DENSITY_MASSFRACTION = '''
                PREFIX j.0: <''' + Constants.OWL_BEHAVIOUR + '''>
                PREFIX j.3: <''' + Constants.OWL_GEOMETRY + '''>
                PREFIX system: <''' + Constants.OWL_SYSTEM + '''>
                PREFIX j.1:<''' + Constants.OWL_PHASE_SYSTEM + '''>
                
                SELECT distinct ?p ?''' + Constants.KEY_DIAMETER + ''' ?''' + Constants.KEY_DENSITY + ''' ?''' \
                                           + Constants.KEY_MASS_FRACTION + '''
                
                WHERE{
                    ?p a j.0:SingleParticle.
                    ?p j.3:has_length ?de.
                    ?de system:hasValue ?ve.
                    ?ve system:numericalValue ?''' + Constants.KEY_DIAMETER + '''.
                    ?p j.1:has_density ?dene.
                    ?dene system:hasValue ?dve.
                    ?dve system:numericalValue ?''' + Constants.KEY_DENSITY + '''.
                    ?p system:hasProperty ?mf.
                    ?mf system:hasValue ?mfve.
                    ?mfve system:numericalValue ?''' + Constants.KEY_MASS_FRACTION + '''.
                }
                '''
    SPARQL_MASSRATE = '''
                PREFIX j.0: <''' + Constants.OWL_BEHAVIOUR + '''>
                PREFIX system: <''' + Constants.OWL_SYSTEM + '''>
                PREFIX j.1:<''' + Constants.OWL_PHASE_SYSTEM + '''>
                
                SELECT distinct ?''' + Constants.KEY_MASS_RATE + '''
                
                WHERE{
                    ?p a j.0:ParticulateMaterialAmount.
                    ?p system:hasProperty ?de.
                    ?de system:hasValue ?ve.
                    ?ve system:numericalValue ?''' + Constants.KEY_MASS_RATE + '''.
                }
                '''
    SPARQL_DIAMETER_TEMP_HEIGHT_MASSFLOW_HEATCAPA_DENSITY_MOLEWEIGHT = '''
                PREFIX sys: <''' + Constants.OWL_SYSTEM + '''>
                PREFIX space_and_time_extended: <''' + Constants.OWL_TIMESPACE_EXT + '''>
                PREFIX plant:<''' + Constants.OWL_PLANT + '''>
                PREFIX topology:<''' + Constants.OWL_TOPOLOGY + '''>
                PREFIX behavior: <''' + Constants.OWL_BEHAVIOUR + '''>
                PREFIX chemical_process_system: <''' + Constants.OWL_CHEM_PROC_SYS + '''>
                PREFIX techsys: <''' + Constants.OWL_TECH_SYS + '''>
                PREFIX phase_system:<''' + Constants.OWL_PHASE_SYSTEM + '''>
                PREFIX material: <''' + Constants.OWL_MATERIAL + '''>
                PREFIX substance:<''' + Constants.OWL_SUBSTANCE + '''>
                
                SELECT distinct ?''' + Constants.KEY_O + ''' ?''' + Constants.KEY_DIAMETER + ''' ?''' + \
                                    Constants.KEY_TEMP + ''' ?''' + Constants.KEY_HEIGHT + ''' ?''' + \
                                    Constants.KEY_MASS_FLOW + ''' ?''' + Constants.KEY_HEAT_CAP + ''' ?''' + \
                                    Constants.KEY_DENSITY + ''' ?''' + Constants.KEY_MOLE_WEIGHT + '''
                WHERE {
                
                    ?o plant:hasHeight ?he.
                    ?he sys:hasValue ?hv.
                    ?hv sys:numericalValue ?''' + Constants.KEY_HEIGHT + ''' .
      
                    ?o  techsys:realizes ?process.
                    ?process topology:hasOutput ?stream.
                    ?stream chemical_process_system:refersToGeneralizedAmount ?ga.
                    
                    ?ga sys:hasProperty ?ve.
                    ?ve a behavior:ConvectiveMassFlowrate .
                    ?ve sys:hasValue ?vv.
                    ?vv sys:numericalValue ?''' + Constants.KEY_MASS_FLOW + '''.
                    
                    ?o a plant:Pipe .
                    ?o plant:hasInsideDiameter ?de . #?dev sys:hasValue ?de.
                    ?de sys:hasValue ?ed.
                    ?ed sys:numericalValue ?''' + Constants.KEY_DIAMETER + '''.
                    
                    ?phase phase_system:has_temperature ?tempE.
                    ?tempE sys:hasValue ?vte.
                    ?vte sys:numericalValue ?''' + Constants.KEY_TEMP + ''' .
                    
                    
                    ?cp a phase_system:ThermodynamicStateProperty.
                    ?cp sys:hasValue ?cpv.
                    ?cpv sys:numericalValue ?''' + Constants.KEY_HEAT_CAP + '''.
                    
                    ?singlephase a phase_system:SinglePhase.
                    ?singlephase phase_system:has_density ?den.
                    ?den a phase_system:Density.
                    ?den sys:hasValue ?denv.
                    ?denv sys:numericalValue ?''' + Constants.KEY_DENSITY + '''.
                    
                    
                    ?mw a substance:MolecularWeight.
                    ?mw sys:hasValue ?mwv.
                    ?mwv sys:numericalValue ?''' + Constants.KEY_MOLE_WEIGHT + '''. 
                    OPTIONAL {
                        ?o space_and_time_extended:hasGISCoordinateSystem ?coe .
                        ?coe space_and_time_extended:hasProjectedCoordinate_x ?xe.
                        ?xe sys:hasValue ?xv.
                        ?xv sys:numericalValue ?x.
                        
                        ?coe space_and_time_extended:hasProjectedCoordinate_y ?ye.
                        ?ye sys:hasValue ?yv.
                        ?yv sys:numericalValue ?y.
                        
                    }
                }
            '''

    SPARQL_CONTENT_PARTICULATE = '''
                PREFIX sys: <''' + Constants.OWL_SYSTEM + '''>
                PREFIX substance:<''' + Constants.OWL_SUBSTANCE + '''>
                PREFIX part:<''' + Constants.OWL_BEHAVIOUR + '''>
    
                SELECT DISTINCT  ?''' + Constants.KEY_CONTENT + '''
                WHERE {{
                    {{
                    ?mix a substance:Mixture.
                    ?mix sys:containsDirectly  ?''' + Constants.KEY_CONTENT + '''. 
                    }} UNION {{
                     ?''' + Constants.KEY_CONTENT + '''   a part:ParticulateMaterialAmount.
                    }}
                }}
            '''

    SPARQL_ERATE = '''
                PREFIX sys: <''' + Constants.OWL_SYSTEM + '''>
                PREFIX substance:<''' + Constants.OWL_SUBSTANCE + '''>
                PREFIX behavior:<''' + Constants.OWL_BEHAVIOUR + '''>
                
                SELECT DISTINCT  ?''' + Constants.KEY_ER + ''' ?''' + Constants.KEY_V + '''
                WHERE {{
                    ?mix sys:hasProperty  ?''' + Constants.KEY_ER + '''.
                    ?''' + Constants.KEY_ER + '''  a behavior:ConvectiveMassFlowrate.
                     ?''' + Constants.KEY_ER + ''' sys:hasValue ?erv.
                    ?erv sys:numericalValue ?''' + Constants.KEY_V + '''
                }}
            '''

    SPARQL_HEIGHT_DIAMETER_CONTENT_X_Y_VELOCITY_MASSFLOW_TEMP_HEATCAPA_DENSITY_MOLEWEIGHT = '''
                PREFIX sys: <''' + Constants.OWL_SYSTEM + '''>
                PREFIX space_and_time_extended: <''' + Constants.OWL_TIMESPACE_EXT + '''>
                PREFIX plant:<''' + Constants.OWL_PLANT + '''>
                PREFIX topology:<''' + Constants.OWL_TOPOLOGY + '''>
                PREFIX behavior: <''' + Constants.OWL_BEHAVIOUR + '''>
                PREFIX chemical_process_system:<''' + Constants.OWL_CHEM_PROC_SYS + '''>
                PREFIX phase_system:<''' + Constants.OWL_PHASE_SYSTEM + '''>
                PREFIX material: <''' + Constants.OWL_MATERIAL + '''>
                PREFIX substance:<''' + Constants.OWL_SUBSTANCE + '''>

                SELECT ?''' + Constants.KEY_O + ''' ?''' + Constants.KEY_HEIGHT + ''' ?''' + Constants.KEY_DIAMETER \
                            + ''' ?''' + Constants.KEY_CONTENT + ''' ?x ?y ?''' + Constants.KEY_VELOCITY + ''' ?''' \
                            + Constants.KEY_MASS_FLOW + ''' ?''' + Constants.KEY_TEMP + ''' ?''' \
                            + Constants.KEY_MOLE_WEIGHT + ''' ?''' + Constants.KEY_HEAT_CAP + ''' ?''' \
                            + Constants.KEY_DENSITY + '''
                WHERE {{
                ?o plant:hasHeight ?he.
                ?he sys:numericalValue ?''' + Constants.KEY_HEIGHT + ''' .

                 ?o sys:hasSubsystem ?chm.
                ?chm plant:hasInsideDiameter ?de . #?dev  sys:hasValue ?de.
                ?de sys:numericalValue ?''' + Constants.KEY_DIAMETER + ''' .

                ?phase phase_system:has_temperature  ?tempE.
                ?tempE sys:hasValue ?vte.
                ?vte sys:numericalValue ?''' + Constants.KEY_TEMP + ''' .

                ?o space_and_time_extended:hasGISCoordinateSystem ?coe .
                ?coe space_and_time_extended:hasProjectedCoordinate_x ?xe.
                 ?xe sys:hasValue ?xv.
                  ?xv sys:numericalValue ?''' + Constants.KEY_X + '''.

                   ?coe space_and_time_extended:hasProjectedCoordinate_y ?ye.
                 ?ye sys:hasValue ?yv.
                  ?yv sys:numericalValue ?''' + Constants.KEY_Y + '''.

                  ?stream topology:leaves ?''' + Constants.KEY_O + '''.
                  ?stream chemical_process_system:refersToGeneralizedAmount ?ga.
                  ?ga sys:hasSubsystem ?ma.
                  
                  ?ma sys:hasProperty ?ve.
                  ?ve a behavior:Velocity .
                  ?ve sys:hasValue ?vv.
                  ?vv sys:numericalValue ?''' + Constants.KEY_VELOCITY + '''.
                  
                  ?ma sys:hasProperty ?me.
                  ?me a behavior:ConvectiveMassFlowrate .
                  ?me sys:hasValue ?mv.
                  ?mv sys:numericalValue ?''' + Constants.KEY_MASS_FLOW + ''' .
    
                 ?mw a substance:MolecularWeight.
                 ?mw sys:hasValue ?mwv.
                 ?mwv  sys:numericalValue ?''' + Constants.KEY_MOLE_WEIGHT + '''.
    
                 ?cp a phase_system:ThermodynamicStateProperty.
                                     ?cp sys:hasValue ?cpv.
                 ?cpv  sys:numericalValue ?''' + Constants.KEY_HEAT_CAP + '''.
    
                 ?den a phase_system:Density.
                  ?den sys:hasValue ?denv.
                 ?denv  sys:numericalValue ?''' + Constants.KEY_DENSITY + '''.
    
                }}
                LIMIT 1 
            '''

    SPARQL_CONTENT = '''
                PREFIX sys: <''' + Constants.OWL_SYSTEM + '''>
                PREFIX substance:<''' + Constants.OWL_SUBSTANCE + '''>
    
                SELECT DISTINCT  ?''' + Constants.KEY_CONTENT + '''
                WHERE {{
                ?mix a substance:Mixture.
                ?mix sys:containsDirectly  ?''' + Constants.KEY_CONTENT + '''. 
                }}
            '''


class ExceptionStrings(object):
    DB_CONN_UNKNOWN = 'Db connection method not defined.'
    BDN_NOT_FOUND = 'No closed building found for src: '
    SUBSTANCE_UNDEFINED = 'This substance is not defined.'
