# This module instantiates several sample geospatial time series (i.e. sample
# consumption time series for various utilities (water, gas, electricity) for
# several consumers with location given as geospatial point
# ===============================================================================

import datetime as dt
import random
import uuid


# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module to access
# the TimeSeriesClient in the JPB_BASE_LIB
from jpsSingletons import jpsBaseLibView
# Get settings and functions from utils module
import utils

# ===============================================================================
# Specify Example Data
# All coordinates are given in EPSG:4326 CRS, neglecting any elevation (i.e. Z coordinate)

# Specify length of sample time series
n = 10

# Specify 3 sample consumers and generate sample time series data
consumer1 = {'consumer': 'Pembroke College',
             'lat': '52.201781',   # latitude
             'lon': '0.118648',    # longitude
             'geometry': '52.201781#0.118648#0',
             'times': [(dt.datetime.now() - dt.timedelta(hours=i)).strftime(utils.FORMAT) for i in range(n)],
             'timeseries': {
                 'Electricity': [float(random.randrange(10, 50000)/10) for i in range(n)],
                 #'water': [float(random.randrange(0, 1000)/10) for i in range(n)],
                 #'gas': [float(random.randrange(0, 25000)/10) for i in range(n)],
             },
}

consumer2 = {'consumer': 'Churchill College',
             'lat': '52.212679',
             'lon': '0.103013',
             'geometry': '52.213010945210094#0.10394786103593637#0.0#52.21294499201835#0.10422272276898911#0.0#52'
                         '.212796359598435#0.10412669929866045#0.0#52.21289786884543#0.10371343162435825#0.0#52'
                         '.21271564535361#0.10359503711851005#0.0#52.21266204132736#0.10381778403443923#0.0#52'
                         '.21255330232845#0.10374867839887686#0.0#52.212603335824426#0.10353454610410552#0.0#52'
                         '.2124771294758#0.10345158838374628#0.0#52.21238431314756#0.1038261799911189#0.0#52'
                         '.21165912811063#0.10335132369284608#0.0#52.211859563627684#0.1025179497135583#0.0#52'
                         '.21173586157487#0.10243570032662773#0.0#52.21203806612221#0.10121907523594938#0.0#52'
                         '.212453586212625#0.10148650203178138#0.0#52.212532958398164#0.10116967601847686#0.0#52'
                         '.21283261683736#0.10136898697091049#0.0#52.212718416662774#0.10183391829063554#0.0#52'
                         '.21229511759752#0.10155396972766484#0.0#52.21203037063283#0.10262613700766351#0.0#52'
                         '.211894679196654#0.10254053969324162#0.0#52.21180500523363#0.10290546919462068#0.0#52'
                         '.21262111928812#0.10343979996660319#0.0#52.21276744208706#0.10284274547319236#0.0#52'
                         '.21261962474492#0.10274632668291019#0.0#52.21260167528098#0.10282013290915998#0.0#52'
                         '.2126001830885#0.10282313642384777#0.0#52.21259704246196#0.10282781865388994#0.0#52'
                         '.21259346824993#0.10283160212751734#0.0#52.21259155289176#0.10283312185748#0.0#52'
                         '.21258646958297#0.10283580940478626#0.0#52.21258538625675#0.10283605100601101#0.0#52'
                         '.21258439016854#0.10283644310693822#0.0#52.21258114539977#0.10283687539654893#0.0#52'
                         '.2125758524762#0.10283618630460024#0.0#52.21257477956984#0.10283584287795475#0.0#52'
                         '.212570770498274#0.10283375061504343#0.0#52.21256713903351#0.10283065152493438#0.0#52'
                         '.21256301018977#0.10282518686853868#0.0#52.21256031883415#0.10281979009627723#0.0#52'
                         '.212558087113166#0.10281382951052165#0.0#52.2125564947128#0.10280731359649198#0.0#52'
                         '.21255553642309#0.10280053486772346#0.0#52.21255435171428#0.10276607964703188#0.0#52'
                         '.21224412312224#0.1025646507764276#0.0#52.21236248472787#0.10207825891770461#0.0#52'
                         '.212768157478635#0.10234815909343886#0.0#52.21266993035701#0.10274401803480761#0.0#52'
                         '.21278886391799#0.10282136085488745#0.0#52.2128364191951#0.10262511413041796#0.0#52'
                         '.21293251063293#0.10268747208127339#0.0#52.21302769080621#0.10229102755481514#0.0#52'
                         '.21313917423071#0.10236260188709706#0.0#52.21339438628116#0.10132084167855077#0.0#52'
                         '.21368809112149#0.10151109393402326#0.0#52.21339692617842#0.10271350119113616#0.0#52'
                         '.21312489519342#0.10253831788236166#0.0#52.21315374689625#0.10242286737577584#0.0#52'
                         '.21304364241464#0.10234960138194535#0.0#52.212905871630916#0.10290080888871043#0.0#52'
                         '.21306302591927#0.103003085996275#0.0#52.21308651339749#0.10290611950130761#0.0#52'
                         '.21336530929178#0.10308513773882928#0.0#52.21340673605666#0.10291494811169911#0.0#52'
                         '.2134838809048#0.1029657268135906#0.0#52.21342441706554#0.10320957368634154#0.0#52'
                         '.21353774482377#0.10328358042208487#0.0#52.213512859850994#0.10338326301643287#0.0#52'
                         '.21319548432687#0.10317929150543607#0.0#52.21304853444015#0.10379125222072451#0.0#52'
                         '.21314974358681#0.10385400136891636#0.0#52.21311113992505#0.1040120263895812#0.0#52'
                         '.213010945210094#0.10394786103593637#0.0',
             'times': [(dt.datetime.now() - dt.timedelta(hours=2*i)).strftime(utils.FORMAT) for i in range(n)],
             'timeseries': {
                 'Electricity': [float(random.randrange(10, 50000)/10) for i in range(n)],
                 'Water': [float(random.randrange(0, 1000)/10) for i in range(n)],
                 #'gas': [float(random.randrange(0, 25000)/10) for i in range(n)],
             },
}

consumer3 = {'consumer': 'Department of Chemical Engineering and Biotechnology',
             'lat': '52.209452',
             'lon': '0.085788',
             'geometry': '52.20940935053205#0.08670907445906056#10.0#52.208890613617214#0.08660675428533886#10.0#52'
                         '.208949562965174#0.08581560760871326#10.0#52.20903683985891#0.0858328683430096#10.0#52'
                         '.209054759791215#0.08563303441436784#10.0#52.20905904455623#0.08560937676277162#10.0#52'
                         '.20906809888921#0.08558550334358743#10.0#52.20908239723303#0.08556509561586317#10.0#52'
                         '.209092659003176#0.08555430564952596#10.0#52.209104332450025#0.0855450454829199#10.0#52'
                         '.209116672957#0.08553874395016828#10.0#52.20916712414761#0.08552822581612507#10.0#52'
                         '.2092176781486#0.0855373261200414#10.0#52.20934508383067#0.08560915905273066#10.0#52'
                         '.20936181904184#0.08562384798655853#10.0#52.2093737129303#0.08563772473239796#10.0#52'
                         '.2093866345562#0.08566467664441715#10.0#52.209397526580155#0.08574539811222488#10.0#52'
                         '.209382870570835#0.08595387605914039#10.0#52.209464415411276#0.0859698442124265#10.0#52'
                         '.20940935053205#0.08670907445906056#10.0',
             'building_height': 10.0,
             'times': [(dt.datetime.now() - dt.timedelta(minutes=30*i)).strftime(utils.FORMAT) for i in range(n)],
             'timeseries': {
                 'Electricity': [float(random.randrange(10, 50000)/10) for i in range(n)],
                 'Water': [float(random.randrange(0, 1000)/10) for i in range(n)],
                 'Gas': [float(random.randrange(0, 25000)/10) for i in range(n)],
             },
}

units = {'Electricity': 'kWh/h',
         'Water': 'm^3/h',
         'Gas': 'Btu/h'
         }

# ===============================================================================
# Instantiate Example Data

if __name__ == '__main__':

    # Create specified PostgreSQL database
    utils.create_postgres_db()
    # Create specified Blazegraph namespace
    utils.create_blazegraph_namespace()

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)

    # Retrieve Java classes for time entries (Instant) and data (ALL Double)
    # (required for time series client instantiation)
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE

    # Loop over all consumers
    consumers = [consumer1, consumer2, consumer3]
    for c in consumers:
        print('Current consumer: ', c['consumer'])

        # Create IRI for current consumer
        consumerIRI = utils.PREFIXES['ex'] + 'Consumer_' + str(uuid.uuid4())

        # Initialise list of dataIRIs, which will be represented as time series
        dataIRIs = []

        # Loop over all time series for this consumer
        for ts in list(c['timeseries'].keys()):

            # Create IRI for current time series and attach to dataIRI list
            dataIRI = utils.PREFIXES['ex'] + ts + '_' + str(uuid.uuid4())
            dataIRIs.append(dataIRI)

            # 1) Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
            query = utils.create_sparql_prefix('ex') + \
                    utils.create_sparql_prefix('rdf') + \
                    utils.create_sparql_prefix('rdfs') + \
                    utils.create_sparql_prefix('geolit') + \
                    '''INSERT DATA { \
                    <%s> rdf:type ex:Consumer ; \
                         rdfs:label "%s" ; \
                         ex:consumes <%s> ; \
                         ex:hasLocation "%s ; \
                         ex:hasGeometry "%s" . \
                    <%s> rdf:type ex:Consumption ; \
                         rdfs:label "%s" ; \
                         ex:unit "%s" . }''' % ( consumerIRI, c['consumer'], dataIRI,
                                                 c['lat'] + '#' + c['lon'] + '\"^^geolit:lat-lon', c['geometry'],
                                                 dataIRI, ts+' consumption', units[ts])

            KGClient.executeUpdate(query)
            print("Triples independent of Java TimeSeriesClient successfully instantiated.")

        # 2) Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
        # Initialise time series in both KG and RDB using TimeSeriesClass
        TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, utils.PROPERTIES_FILE)
        TSClient.initTimeSeries(dataIRIs, [double_class]*len(dataIRIs), utils.FORMAT)

        print("Time series triples via Java TimeSeriesClient successfully instantiated.")

        # 3) Add actual time series data
        # Create Java TimeSeries object with data to attach
        times = c['times']
        variables = dataIRIs
        values = [c['timeseries'][v] for v in list(c['timeseries'].keys())]
        timeseries = jpsBaseLibView.TimeSeries(times, variables, values)
        # Add data
        TSClient.addTimeSeriesData(timeseries)

        print("Time series data successfully added.\n")

    print("All data successfully instantiated")


