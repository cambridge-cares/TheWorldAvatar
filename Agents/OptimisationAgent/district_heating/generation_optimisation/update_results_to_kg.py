"""
Upload results of optimisation to the knowledge graph
"""
import uuid

import utils
from javagateway import jpsBaseLibView


#TODO: currently new triples are being added, need to delete old triples as well
def update_timeseries_data(data, index):

    index = index.strftime(utils.FORMAT)[:data.shape[0]]
    times = index
    data.reset_index(drop=True, inplace=True)
    data.drop(data.columns[[-1,-2]], axis=1, inplace=True)

    # Initialise TimeSeriesClass
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE
    KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)

    dataIRIs = []
    values = []

    # Loop over all results
    # All new concepts from ontology have been used instead of EnergyInTimeInterval
    for column_name in data:
        # SWPS_GT - gas turbine
        if column_name == 'SWPS_GT':
            gen_heat_IRI = utils.PREFIXES['kg'] + "GeneratedHeatAmount_" + str(uuid.uuid4())
            measure_IRI = utils.PREFIXES['kg'] + "Measure_" + str(uuid.uuid4())
            query = utils.create_sparql_prefix('ohn') + \
                    utils.create_sparql_prefix('om') + \
                    utils.create_sparql_prefix('rdf') + \
                    '''INSERT { \
                    ?s ohn:hasGeneratedHeatAmount <%s> . \
                    <%s> om:hasValue <%s> ; \
                    rdf:type ohn:GeneratedHeatAmount . \
                    <%s> om:hasUnit om:megawattHour ;
                    rdf:type om:Measure  }
                    WHERE {?s rdf:type ohn:GasTurbine  }''' % (gen_heat_IRI, gen_heat_IRI, measure_IRI, measure_IRI)
            KGClient.executeUpdate(query)
            print("Triples independent of Java TimeSeriesClient successfully instantiated.")
            dataIRIs.append(measure_IRI)
            values.append(data[column_name].tolist())

        # Q_demand - consumer
        if column_name == 'Q_demand':
            heat_demand_IRI = utils.PREFIXES['kg'] + "HeatDemand_" + str(uuid.uuid4())
            measure_IRI = utils.PREFIXES['kg'] + "Measure_" + str(uuid.uuid4())
            query = utils.create_sparql_prefix('ohn') + \
                    utils.create_sparql_prefix('om') + \
                    utils.create_sparql_prefix('rdf') + \
                    '''INSERT { \
                    ?s ohn:hasHeatDemand <%s> . \
                    <%s> om:hasValue <%s> ; \
                    rdf:type ohn:HeatDemand . \
                    <%s> om:hasUnit om:megawattHour ;
                    rdf:type om:Measure  }
                    WHERE {?s rdf:type ohn:Consumer  }''' % (heat_demand_IRI, heat_demand_IRI, measure_IRI, measure_IRI)
            KGClient.executeUpdate(query)
            print("Triples independent of Java TimeSeriesClient successfully instantiated.")
            dataIRIs.append(measure_IRI)
            values.append(data[column_name].tolist())

        # TODO: fill in correct unit for minimum cost, then uncomment
        # # Min_cost
        # if column_name == 'Min_cost':
        #     min_cost_IRI = utils.PREFIXES['kg'] + "CostInTimeInterval_" + str(uuid.uuid4())
        #     measure_IRI = utils.PREFIXES['kg'] + "Measure_" + str(uuid.uuid4())
        #     query = utils.create_sparql_prefix('ohn') + \
        #             utils.create_sparql_prefix('om') + \
        #             utils.create_sparql_prefix('rdf') + \
        #             utils.create_sparql_prefix('ocape') + \
        #             '''INSERT { \
        #             ?s ocape:hasCost <%s> . \
        #             <%s> om:hasValue <%s> ; \
        #             rdf:type ohn:CostInTimeInterval . \
        #             <%s> om:hasUnit om:megawattHour ;
        #             rdf:type om:Measure  }
        #             WHERE {?s rdf:type ohn:MunicipalUtility  }''' % (min_cost_IRI, min_cost_IRI, measure_IRI, measure_IRI)
        #     KGClient.executeUpdate(query)
        #     print("Triples independent of Java TimeSeriesClient successfully instantiated.")
        #     dataIRIs.append(measure_IRI)
        #     values.append(data[column_name].tolist())

        # MHKW_ToP - incineration plant
        if column_name == 'MHKW_ToP':
            prov_heat_IRI = utils.PREFIXES['kg'] + "ProvidedHeatAmount_" + str(uuid.uuid4())
            measure_IRI = utils.PREFIXES['kg'] + "Measure_" + str(uuid.uuid4())
            query = utils.create_sparql_prefix('ohn') + \
                    utils.create_sparql_prefix('om') + \
                    utils.create_sparql_prefix('rdf') + \
                    '''INSERT { \
                    ?s ohn:hasProvidedHeatAmount <%s> . \
                    <%s> om:hasValue <%s> ; \
                    rdf:type ohn:ProvidedHeatAmount . \
                    <%s> om:hasUnit om:megawattHour ;
                    rdf:type om:Measure  }
                    WHERE {?s rdf:type ohn:IncinerationPlant  }''' % (prov_heat_IRI, prov_heat_IRI, measure_IRI, measure_IRI)
            KGClient.executeUpdate(query)
            print("Triples independent of Java TimeSeriesClient successfully instantiated.")
            dataIRIs.append(measure_IRI)
            values.append(data[column_name].tolist())

        # Gas_consumption - gas turbine
        if column_name == 'Gas_consumption':
            cons_gas_IRI = utils.PREFIXES['kg'] + "ConsumedGasAmount_" + str(uuid.uuid4())
            measure_IRI = utils.PREFIXES['kg'] + "Measure_" + str(uuid.uuid4())
            query = utils.create_sparql_prefix('ohn') + \
                    utils.create_sparql_prefix('om') + \
                    utils.create_sparql_prefix('rdf') + \
                    '''INSERT { \
                    ?s ohn:hasConsumedGasAmount <%s> . \
                    <%s> om:hasValue <%s> ; \
                    rdf:type ohn:ConsumedGasAmount . \
                    <%s> om:hasUnit om:megawattHour ;
                    rdf:type om:Measure  }
                    WHERE {?s rdf:type ohn:GasTurbine  }''' % (cons_gas_IRI, cons_gas_IRI, measure_IRI, measure_IRI)
            KGClient.executeUpdate(query)
            print("Triples independent of Java TimeSeriesClient successfully instantiated.")
            dataIRIs.append(measure_IRI)
            values.append(data[column_name].tolist())

        # Electricity_generation - gas turbine
        if column_name == 'Electricity_generation':
            elec_gen_IRI = utils.PREFIXES['kg'] + "CoGenElectricityAmount_" + str(uuid.uuid4())
            measure_IRI = utils.PREFIXES['kg'] + "Measure_" + str(uuid.uuid4())
            query = utils.create_sparql_prefix('ohn') + \
                    utils.create_sparql_prefix('om') + \
                    utils.create_sparql_prefix('rdf') + \
                    '''INSERT { \
                    ?s ohn:hasCoGenElectricityAmount <%s> . \
                    <%s> om:hasValue <%s> ; \
                    rdf:type ohn:CoGenElectricityAmount . \
                    <%s> om:hasUnit om:megawattHour ;
                    rdf:type om:Measure  }
                    WHERE {?s rdf:type ohn:GasTurbine  }''' % (elec_gen_IRI, elec_gen_IRI, measure_IRI, measure_IRI)
            KGClient.executeUpdate(query)
            print("Triples independent of Java TimeSeriesClient successfully instantiated.")
            dataIRIs.append(measure_IRI)
            values.append(data[column_name].tolist())


    # Update timeseries data
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, utils.PROPERTIES_FILE)
    TSClient.initTimeSeries(dataIRIs, [double_class]*len(dataIRIs), utils.FORMAT)

    timeseries = jpsBaseLibView.TimeSeries(times, dataIRIs, values)
    TSClient.addTimeSeriesData(timeseries)

    print("Time series data successfully added.\n")








