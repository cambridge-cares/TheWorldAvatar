''' Create SPARQL queries for optimisation inputs'''
import json

import utils
from javagateway import jpsBaseLibView


def query_optimisation_inputs():
        response_list = []
        title_list = []
        KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)

        #TODO: Update query to use "HeatDemand" concept
        # Waermeeinspeisung
        query_Waermeeinspeisung = utils.create_sparql_prefix('ts') + \
                                utils.create_sparql_prefix('om') + \
                                utils.create_sparql_prefix('ohn') + \
                                '''SELECT ?dataIRI \
                                WHERE { ?s ohn:hasHeatDemand ?o . \
                                ?o om:hasValue ?dataIRI . \
                                ?dataIRI ts:hasTimeSeries ?timeseries}'''
        response = KGClient.execute(query_Waermeeinspeisung)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('Waermeeinspeisung')

        #TODO: For GaspreisKessel and GaspreisGT, there is no way to differentiate between these concepts, to be fixed.
        # Currently using string manipulation
        # GaspreisGT
        query_GaspreisGT = utils.create_sparql_prefix('ts') + \
                           utils.create_sparql_prefix('om') + \
                           utils.create_sparql_prefix('ohn') + \
                           utils.create_sparql_prefix('rdf') + \
                           '''SELECT ?dataIRI \
                           WHERE { ?s rdf:type ohn:GasUnitCost . \
                           ?s om:hasValue ?dataIRI . \
                           ?dataIRI ts:hasTimeSeries ?timeseries \
                           FILTER regex(str(?s), "GT")}'''
        response = KGClient.execute(query_GaspreisGT)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('GaspreisGT')

        #TODO: Uses string manipulation, to be fixed
        # GaspreisKessel
        query_GaspreisKessel = utils.create_sparql_prefix('ts') + \
                               utils.create_sparql_prefix('om') + \
                               utils.create_sparql_prefix('ohn') + \
                               utils.create_sparql_prefix('rdf') + \
                               '''SELECT ?dataIRI \
                               WHERE { ?s rdf:type ohn:GasUnitCost . \
                               ?s om:hasValue ?dataIRI . \
                               ?dataIRI ts:hasTimeSeries ?timeseries \
                               FILTER regex(str(?s), "Kessel")}'''
        response = KGClient.execute(query_GaspreisKessel)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('GaspreisKessel')

        # Spotpreis
        query_Spotpreis = utils.create_sparql_prefix('ts') + \
                               utils.create_sparql_prefix('om') + \
                               utils.create_sparql_prefix('ohn') + \
                               utils.create_sparql_prefix('rdf') + \
                               '''SELECT ?dataIRI \
                               WHERE { ?s rdf:type ohn:ElectricitySpotPrice . \
                               ?s om:hasValue ?dataIRI . \
                               ?dataIRI ts:hasTimeSeries ?timeseries}'''
        response = KGClient.execute(query_Spotpreis)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('Spotpreis')

        # CO2Preis
        query_CO2Preis = utils.create_sparql_prefix('ts') + \
                          utils.create_sparql_prefix('om') + \
                          utils.create_sparql_prefix('ohn') + \
                          utils.create_sparql_prefix('rdf') + \
                          '''SELECT ?dataIRI \
                          WHERE { ?s rdf:type ohn:CO2CertificatePrice . \
                          ?s om:hasValue ?dataIRI . \
                          ?dataIRI ts:hasTimeSeries ?timeseries}'''
        response = KGClient.execute(query_CO2Preis)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('CO2Preis')

        # TempVorlauf
        query_TempVorlauf = utils.create_sparql_prefix('ts') + \
                         utils.create_sparql_prefix('om') + \
                         utils.create_sparql_prefix('ohn') + \
                         utils.create_sparql_prefix('rdf') + \
                         '''SELECT ?dataIRI \
                         WHERE { ?s rdf:type ohn:GridConnection . \
                         ?grid ohn:hasUpstreamGridConnection ?s . \
                         ?grid rdf:type ohn:MunicipalUtility . \
                         ?s ohn:hasObservableProperty ?x . \
                         ?x rdf:type om:Temperature . \
                         ?x om:hasValue ?dataIRI . \
                         ?dataIRI ts:hasTimeSeries ?timeseries}'''
        response = KGClient.execute(query_TempVorlauf)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('TempVorlauf')

        # MHKWTempVorlauf
        query_MHKWTempVorlauf = utils.create_sparql_prefix('ts') + \
                            utils.create_sparql_prefix('om') + \
                            utils.create_sparql_prefix('ohn') + \
                            utils.create_sparql_prefix('rdf') + \
                            '''SELECT ?dataIRI \
                            WHERE { ?s rdf:type ohn:GridConnection . \
                            ?grid ohn:hasUpstreamGridConnection ?s . \
                            ?grid rdf:type ohn:IncinerationPlant . \
                            ?s ohn:hasObservableProperty ?x . \
                            ?x rdf:type om:Temperature . \
                            ?x om:hasValue ?dataIRI . \
                            ?dataIRI ts:hasTimeSeries ?timeseries}'''
        response = KGClient.execute(query_MHKWTempVorlauf)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('MHKWTempVorlauf')

        # TempRuecklauf
        query_TempRuecklauf = utils.create_sparql_prefix('ts') + \
                                utils.create_sparql_prefix('om') + \
                                utils.create_sparql_prefix('ohn') + \
                                utils.create_sparql_prefix('rdf') + \
                                '''SELECT ?dataIRI \
                                WHERE { ?s rdf:type ohn:GridConnection . \
                                ?grid ohn:hasDownstreamGridConnection ?s . \
                                ?grid rdf:type ohn:MunicipalUtility . \
                                ?s ohn:hasObservableProperty ?x . \
                                ?x rdf:type om:Temperature . \
                                ?x om:hasValue ?dataIRI . \
                                ?dataIRI ts:hasTimeSeries ?timeseries}'''
        response = KGClient.execute(query_TempRuecklauf)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('TempRuecklauf')

        # MHKWTempRuecklauf
        query_MHKWTempRuecklauf = utils.create_sparql_prefix('ts') + \
                              utils.create_sparql_prefix('om') + \
                              utils.create_sparql_prefix('ohn') + \
                              utils.create_sparql_prefix('rdf') + \
                              '''SELECT ?dataIRI \
                              WHERE { ?s rdf:type ohn:GridConnection . \
                              ?grid ohn:hasDownstreamGridConnection ?s . \
                              ?grid rdf:type ohn:IncinerationPlant . \
                              ?s ohn:hasObservableProperty ?x . \
                              ?x rdf:type om:Temperature . \
                              ?x om:hasValue ?dataIRI . \
                              ?dataIRI ts:hasTimeSeries ?timeseries}'''
        response = KGClient.execute(query_MHKWTempRuecklauf)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('MHKWTempRuecklauf')


        return response_list, title_list


#TODO: Contains some similar queries to previous method, this needs to be refactored according to forecasting agent requirements
def query_forecasting_inputs():

        response_list = []
        title_list = []
        KGClient = jpsBaseLibView.RemoteStoreClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)

        # Aussentemperatur
        query_Aussentemperatur = utils.create_sparql_prefix('ts') + \
                         utils.create_sparql_prefix('om') + \
                         utils.create_sparql_prefix('ohn') + \
                         utils.create_sparql_prefix('rdf') + \
                         utils.create_sparql_prefix('oems') + \
                         '''SELECT ?dataIRI \
                         WHERE { ?s rdf:type oems:AirTemperature . \
                         ?s om:hasValue ?dataIRI . \
                         ?dataIRI ts:hasTimeSeries ?timeseries}'''
        response = KGClient.execute(query_Aussentemperatur)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('Aussentemperatur')

        #TODO: Update query to use "HeatDemand" concept
        # Waermeeinspeisung
        query_Waermeeinspeisung = utils.create_sparql_prefix('ts') + \
                                  utils.create_sparql_prefix('om') + \
                                  utils.create_sparql_prefix('ohn') + \
                                  '''SELECT ?dataIRI \
                                  WHERE { ?s ohn:hasHeatDemand ?o . \
                                  ?o om:hasValue ?dataIRI . \
                                  ?dataIRI ts:hasTimeSeries ?timeseries}'''
        response = KGClient.execute(query_Waermeeinspeisung)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('Waermeeinspeisung')

        # TempVorlauf
        query_TempVorlauf = utils.create_sparql_prefix('ts') + \
                            utils.create_sparql_prefix('om') + \
                            utils.create_sparql_prefix('ohn') + \
                            utils.create_sparql_prefix('rdf') + \
                            '''SELECT ?dataIRI \
                            WHERE { ?s rdf:type ohn:GridConnection . \
                            ?grid ohn:hasUpstreamGridConnection ?s . \
                            ?grid rdf:type ohn:MunicipalUtility . \
                            ?s ohn:hasObservableProperty ?x . \
                            ?x rdf:type om:Temperature . \
                            ?x om:hasValue ?dataIRI . \
                            ?dataIRI ts:hasTimeSeries ?timeseries}'''
        response = KGClient.execute(query_TempVorlauf)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('TempVorlauf')

        # TempRuecklauf
        query_TempRuecklauf = utils.create_sparql_prefix('ts') + \
                              utils.create_sparql_prefix('om') + \
                              utils.create_sparql_prefix('ohn') + \
                              utils.create_sparql_prefix('rdf') + \
                              '''SELECT ?dataIRI \
                              WHERE { ?s rdf:type ohn:GridConnection . \
                              ?grid ohn:hasDownstreamGridConnection ?s . \
                              ?grid rdf:type ohn:MunicipalUtility . \
                              ?s ohn:hasObservableProperty ?x . \
                              ?x rdf:type om:Temperature . \
                              ?x om:hasValue ?dataIRI . \
                              ?dataIRI ts:hasTimeSeries ?timeseries}'''
        response = KGClient.execute(query_TempRuecklauf)
        # Convert JSONArray String back to list
        response = json.loads(response)
        response_list.append(response)
        title_list.append('TempRuecklauf')

        return (response_list, title_list)



