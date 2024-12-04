################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 17 May 2023                            #
################################################

# The purpose of this module is to provide functionality to execute KG queries
# and updates using the PySparqlClient module from pyderivationagent

from rdflib import URIRef, Literal
import uuid

from py4jps import agentlogging
from pyderivationagent.kg_operations import PySparqlClient

from copcalculationagent.datamodel.iris import *
from copcalculationagent.utils.env_configs import HEATPUMP_EFFICIENCY, HOTSIDE_TEMPERATURE
from copcalculationagent.errorhandling.exceptions import *
from copcalculationagent.kg_operations.tsclient import TSClient

# Initialise logger instance (ensure consistent logger level with `entrypoint.py`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    #
    # SPARQL QUERIES
    #
    def get_country_iri(self):
        # check if there is country exist
        query_string = f'''
        SELECT ?country
        {{
        ?country <{RDF_TYPE}> <{ONTOCAPE_COUNTRY}>.
        }}
        '''
        res = self.performQuery(query_string)

        if not res:
            logger.error("No existed country iri, please go to run 'upper_level_ontology_update.py'")
            raise InvalidInput("No existed country iri, please go to run 'upper_level_ontology_update.py'")
        else: 
            res = res[0]
            country_iri = str(res["country"])
            logger.info(f'country iri: {country_iri} has been identified')
        
        return country_iri

    def get_assumption_iri(self, country_iri):
        query_string = f"""
        SELECT ?assumption_iri
        WHERE {{
        ?assumption_iri <{REGION_APPLICABLETO}> <{country_iri}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            assumption_iri = REGION + "Assumption_" + str(uuid.uuid4())
            logger.info(f'No existed assumption_iri, created {assumption_iri}')
        else: 
            res = res[0]
            assumption_iri = str(res["assumption_iri"])
            logger.info(f'assumption_iri: {assumption_iri} will be used')
        
        return assumption_iri
    
    def get_heatpumpefficiency_iri(self, assumption_iri):
        query_string = f"""
        SELECT ?heatpumpefficiency_iri
        WHERE {{
        ?heatpumpefficiency_iri <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_HEATPUMP_EFFICIENCY}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            heatpumpefficiency_iri = REGION + "HeatPumpEfficiency_" + str(uuid.uuid4())
            logger.info(f'No existed heatpumpefficiency_iri, created {heatpumpefficiency_iri}')
        else: 
            res = res[0]
            heatpumpefficiency_iri = str(res["heatpumpefficiency_iri"])
            logger.info(f'heatpumpefficiency_iri: {heatpumpefficiency_iri} will be used')

            # Remove originial triples
            query_string = f"""
            DELETE
            WHERE {{
            <{heatpumpefficiency_iri}> ?p ?o .
            }}
            """
            query_string = self.remove_unnecessary_whitespace(query_string)
            self.performUpdate(query_string)
        
        return heatpumpefficiency_iri
    
    def get_hotsidetemperature_iri(self, assumption_iri):
        query_string = f"""
        SELECT ?hotsidetemperature_iri
        WHERE {{
        ?hotsidetemperature_iri <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_HOTSIDE_TEMPERATURE}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            hotsidetemperature_iri = REGION + "HotSideTemperature_" + str(uuid.uuid4())
            logger.info(f'No existed hotsidetemperature_iri, created {hotsidetemperature_iri}')
        else: 
            res = res[0]
            hotsidetemperature_iri = str(res["hotsidetemperature_iri"])
            logger.info(f'hotsidetemperature_iri: {hotsidetemperature_iri} will be used')
        
            # Remove originial triples
            query_string = f"""
            DELETE
            WHERE {{
            <{hotsidetemperature_iri}> ?p ?o .
            }}
            """
            query_string = self.remove_unnecessary_whitespace(query_string)
            self.performUpdate(query_string)

        return hotsidetemperature_iri
    
    def update_assumptions(self):
        country_iri = self.get_country_iri()
        assumption_iri = self.get_assumption_iri(country_iri)
        heatpumpefficiency_iri = self.get_heatpumpefficiency_iri(assumption_iri)
        hotsidetemperature_iri = self.get_hotsidetemperature_iri(assumption_iri)
        query_string = f"""
        INSERT DATA {{
        <{assumption_iri}> <{REGION_APPLICABLETO}> <{country_iri}> .
        <{heatpumpefficiency_iri}> <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_HEATPUMP_EFFICIENCY}> ;
                <{OM_HAS_NUMERICALVALUE}> "{HEATPUMP_EFFICIENCY}"^^<{XSD_FLOAT}> .
        <{hotsidetemperature_iri}> <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_HOTSIDE_TEMPERATURE}> ;
            <{OM_HAS_NUMERICALVALUE}> "{HOTSIDE_TEMPERATURE}"^^<{XSD_FLOAT}> .
        }}
        """
        query_string = self.remove_unnecessary_whitespace(query_string)
        self.performUpdate(query_string)

    def get_temperature(self, temperature_iri):
        # By using temperature_iri (from mean temperature) to retieve region & month
        query_string = f"""
        SELECT ?region ?start ?end ?meantemperature
        WHERE {{?region <{CLIMB_HASMEASURE}>  <{temperature_iri}> .
                <{temperature_iri}> <{COMP_HAS_STARTUTC}> ?start;
                    <{COMP_HAS_ENDUTC}> ?end ;
                    <{CLIMB_HASVAR}> "{CLIMA_TAS}"^^<{XSD_STRING}> ;
                    <{OM_HAS_NUMERICALVALUE}> ?meantemperature . }}
        """
        query_string = self.remove_unnecessary_whitespace(query_string)
        res = self.performQuery(query_string)
        
        if not res:
            # In case some var are missing (i.e. empty SPARQL result), return Nones
            res = dict(zip(['region', 'start', 'end', 'meantemperature','maxtemperature','mintemperature'], (None,)*6))
        else:
            res = res[0]
            try:
                res['region'] = str(res['region'])
            except:
                res['region'] = None

            try:
                res['meantemperature'] = float(res['meantemperature'])
            except:
                res['meantemperature'] = None
        
        # By using the region/month to retrieve the max temperature
        query_string = f"""
            SELECT ?maxtemperature
            WHERE {{<{res['region']}> <{CLIMB_HASMEASURE}>  ?m.
                    ?m <{COMP_HAS_STARTUTC}> "{res['start']}"^^<{XSD_DATETIME}>;
                        <{COMP_HAS_ENDUTC}> "{res['end']}"^^<{XSD_DATETIME}>;
                        <{CLIMB_HASVAR}> "{CLIMA_TASMAX}"^^<{XSD_STRING}> ;
                        <{OM_HAS_NUMERICALVALUE}> ?maxtemperature .}}
            """
        
        query_string = self.remove_unnecessary_whitespace(query_string)
        res_max = self.performQuery(query_string)
        
        if not res_max:
            # In case date or price (or both) are missing (i.e. empty SPARQL result), return Nones
            res_max = dict(zip(['maxtemperature'], (None,)))
        else:
            res_max = res_max[0]
            try:
                res['maxtemperature'] = float(res_max['maxtemperature'])
            except:
                res['maxtemperature'] = None

        # By using the region/month to retrieve the min temperature
        query_string = f"""
            SELECT ?mintemperature
            WHERE {{<{res['region']}> <{CLIMB_HASMEASURE}>  ?m.
                    ?m <{COMP_HAS_STARTUTC}> "{res['start']}"^^<{XSD_DATETIME}>;
                        <{COMP_HAS_ENDUTC}> "{res['end']}"^^<{XSD_DATETIME}>;
                        <{CLIMB_HASVAR}> "{CLIMA_TASMIN}"^^<{XSD_STRING}> ;
                        <{OM_HAS_NUMERICALVALUE}> ?mintemperature .}}
            """
        
        query_string = self.remove_unnecessary_whitespace(query_string)
        res_min = self.performQuery(query_string)
        
        if not res_min:
            # In case date or price (or both) are missing (i.e. empty SPARQL result), return Nones
            res_min = dict(zip(['mintemperature'], (None,)))
        else:
            res_min = res_min[0]
            try:
                res['mintemperature'] = float(res_min['mintemperature'])
            except:
                res['mintemperature'] = None

        return res
    
    def get_heatpumpefficiency(self, heatpumpefficiency_iri):
        query_string = f"""
        SELECT ?heatpumpefficiency
        WHERE {{
        <{heatpumpefficiency_iri}> <{IS_A}> ?assumption_iri ;
                    <{RDF_TYPE}> <{REGION_HEATPUMP_EFFICIENCY}> ;
                    <{OM_HAS_NUMERICALVALUE}>  ?heatpumpefficiency .
        }}
        """
        res = self.performQuery(query_string)
        if not res:
            logger.error("heatpumpefficiency result can not be found -- go check if it exist")
            raise InvalidInput("heatpumpefficiency result can not be found -- go check if it exist")
        else:
            res = res[0]
            try:
                heatpumpefficiency = float(res['heatpumpefficiency'])
            except:
                heatpumpefficiency = None

        return heatpumpefficiency

    def get_hotsidetemperature(self,hotsidetemperature_iri):
        query_string = f"""
        SELECT ?hotsidetemperature
        WHERE {{
        <{hotsidetemperature_iri}> <{IS_A}> ?assumption_iri ;
                    <{RDF_TYPE}> <{REGION_HOTSIDE_TEMPERATURE}> ;
            <{OM_HAS_NUMERICALVALUE}> ?hotsidetemperature .
        }}
        """
        res = self.performQuery(query_string)
        if not res:
            logger.error("hotsidetemperature result can not be found -- go check if it exist")
            raise InvalidInput("hotsidetemperature result can not be found -- go check if it exist")
        else:
            res = res[0]
            try:
                hotsidetemperature = float(res['hotsidetemperature'])
            except:
                hotsidetemperature = None

        return hotsidetemperature

    def verify_cop_iri(self, region):

        query_string = f"""
        SELECT ?mean_cop_iri ?min_cop_iri ?max_cop_iri
        WHERE {{
        <{region}> <{REGION_HASCOP}> ?mean_cop_iri.
        ?mean_cop_iri  <{RDF_TYPE}> <{REGION_COP}> ;
                   <{CLIMB_HASVAR}> "{CLIMA_TAS}".
        <{region}> <{REGION_HASCOP}> ?min_cop_iri.
        ?min_cop_iri  <{RDF_TYPE}> <{REGION_COP}> ;
                   <{CLIMB_HASVAR}> "{CLIMA_TASMIN}" .
        <{region}> <{REGION_HASCOP}> ?max_cop_iri.
        ?max_cop_iri  <{RDF_TYPE}> <{REGION_COP}> ;
                   <{CLIMB_HASVAR}> "{CLIMA_TASMAX}" .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            mean_cop_iri = REGION + "COPmean_" + str(uuid.uuid4())
            logger.info(f'No existed mean_cop_iri, created {mean_cop_iri}')
            max_cop_iri = REGION + "COPmax_" + str(uuid.uuid4())
            logger.info(f'No existed max_cop_iri, created {max_cop_iri}')
            min_cop_iri = REGION + "COPmin_" + str(uuid.uuid4())
            logger.info(f'No existed min_cop_iri, created {min_cop_iri}')
            deletion = False

        else: 
            res = res[0]
            mean_cop_iri = str(res["mean_cop_iri"])
            logger.info(f'mean_cop_iri: {mean_cop_iri} will be reused')
            max_cop_iri = str(res["max_cop_iri"])
            logger.info(f'max_cop_iri: {max_cop_iri} will be reused')
            min_cop_iri = str(res["min_cop_iri"])
            logger.info(f'min_cop_iri: {min_cop_iri} will be reused')
            deletion = True

        return mean_cop_iri, max_cop_iri, min_cop_iri, deletion

    def clear_cop_iris_triples(self,mean_cop_iri, max_cop_iri, min_cop_iri):
            
            # Remove originial triples
            query_string = f"""
            DELETE
            WHERE {{
            <{mean_cop_iri}> ?p ?o .
            }}
            """
            query_string = self.remove_unnecessary_whitespace(query_string)
            self.performUpdate(query_string)

            # Remove originial triples
            query_string = f"""
            DELETE
            WHERE {{
            ?s ?p <{mean_cop_iri}> .
            }}
            """
            query_string = self.remove_unnecessary_whitespace(query_string)
            self.performUpdate(query_string)


            # Remove originial triples
            query_string = f"""
            DELETE
            WHERE {{
            <{max_cop_iri}> ?p ?o .
            }}
            """
            query_string = self.remove_unnecessary_whitespace(query_string)
            self.performUpdate(query_string)

            # Remove originial triples
            query_string = f"""
            DELETE
            WHERE {{
            ?s ?p <{max_cop_iri}> .
            }}
            """
            query_string = self.remove_unnecessary_whitespace(query_string)
            self.performUpdate(query_string)


            # Remove originial triples
            query_string = f"""
            DELETE
            WHERE {{
            <{min_cop_iri}> ?p ?o .
            }}
            """
            query_string = self.remove_unnecessary_whitespace(query_string)
            self.performUpdate(query_string)

            # Remove originial triples
            query_string = f"""
            DELETE
            WHERE {{
            ?s ?p <{min_cop_iri}> .
            }}
            """
            query_string = self.remove_unnecessary_whitespace(query_string)
            self.performUpdate(query_string)

    def instantiate_COP(self, g, mean_cop_iri, max_cop_iri, min_cop_iri, region):
        g.add((URIRef(region),URIRef(REGION_HASCOP),URIRef(mean_cop_iri)))
        g.add((URIRef(mean_cop_iri),URIRef(RDF_TYPE),URIRef(REGION_COP)))
        g.add((URIRef(mean_cop_iri),URIRef(CLIMB_HASVAR),Literal(CLIMA_TAS, datatype=XSD_STRING)))
        g.add((URIRef(region),URIRef(REGION_HASCOP),URIRef(max_cop_iri)))
        g.add((URIRef(max_cop_iri),URIRef(RDF_TYPE),URIRef(REGION_COP)))
        g.add((URIRef(max_cop_iri),URIRef(CLIMB_HASVAR),Literal(CLIMA_TASMAX, datatype=XSD_STRING)))
        g.add((URIRef(region),URIRef(REGION_HASCOP),URIRef(min_cop_iri)))
        g.add((URIRef(min_cop_iri),URIRef(RDF_TYPE),URIRef(REGION_COP)))
        g.add((URIRef(min_cop_iri),URIRef(CLIMB_HASVAR),Literal(CLIMA_TASMIN, datatype=XSD_STRING)))
        return g

    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

        return query

    def retrieve_temperature_iri(self, region_iri):
            
            query_string = f"""
            SELECT DISTINCT ?temperature_iri ?start
            WHERE {{<{region_iri}> <{CLIMB_HASMEASURE}>  ?temperature_iri.
                    ?temperature_iri <{COMP_HAS_STARTUTC}> ?start;
                        <{COMP_HAS_ENDUTC}> ?end ;
                        <{CLIMB_HASVAR}> "{CLIMA_TAS}"^^<{XSD_STRING}> ;
                        <{OM_HAS_NUMERICALVALUE}> ?meantemperature. }}
            """

            res = self.performQuery(query_string)
            
            if not res:
                raise IndexError('No temperature_iri found -- Are you sure you are using the correct namespace?')
            else:
                temperature_iri_list = [d['temperature_iri'] for d in res]

                return temperature_iri_list
