from forecasting.datamodel.iris import *
from forecasting.utils.tools import *
import pandas as pd
from darts import concatenate



def get_properties_for_subj(subj: str, verb_obj: dict = {}, verb_literal: dict = {}):
    """
         Constructs proper SPARQL update string from given name, verb-object and verb-literal dictionary.
         Arguments:
              subj: subject
              verb_obj:  object properties: dictionary of verb-object pairs, if value is a list, then it will be treated as multiple objects for the same verb
              verb_literal: data properties: dictionary of verb-literal pairs, if value is a list, then it will be treated as a literal with type
         Returns:
              SPARQL update string
    """
    output = f''''''
    #  object properties
    for verb, obj in verb_obj.items():
        # turn obj to list if it is not to iterate over it
        if not isinstance(obj, list):
            obj = [obj]
        for o in obj:
            output += f'''{create_triple(subj, verb, o)}'''

    # data properties
    for verb, literal in verb_literal.items():
        if not isinstance(literal, list):
            literal = [literal]
        if len(literal) == 1:
            literal.append(None)
        # check if len is max 2 for [literal, type]
        if len(literal) > 2:
            raise ValueError(
                f'Literal {literal} has more than two elements expected [literal, type] or [literal]')
        if not isinstance(literal[0], str):
            literal[0] = str(literal[0])
        # check that literal does not start with < and end with >
        if literal[0].startswith('<') and literal[0].endswith('>'):
            raise ValueError(f'Literal {literal} starts with < or ends with >')
        output += f'''{create_triple_literal(subj, verb, literal[0], literal[1])}'''

    return output

def num_instances_with_type():

    return f"""
     prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     SELECT ?Search (COUNT(?Search) AS ?Count)
     WHERE {{
          ?o rdf:type ?Search .
     }}
     GROUP BY ?Search
     """
def get_predecessor_type_by_predicate(dataIRI, predicate, kgClient):
    # get predecessor type of dataIRI
    query = f"""
    SELECT ?predecessorType
    WHERE {{
        ?o <{predicate}> <{dataIRI}> . 
        ?o <{RDF_TYPE}> ?predecessorType . 
        }}"""
    predecessor_type = kgClient.performQuery(query)[0]["predecessorType"]
    return predecessor_type


def get_ts_value_iri(dataIRI, kgClient):
    # get ts value iri of dataIRI
    query = f"""
    SELECT ?tsValueIRI
    WHERE {{
        <{dataIRI}> <{OM_HASVALUE}> ?tsValueIRI . 
        }}"""
    ts_value_iri = kgClient.performQuery(query)[0]["tsValueIRI"]
    return ts_value_iri

def get_df_for_heat_supply(data_type, kgClient, tsClient):
    cov_iris = []
    heat_supply_ts_iri = get_ts_value_iri(data_type, kgClient)
    heat_supply_dates, heat_supply_ts = get_ts_data( heat_supply_ts_iri, tsClient)
    df_heat_supply = pd.DataFrame(zip(heat_supply_ts, heat_supply_dates), columns=["ForecastColumn", "Date"])

    # find ts by specific data iri. This just works for the data iri that is used and unique.
    ts_by_type = kgClient.performQuery(get_ts_by_type())
    for row in ts_by_type:
        if 'type' in row and row['type'] == ONTOEMS_AIRTEMPERATURE:
            data_iri = row['tsIRI']
            cov_iris.append(data_iri)
            air_temp_dates, air_temp = get_ts_data(data_iri, tsClient)
            df_air_temp = pd.DataFrame(zip(air_temp, air_temp_dates), columns=["AirTemp", "Date"])
        elif 'type2' in row and  row['type2'] == OHN_ISPUBLICHOLIDAY:
            data_iri = row['tsIRI']
            cov_iris.append(data_iri)
            public_holiday_dates, public_holiday = get_ts_data(data_iri, tsClient)
            df_public_holiday = pd.DataFrame(zip(public_holiday, public_holiday_dates), columns=["isHoliday", "Date"])
            

    # merge vacation, public holidays air temp and heat supply
    #df = pd.merge(df_vacation, df_public_holidays, on="Date", how="outer")
    df = pd.merge(df_public_holiday, df_air_temp, on="Date", how="outer")
    df = pd.merge(df, df_heat_supply, on="Date", how="outer")
    df = df.sort_values(by="Date")
    df.Date = pd.to_datetime(df.Date).dt.tz_localize(None)
    #df = df.set_index("Date")
    covariates = concatenate(
        [
            get_data_cov(df, "AirTemp"),
            get_time_cov(df, {"dayofyear": True, "dayofweek": True, "hour": True}),
            get_data_cov(df, "isHoliday"),
        ],
        axis="component",
    ) 
    return df, cov_iris, covariates

def get_unit(dataIRI, kgClient):
    # get unit of dataIRI
    query = f"""
    SELECT ?unit
    WHERE {{
        <{dataIRI}> <{OM_HASVALUE}> ?measure . 
        ?measure <{OM_HASUNIT}> ?unit . 
        }}"""
    unit = kgClient.performQuery(query)[0]["unit"]
    return unit

def get_time_format(dataIRI, kgClient):
    # get unit of dataIRI
    query = f"""
    SELECT ?format
    WHERE {{
        <{dataIRI}> <{OM_HASVALUE}> ?measure . 
        ?measure <{TS_HASTIMESERIES}> ?ts . 
        ?ts <{TS_HASTIMEUNIT}> ?format . 
        }}"""
    time_format = kgClient.performQuery(query)[0]["format"]
    return time_format



def get_ts_of_predicate_by_label(label, predicate):
    """
    Returns a SPARQL query that counts the total number of predicates, grouped by predicates.

    :return: the SPARQL query
    """
    return f"""
     prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
     prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>      prefix om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>     
     SELECT  ?genIRI ?dataIRI ?tsIRI 
     WHERE {{        
  		            
          ?genIRI rdfs:label '{label}' . 
          ?genIRI <{predicate}> ?dataIRI . 
          ?dataIRI <{OM_HASVALUE}> ?tsIRI
     }}      
     """


def get_ts_data(dataIRI, ts_client):
     """
     It takes a data IRI and a client object, and returns the dates and values of the time series

     :param dataIRI: The IRI of the data you want to get
     :param ts_client: a TimeSeriesClient object
     :return: A tuple of two lists. The first list is a list of dates, the second list is a list of
     values.
     """
     ts = ts_client.tsclient.getTimeSeries([dataIRI], ts_client.conn)
     dates = ts.getTimes()
     # Unwrap Java time objects
     dates = [d.toString() for d in dates]
     values = ts.getValues(dataIRI)
     return dates, values


def num_instance(p=False, o=False, s=False):
     """
     It takes three boolean arguments, and returns a SPARQL query that counts the number of instances of
     the first argument that is True

     :param p: True if you want to count the number of instances of each predicate, defaults to False
     (optional)
     :param o: If True, count the number of instances of each object, defaults to False (optional)
     :param s: subject, defaults to False (optional)
     :return: The number of instances of each object, predicate, or subject.
     """

     assert p or o or s, "At least one of p, o, s must be True"
     to_count = "p" if p else "o" if o else "s"

     return f"""SELECT (?{to_count} as ?Search) (COUNT(?{to_count}) as ?Count)
          WHERE
               {{
                    ?s ?p ?o .
               }}
          GROUP BY ?{to_count}
     """


def num_total_instances_with_type():
    """
    Returns a SPARQL query that counts the total number of instances.

    :return: the SPARQL query
    """
    return f"""
     prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     SELECT (COUNT(distinct  ?object) AS ?num_object) 
     WHERE {{
          ?object rdf:type ?type .
     }}
     """


def get_time_series_with_measure():
    """     
    :return: the SPARQL query
    """
    return f"""
     prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     SELECT  ?parent2 ?parent ?hasTS
     WHERE {{
          ?parent2 ?v2 ?parent . 
          ?parent ?v ?hasTS . 
          ?hasTS <{TS_HASTIMESERIES}> ?ts .
          FILTER EXISTS {{ ?hasTS rdf:type <{OM_MEASURE}>}}
     }}
     order by ?parent2 ?parent ?hasTS
     """


def get_all_ts():
    """     
    :return: the SPARQL query
    """
    return f"""
     prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     SELECT  ?parent ?hasTS
     WHERE {{
          ?parent ?v ?hasTS . 
          ?hasTS <{TS_HASTIMESERIES}> ?ts .
          
     }}
     order by  ?parent ?hasTS
     """


def get_time_series_without_measure():
     """
     It returns a list of all the time series that are not measures.
     :return: The query returns the parent and the hasTimeSeries of the parent.
     """

     return f"""
     prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     SELECT  ?parent ?hasTS
     WHERE {{

          ?parent ?v ?hasTS . 
          ?hasTS <{TS_HASTIMESERIES}> ?ts .
          FILTER NOT EXISTS  {{ ?hasTS rdf:type <{OM_MEASURE}>}}
     }}
     order by ?parent ?hasTS
     """


def get_ts_by_type():
     """
     It returns a SPARQL query that will return all the dataIRIs and tsIRIs in the graph, along with the
     type of the dataIRI.
     
     It distinguishes between measures and non-measures. 
     The type of Timeseries with measures is returned under the key 'type' and those without measures are returned under the key 'type2'.
     """

     return f"""
     prefix rdf: <{RDF}>
     prefix om: <{OM}>

     prefix ts: <{TS}>

     SELECT  distinct ?dataIRI ?tsIRI ?type ?type2
     WHERE {{
      
       ?tsIRI ts:hasTimeSeries ?ts . 
       ?tsIRI rdf:type ?type2 .
       OPTIONAL {{ ?dataIRI om:hasValue ?tsIRI . ?dataIRI rdf:type ?type }} . 
       
     }}
     """
