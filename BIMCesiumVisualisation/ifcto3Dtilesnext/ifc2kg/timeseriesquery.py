# Standard library imports
import pandas as pd
import re

# Third party imports
from SPARQLWrapper import SPARQLWrapper, POST, GET, JSON

# Reader imports
import utils
from ifc2kg.jpsSingletons import jpsBaseLibView
from ifc2kg.ttlutil import querykg

def timeseriesquery(endpoint, ifcdataframe):
    """
    Search for assets with additional data sources. If any is found, query their time series
    and other data on the new endpoints.
    Refactor the code once the triples are loaded on the same endpoint
    - Simplify the queries and endpoints
    
    Argument:
     endpoint - blazegraph server endpoint
     ifcdataframe - dataframe containing query results from the ifc file in ifcquery()
    Returns a dataframe in the same format as ifcquery() for tileset conversion
    """
    query = """
        PREFIX ifc:  <http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#>
        PREFIX inst:  <http://linkedbuildingdata.net/ifc/resources20220519_162748/>
        PREFIX list:  <https://w3id.org/list#>
        PREFIX express:  <https://w3id.org/express#>
        PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX xsd:  <http://www.w3.org/2001/XMLSchema#>
        PREFIX owl:  <http://www.w3.org/2002/07/owl#>
        PREFIX dcmi: <http://purl.org/dc/terms/>
        
        SELECT DISTINCT ?uid ?name ?geomfile ?asset
        WHERE {
            # Get all furnishing elements
            ?furnishing rdf:type ifc:IfcFurnishingElement;
                owl:sameAs ?asset;
                ifc:globalId_IfcRoot ?uidinst;
                dcmi:source ?geomfile.

            # Get their ids to link the properties to their asset
            ?uidinst rdf:type ifc:IfcGloballyUniqueId ; 
                express:hasString ?uid .  
                
            # Get their names from the related Property Set "Pset_QuantityTakeOff"
            ?reldefinebyproperty ifc:relatedObjects_IfcRelDefines ?furnishing;
                ifc:relatingPropertyDefinition_IfcRelDefinesByProperties ?pset.
            ?pset ifc:hasProperties_IfcPropertySet ?property;
                ifc:name_IfcRoot ?label_qty .
            ?label_qty express:hasString "Pset_QuantityTakeOff" ; 
                rdf:type ifc:IfcLabel.       
            ?property a ifc:IfcPropertySingleValue; 
                ifc:nominalValue_IfcPropertySingleValue ?identifier.
            ?identifier a ifc:IfcIdentifier ; 
                express:hasString ?name.
        } ORDER BY ?uid
        """
    # Should only have one row/ additional source per asset
    dataframe = querykg(endpoint, query)

    if not dataframe.empty:
        # WIP consolidate the different knowledge graph endpoints
        fridge_endpoint="http://171.0.148.231:9999/blazegraph/namespace/ArduinoSmartMeterForFridge01/sparql"
        
        # Create a new dataframe to store multiple parameters for same asset
        property_dataframe= pd.DataFrame({'uid': pd.Series(dtype='str'),
            'name': pd.Series(dtype='str'),
            'geomfile': pd.Series(dtype='str'),
            'paramtitle': pd.Series(dtype='str'),
            'hastype': pd.Series(dtype='str'),
            'paramvalue': pd.Series(dtype='str')})
        
        for row in dataframe.index:
            # A template list containing static values for some columns
            rowtemplate=[dataframe["uid"][row], dataframe["name"][row], dataframe["geomfile"][row]]

            # Miscellaneous query for properties
            misc_query = """
                PREFIX saref:  <https://saref.etsi.org/core/>
                SELECT ?paramvalue 
                WHERE {
                    # Get all properties measured by a device of the Fridge
                    <""" +dataframe['asset'][row] + """> saref:hasManufacturer ?paramvalue. 
                }
                """
            misc_df = querykg(fridge_endpoint, misc_query)
            newrow= list(rowtemplate)
            newrow.append("Manufacturer")
            newrow.append("String")
            newrow.append(str(misc_df["paramvalue"][0]))
            property_dataframe.loc[len(property_dataframe.index)]=newrow
            
            # Query for time series data
            timeseries_query = """
                PREFIX saref:  <https://saref.etsi.org/core/>
                PREFIX units:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>
                SELECT ?property ?datairi
                WHERE {
                    # Get all properties measured by a device of the Fridge
                    <""" +dataframe['asset'][row] + """> saref:hasProperty ?property. 
                    
                    ?property <https://saref.etsi.org/core/isMeasuredByDevice> ?device; 
                        units:hasValue ?datairi. 
                }
                """
            ts_df = querykg(fridge_endpoint, timeseries_query)
            
            # Retrieve Java's Instant class to initialise TimeSeriesClient
            Instant = jpsBaseLibView.java.time.Instant
            instant_class = Instant.now().getClass()
            # Initialise TimeSeriesClient
            tsclient = jpsBaseLibView.TimeSeriesClient(instant_class, utils.TSCLIENT_FILE)
            
            for rowts in ts_df.index:
                # Split String to get parameter name
                paramtitle=ts_df['property'][rowts].split(dataframe["asset"][row]).pop(1)
                # Add space before each capital
                paramtitle=re.sub(r"(?<![A-Z])(?<!^)([A-Z])",r" \1",paramtitle)
                
                # Retrieve time series value for parameter
                timeseries = tsclient.getLatestData(ts_df['datairi'][rowts])
                paramvalue = timeseries.getValuesAsInteger(ts_df['datairi'][rowts])           
                # Convert Java List into Python List
                paramvalue = list(paramvalue)

                # Add parameters as a new row
                newrow= list(rowtemplate)
                newrow.append(paramtitle)
                newrow.append("Integer")
                # Convert integer to string as a design choice for tileset conversion
                newrow.append(str(paramvalue[0]))
                property_dataframe.loc[len(property_dataframe.index)]=newrow
    
    # Disconnect from the PostgreSQL server
    tsclient.disconnectRDB()
    ifcdataframe=pd.concat([ifcdataframe,property_dataframe],ignore_index=True)
    return ifcdataframe