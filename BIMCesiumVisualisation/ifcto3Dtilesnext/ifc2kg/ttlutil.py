# Standard library imports
import subprocess 
import pandas as pd

# Third party imports
from SPARQLWrapper import SPARQLWrapper, GET, JSON

def ifc2ttl(input_ttl, namespace):
    """
    Converts the ifc files in the ./data/ifc directory to the TTL format and upload it to a local blazegraph server

    Arguments:
     input_ttl - local file path for ttl file
     namespace - a namespace for the blazegraph database
    """
    # Convert all ifc files within this directory to TTL
    subprocess.run(["java", "-jar", "resources/IFCtoRDF-0.4-shaded.jar", "--dir", 'data/ifc/'])
    print("Conversion to TTL format completed...")

    # Conduct a bulk data load to the local blazegraph server 
    # Does not require server to be running but blazegraph.jar must exist
    subprocess.run(['java', '-Xmx6g', '-cp', 'resources\\blazegraph.jar' , 'com.bigdata.rdf.store.DataLoader', '-namespace',namespace, 'resources\\fastload.properties', input_ttl])
    print("TTL file uploaded to Blazegraph server...")

def querykg(endpoint, query):
    """
    Returns a dataframe that stores the SPARQL SELECT query results from the Blazegraph endpoint

    Arguments:
        endpoint - blazegraph server endpoint
        query - SPARQL query syntax
    """
    # Conduct the query on a Blazegraph database
    sparql = SPARQLWrapper(endpoint)
    sparql.setReturnFormat(JSON)
    sparql.setQuery(query)
    sparql.setMethod(GET)

    # Convert the query into a dataframe for easier manipulation
    try:
        ret = sparql.queryAndConvert()
        # Fill missing values as 0 for our usecase
        dataframe= pd.DataFrame(ret['results']['bindings']).fillna(0) 
    except Exception as e:
        print(e)

    # Extract the value from the nested dictionary returned and leave missing values as empty string
    for index in range(len(dataframe.columns)):
        dataframe.iloc[:, index]= dataframe.iloc[:, index].apply(lambda x: "" if x==0 else x['value'])
    
    return dataframe

def ifcquery(endpoint):
    """
    Queries the information on metadata stored in the original BIM file and geometry file path to generate a tileset
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

            SELECT  ?uid ?paramtitle ?hastype ?paramvalue ?name ?geomfile
            WHERE {
                    # Get all furnishing elements
                    ?furnishing rdf:type  ifc:IfcFurnishingElement ;
                            ifc:globalId_IfcRoot ?uidinst;
                            dcmi:source ?geomfile.

                    # Get their ids to link the properties to their asset
                    ?uidinst rdf:type ifc:IfcGloballyUniqueId ; 
                            express:hasString  ?uid .  
                    
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

                    # Get their properties if it exist in Pset_Asset
                    OPTIONAL{
                            ?propertyrelation ifc:relatedObjects_IfcRelDefines ?furnishing; 
                                    ifc:relatingPropertyDefinition_IfcRelDefinesByProperties ?pset_asset . 
                            ?pset_asset  ifc:name_IfcRoot ?label;  
                                    ifc:hasProperties_IfcPropertySet ?propertyvalue. 
                            ?label express:hasString "Pset_Asset" ; 
                                    rdf:type ifc:IfcLabel.         

                            # Get the values within this property set
                            ?propertyvalue rdf:type  ifc:IfcPropertySingleValue; 
                                    ifc:name_IfcProperty  ?assetidentifier ; 
                                    ifc:nominalValue_IfcPropertySingleValue ?nomvalue. 

                            # Get the parameter name
                            ?assetidentifier rdf:type  ifc:IfcIdentifier ; 
                                    express:hasString ?paramtitle. 

                            # Get the parameter value
                            ?nomvalue ?hastype ?paramvalue 
                            FILTER( !strstarts(str(?hastype), "http://www.w3.org/1999/02/22-rdf-syntax-ns") )
                    }

            } ORDER BY ?uid
            """
    dataframe = querykg(endpoint, query)
    return dataframe


