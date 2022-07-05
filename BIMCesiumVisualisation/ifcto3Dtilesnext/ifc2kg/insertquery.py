# Standard library imports
import pandas as pd

# Third party imports
from SPARQLWrapper import SPARQLWrapper, POST

# Reader imports
from ifc2kg.ttlutil import querykg

def gengltftriples(endpoint, hashtable):
    """
    Generate a dataframe containing the triple linking the asset instance
    to their glTF file path

     Arguments:
        endpoint - blazegraph server endpoint
        hashtable - a hashtable to match assets to their IFC ID
    """
    # Query the instances and uid to facilitate assignment later
    query = """
        PREFIX ifc:  <http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#>
        PREFIX express:  <https://w3id.org/express#>
        PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT DISTINCT ?inst ?uid
        WHERE {
            # Get all furnishing elements
            ?inst rdf:type  ifc:IfcFurnishingElement ;
                ifc:globalId_IfcRoot ?uidinst. 
                
            # Get their ids to link the properties to their asset
            ?uidinst rdf:type ifc:IfcGloballyUniqueId ;
                express:hasString ?uid. 
        }
        """
    dataframe= querykg(endpoint, query)

    # Adding the predicate and geometry file source to the dataframe
    dataframe['predicate']=""
    dataframe['object']=""
    for row in dataframe.index:
        #A good generic predicate for all file sources is https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#source
        dataframe['predicate'][row]="http://purl.org/dc/terms/source"
        # Extract file name from the hashtable matching the uid
        dataframe['object'][row]="'./gltf/"+hashtable.get(dataframe['uid'][row]) +".gltf'"
    
    return dataframe

def gen_sameas_triples(endpoint):
    """
    Generate a dataframe containing the triple linking the asset instance
    to their equivalent instances in other ontologies

     Arguments:
        endpoint - blazegraph server endpoint
    """
    # Query for instances if they have additional data sources
    query = """
        PREFIX ifc:  <http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#>
        PREFIX express:  <https://w3id.org/express#>
        PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

        SELECT ?inst ?name
        WHERE {
            # Get all furnishing elements
            ?inst rdf:type  ifc:IfcFurnishingElement;
                ifc:name_IfcRoot  ?labelname.
            
            # Get the furniture name
            ?labelname rdf:type ifc:IfcLabel ; 
                express:hasString ?name.
                    
            # Check if the element has additional data sources 
            # Search for the parameter "HasAdditionalDataSource" in Pset_Asset
            ?propertyrelation ifc:relatedObjects_IfcRelDefines ?inst; 
                ifc:relatingPropertyDefinition_IfcRelDefinesByProperties ?pset_asset . 
            ?pset_asset  ifc:name_IfcRoot ?label;  
                ifc:hasProperties_IfcPropertySet ?propertyvalue. 
            ?label express:hasString "Pset_Asset" ; 
                rdf:type ifc:IfcLabel.         

            # To get parameter
            ?propertyvalue rdf:type  ifc:IfcPropertySingleValue; 
                ifc:name_IfcProperty  ?assetidentifier ; 
                ifc:nominalValue_IfcPropertySingleValue ?nomvalue. 
            ?assetidentifier rdf:type  ifc:IfcIdentifier ; 
                express:hasString "HasAdditionalDataSource". 
            # To get parameter's value
            ?nomvalue express:hasString "Yes" 

        } ORDER BY ?uid
            """
    dataframe= querykg(endpoint, query)
    
    if not dataframe.empty:
        # Adding the predicate and geometry file source to the dataframe
        dataframe['predicate']=""
        dataframe['object']=""
        for row in dataframe.index:
            # OWL property to link equivalent instances
            dataframe['predicate'][row]="http://www.w3.org/2002/07/owl#sameAs"
            
            # WIP to link each asset to their equivalent instance on TWA server
            if "Fridge" in dataframe['name'][row]:
                dataframe['object'][row]="<http://www.theworldavatar.com/devices/Fridge-01>"
        
            # Drop rows if they have no equivalent instance ie object
            dataframe['object'].replace("", float("NaN"), inplace=True)
            dataframe.dropna(subset = ["object"], inplace=True)
            # Cleaning the dataframe of excess column
            dataframe.drop(columns='name')

        return dataframe
    else:
        return dataframe

def insertkg(endpoint, hashtable):
    """
    Adds required triples for glTF file path and equivalent nodes 
    into the Blazegraph namespace containing the IFC file

     Arguments:
        endpoint - blazegraph server endpoint
        hashtable - a hashtable to match assets to their IFC ID
    """
    # Construct the required subject, predicate, object in one dataframe
    gltfdf = gengltftriples(endpoint, hashtable)
    nodedf= gen_sameas_triples(endpoint)
    frames=[gltfdf,nodedf]
    dataframe = pd.concat(frames,ignore_index=True)

    # Constructing the query for inserting data
    insertquery="INSERT DATA {"
    for row in dataframe.index:
        insertquery=insertquery+"<"+dataframe['inst'][row]+"> <"+dataframe['predicate'][row]+"> "+dataframe['object'][row]+". "
    
    insertquery=insertquery+"}"
    
    # Insert the triples using POST method
    sparql = SPARQLWrapper(endpoint)
    sparql.setQuery(insertquery)
    sparql.setMethod(POST)
    results = sparql.query()
    print("Triples for geometry file sources and equivalent nodes have been added...")
