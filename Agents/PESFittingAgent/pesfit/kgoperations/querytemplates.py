def ontocompchem_data_query(ocIRI):
    query = """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX oc:  <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
        PREFIX gc:  <http://purl.org/gc/>

        SELECT ?elem ?XCoordValue ?YCoordValue ?ZCoordValue ?ScfElecValue ?ScfElecUnit
        WHERE {
            <#ocIRI#> a oc:G09;
                gc:isCalculationOn ?geomOpt ;
                gc:isCalculationOn ?ScfEnergy .
            
  
            ?ScfEnergy a oc:ScfEnergy ;
                gc:hasElectronicEnergy ?ScfElecEn .
  
            ?ScfElecEn gc:hasValue ?ScfElecValue ;
                 gc:hasUnit ?ScfElecUnit .
                 
  
  	        ?geomOpt a gc:GeometryOptimization ;
                 gc:hasMolecule ?mol .
  
  	        ?mol gc:hasAtom ?atom .
            ?atom gc:isElement ?elem;
                gc:hasAtomCoordinateX ?coordX;
                gc:hasAtomCoordinateY ?coordY;
                gc:hasAtomCoordinateZ ?coordZ.
  
  	        ?coordX gc:hasValue ?XCoordValue.
  	        ?coordY gc:hasValue ?YCoordValue.
  	        ?coordZ gc:hasValue ?ZCoordValue.
        }""".replace('#ocIRI#',ocIRI)
    return query

def ontopesscan_data_query(opesIRI):
    query = """PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX ops: <http://www.theworldavatar.com/ontology/ontopesscan/OntoPESScan.owl#>
        PREFIX gc:  <http://purl.org/gc/>

        SELECT ?oc_IRIs ?scan_coord_value ?scan_coord_unit
        WHERE {
            <#opesIRI#> a ops:PotentialEnergySurfaceScan ;
                ops:hasScanPoint ?ScanPoints .
        
            ?ScanPoints a ops:ScanPoint ;
                ops:hasCalculation ?oc_IRIs ;
                ops:hasScanCoordinateValue ?Scan_Value .
        
            ?Scan_Value a ops:ScanCoordinateValue ;
                gc:hasValue ?scan_coord_value ;
                gc:hasUnit ?scan_coord_unit .                                                                                                                       
        }""".replace('#opesIRI#',opesIRI)
    return query