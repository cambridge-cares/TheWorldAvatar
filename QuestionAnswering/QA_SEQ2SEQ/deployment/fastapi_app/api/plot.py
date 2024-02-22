# services/sparql_service.py
from SPARQLWrapper import SPARQLWrapper, JSON
from typing import List, Dict, Any
import os

class SparqlService:
    def __init__(self, endpoint_url: str):
        self.endpoint_url = endpoint_url

    def execute_query(self, query: str) -> List[Dict[str, Any]]:
        endpoint = SPARQLWrapper(self.endpoint_url)
        BG_USER = os.getenv("BG_USER")
        BG_PASSWORD = os.getenv("BG_PASSWORD")
        endpoint.setCredentials("bg_user", "admin")  # Adjust credentials as needed
        endpoint.setQuery(query)
        endpoint.setReturnFormat(JSON)
        results = endpoint.query().convert()
        return results["results"]["bindings"]

    def plot(self, x_axis: str, y_axis: str, c_axis: str = None, chemical_class: str = None) -> List[Dict[str, Any]]:

        if 'ontozeolite' in self.endpoint_url:
        # Define the base of your SPARQL query
            query=query_zeolite(x_axis, y_axis, c_axis)
        elif 'ontospecies' in self.endpoint_url:
            query=query_species(x_axis, y_axis, c_axis, chemical_class)

        # Execute the query and return the results
        return self.execute_query(query)
    
    def get_chemical_class_list(self):

        query=f"""
        PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT DISTINCT ?label
	    WHERE {{ ?classIRI rdf:type os:ChemicalClass; rdfs:label ?label . }} ORDER BY ?label
        """

        return self.execute_query(query)

def query_zeolite(x_axis, y_axis, c_axis):
    select_statement = ""
    if x_axis != y_axis:
        select_statement += f"{x_axis} {y_axis} "
    else:
        select_statement += f"{x_axis} "
    if c_axis:
        if c_axis != x_axis and c_axis != y_axis:
            select_statement += f"{c_axis} "

    query = f"""
            PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
            PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
            PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>

            SELECT ?id {select_statement}
            WHERE {{
            {{
                SELECT ?id (MIN(?ring_sizes_v) AS ?min_ring_size) (MAX(?ring_sizes_v) AS ?max_ring_size)
                WHERE {{
                    ?zeo        zeo:hasFrameworkCode            ?id .
                    ?zeo        zeo:hasZeoliteTopology          ?topo .
                OPTIONAL{{
                    ?topo       zeo:hasRingSizes                  ?rsizes_v .
                    ?rsizes_v   ocr:hasVectorComponent            ?rs_comp .
                    ?rs_comp    ocr:hasComponentValue             ?ring_sizes_v.}}
                }}GROUP BY ?id
            }}
            {{
                SELECT ?id (MIN(?nOfEdges) AS ?min_nOfEdges) (MAX(?nOfEdges) AS ?max_nOfEdges)
                WHERE {{
                    ?zeoFrame   zeo:hasFrameworkCode  ?id .
                    ?zeoFrame   ocr:hasCrystalInformation ?cifcore .
                OPTIONAL{{
                    ?cifcore    ocr:hasTiledStructure ?structure .
                    ?structure  ocr:hasTileNumber     ?tileNumber .
                    ?tileNumber ocr:hasValue          ?nOfTiles .
                    ?tileNumber ocr:isNumberOfTiles   ?tile  .
                    ?tile       ocr:hasTileFaceNumber ?tfn .
                    ?tfn        ocr:isNumberOfTileFaces ?face.
                    ?face       ocr:hasNumberOfEdges  ?nOfEdges .}}
                }}GROUP BY ?id
            }}
                ?zeo        zeo:hasFrameworkCode      ?id .
                ?zeo        zeo:hasZeoliteTopology    ?topo .
                OPTIONAL {{
                    ?topo       zeo:hasFrameworkDensity   ?fr_dens_m .
                    ?fr_dens_m  om:hasNumericalValue      ?fr_dens .
                }}
                OPTIONAL{{
                    ?topo       zeo:hasTopologicalDensity      ?t_dens .
                    ?t_dens     zeo:hasValueTD                 ?topo_dens .
                }}
                OPTIONAL{{
                    ?topo       zeo:hasTopologicalDensity      ?t_dens .
                    ?t_dens     zeo:hasValueTD10               ?topo_dens10 .
                }}
                OPTIONAL{{
                    ?topo        zeo:hasSphereDiameter            ?sph_diam .
                    ?sph_diam    ocr:hasVectorComponent           ?sph_diam_a, ?sph_diam_b, ?sph_diam_c, ?sph_diam_inc .

                    ?sph_diam_a   ocr:hasComponentLabel            "a" ;
                                ocr:hasComponentValue           ?sp_diam_a .
                    ?sph_diam_b   ocr:hasComponentLabel            "b" ;
                                ocr:hasComponentValue           ?sp_diam_b .
                    ?sph_diam_c   ocr:hasComponentLabel            "c" ;
                                ocr:hasComponentValue           ?sp_diam_c .
                    ?sph_diam_inc ocr:hasComponentLabel            "included" ;
                                ocr:hasComponentValue           ?sp_diam_inc .
                }}
                OPTIONAL{{
                    ?topo         zeo:hasDensity             ?dens_m .
                    ?dens_m       om:hasNumericalValue       ?dens .
                }}
                OPTIONAL{{
                    ?topo         zeo:hasAccessibleVolume           ?acc_vol_m .
                    ?acc_vol_m     om:hasNumericalValue             ?acc_vol .
                }}
                OPTIONAL{{
                    ?topo            zeo:hasAccessibleVolumePerCell ?acc_vol_cell_m .
                    ?acc_vol_cell_m   om:hasNumericalValue          ?acc_vol_cell .
                }}
                OPTIONAL{{
                    ?topo         zeo:hasOccupiableVolume           ?occ_vol_m .
                    ?occ_vol_m    om:hasNumericalValue              ?occ_vol .
                }}
                OPTIONAL{{
                    ?topo            zeo:hasOccupiableVolumePerCell ?occ_vol_cell_m .
                    ?occ_vol_cell_m  om:hasNumericalValue           ?occ_vol_cell .
                }}
                OPTIONAL{{
                    ?topo              zeo:hasAccessibleAreaPerCell ?acc_area_cell_m .
                    ?acc_area_cell_m    om:hasNumericalValue        ?acc_area_cell .
                }}
                OPTIONAL{{
                    ?topo             zeo:hasAccessibleAreaPerGram  ?acc_area_gram_m .
                    ?acc_area_gram_m   om:hasNumericalValue         ?acc_area_gram .
                }}
                OPTIONAL{{
                    ?topo             zeo:hasOccupiableAreaPerCell  ?occ_area_cell_m .
                    ?occ_area_cell_m   om:hasNumericalValue         ?occ_area_cell .
                }}
                OPTIONAL{{
                    ?topo             zeo:hasOccupiableAreaPerGram  ?occ_area_gram_m .
                    ?occ_area_gram_m   om:hasNumericalValue         ?occ_area_gram .
                }}
                OPTIONAL{{
                    ?topo             zeo:hasSpecificAccessibleArea ?acc_area_cm3_m .
                    ?acc_area_cm3_m    om:hasNumericalValue         ?acc_area_cm3 .
                }}
                OPTIONAL{{
                    ?topo            zeo:hasSpecificOccupiableArea ?occ_area_cm3_m .
                    ?occ_area_cm3_m   om:hasNumericalValue         ?occ_area_cm3 .
                }}
            }}ORDER BY ( ?id )
            """
    return query

def query_species(x_axis, y_axis, c_axis, chemical_class):

    select_statement = ""
    if x_axis != y_axis:
        select_statement += f"?{x_axis} ?{y_axis} "
    else:
        select_statement += f"?{x_axis} "
    if c_axis:
        if c_axis != x_axis and c_axis != y_axis:
            select_statement += f"?{c_axis} "

    query=f"""
	PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
	PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
	PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

	SELECT DISTINCT ?id {select_statement}
	WHERE {{
	   #?speciesIRI rdf:type os:Species ; rdfs:label ?id .
       ?id rdf:type os:Species .
       """
    for item in (x for x in [x_axis, y_axis, c_axis] if x is not None):
       query += f"""
        ?id os:has{item} ?property{item} .
        ?property{item} os:value ?{item} .
        OPTIONAL{{ ?property{item} os:hasProvenance ?provenance{item} .
	            ?provenance{item} rdfs:label ?provenance{item}label .
                FILTER(?provenance{item}label = "PubChem agent")}}
        """
    query += f"""
	   ?id (rdf:|!rdf:)* ?x .
	   ?x ?y ?z .
	   ?z (rdf:|!rdf:)* ?classIRI .
	   ?classIRI rdf:type os:ChemicalClass ; rdfs:label "{chemical_class}" .
      }} LIMIT 200"""

    return query

