from fastapi import APIRouter, HTTPException
import logging
import os
import time

from api.plot import SparqlService
from api.sparql import SparqlResponse, UnexpectedDomainError, QueryBadFormed
from pydantic import create_model

from importlib.resources import files
import json

logger = logging.getLogger(__name__)

router = APIRouter()

SEARCH_FIELDS = json.loads(files("resources.chemistry").joinpath("search_groups.json").read_text())
fields = {}
for group in SEARCH_FIELDS:
    for section in group["sections"]:
        for input_row in section["input_rows"]:
            for input_field in input_row:
                fields[input_field["entry"]] = (str, None)

SearchModel = create_model('SearchRequest', **fields)

class SearchInput(SearchModel):
    domain : str = None

@router.post("")
def adv_search(search_form:SearchInput, domain):
    logger.info(
        "Received request to search KG with the following request body"
    )
    logger.info(search_form)
    
    KG_URL_CHEMISTRY = os.getenv("KG_URL_CHEMISTRY")
    sparql_service = SparqlService(endpoint_url=f"{KG_URL_CHEMISTRY}/namespace/{domain.split('_')[0]}/sparql")

    logger.info("Sending query to KG")  
    start = time.time()
    try:
        if 'ontozeolite' in domain: 
            if 'framework' in domain:
                query = zeolite_framework_adv_search_query(search_form)
            if 'material' in domain:
                query = zeolite_material_adv_search_query(search_form)
        end = time.time()
        logger.info("Results from KG received")
        data = sparql_service.execute_query(query=query)

        return SparqlResponse(data=data, latency=end - start)
      
    except (UnexpectedDomainError, QueryBadFormed) as e:
        logger.error(e)
        raise HTTPException(status_code=400, detail=str(e))
    
def zeolite_framework_adv_search_query(params):

    base_query = """
        PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
        PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
        PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>

        SELECT ?zeo ?zeoname #SELECTSTATEMENT#
        WHERE {
            ?zeo zeo:hasFrameworkCode ?zeoname .
        """
    
    spectrum_section_added = False
    unitcell_section_added = False
    property_section_added = False
    cbu_section_added = False
    select_statement = ''
    filters = []
    for i in range(1, 4):  # Assuming up to 3 peaks
        attr_peak_pos = f"peak{i}pos"
        peak_pos = getattr(params, attr_peak_pos)
        attr_peak_wid = f"peak{i}wid"
        peak_wid = getattr(params, attr_peak_wid) or 0.5
        attr_peak_int = f"peak{i}int"
        peak_int = getattr(params, attr_peak_int) or 50
        if peak_pos:
            peakmin = float(peak_pos) - float(peak_wid)
            peakmax = float(peak_pos) + float(peak_wid)
            if not spectrum_section_added:
                base_query += f"""
                ?zeo ocr:hasCrystalInformation ?cifcore .
                ?cifcore ocr:hasXRDSpectrum ?spectrum.
                """
                spectrum_section_added = True

            base_query += f"""
            ?spectrum ocr:hasCharacteristicPeak ?peak{i}.
            ?peak{i} ocr:hasTwoThetaPosition ?2thetaposition{i} ;
            ocr:hasRelativeIntensity ?intensity{i} .
            FILTER(?2thetaposition{i} >= {peakmin} && ?2thetaposition{i} <= {peakmax} && ?intensity{i} > {peak_int})
            """
            select_statement += f" ?2thetaposition{i} ?intensity{i}"

    # Unit cell parameters
    for param in ['A', 'B', 'C', 'ALPHA', 'BETA', 'GAMMA']:
        attr_min_val = f"uc{param}min"
        min_val = getattr(params, attr_min_val)
        attr_max_val = f"uc{param}max"
        max_val = getattr(params, attr_max_val)
        if min_val or max_val:
            if not unitcell_section_added:
                base_query += f"""
                ?zeo ocr:hasCrystalInformation ?cifdata .
                ?cifdata ocr:hasUnitCell ?unitcell .
                ?unitcell ocr:hasUnitCellLengths ?abc .
                ?abc ocr:hasVectorComponent ?abc_a, ?abc_b, ?abc_c .
                ?abc_a ocr:hasComponentLabel "a"; ocr:hasComponentValue ?a .
                ?abc_b ocr:hasComponentLabel "b"; ocr:hasComponentValue ?b .
                ?abc_c ocr:hasComponentLabel "c"; ocr:hasComponentValue ?c .
                ?unitcell ocr:hasUnitCellAngles ?abg .
                ?abg ocr:hasVectorComponent ?abg_a, ?abg_b, ?abg_g .
                ?abg_a ocr:hasComponentLabel "alpha"; ocr:hasComponentValue ?alpha .
                ?abg_b ocr:hasComponentLabel "beta"; ocr:hasComponentValue ?beta .
                ?abg_g ocr:hasComponentLabel "gamma"; ocr:hasComponentValue ?gamma .
                """
                unitcell_section_added = True
            
            if min_val and max_val:
                filters.append(f"xsd:decimal(xsd:string(?{param.lower()})) >= {min_val} && xsd:decimal(xsd:string(?{param.lower()})) <= {max_val}")
            elif min_val and not max_val:
                filters.append(f"xsd:decimal(xsd:string(?{param.lower()})) >= {min_val}")
            elif max_val and not min_val:
                filters.append(f"xsd:decimal(xsd:string(?{param.lower()})) <= {max_val}") 
            select_statement += f" ?{param.lower()}"           

    # Framework, Topological Density, and Density
    for param in ['FWD', 'TPD', 'DENS']:
        attr_min_val = f"{param}min"
        min_val = getattr(params, attr_min_val)
        attr_max_val = f"{param}max"
        max_val = getattr(params, attr_max_val)
        if min_val or max_val:
            if not property_section_added:
                base_query += f"""
                ?zeo zeo:hasTopologicalProperties ?topo .
                ?topo zeo:hasFrameworkDensity ?fr_dens_m .
                ?fr_dens_m om:hasNumericalValue ?FWD .
                ?topo zeo:hasTopologicalDensity ?t_dens .
                ?t_dens zeo:hasValueTD ?TPD .
                ?topo zeo:hasDensity ?dens_m .
                ?dens_m om:hasNumericalValue ?DENS .
                """
                property_section_added = True

            if min_val and max_val:
                filters.append(f"?{param} >= {min_val} && ?{param} <= {max_val}")
            elif min_val and not max_val:
                filters.append(f"?{param} >= {min_val}")
            elif max_val and not min_val:
                filters.append(f"?{param} <= {max_val}")   
            select_statement += f" ?{param}"

    for i in range(1, 4):  # Assuming up to 3 peaks
        attr = f"CBU{i}"
        cbu = getattr(params, attr)
        if cbu:
            if not cbu_section_added:
                base_query += f"""
                ?zeo zeo:hasTopologicalProperties ?topo .
                """
                spectrum_section_added = True

            base_query += f"""
            ?topo zeo:hasCompositeBU/(zeo:hasCage|zeo:hasTCage|zeo:hasChain) ?cbu{i} .
            """
            filters.append(f"?cbu{i} = \"{cbu}\"")

    sbu = getattr(params, "SBU")
    if sbu:
        base_query += f"""
            ?zeo zeo:hasTopologicalProperties ?topo .
            ?topo zeo:hasSecondaryBU ?sbu.    
            """
        filters.append(f"?sbu = \"{sbu}\"")

    if filters:
        base_query += "FILTER (" + " && ".join(filters) + ")\n"

    base_query += "} ORDER BY ?zeoname"
    base_query = base_query.replace('#SELECTSTATEMENT#', select_statement)

    return base_query


def zeolite_material_adv_search_query(params):

    base_query = """
        PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
        PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
        PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>     
        PREFIX bibo: <http://purl.org/ontology/bibo/>
        PREFIX dcterm: <http://purl.org/dc/terms/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

        SELECT DISTINCT ?zeo_material ?formula ?framework #SELECTSTATEMENT#
        WHERE {
            ?zeo_framework zeo:hasZeoliticMaterial ?zeo_material ;
                zeo:hasFrameworkCode ?framework .
            ?zeo_material rdf:type zeo:ZeoliticMaterial .
            ?zeo_material zeo:hasChemicalFormula ?formula .
        """
    
    unitcell_section_added = False
    select_statement = ''
    filters = []

    framework = getattr(params, "framework")
    if framework:
        filters.append(f"?framework = \"{framework}\"")

    label = getattr(params, "label")
    if label:
        base_query += f"""
            ?zeo_material os:name ?label .
        """
        filters.append(f"?label = \"{label}\"")
        select_statement += f" ?label"
    
    formula = getattr(params, "formula")
    if formula:
        filters.append(f"CONTAINS(?formula, \"{formula}\" )")

    # Unit cell parameters
    for param in ['A', 'B', 'C', 'ALPHA', 'BETA', 'GAMMA']:
        attr_min_val = f"uc{param}min"
        min_val = getattr(params, attr_min_val)
        attr_max_val = f"uc{param}max"
        max_val = getattr(params, attr_max_val)
        if min_val or max_val:
            if not unitcell_section_added:
                base_query += f"""
                ?zeo_material ocr:hasCrystalInformation ?cifdata .
                ?cifdata ocr:hasUnitCell ?unitcell .
                ?unitcell ocr:hasUnitCellLengths ?abc .
                ?abc ocr:hasVectorComponent ?abc_a, ?abc_b, ?abc_c .
                ?abc_a ocr:hasComponentLabel "a"; ocr:hasComponentValue ?a .
                ?abc_b ocr:hasComponentLabel "b"; ocr:hasComponentValue ?b .
                ?abc_c ocr:hasComponentLabel "c"; ocr:hasComponentValue ?c .
                ?unitcell ocr:hasUnitCellAngles ?abg .
                ?abg ocr:hasVectorComponent ?abg_a, ?abg_b, ?abg_g .
                ?abg_a ocr:hasComponentLabel "alpha"; ocr:hasComponentValue ?alpha .
                ?abg_b ocr:hasComponentLabel "beta"; ocr:hasComponentValue ?beta .
                ?abg_g ocr:hasComponentLabel "gamma"; ocr:hasComponentValue ?gamma .
                """
                unitcell_section_added = True
            
            if min_val and max_val:
                filters.append(f"xsd:decimal(xsd:string(?{param.lower()})) >= {min_val} && xsd:decimal(xsd:string(?{param.lower()})) <= {max_val}")
            elif min_val and not max_val:
                filters.append(f"xsd:decimal(xsd:string(?{param.lower()})) >= {min_val}")
            elif max_val and not min_val:
                filters.append(f"xsd:decimal(xsd:string(?{param.lower()})) <= {max_val}") 
            select_statement += f" ?{param.lower()}"   

    author = getattr(params, "author")
    year = getattr(params, "year")
    journal = getattr(params, "journal")
    doi = getattr(params, "doi")
    if author or year or journal or doi:
        base_query += f"""
        ?zeo_material ocr:hasCitation ?citation .
        """
        if author:
            base_query += f"""
            ?citation ocr:hasAuthor/foaf:family_name "{author}" .
            """
        if doi:
            base_query += f"""
            ?citation bibo:doi "{doi}" .
            """
        if year:
            base_query += f"""
            ?citation dcterm:isPartOf ?journalversion .
            ?journalversion dcterm:issued ?year .
            FILTER(xsd:string(?year) = "{year}")
            """
        if journal:
            base_query += f"""
            ?citation dcterm:isPartOf/dcterm:isPartOf ?j .
            ?j dcterm:title ?journal .
            """
            filters.append(f"?journal = \'{journal}\'") 
            select_statement += f" ?journal"
    
    for i in range(1, 6):  # up to 5 elements
        attr_el = f"el{i}"
        el_symbol = getattr(params, attr_el)
        if el_symbol:
            iri = query_element(el_symbol)
            base_query += f"""
            ?zeo_material zeo:hasFrameworkComponent <{iri}> .
            """

    for i in range(1, 4):  # up to 3 elements
        attr_guest = f"guest{i}"
        guest_id = getattr(params, attr_guest)
        if guest_id:
            iri = query_species(guest_id)
            base_query += f"""
            ?zeo_material zeo:hasGuestCompound ?guest{i}IRI .
            FILTER(?guest{i}IRI = <{iri}>)
            SERVICE <{os.environ["KG_ENDPOINT_ONTOSPECIES"]}> {{
            ?guest{i}IRI rdfs:label ?guest{i}formula
            }}
            """
            select_statement += f" ?guest{i}IRI ?guest{i}formula"
   
    if filters:
        base_query += "FILTER (" + " && ".join(filters) + ")\n"

    base_query += "} ORDER BY ?name"
    base_query = base_query.replace('#SELECTSTATEMENT#', select_statement)

    print(base_query)

    return base_query

def query_element(el_symbol):
    query = f"""
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable#>

    SELECT ?element
    WHERE {{
        ?element rdf:type pt:Element .
        ?element os:hasElementSymbol ?symbol .
        ?symbol os:value "{el_symbol}" .
        }}
    """
    KG_URL_CHEMISTRY = os.getenv("KG_URL_CHEMISTRY")
    sparql_service_el = SparqlService(endpoint_url=f"{KG_URL_CHEMISTRY}/namespace/ontospecies/sparql")
    data = sparql_service_el.execute_query(query=query)
    iri = data['results']['bindings'][0]['element']['value']
    return iri

def query_species(species_id):
    query = f"""
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT DISTINCT ?species 
    WHERE {{
    ?species a os:Species .
    VALUES ?SpeciesIdentifierValue {{ "{species_id}" }}
    {{
    ?species rdfs:label ?SpeciesIdentifierValue .
    }}
    UNION
    {{
    ?species ?hasIdentifier [ a/rdfs:subClassOf os:Identifier ; os:value ?SpeciesIdentifierValue ] .
    }}
    }}
    """
    KG_URL_CHEMISTRY = os.getenv("KG_URL_CHEMISTRY")
    sparql_service_el = SparqlService(endpoint_url=f"{KG_URL_CHEMISTRY}/namespace/ontospecies/sparql")
    data = sparql_service_el.execute_query(query=query)
    iri = data['results']['bindings'][0]['species']['value']
    return iri




