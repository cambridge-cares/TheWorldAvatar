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
    print('class created')

@router.post("")
def adv_search(search_form:SearchInput, domain):
    logger.info(
        "Received request to search KG with the following request body"
    )
    logger.info(search_form)
    
    KG_URL_CHEMISTRY = os.getenv("KG_URL_CHEMISTRY")
    sparql_service = SparqlService(endpoint_url=f"{KG_URL_CHEMISTRY}/namespace/{domain}/sparql")

    logger.info("Sending query to KG")  
    start = time.time()
    try:
        if 'ontozeolite' in domain: 
            query = zeolite_adv_search_query(search_form)
        end = time.time()
        logger.info("Results from KG received")
        data = sparql_service.execute_query(query=query)

        return SparqlResponse(data=data, latency=end - start)
      
    except (UnexpectedDomainError, QueryBadFormed) as e:
        logger.error(e)
        raise HTTPException(status_code=400, detail=str(e))
    
def zeolite_adv_search_query(params):

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

    print(select_statement)

    base_query += "} ORDER BY ?zeoname"
    base_query = base_query.replace('#SELECTSTATEMENT#', select_statement)

    print(base_query)

    return base_query