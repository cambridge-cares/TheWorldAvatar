from fastapi import APIRouter, HTTPException
import logging
from typing import Optional
import os
from api.plot import SparqlService
import requests
import json
import re
from periodictable import elements

logger = logging.getLogger(__name__)

router = APIRouter()
KG_URL_CHEMISTRY = os.getenv("KG_URL_CHEMISTRY")

@router.get("")
def get_iri_details(iri: Optional[str] = None):
    logger.info(
        f"Received request to search KG with the following {iri}"
    )

    if iri is None:
        raise HTTPException(status_code=400, detail="IRI parameter is required")

    details, filename = fetch_iri_details(iri)
    if not details:
        raise HTTPException(status_code=404, detail="IRI details not found")

    return {"details": details, "filename": filename}

def fetch_iri_details(iri: str) -> dict:

    if iri:
        segments = iri.split('/')
        namespace = segments[4] 
        if namespace == 'ontozeolite':
            namespace = 'ontozeolite'

        sparql_service = SparqlService(endpoint_url=f"{KG_URL_CHEMISTRY}/namespace/{namespace}/sparql")

        query, filename = iri_info_query(iri)
        print(query)
        print(filename)
        data = sparql_service.execute_query(query=query)
        
        simplified_format = {}

        for binding in data['results']['bindings']:
            p_value = binding['type']['value'].split('/')[-1].split('#')[-1]
            if p_value != 'InChI' and p_value != 'IUPACName':
                p_value = re.sub(r"(\B[A-Z])", r" \1", p_value)
            o_value = binding['o']['value']
            if binding.get('u'):
                u_value = binding['u']['value']
            else:
                u_value = ''
            simplified_format[p_value] = str(o_value) + ' ' + str(u_value)

        print(simplified_format)

        return simplified_format, filename
    
    
def iri_info_query(iri):

    if 'ontospecies/Species_' in iri:
        query = f"""
        PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        
        SELECT DISTINCT ?type ?o ?u
        WHERE {{ 
            <{iri}> (rdf:|!rdf:)* ?x .
            ?x ?y ?z .
            ?z os:value ?o ;
            rdf:type|rdfs:subClassOf ?type .
            OPTIONAL{{?z os:unit ?unit . ?unit rdfs:label ?u .}}
        FILTER(datatype(?o) = xsd:float || datatype(?o) = xsd:string) .
        FILTER(?type = os:BoilingPoint || ?type = os:InChI || ?type = os:MolecularFormula || ?type = os:MolecularWeight || ?type = os:Density || ?type = os:MeltingPoint || ?type = os:IUPACName)
        }} ORDER BY ?type
        """
        makeXYZFilePubchem(iri)
        filename = 'result.xyz'
    
    if 'ontozeolite/ZeoFramework_' in iri:
        query = f"""
        PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
        PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
        PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
        
        SELECT DISTINCT ?type ?o ?u
        WHERE {{ 
            <{iri}> zeo:hasTopologicalProperties ?x .
            ?x ?y ?z .
            ?z rdf:type ?type .
          	?z om:hasNumericalValue ?o .
          	?z om:hasUnit ?unit .
            ?unit rdfs:label ?u .
        }} 
        """
        makeCifFile(iri)
        filename = 'result.cif'

    return query, filename

def makeCifFile(iri):
        
        sparql_service = SparqlService(endpoint_url=f"{KG_URL_CHEMISTRY}/namespace/ontozeolite/sparql")
       
        # SPARQL query to retrieve unit cell parameters and atom coordinates
        query = f"""
            PREFIX zeo:   <http://www.theworldavatar.com/kg/ontozeolite/>
            PREFIX ocr:   <http://www.theworldavatar.com/kg/ontocrystal/>
            PREFIX om:    <http://www.ontology-of-units-of-measure.org/resource/om-2/>

            SELECT ?zeoName ?a ?b ?c ?alpha ?beta ?gamma ?afx ?afy ?afz ?alb
            WHERE {{
            ?zeo       zeo:hasFrameworkCode    ?zeoName .
            ?zeo       ocr:hasCrystalInformation  ?cifdata .
            ?cifdata   ocr:hasUnitCell            ?unitcell .
            ?cifdata   ocr:hasAtomicStructure     ?atomic.

            ?unitcell  ocr:hasUnitCellLengths   ?abc .
            ?abc       ocr:hasVectorComponent   ?abc_a, ?abc_b, ?abc_c .
            ?abc_a     ocr:hasComponentLabel    "a"; ocr:hasComponentValue ?a .
            ?abc_b     ocr:hasComponentLabel    "b"; ocr:hasComponentValue ?b .
            ?abc_c     ocr:hasComponentLabel    "c"; ocr:hasComponentValue ?c .

            ?unitcell  ocr:hasUnitCellAngles     ?abg .
            ?abg       ocr:hasVectorComponent    ?abg_a, ?abg_b, ?abg_g .
            ?abg_a     ocr:hasComponentLabel     "alpha"; ocr:hasComponentValue ?alpha .
            ?abg_b     ocr:hasComponentLabel     "beta"; ocr:hasComponentValue ?beta .
            ?abg_g     ocr:hasComponentLabel     "gamma"; ocr:hasComponentValue ?gamma .

            ?atomic    ocr:hasAtomSite            ?a1 .
            ?a1        ocr:hasFractionalPosition  ?aF_xyz.
            ?aF_xyz    ocr:hasVectorComponent     ?aF_x, ?aF_y, ?aF_z .
            ?aF_x      ocr:hasComponentValue      ?afx ; ocr:hasComponentLabel "x" .
            ?aF_y      ocr:hasComponentValue      ?afy ; ocr:hasComponentLabel "y" .
            ?aF_z      ocr:hasComponentValue      ?afz ; ocr:hasComponentLabel "z" .
            OPTIONAL {{
            ?a1       ocr:hasAtomSiteLabel        ?alb .
                }}
            FILTER(?zeo = <{iri}>) .
            }}
            
        """

        output = []
        results = sparql_service.execute_query(query=query)

        if results["results"]["bindings"]:
            res = results["results"]["bindings"][0]

            output.append("data_" + res["zeoName"]["value"])
            output.append("")
            output.append("#####################################")
            output.append("#")
            output.append("# CIF file for zeolite '" + res["zeoName"]["value"] + "' generated by ZeoliteAgent")
            output.append("#")
            output.append("#####################################")
            output.append("")

            # Append unit cell parameters to the output
            output.append("_cell_length_a     " + res["a"]["value"])
            output.append("_cell_length_b     " + res["b"]["value"])
            output.append("_cell_length_c     " + res["c"]["value"])
            output.append("_cell_angle_alpha  " + res["alpha"]["value"])
            output.append("_cell_angle_beta   " + res["beta"]["value"])
            output.append("_cell_angle_gamma  " + res["gamma"]["value"])
            output.append("")

            # Append atom coordinates to the output
            output.append("loop_")
            output.append("_atom_site_type_symbol")
            output.append("_atom_site_fract_x")
            output.append("_atom_site_fract_y")
            output.append("_atom_site_fract_z")

            for res in results["results"]["bindings"]:
                output.append(str(res["alb"]["value"]) + "\t" +
                            str(res["afx"]["value"]) + "\t" +
                            str(res["afy"]["value"]) + "\t" +
                            str(res["afz"]["value"]))

            static_dir = os.path.join(os.getcwd(), 'static')
            cif_file_path = os.path.join(static_dir, 'result.cif')
            with open(cif_file_path, "w", encoding="utf-8") as fp:
                for line in output:
                    fp.write(line + "\n")


def makeXYZFile(iri):
        
        sparql_service = SparqlService(endpoint_url=f"{KG_URL_CHEMISTRY}/namespace/ontospecies/sparql")
       
        # SPARQL query to retrieve atom coordinates
        query = f"""
            PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX gc: <http://purl.org/gc/> 

            SELECT DISTINCT ?symbol ?X ?Y ?Z
            WHERE {{
            <{iri}> gc:hasAtom ?atom .
            ?atom gc:isElement ?element ;
                    os:hasXCoordinate ?XCoord ;
                    os:hasYCoordinate ?YCoord ;
                    os:hasZCoordinate ?ZCoord .
            ?element os:hasElementSymbol ?el .
            ?el os:value ?symbol .
            ?XCoord os:value ?X .
            ?YCoord os:value ?Y .
            ?ZCoord os:value ?Z .
	        }}
        """

        output = []
        results = sparql_service.execute_query(query=query)
        bindings = results["results"]["bindings"]

        if bindings:
            # Preparing the content for the XYZ file
            output = [str(len(bindings))]  # Number of atoms
            output.append("Generated from SPARQL query")  # Comment line

            for res in bindings:
                symbol = res["symbol"]["value"]
                x = res["X"]["value"]
                y = res["Y"]["value"]
                z = res["Z"]["value"]
                output.append(f"{symbol} {x} {y} {z}")

            # Write the output to the XYZ file
            static_dir = os.path.join(os.getcwd(), 'static')
            xyz_file_path = os.path.join(static_dir, 'result.xyz')
            with open(xyz_file_path, "w", encoding="utf-8") as fp:
                for line in output:
                    fp.write(line + "\n")



def makeXYZFilePubchem(iri):
        
        sparql_service = SparqlService(endpoint_url=f"{KG_URL_CHEMISTRY}/namespace/ontospecies/sparql")
       
        # SPARQL query to retrieve atom coordinates
        query = f"""
            PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX gc: <http://purl.org/gc/> 

            SELECT DISTINCT ?cidvalue
            WHERE {{
            <{iri}> os:hasCID ?cid .
            ?cid os:value ?cidvalue .
	        }}
        """

        output = []
        results = sparql_service.execute_query(query=query)
        bindings = results["results"]["bindings"]

        if bindings:
            for res in bindings:
                cid = res["cidvalue"]["value"]
                data_3d = pug_request_prop_3d(cid)
                atoms_dict= get_structure(data_3d)

                output.append(str(len(atoms_dict))) 
                output.append("Generated from SPARQL query")  

                for key, atom in atoms_dict.items():
                    symbol = atom["element"]
                    x = atom["x"]
                    y = atom["y"]
                    z = atom["z"]
                    output.append(f"{symbol} {x} {y} {z}")

            # Write the output to the XYZ file
            static_dir = os.path.join(os.getcwd(), 'static')
            xyz_file_path = os.path.join(static_dir, 'result.xyz')
            with open(xyz_file_path, "w", encoding="utf-8") as fp:
                for line in output:
                    fp.write(line + "\n")

def pug_request_prop_3d(cid) -> dict:
    pubchem_domain = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/'
    input_domain = 'compound/cid/' 
    input_identifier = str(cid) + '/'
    output= 'JSON?record_type=3d'
    link = pubchem_domain+input_domain+input_identifier+output
    data = requests.get(link)
    file = json.loads(data.text)

    return file

def get_structure(data : dict) -> dict[str , list] :
    geometry = {}
    atom_coords = data.get('PC_Compounds')[0].get('coords')[0].get('conformers')[0]
    x=atom_coords.get('x')
    y=atom_coords.get('y')
    z=atom_coords.get('z')
    elem = data.get('PC_Compounds')[0].get('atoms').get('element')

    i=0
    for item in x:
        for el in elements:
            if elem[i]==el.number:
                elem[i]=el.symbol
        geometry[i] = {}
        geometry[i]['id'] = i+1
        geometry[i]['element'] = elem[i]
        geometry[i]['x'] = x[i]
        geometry[i]['y'] = y[i]
        geometry[i]['z'] = z[i]

        i=i+1

    return geometry