from collections import defaultdict
from dataclasses import dataclass
from decimal import Decimal

from typing import DefaultDict, List, Literal
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontozeolite.model import OZCrystalInfo


def clsname2iri(clsname: Literal["Framework", "Material"]):
    if clsname == "Framework":
        return "zeo:ZeoliteFramework"
    return "zeo:ZeoliticMaterial"


def retrieve_seed_crystalInfo(
    kg_client: KgClient, clsname: Literal["Framework", "Material"]
):
    query = """PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT ?UnitCellVolume ?TileCode WHERE {{
    ?s a {type} . 
    ?s {pred_prefix}ocr:hasCrystalInformation/ocr:hasUnitCell/ocr:hasUnitCellVolume/om:hasNumericalValue ?UnitCellVolume .
    ?s {pred_prefix}ocr:hasCrystalInformation/ocr:hasTiledStructure/ocr:hasTile/ocr:hasTileCode ?TileCode .
}}
LIMIT 100""".format(
        type=clsname2iri(clsname),
        pred_prefix="^zeo:hasZeoliticMaterial?/" if clsname == "Material" else "",
    )

    return [
        OZCrystalInfo(
            unit_cell_volume=Decimal(binding["UnitCellVolume"]["value"]),
            tile_code=binding["TileCode"]["value"],
        )
        for binding in kg_client.query(query)["results"]["bindings"]
    ]


def retrieve_seed_frameworkComponents(
    kg_client: KgClient,
    ontospecies_endpoint: str,
    clsname: Literal["Framework", "Material"],
):
    query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
    
SELECT DISTINCT ?s (SAMPLE(?ElementLabel) AS ?ElementLabelSample) WHERE {{
    ?s a {rdf_type} .
    ?s {pred_prefix}zeo:hasFrameworkComponent ?Element .
    SERVICE <{ontospecies_endpoint}> {{
        ?Element (os:hasElementName|os:hasElementSymbol)/os:value ?ElementLabel
    }}
}}
GROUP BY ?s ?Element
LIMIT 500""".format(
        rdf_type=clsname2iri(clsname),
        pred_prefix="zeo:hasZeoliticMaterial/" if clsname == "Framework" else "",
        ontospecies_endpoint=ontospecies_endpoint,
    )

    zeolite2elements: DefaultDict[str, List[str]] = defaultdict(list)
    for binding in kg_client.query(query)["results"]["bindings"]:
        zeolite2elements[binding["s"]["value"]].append(
            binding["ElementLabelSample"]["value"]
        )

    return list(zeolite2elements.values())


def retrieve_seed_guestSpeciesCounts(
    kg_client: KgClient, clsname: Literal["Framework", "Material"]
):
    query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT (COUNT(?Guest) AS ?Count) WHERE {{
    ?s a {rdf_type} .
    ?s {pred_prefix}zeo:hasGuestCompound ?Guest .
}}
GROUP BY ?s
LIMIT 100""".format(
        rdf_type=clsname2iri(clsname),
        pred_prefix="zeo:hasZeoliticMaterial/" if clsname == "Framework" else "",
    )

    return [
        int(binding["Count"]["value"])
        for binding in kg_client.query(query)["results"]["bindings"]
    ]
