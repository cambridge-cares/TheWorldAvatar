from decimal import Decimal
from typing import Dict

from constants.ontozeolite import ZEOTOPO_SCALAR_KEYS
from locate_then_ask.kg_client import KgClient
from .model import OZCrystalInfo, OZFramework, OZMaterial


class OZEntityStore:
    def __init__(self, ontozeolite_endpoint: str, ontospecies_endpoint: str):
        self.ontozeolite_client = KgClient(ontozeolite_endpoint)
        self.ontospecies_client = KgClient(ontospecies_endpoint)
        self.iri2framework: Dict[str, OZFramework] = dict()
        self.iri2material: Dict[str, OZMaterial] = dict()

    def get_framework(self, entity_iri: str):
        if entity_iri not in self.iri2framework:
            material_iris = (
                self._retrieve_by_property(entity_iri, "zeo:hasZeoliticMaterial"),
            )
            guest_species_iris = [
                guest_species_iri
                for material_iri in material_iris
                for guest_species_iri in self.get_material(
                    material_iri
                ).guest_species_iris
            ]
            guest_formulae = [
                self.get_material(material_iri).guest_formula
                for material_iri in material_iris
            ]
            guest_formulae = [x for x in guest_formulae if x]
            self.iri2framework[entity_iri] = OZFramework(
                iri=entity_iri,
                framework_code=self._retrieve_by_property(
                    entity_iri, "zeo:hasFrameworkCode"
                )[0],
                framework_components=self._retrieve_framework_components(entity_iri),
                crystal_info=self._retrieve_crystal_info(entity_iri),
                topo_scalar=self._retrieve_topomeasures(entity_iri),
                material_iris=material_iris,
                guest_species_iris=guest_species_iris,
                guest_formulae=guest_formulae,
            )
        return self.iri2framework[entity_iri]

    def get_material(self, entity_iri: str):
        if entity_iri not in self.iri2material:
            guest_formula = self._retrieve_by_property(
                entity_iri, "zeo:hasGuestFormula"
            )
            if guest_formula:
                guest_formula = guest_formula[0]
            else:
                guest_formula = None
            self.iri2material[entity_iri] = OZMaterial(
                iri=entity_iri,
                framework_iri=self._retrieve_by_property(
                    entity_iri, "^zeo:hasZeoliticMaterial"
                )[0],
                formulae=self._retrieve_by_property(
                    entity_iri, "zeo:hasChemicalFormula"
                ),
                guest_species_iris=self._retrieve_by_property(
                    entity_iri, "zeo:hasGuestCompound"
                ),
                guest_formula=guest_formula,
            )
        return self.iri2material[entity_iri]

    def get_guest_species_identifiers(self, entity_iri: str):
        query = """
SELECT ?SpeciesIdentifier {{
    <{IRI}> ?hasIdentifier [ a/rdfs:subClassOf os:Identifier ; os:value ?SpeciesIdentifier ] .
}""".format(
            IRI=entity_iri
        )
        return [
            x["SpeciesIdentifier"]["value"]
            for x in self.ontospecies_client.query(query)["results"]["bindings"]
        ]

    def _retrieve_by_property(self, entity_iri: str, prop: str):
        template = """PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT ?x WHERE {{
    <{IRI}> {prop} ?x .
}}
"""
        query = template.format(IRI=entity_iri, prop=prop)
        return [
            row["x"]["value"]
            for row in self.ontozeolite_client.query(query)["results"]["bindings"]
        ]

    def _retrieve_framework_components(self, framework_iri: str):
        query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT ?ElementSymbol WHERE {{
    <{IRI}> zeo:hasFrameworkComponent ?Element .
    SERVICE <{ontospecies_endpoint}> {{
        ?Element os:hasElementSymbol/os:value ?ElementSymbol .
    }}
}}""".format(
            IRI=framework_iri,
            ontospecies_endpoint=self.ontospecies_client.client.endpoint,
        )
        return [
            x["ElementSymbol"]["value"]
            for x in self.ontozeolite_client.query(query)["results"]["bindings"]
        ]

    def _retrieve_crystal_info(self, entity_iri: str):
        unit_cell_volume = Decimal(
            self._retrieve_by_property(
                entity_iri,
                "ocr:hasCrystalInformation/ocr:hasUnitCell/ocr:hasUnitCellVolume/om:hasNumericalValue",
            )[0]
        )
        tile_code_values = self._retrieve_by_property(
            entity_iri,
            "ocr:hasCrystalInformation/ocr:hasTiledStructure/ocr:hasTile/ocr:hasTileCode",
        )
        if len(tile_code_values) == 0:
            tile_code = None
        else:
            tile_code = tile_code_values[0]
        return OZCrystalInfo(
            unit_cell_volume=unit_cell_volume,
            tile_code=tile_code,
        )

    def _retrieve_topomeasures(self, entity_iri: str):
        key2bindings = {
            key: self._retrieve_by_property(
                entity_iri,
                "zeo:hasZeoliticProperties/zeo:has{key}/om:hasNumericalValue".format(
                    key=key.value
                ),
            )
            for key in ZEOTOPO_SCALAR_KEYS
        }
        return {k: Decimal(v[0]) for k, v in key2bindings.items() if v}
