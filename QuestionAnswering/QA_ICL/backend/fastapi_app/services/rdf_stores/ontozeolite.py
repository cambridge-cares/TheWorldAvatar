from collections import defaultdict
from functools import cache
import itertools
from typing import Annotated

from fastapi import Depends
from rdflib import DCTERMS
from constants.namespace import BIBO, OM2, ONTOCRYSTAL, ONTOZEOLITE
from model.web.comp_op import COMP_OP_2_SPARQL_SYMBOL
from model.kg.ontozeolite import (
    OntocrystalAtomicStructure,
    OntocrystalCoordinateTransformation,
    OntocrystalCrystalInfoPartial,
    OntocrystalMeasureMatrix,
    OntocrystalMeasureVector,
    OntocrystalQuantity,
    OntocrystalTiledStructure,
    OntocrystalUnitCell,
    OntozeoliteTopoPropsPartial,
    OntozeoliteZeoliteFramework,
    OntozeoliteZeoliteFrameworkBase,
    OntozeoliteZeoliteFrameworkPartial,
    OntozeoliteZeoliticMaterial,
    OntozeoliteZeoliticMaterialBase,
)
from model.web.ontozeolite import (
    UnitCellAngleKey,
    UnitCellLengthKey,
    UnitCellRequest,
    ZeoliteFrameworkReturnFields,
    ZeoliteFrameworkRequest,
    ZeoliticMaterialRequest,
)
from services.rdf_ogm import RDFStore
from services.rdf_stores.base import Cls2NodeGetter
from services.sparql import SparqlClient, get_ontozeolite_endpoint


class OntozeoliteRDFStore(Cls2NodeGetter, RDFStore):
    @property
    def cls2getter(self):
        return {
            "zeo:ZeoliteFramework": self.get_zeolite_framework_base_many,
            "zeo:ZeoliticMaterial": self.get_zeolitic_material_base_many,
            "ocr:Quantity": self.get_quantities_many,
            "ocr:MeasureVector": self.get_vectors_many,
            "ocr:MeasureMatrix": self.get_matrices_many,
            "ocr:AtomicStructure": self.get_atomic_structures_many,
            "ocr:CoordinateTransformation": self.get_coordinate_transforms_many,
            "ocr:UnitCell": self.get_unit_cells_many,
            "ocr:TiledStructure": self.get_tiled_structures_many,
        }

    def _make_unit_cell_patterns(self, req: UnitCellRequest):
        unit_cell_length_patterns = [
            pattern
            for key, conds in req.items()
            if isinstance(key, UnitCellLengthKey) and conds
            for pattern in (
                f'?Lengths ocr:hasVectorComponent [ ocr:hasComponentLabel "{key.value}" ; ocr:hasComponentValue ?{key.value} ] .',
                "FILTER ( {} )".format(
                    " && ".join(
                        f"?{key.value} {COMP_OP_2_SPARQL_SYMBOL[op]} {rhs}"
                        for op, rhs in conds
                    )
                ),
            )
        ]
        unit_cell_angle_patterns = [
            pattern
            for key, conds in req.items()
            if isinstance(key, UnitCellAngleKey) and conds
            for pattern in (
                f'?Angles ocr:hasVectorComponent [ ocr:hasComponentLable "{key.value}" ; ocr:hasComponentValue ?{key.value} ] .',
                "FILTER ( {} )".format(
                    " && ".join(
                        f"?{key.value} {COMP_OP_2_SPARQL_SYMBOL[op]} {rhs}"
                        for op, rhs in conds
                    )
                ),
            )
        ]
        return [
            pattern
            for pattern in (
                (
                    "?CrystalInfo ocr:hasUnitCell ?UnitCell ."
                    if unit_cell_length_patterns or unit_cell_angle_patterns
                    else None
                ),
                (
                    "?UnitCell ocr:hasUnitCellLengths ?Lengths ."
                    if unit_cell_length_patterns
                    else None
                ),
                *unit_cell_length_patterns,
                (
                    "?UnitCell ocr:hasUnitCellAngles ?Angles ."
                    if unit_cell_angle_patterns
                    else None
                ),
                *unit_cell_angle_patterns,
            )
            if pattern
        ]

    def get_zeolite_framework_IRIs(self, req: ZeoliteFrameworkRequest):
        xrd_patterns = (
            [
                "?CrystalInfo ocr:hasXRDSpectrum ?Spectrum .",
                *(
                    triple
                    for i, peak in enumerate(req.crystal_info.xrd_peak)
                    for triple in (
                        f"?Spectrum ocr:hasCharacteristicPeak ?Peak{i} .",
                        f"?Peak{i} ocr:hasTwoThetaPosition ?TwoThetaPosition{i} ; ocr:hasRelativeIntensity ?Intensity{i} ."
                        f"FILTER ( ?TwoThetaPosition{i} >= {peak.position - peak.width} && ?TwoThetaPosition{i} <= {peak.position + peak.width} && ?Intensity{i} > {peak.threshold} )",
                    )
                ),
            ]
            if req.crystal_info.xrd_peak
            else []
        )

        unit_cell_patterns = self._make_unit_cell_patterns(req.crystal_info.unit_cell)

        scalar_topo_prop_patterns = [
            pattern
            for key, conds in req.topo_props.scalars.items()
            if conds
            for pattern in (
                f"?TopoProps zeo:has{key}/om:hasNumericalValue ?{key}Value .",
                "FILTER ( {} )".format(
                    " && ".join(
                        f"?{key}Value {COMP_OP_2_SPARQL_SYMBOL[op]} {rhs}"
                        for op, rhs in conds
                    )
                ),
            )
        ]

        composite_bu_patterns = [
            f'?TopoProps zeo:hasCompositeBU/(zeo:hasCage|zeo:hasTCage|zeo:hasChain) "{cbu}" .'
            for cbu in req.topo_props.composite_bu
        ]
        secondary_bu_patterns = [
            f'?TopoProps zeo:hasSecondaryBU "{sbu}" .'
            for sbu in req.topo_props.secondary_bu
        ]

        patterns = [
            pattern
            for pattern in (
                (
                    "?Framework ocr:hasCrystalInformation ?CrystalInfo ."
                    if xrd_patterns or unit_cell_patterns
                    else None
                ),
                *xrd_patterns,
                *unit_cell_patterns,
                (
                    "?Framework zeo:hasTopologicalProperties ?TopoProps ."
                    if scalar_topo_prop_patterns
                    or composite_bu_patterns
                    or secondary_bu_patterns
                    else None
                ),
                *scalar_topo_prop_patterns,
                *composite_bu_patterns,
                *secondary_bu_patterns,
            )
            if pattern
        ]

        query = """PREFIX om: <{om}>
PREFIX ocr: <{ocr}>
PREFIX zeo: <{zeo}>

SELECT DISTINCT ?Framework
WHERE {{
    ?Framework a zeo:ZeoliteFramework .
    {clauses}
}}""".format(
            om=OM2,
            ocr=ONTOCRYSTAL,
            zeo=ONTOZEOLITE,
            clauses="\n    ".join(patterns),
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        return [binding["Framework"] for binding in bindings]

    def get_zeolite_framework_partial_many(
        self, iris: list[str] | tuple[str], return_fields: ZeoliteFrameworkReturnFields
    ):
        frameworks_base = self.get_zeolite_framework_base_many(iris=iris)

        if return_fields.crystal_info:
            query = f"""PREFIX ocr: <{ONTOCRYSTAL}>

SELECT *
WHERE {{
    VALUES ?Framework {{ {" ".join(f"<{iri}>" for iri in iris)} }}
    ?Framework ocr:hasCrystalInformation ?CrystalInfo .
}}"""
            _, bindings = self.sparql_client.querySelectThenFlatten(query)
            frameworkIRI_to_crystalInfoIRI = {
                binding["Framework"]: binding["CrystalInfo"] for binding in bindings
            }
            crystalInfo_returnFieldsSet = set(return_fields.crystal_info)
            crystalInfo_returnFields = [
                field
                for field, info in OntocrystalCrystalInfoPartial.model_fields.items()
                if info.alias in crystalInfo_returnFieldsSet
            ]
            crystalInfo_models = self.get_many(
                OntocrystalCrystalInfoPartial,
                iris=list(frameworkIRI_to_crystalInfoIRI.values()),
                return_fields=crystalInfo_returnFields,
            )
            frameworkIRI_to_crystalInfo = {
                iri: model
                for iri, model in zip(
                    frameworkIRI_to_crystalInfoIRI, crystalInfo_models
                )
            }
        else:
            frameworkIRI_to_crystalInfo = dict()

        if return_fields.topo_props:
            query = f"""PREFIX zeo: <{ONTOZEOLITE}>

SELECT *
WHERE {{
    VALUES ?Framework {{ {" ".join(f"<{iri}>" for iri in iris)} }}
    ?Framework zeo:hasTopologicalProperties ?TopoProps .
}}"""
            _, bindings = self.sparql_client.querySelectThenFlatten(query)
            frameworkIRI_to_topoPropsIRI = {
                binding["Framework"]: binding["TopoProps"] for binding in bindings
            }
            topoProps_returnFieldsSet = set(return_fields.topo_props)
            topoProps_returnFields = [
                field
                for field, info in OntozeoliteTopoPropsPartial.model_fields.items()
                if info.alias in topoProps_returnFieldsSet
            ]
            topoProps_models = self.get_many(
                OntozeoliteTopoPropsPartial,
                iris=list(frameworkIRI_to_topoPropsIRI.values()),
                return_fields=topoProps_returnFields,
            )
            frameworkIRI_to_topoProps = {
                iri: model
                for iri, model in zip(frameworkIRI_to_topoPropsIRI, topoProps_models)
            }
        else:
            frameworkIRI_to_topoProps = dict()

        if return_fields.material:
            query = f"""PREFIX zeo: <{ONTOZEOLITE}>

SELECT *
WHERE {{
    VALUES ?Framework {{ {" ".join(f"<{iri}>" for iri in iris)} }}
    ?Framework zeo:hasZeoliticMaterial ?Material
}}"""
            _, bindings = self.sparql_client.querySelectThenFlatten(query)
            frameworkIRI_to_materialIRIs: defaultdict[str, list[str]] = defaultdict(
                list
            )
            for binding in bindings:
                frameworkIRI_to_materialIRIs[binding["Framework"]].append(
                    binding["Material"]
                )
            materials = (
                x
                for x in self.get_zeolitic_material_base_many(
                    iris=list(itertools.chain(*frameworkIRI_to_materialIRIs.values()))
                )
            )
            frameworkIRI_to_materials = {
                frameworkIRI: [x for x in [next(materials) for _ in materialIRIs] if x]
                for frameworkIRI, materialIRIs in frameworkIRI_to_materialIRIs.items()
            }
        else:
            frameworkIRI_to_materials = dict()

        return [
            (
                OntozeoliteZeoliteFrameworkPartial(
                    **base_model.model_dump(),
                    CrystalInformation=frameworkIRI_to_crystalInfo.get(base_model.IRI),
                    TopologicalProperties=frameworkIRI_to_topoProps.get(base_model.IRI),
                    ZeoliticMaterial=frameworkIRI_to_materials.get(base_model.IRI),
                )
                if base_model
                else None
            )
            for base_model in frameworks_base
        ]

    def get_zeolite_framework_base_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntozeoliteZeoliteFrameworkBase, iris)

    def get_zeolite_framework_one(self, iri: str):
        return self.get_one(OntozeoliteZeoliteFramework, iri)

    def get_zeolitic_material_IRIs(self, req: ZeoliticMaterialRequest):
        patterns: list[str] = [
            x
            for x in [
                "?Material a zeo:ZeoliticMaterial .",
                (
                    f"?Material ^zeo:hasZeoliticMaterial <{req.framework}> ."
                    if req.framework
                    else None
                ),
                (f'?Material os:name "{req.name}" .' if req.name else None),
                (
                    f'?Material zeo:hasChemicalFormula "{req.formula}" .'
                    if req.formula
                    else None
                ),
                *(
                    f"?Material zeo:hasFrameworkComponent <{iri}> ."
                    for iri in req.framework_components
                ),
                *(
                    f"?Material zeo:hasGuestComponent <{iri}> ."
                    for iri in req.guest_components
                ),
                *(
                    [
                        "?Material ocr:hasCrystalInformation ?CrystalInfo .",
                        *self._make_unit_cell_patterns(req.unit_cell),
                    ]
                    if req.unit_cell
                    else []
                ),
                (
                    f'?Material ocr:hasCitation/ocr:hasAuthor/foaf:family_name "{req.citation.author_family_name}" .'
                    if req.citation.author_family_name
                    else None
                ),
                (
                    f"?Material ocr:hasCitation/dcterm:isPartOf/dcterm:issued {req.citation.year} ."
                    if req.citation.year
                    else None
                ),
                (
                    f"?Material ocr:hasCitation/dcterm:isPartOf/dcterm:isPartOf <{req.citation.journal}> ."
                    if req.citation.journal
                    else None
                ),
                (
                    f'?Material ocr:hasCitation/bibo:doi "{req.citation.doi}" .'
                    if req.citation.doi
                    else None
                ),
            ]
            if x
        ]
        query = """PREFIX dcterm: <{dcterm}>
PREFIX bibo: <{bibo}>
PREFIX ocr: <{ocr}>
PREFIX zeo: <{zeo}>

SELECT ?Material
WHERE {{
    {patterns}
}}""".format(
            dcterm=DCTERMS,
            bibo=BIBO,
            ocr=ONTOCRYSTAL,
            zeo=ONTOZEOLITE,
            patterns="\n    ".join(patterns),
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        return [binding["Material"] for binding in bindings]

    def get_zeolitic_material_base_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntozeoliteZeoliticMaterialBase, iris)

    def get_zeolitic_material_one(self, iri: str):
        return self.get_one(OntozeoliteZeoliticMaterial, iri)

    def get_quantities_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntocrystalQuantity, iris)

    def get_vectors_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntocrystalMeasureVector, iris)

    def get_matrices_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntocrystalMeasureMatrix, iris)

    def get_atomic_structures_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntocrystalAtomicStructure, iris)

    def get_coordinate_transforms_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntocrystalCoordinateTransformation, iris)

    def get_unit_cells_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntocrystalUnitCell, iris)

    def get_tiled_structures_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntocrystalTiledStructure, iris)

    def get_cbu_all(self):
        query = f"""PREFIX zeo: <{ONTOZEOLITE}>

SELECT DISTINCT ?o
WHERE {{
    ?s zeo:hasCompositeBU/(zeo:hasCage|zeo:hasTCage|zeo:hasChain) ?o
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        return [binding["o"] for binding in bindings]

    def get_sbu_all(self):
        query = f"""PREFIX zeo: <{ONTOZEOLITE}>

SELECT DISTINCT ?o
WHERE {{
    ?s zeo:hasSecondaryBU ?o
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        return [binding["o"] for binding in bindings]


@cache
def get_ontozeolite_rdfStore(
    endpoint: Annotated[str, Depends(get_ontozeolite_endpoint)]
):
    return OntozeoliteRDFStore(endpoint)
