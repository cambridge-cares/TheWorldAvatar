from functools import cache
from typing import Annotated

from fastapi import Depends
from model.web.comp_op import COMP_OP_2_SPARQL_SYMBOL
from model.kg.ontozeolite import (
    OntocrystalAtomicStructure,
    OntocrystalCoordinateTransformation,
    OntocrystalMeasureMatrix,
    OntocrystalMeasureVector,
    OntocrystalQuantity,
    OntocrystalTiledStructure,
    OntocrystalUnitCell,
    OntozeoliteZeoliteFrameworkBase,
    OntozeoliteZeoliticMaterialBase,
)
from model.web.ontozeolite import (
    UnitCellAngleKey,
    UnitCellLengthKey,
    ZeoliteFrameworkRequest,
)
from services.rdf_orm import RDFStore
from services.rdf_stores.base import Cls2NodeGetter
from services.sparql import get_ontozeolite_endpoint


class OntozeoliteRDFStore(Cls2NodeGetter, RDFStore):
    @property
    def cls2getter(self):
        return {
            "zeo:ZeoliteFramework": self.get_zeolite_frameworks_many,
            "zeo:ZeoliticMaterial": self.get_zeolitic_materials_many,
            "ocr:Quantity": self.get_quantities_many,
            "ocr:MeasureVector": self.get_vectors_many,
            "ocr:MeasureMatrix": self.get_matrices_many,
            "ocr:AtomicStructure": self.get_atomic_structures_many,
            "ocr:CoordinateTransformation": self.get_coordinate_transforms_many,
            "ocr:UnitCell": self.get_unit_cells_many,
            "ocr:TiledStructure": self.get_tiled_structures_many,
        }

    def get_zeolite_frameworks(self, req: ZeoliteFrameworkRequest):
        xrd_patterns = (
            [
                "?CrystalInfo ocr:hasXRDSpectrum ?Spectrum .",
                *(
                    triple
                    for i, peak in enumerate(req.xrd_peak)
                    for triple in (
                        f"?Spectrum ocr:hasCharacteristicPeak ?Peak{i} .",
                        f"?Peak{i} ocr:hasTwoThetaPosition ?TwoThetaPosition{i} ; ocr:hasRelativeIntensity ?Intensity{i} ."
                        f"FILTER ( ?TwoThetaPosition{i} >= {peak.position - peak.width} && ?TwoThetaPosition{i} <= {peak.position + peak.width} && ?Intensity{i} > {peak.threshold} )",
                    )
                ),
            ]
            if req.xrd_peak
            else []
        )

        unit_cell_length_patterns = [
            pattern
            for key, conds in req.unit_cell.items()
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
            for key, conds in req.unit_cell.items()
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
        unit_cell_patterns = [
            pattern
            for pattern in (
                (
                    "?CrystalInfo ocr:hasUnitCell ?UnitCell ."
                    if unit_cell_length_patterns or unit_cell_angle_patterns
                    else None
                ),
                (
                    "?UnitCell ?ocr:hasUnitCellLengths ?Lengths ."
                    if unit_cell_length_patterns
                    else None
                ),
                *unit_cell_length_patterns,
                (
                    "?UnitCell ?ocr:hasUnitCellAngles ?Angles ."
                    if unit_cell_angle_patterns
                    else None
                ),
                *unit_cell_angle_patterns,
            )
            if pattern
        ]

        scalar_topo_prop_patterns = [
            pattern
            for key, conds in req.scalar_topological_properties.items()
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
            for cbu in req.composite_bu
        ]
        secondary_bu_patterns = [
            f'?TopoProps zeo:hasSecondaryBU "{sbu}" .' for sbu in req.secondary_bu
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

        query = """PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT DISTINCT ?Framework
WHERE {{
    ?Framework a zeo:ZeoliteFramework .
    {}
}}""".format(
            "\n    ".join(patterns)
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        models = self.get_zeolite_frameworks_many(
            [binding["Framework"] for binding in bindings]
        )
        return [model for model in models]

    def get_zeolite_frameworks_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntozeoliteZeoliteFrameworkBase, iris)

    def get_zeolitic_materials_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntozeoliteZeoliticMaterialBase, iris)

    def get_quantities_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntocrystalQuantity, iris)

    def get_vectors_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntocrystalMeasureVector, iris)

    def get_matrices_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntocrystalMeasureMatrix, iris)

    def get_atomic_structures_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntocrystalAtomicStructure, iris)

    def get_coordinate_transforms_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntocrystalCoordinateTransformation, iris)

    def get_unit_cells_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntocrystalUnitCell, iris)

    def get_tiled_structures_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntocrystalTiledStructure, iris)

    def get_cbu_all(self):
        query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT DISTINCT ?o
WHERE {{
    ?s zeo:hasCompositeBU/(zeo:hasCage|zeo:hasTCage|zeo:hasChain) ?o
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        return [binding["o"] for binding in bindings]

    def get_sbu_all(self):
        query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

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
