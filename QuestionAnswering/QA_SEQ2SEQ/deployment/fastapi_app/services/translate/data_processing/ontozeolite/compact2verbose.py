from collections import defaultdict
from typing import DefaultDict, List, Tuple

from services.translate.sparql import SparqlQuery
from services.translate.sparql.query_form import SelectClause
from services.translate.sparql.where_clause import WhereClause
from services.translate.sparql.graph_pattern import (
    FilterClause,
    GraphPattern,
    OptionalClause,
    ServicePattern,
    TriplePattern,
)
from services.translate.sparql.constraint import (
    BrackettedExpression,
    NotExistsFunc,
)


class OZCompact2VerboseConverter:
    def __init__(self, ontospecies_endpoint: str):
        self.ontospecies_endpoint = ontospecies_endpoint

    def _try_convert_hasAtomicStructure_triple(
        self, subj: str, pred: str, objs: List[str]
    ):
        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasAtomicStructure ?AtomicStructure .
        """
        if pred != "ocr:hasCrystalInformation/ocr:hasAtomicStructure" or len(objs) != 1:
            return None
        obj = objs[0]

        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasAtomicStructure ?AtomicStructure .
        ?AtomicStructure ocr:hasAtomSite ?AtomSite .
        ?AtomSite ocr:hasFractionalPosition [
            ocr:hasVectorComponent [ ocr:hasComponentLabel "x" ; ocr:hasComponentValue ?FractionalPositionX ] , 
                                   [ ocr:hasComponentLabel "y" ; ocr:hasComponentValue ?FractionalPositionY ] ,
                                   [ ocr:hasComponentLabel "z" ; ocr:hasComponentValue ?FractionalPositionZ ] ;
            om:hasUnit ?FractionalPositionUnit
        ] ;
        ocr:hasCartesianPosition [
            ocr:hasVectorComponent [ ocr:hasComponentLabel "x" ; ocr:hasComponentValue ?CartesianPositionX ] ,
                                   [ ocr:hasComponentLabel "y" ; ocr:hasComponentValue ?CartesianPositionY ] ,
                                   [ ocr:hasComponentLabel "z" ; ocr:hasComponentValue ?CartesianPositionZ ] ;
            om:hasUnit ?CartesianPositionUnit
        ] .
        OPTIONAL {
            ?AtomSite ocr:hasAtomSiteLabel ?AtomSiteLabel .
        }
        """
        patterns = [
            TriplePattern.from_triple(subj, pred, obj),
            TriplePattern.from_triple(obj, "ocr:hasAtomSite", "?AtomSite"),
            TriplePattern(
                "?AtomSite",
                tails=[
                    (
                        "ocr:hasFractionalPosition",
                        '[ ocr:hasVectorComponent [ ocr:hasComponentLabel "x" ; ocr:hasComponentValue ?FractionalPositionX ] , [ ocr:hasComponentLabel "y" ; ocr:hasComponentValue ?FractionalPositionY ] , [ ocr:hasComponentLabel "z" ; ocr:hasComponentValue ?FractionalPositionZ ] ; om:hasUnit ?FractionalPositionUnit ]',
                    ),
                    (
                        "ocr:hasCartesianPosition",
                        ' [ ocr:hasVectorComponent [ ocr:hasComponentLabel "x" ; ocr:hasComponentValue ?CartesianPositionX ] , [ ocr:hasComponentLabel "y" ; ocr:hasComponentValue ?CartesianPositionY ] , [ ocr:hasComponentLabel "z" ; ocr:hasComponentValue ?CartesianPositionZ ] ; om:hasUnit ?CartesianPositionUnit ]',
                    ),
                ],
            ),
            OptionalClause(
                [
                    TriplePattern.from_triple(
                        "?AtomSite", "ocr:hasAtomSiteLabel", "?AtomSiteLabel"
                    )
                ]
            ),
        ]
        vars = [
            "?FractionalPositionX",
            "?FractionalPositionY",
            "?FractionalPositionZ",
            "?FractionalPositionUnit",
            "?CartesianPositionX",
            "?CartesianPositionY",
            "?CartesianPositionZ",
            "?CartesianPositionUnit",
            "?AtomSiteLabel",
        ]
        return vars, patterns, []

    def _try_convert_hasUnitCell_triple(self, subj: str, pred: str, objs: List[str]):
        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasUnitCell ?UnitCell .
        """
        if pred != "ocr:hasCrystalInformation/ocr:hasUnitCell" or len(objs) != 1:
            return None
        obj = objs[0]

        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasUnitCell ?UnitCell .
        ?UnitCell 
            ocr:hasUnitCellLengths [
                ocr:hasVectorComponent [ ocr:hasComponentLabel "a" ; ocr:hasComponentValue ?a ] ,
                                       [ ocr:hasComponentLabel "b" ; ocr:hasComponentValue ?b ] ,
                                       [ ocr:hasComponentLabel "c" ; ocr:hasComponentValue ?c ] ;
                om:hasUnit ?LengthsUnit
            ] ;
            ocr:hasUnitCellAngles [
                ocr:hasVectorComponent [ ocr:hasComponentLabel "alpha" ; ocr:hasComponentValue ?alpha ] ,
                                       [ ocr:hasComponentLabel "beta" ; ocr:hasComponentValue ?beta ] ,
                                       [ ocr:hasComponentLabel "gamma" ; ocr:hasComponentValue ?gamma ] ;
                om:hasUnit ?AnglesUnit 
            ] ;
            ocr:hasUnitCellVolume [
                om:hasNumericalValue ?VolumeNumericalValue ;
                om:hasUnit ?VolumeUnit
            ] .
        OPTIONAL {
            ?UnitCell ocr:hasLatticeSystem ?LatticeSystem .
        }
        OPTIONAL {
            ?UnitCell ocr:hasSymmetryNumber ?SpaceGroupNumber_ITCr .
        }
        """
        patterns = [
            TriplePattern.from_triple(subj, pred, obj),
            TriplePattern(
                obj,
                [
                    (
                        "ocr:hasUnitCellLengths",
                        '[ ocr:hasVectorComponent [ ocr:hasComponentLabel "a" ; ocr:hasComponentValue ?a ] , [ ocr:hasComponentLabel "b" ; ocr:hasComponentValue ?b ] , [ ocr:hasComponentLabel "c" ; ocr:hasComponentValue ?c ] ; om:hasUnit ?LengthsUnit ]',
                    ),
                    (
                        "ocr:hasUnitCellAngles",
                        '[ ocr:hasVectorComponent [ ocr:hasComponentLabel "alpha" ; ocr:hasComponentValue ?alpha ] , [ ocr:hasComponentLabel "beta" ; ocr:hasComponentValue ?beta ] , [ ocr:hasComponentLabel "gamma" ; ocr:hasComponentValue ?gamma ] ; om:hasUnit ?AnglesUnit ]',
                    ),
                    (
                        "ocr:hasUnitCellVolume",
                        "[ om:hasNumericalValue ?VolumeNumericalValue ; om:hasUnit ?VolumeUnit ]",
                    ),
                ],
            ),
            OptionalClause(
                [
                    TriplePattern.from_triple(
                        obj, "ocr:hasLatticeSystem", "?LatticeSystem"
                    )
                ]
            ),
            OptionalClause(
                [
                    TriplePattern.from_triple(
                        obj, "ocr:hasSymmetryNumber", "?SpaceGroupNumber_ITCr"
                    )
                ]
            ),
        ]
        vars = [
            "?a",
            "?b",
            "?c",
            "?LengthsUnit",
            "?alpha",
            "?beta",
            "?gamma",
            "?AnglesUnit",
            "?VolumeNumericalValue",
            "?VolumeUnit",
            "?LatticeSystem",
            "?SpaceGroupNumber_ITCr",
        ]
        return vars, patterns, []

    def _try_convert_hasTiledStructure_triple(
        self, subj: str, pred: str, objs: List[str]
    ):
        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasTiledStructure ?TiledStructure .
        """
        if pred != "ocr:hasCrystalInformation/ocr:hasTiledStructure" or len(objs) != 1:
            return None
        obj = objs[0]

        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasTiledStructure ?TiledStructure .
        ?TiledStructure 
            ocr:hasTileNumber [ 
                ocr:hasValue ?NumberOfTiles ; 
                ocr:isNumberOfTiles [ 
                    ocr:hasTileCode ?TileCode ; 
                    ocr:hasNumberOfEdges ?NumberOfEdges ;
                    ocr:hasNumberOfVertices ?NumberOfVertices ;
                    ocr:hasNumberOfFaces ?NumberOfFaces ;
                    ocr:hasTileFaceNumber [ 
                        ocr:hasValue ?NumberOfTileFaces ;
                        ocr:isNumberOfTileFaces [
                            ocr:hasFaceCode ?FaceCode ;
                            ocr:hasNumberOfEdges ?TileFaceNumberOfEdges 
                        ] 
                    ]
                ]
            ] ;
            ocr:hasTileSignature ?TileSignature ;
            ocr:hasTransitivity/ocr:hasVectorComponent  
                [ ocr:hasComponentLabel "p" ;  ocr:hasComponentValue ?TransitivityP ] , 
                [ ocr:hasComponentLabel "q" ; ocr:hasComponentValue ?TransitivityQ ] , 
                [ ocr:hasComponentLabel "r" ;  ocr:hasComponentValue ?TransitivityR ] , 
                [ ocr:hasComponentLabel "s" ; ocr:hasComponentValue ?TransitivityS ] . 
        """
        patterns = [
            TriplePattern.from_triple(subj, pred, obj),
            TriplePattern(
                obj,
                [
                    (
                        "ocr:hasTileNumber",
                        "[ ocr:hasValue ?NumberOfTiles ; ocr:isNumberOfTiles [ ocr:hasTileCode ?TileCode ; ocr:hasNumberOfEdges ?NumberOfEdges ; ocr:hasNumberOfVertices ?NumberOfVertices ; ocr:hasNumberOfFaces ?NumberOfFaces ; ocr:hasTileFaceNumber [ ocr:hasValue ?NumberOfTileFaces ; ocr:isNumberOfTileFaces [ ocr:hasFaceCode ?FaceCode ; ocr:hasNumberOfEdges ?TileFaceNumberOfEdges ] ] ] ]",
                    ),
                    ("ocr:hasTileSignature", "?TileSignature"),
                    (
                        "ocr:hasTransitivity/ocr:hasVectorComponent",
                        '[ ocr:hasComponentLabel "p" ;  ocr:hasComponentValue ?TransitivityP ] , [ ocr:hasComponentLabel "q" ; ocr:hasComponentValue ?TransitivityQ ] , [ ocr:hasComponentLabel "r" ;  ocr:hasComponentValue ?TransitivityR ] , [ ocr:hasComponentLabel "s" ; ocr:hasComponentValue ?TransitivityS ]',
                    ),
                ],
            ),
        ]
        vars = [
            "?TileCode",
            "?NumberOfTiles",
            "?NumberOfEdges",
            "?NumberOfVertices",
            "?NumberOfFaces",
            "?FaceCode",
            "?NumberOfTileFaces",
            "?TileFaceNumberOfEdges",
            "?TransitivityP",
            "?TransitivityQ",
            "?TransitivityR",
            "?TransitivityS",
            "?TileSignature",
        ]
        return vars, patterns, []

    def _try_convert_hasCoordTrans_triple(self, subj: str, pred: str, objs: List[str]):
        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasCoordinateTransformation [
            ocr:hasTransformationMatrixTo[Cartesian|Fractional] ?TransformationMatrixTo[Cartesian|Fractional] ;
            ocr:hasTransformationVectorTo[Cartesian|Fractional] ?TransformationVectorTo[Cartesian|Fractional]
        ]
        """
        if (
            pred != "ocr:hasCrystalInformation/ocr:hasCoordinateTransformation"
            or len(objs) != 1
        ):
            return None
        obj = objs[0]

        if not (obj.startswith("[") and obj.endswith("]") and ";" in obj):
            return None

        tail_matrix, tail_vector = (x.strip() for x in obj.split(";", maxsplit=1))
        pred_matrix, obj_matrix = tail_matrix.split(maxsplit=1)
        if not (pred_matrix.startswith("ocr:hasTransformationMatrixTo")):
            return None
        pred_vector, obj_vector = tail_vector.split(maxsplit=1)
        if not (pred_vector.startswith("ocr:hasTransformationVectorTo")):
            return None

        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasCoordinateTransformation [ 
            ocr:hasTransformationMatrixTo[Cartesian|Fractional] ?TransformationMatrixTo[Cartesian|Fractional] ; 
            ocr:hasTransformationVectorTo[Cartesian|Fractional] ?TransformationVectorTo[Cartesian|Fractional] 
        ]
        ?TransformationMatrixTo[Cartesian|Fractional] ocr:hasMatrixComponent
            [ ocr:hasComponentLabel "xx" ; ocr:hasComponentValue ?TransformMatrixXX] ,
            ...
        ?TransformationVectorTo[Cartesian|Fractional] ocr:hasVectorComponent
            [ ocr:hasComponentLabel "x" ; ocr:hasComponentValue ?TransformVectorX ] ,
            ...
        """
        patterns = [
            TriplePattern.from_triple(subj, pred, obj),
            TriplePattern.from_triple(
                obj_matrix,
                "ocr:hasMatrixComponent",
                " , ".join(
                    [
                        '[ ocr:hasComponentLabel "{key}" ; ocr:hasComponentValue ?TransformMatrix{value} ]'.format(
                            key=x, value=x.upper()
                        )
                        for x in ["xx", "xy", "xz", "yx", "yy", "yz", "zx", "zy", "zz"]
                    ]
                ),
            ),
            TriplePattern.from_triple(
                obj_vector,
                "ocr:hasVectorComponent",
                " , ".join(
                    [
                        '[ ocr:hasComponentLabel "{key}" ; ocr:hasComponentValue ?TransformVector{value} ]'.format(
                            key=x, value=x.upper()
                        )
                        for x in ["x", "y", "z"]
                    ]
                ),
            ),
        ]
        vars = [
            "?TransformMatrix" + x.upper()
            for x in ["xx", "xy", "xz", "yx", "yy", "yz", "zx", "zy", "zz"]
        ] + ["?TransformVector" + x.upper() for x in ["x", "y", "z"]]
        return vars, patterns, []

    def _try_convert_hasZeoTopoScalar_triple(
        self, subj: str, pred: str, objs: List[str]
    ):
        """
        ?Zeolite zeo:hasTopologicalProperties/zeo:has{key} ?{key} .
        OR
        ?Zeolite zeo:hasTopologicalProperties/zeo:has{key}/om:hasNumericalValue ?{key}NumericalValue
        """
        if (
            not pred.startswith("zeo:hasTopologicalProperties/zeo:has")
            or len(objs) != 1
        ):
            return None
        obj = objs[0]

        key = pred[len("zeo:hasTopologicalProperties/zeo:has") :]
        if key.endswith("/om:hasNumericalValue"):
            key = key[: -len("/om:hasNumericalValue")]
        if not (
            key
            in [
                "AccessibleAreaPerCell",
                "AccessibleAreaPerGram",
                "AccessibleVolume",
                "AccessibleVolumePerCell",
                "OccupiableAreaPerCell",
                "OccupiableAreaPerGram",
                "OccupiableVolume",
                "OccupiableVolumePerCell",
                "SpecificAccessibleArea",
                "SpecificOccupiableArea",
                "Density",
                "FrameworkDensity",
            ]
        ):
            return None

        """
        ?Zeolite zeo:hasTopologicalProperties/zeo:has{key} ?{key}
        ?{key} om:hasNumericalValue ?{key}NumericalValue ;
               om:hasUnit ?{key}Unit
        """
        if pred.endswith("/om:hasNumericalValue"):
            numval = obj
            unitlabel = "?" + key + "Unit"
        else:
            numval = obj + "NumericalValue"
            unitlabel = obj + "Unit"
        patterns = [
            TriplePattern.from_triple(
                subj, "zeo:hasTopologicalProperties/zeo:has" + key, "?" + key
            ),
            TriplePattern(
                "?" + key,
                tails=[
                    ("om:hasNumericalValue", numval),
                    ("om:hasUnit", unitlabel),
                ],
            ),
        ]
        vars = [numval, unitlabel]
        return vars, patterns, []

    def _try_convert_hasTopoDensity_triple(self, subj: str, pred: str, objs: List[str]):
        """
        ?Zeolite zeo:hasTopologicalDensity ?TopologicalDensity .
        """
        if pred != "zeo:hasTopologicalDensity" or len(objs) != 1:
            return None
        obj = objs[0]

        """
        ?Zeolite zeo:hasTopologicalDensity ?TopologicalDensity .
        ?TopologicalDensity zeo:hasValueTD ?ValueTD ;
                            zeo:hasValueTD10 ?ValueTD10 .
        """
        patterns = [
            TriplePattern.from_triple(subj, pred, obj),
            TriplePattern(
                obj,
                tails=[
                    ("zeo:hasValueTD", "?ValueTD"),
                    ("zeo:hasValueTD10", "ValueTD10"),
                ],
            ),
        ]
        vars = ["?ValueTD", "?ValueTD10"]
        return vars, patterns, []

    def _try_convert_hasRingSizes_triple(self, subj: str, pred: str, objs: List[str]):
        """
        ?Zeolite zeo:hasTopologicalProperties/zeo:hasRingSizes ?RingSizes
        """
        if pred != "zeo:hasTopologicalProperties/zeo:hasRingSizes" or len(objs) != 1:
            return None
        obj = objs[0]

        """
        ?Zeolite zeo:hasTopologicalProperties/zeo:hasRingSizes ?RingSizes .
        ?RingSizes ocr:hasVectorComponent [ ocr:hasComponentIndex ?RingSizesIndex ; ocr:hasComponentValue ?RingSizesValue ]
        """
        patterns = [
            TriplePattern.from_triple(subj, pred, obj),
            TriplePattern.from_triple(
                obj,
                "ocr:hasVectorComponent",
                "[ ocr:hasComponentIndex ?RingSizesIndex ; ocr:hasComponentValue ?RingSizesValue ]",
            ),
        ]
        vars = ["?RingSizes", "?RingSizesIndex", "?RingSizesValue"]
        return vars, patterns, []

    def _try_convert_hasCompositeBU_triple(self, subj: str, pred: str, objs: List[str]):
        """
        ?Zeolite zeo:hasCompositeBU ?CompositeBU
        """
        if pred != "zeo:hasCompositeBU" or len(objs) != 1:
            return None
        obj = objs[0]

        """
        ?Zeolite zeo:hasCompositeBU ?CompositeBU .
        ?CompositeBU zeo:hasCage ?Cage ; zeo:hasTCage ?TCage .
        """
        patterns = [
            TriplePattern.from_triple(subj, pred, obj),
            TriplePattern(
                obj,
                tails=[("zeo:hasCage", "?Cage"), ("zeo:hasTCage", "?TCage")],
            ),
        ]
        vars = ["?Cage", "?TCage"]
        return vars, patterns, []

    def _try_convert_hasSphereDiameter_triple(
        self, subj: str, pred: str, objs: List[str]
    ):
        """
        ?Zeolite zeo:hasSphereDiameter ?SphereDiameter .
        """
        if pred != "zeo:hasSphereDiameter" or len(objs) != 1:
            return None
        obj = objs[0]

        """
        ?Zeolite zeo:hasSphereDiameter ?SphereDiameter .
        ?SphereDiameter ocr:hasVectorComponent [ ocr:hasComponentLabel "a" ; ocr:hasComponentValue ?SphereDiameterA ] ... ;
                        om:hasUnit ?SphereDiameterUnit .
        """
        patterns = [
            TriplePattern.from_triple(subj, pred, obj),
            TriplePattern(
                obj,
                tails=[
                    (
                        "ocr:hasVectorComponent",
                        " , ".join(
                            [
                                '[ ocr:hasComponentLabel "{label}" ; ocr:hasComponentValue ?SphereDiameter{value} ]'.format(
                                    label=x, value=x.capitalize()
                                )
                                for x in ["a", "b", "c", "included"]
                            ]
                        ),
                    ),
                    ("om:hasUnit", "?SphereDiameterUnit"),
                ],
            ),
        ]
        vars = [
            "?SphereDiameter" + x.capitalize() for x in ["a", "b", "c", "included"]
        ] + ["?SphereDiameterUnit"]
        return vars, patterns, []

    def _try_convert_hasTAtom_triple(self, subj: str, pred: str, objs: List[str]):
        """
        ?Zeolite zeo:hasTAtom ?TAtom .
        """
        if pred != "zeo:hasTAtom" or len(objs) != 1:
            return None
        obj = objs[0]

        """
        ?Zeolite zeo:hasTAtom ?TAtom .
        ?TAtom zeo:hasTAtomIndex ?TAtomIndex ;
               zeo:hasTAtomName ?TAtomName ;
               zeo:hasVertexSymbol ?VertexSymbol ;
        """
        patterns = [
            TriplePattern.from_triple(subj, pred, obj),
            TriplePattern(
                obj,
                tails=[
                    ("zeo:hasTAtomIndex", "?TAtomIndex"),
                    ("zeo:hasTAtomName", "?TAtomName"),
                    ("zeo:hasVertexSymbol", "?VertexSymbol"),
                ],
            ),
        ]
        vars = ["?TAtomIndex", "?TAtomName", "?VertexSymbol"]
        return vars, patterns, []

    def _try_convert_hasGuestCompoundUngrounded_triple(
        self, subj: str, pred: str, objs: List[str]
    ):
        """
        ?Material zeo:hasGuestCompound/rdfs:label ?GuestCompoundLabel .
        """
        if pred != "zeo:hasGuestCompound/rdfs:label" or len(objs) != 1:
            return None

        obj = objs[0]
        if obj != "?GuestCompoundLabel":
            return None

        """
        ?Material zeo:hasGuestComponent ?Guest .
        SERVICE <{ONTOSPECIES_ENDPOINT}> {
            ?Guest rdfs:label ?GuestLabel
        }
        """
        vars = ["?Guest", "?GuestLabel"]
        triples = [
            TriplePattern.from_triple(subj, "zeo:hasGuestComponent", "?Guest"),
        ]
        ontospecies_triples = [
            TriplePattern.from_triple("?Guest", "rdfs:label", "?GuestLabel")
        ]
        return vars, triples, ontospecies_triples

    def _try_convert_hasGuestCompoundGrounded_triple(
        self, subj: str, pred: str, objs: List[str]
    ):
        """
        ?Material zeo:hasGuestCompound/rdfs:label "H2O" .
        ?Material zeo:hasGuestCompound/rdfs:label "Cl-" .
        """
        if not pred == "zeo:hasGuestCompound/rdfs:label":
            return None

        """
        ?Material zeo:hasGuestComponent ?Guest1 .
        ?Material zeo:hasGuestComponent ?Guest2 .
        SERVICE <{ONTOSPECIES_ENDPOINT}> {
            ?Guest1 ?hasIdentifier1 [ a/rdfs:subClassOf os:Identifier ; os:value "H2O" ] .
            ?Guest2 ?hasIdentifier2 [ a/rdfs:subClassOf os:Identifier ; os:value "Cl-" ] .
        } 
        """
        vars = []
        triples = []
        ontospecies_triples = []

        for i, obj in enumerate(objs):
            varnode = "?Guest" + str(i + 1)
            vars.append(varnode)
            triples.append(
                TriplePattern.from_triple(subj, "zeo:hasGuestComponent", varnode)
            )
            ontospecies_triples.append(
                TriplePattern.from_triple(
                    varnode,
                    "?hasIdentifier" + str(i + 1),
                    "[ a/rdfs:subClassOf os:Identifier ; os:value {value} ]".format(
                        value=obj
                    ),
                )
            )

        return vars, triples, ontospecies_triples

    def _try_convert_hasFrameworkComponentUngrounded_triple(
        self, subj: str, pred: str, objs: List[str]
    ):
        """
        ?Material zeo:hasFrameworkComponent/rdfs:label ?FrameworkComponentLabel .
        """
        if pred != "zeo:hasFrameworkComponent/rdfs:label" or len(objs) != 1:
            return None

        obj = objs[0]
        if obj != "?FrameworkComponentLabel":
            return None

        """
        ?Material zeo:hasFrameworkComponent ?Element .
        SERVICE <ontospecies_endpoint> {
            ?Element rdfs:label ?FrameworkComponentLabel .
        }
        """
        vars = ["?Element"]
        triples = [
            TriplePattern.from_triple(subj, "zeo:hasFrameworkComponent", "?Element")
        ]
        ontospecies_triples = [TriplePattern.from_triple("?Element", "rdfs:label", obj)]
        return vars, triples, ontospecies_triples

    def _try_convert_hasFrameworkComponentGrounded_triple(
        self, subj: str, pred: str, objs: List[str]
    ):
        """
        ?Material zeo:hasFrameworkComponent/rdfs:label "Si" .
        ?Material zeo:hasFrameworkComponent/rdfs:label "Al" .
        """
        if pred not in [
            "zeo:hasFrameworkComponent/rdfs:label",
            "zeo:hasFrameworkComponentOnly/rdfs:label",
        ]:
            return None

        """
        ?Material zeo:hasFrameworkComponent ?Element1 .
        ?Material zeo:hasFrameworkComponent ?Element2 .

        SERVICE <ontospecies_endpoint> {
            ?Element1 (os:hasElementSymbol|os:hasElementName)/os:value "Si" .
            ?Element2 (os:hasElementSymbol|os:hasElementName)/os:value "Al" .
            
            # if ONLY
            ?Oxygen os:hasElementSymbol/os:value "O" .
        }
        
        # if ONLY
        FILTER NOT EXISTS {
            ?Material zeo:hasFrameworkComponent ?ElementExclude .
            FILTER ( ?ElementExclude NOT IN ( ?Element1, ?Element2, ?Oxygen ) )
        }
        """
        vars = []
        triples = []
        ontospecies_triples = []

        for i, obj in enumerate(objs):
            varnode = "?Element" + str(i + 1)
            vars.append(varnode)
            triples.append(
                TriplePattern.from_triple(subj, "zeo:hasFrameworkComponent", varnode)
            )
            ontospecies_triples.append(
                TriplePattern.from_triple(
                    varnode, "(os:hasElementSymbol|os:hasElementName)/os:value", obj
                )
            )

        if pred == "zeo:hasFrameworkComponentOnly/rdfs:label":
            ontospecies_triples.append(
                TriplePattern.from_triple(
                    "?Oxygen", "os:hasElementSymbol/os:value", '"O"'
                )
            )
            triples.append(
                FilterClause(
                    NotExistsFunc(
                        [
                            TriplePattern.from_triple(
                                subj, "zeo:hasFrameworkComponent", "?ElementExclude"
                            ),
                            FilterClause(
                                BrackettedExpression(
                                    "?ElementExclude NOT IN ( {elements}, ?Oxygen )".format(
                                        elements=", ".join(vars)
                                    )
                                )
                            ),
                        ]
                    )
                )
            )

        return vars, triples, ontospecies_triples

    def convert(self, sparql_compact: SparqlQuery):
        select_vars_verbose = list(sparql_compact.select_clause.vars)
        patterns_verbose = []
        ontospecies_patterns = []

        if "?Framework" in sparql_compact.select_clause.vars and not any(
            pred == "zeo:hasFrameworkCode"
            and obj.startswith('"')
            and obj.startswith('"')
            for pattern in sparql_compact.where_clause.graph_patterns
            if isinstance(pattern, TriplePattern)
            for pred, obj in pattern.tails
        ):
            select_vars_verbose.append("?FrameworkCode")
            patterns_verbose.append(
                TriplePattern.from_triple(
                    "?Framework", "zeo:hasFrameworkCode", "?FrameworkCode"
                )
            )
        elif "?Material" in sparql_compact.select_clause.vars and not any(
            pred == "zeo:hasChemicalFormula"
            and obj.startswith('"')
            and obj.startswith('"')
            for pattern in sparql_compact.where_clause.graph_patterns
            if isinstance(pattern, TriplePattern)
            for pred, obj in pattern.tails
        ):
            select_vars_verbose.append("?ChemicalFormula")
            patterns_verbose.append(
                TriplePattern.from_triple(
                    "?Material", "zeo:hasChemicalFormula", "?ChemicalFormula"
                )
            )

        subjPredLit2Objs: DefaultDict[Tuple[str, str, bool], List[str]] = defaultdict(
            list
        )
        nontriples: List[GraphPattern] = []
        for pattern in sparql_compact.where_clause.graph_patterns:
            if isinstance(pattern, TriplePattern):
                for pred, obj in pattern.tails:
                    is_literal = obj.startswith('"') and obj.endswith('"')
                    subjPredLit2Objs[(pattern.subj, pred, is_literal)].append(obj)
            else:
                nontriples.append(pattern)

        for (subj, pred, is_literal), objs in subjPredLit2Objs.items():
            if is_literal:
                funcs = [
                    self._try_convert_hasGuestCompoundGrounded_triple,
                    self._try_convert_hasFrameworkComponentGrounded_triple,
                ]
            else:
                funcs = [
                    self._try_convert_hasAtomicStructure_triple,
                    self._try_convert_hasUnitCell_triple,
                    self._try_convert_hasTiledStructure_triple,
                    self._try_convert_hasCoordTrans_triple,
                    self._try_convert_hasZeoTopoScalar_triple,
                    self._try_convert_hasTopoDensity_triple,
                    self._try_convert_hasRingSizes_triple,
                    self._try_convert_hasCompositeBU_triple,
                    self._try_convert_hasSphereDiameter_triple,
                    self._try_convert_hasTAtom_triple,
                    self._try_convert_hasGuestCompoundUngrounded_triple,
                    self._try_convert_hasFrameworkComponentUngrounded_triple,
                ]

            flag = False
            for func in funcs:
                optional = func(subj, pred, objs)
                if optional is not None:
                    vars, patterns, os_patterns = optional

                    select_vars_verbose.extend(vars)
                    patterns_verbose.extend(patterns)
                    ontospecies_patterns.extend(os_patterns)

                    flag = True
                    break

            if not flag:
                patterns_verbose.extend(
                    [TriplePattern.from_triple(subj, pred, obj) for obj in objs]
                )

        patterns_verbose.extend(nontriples)

        if ontospecies_patterns:
            patterns_verbose.append(
                ServicePattern(
                    endpoint=self.ontospecies_endpoint,
                    graph_patterns=ontospecies_patterns,
                )
            )

        return SparqlQuery(
            select_clause=SelectClause(
                solution_modifier="DISTINCT", vars=select_vars_verbose
            ),
            where_clause=WhereClause(patterns_verbose),
            solution_modifier=sparql_compact.solution_modifier,
        )
