from services.translate.sparql.query_form import SelectClause
from services.translate.sparql.where_clause import WhereClause
from services.translate.sparql import SparqlQuery
from services.translate.sparql.graph_pattern import (
    OptionalClause,
    TriplePattern,
)


class OZCompact2VerboseConverter:
    def _try_convert_hasAtomicStructure_triple(self, subj: str, pred: str, obj: str):
        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasAtomicStructure ?AtomicStructure .
        """
        if not (pred == "ocr:hasCrystalInformation/ocr:hasAtomicStructure"):
            return None

        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasAtomicStructure ?AtomicStructure .
        ?AtomicStructure ocr:hasAtomSite ?AtomSite .
        ?AtomSite ocr:hasFractionalPosition [
            ocr:hasVectorComponent [ ocr:hasComponentLabel "x" ; ocr:hasComponentValue ?FractionalPositionX ] , 
                                   [ ocr:hasComponentLabel "y" ; ocr:hasComponentValue ?FractionalPositionY ] ,
                                   [ ocr:hasComponentLabel "z" ; ocr:hasComponentValue ?FractionalPositionZ ] ;
            om:hasUnit/rdfs:label ?FractionalPositionUnitLabel
        ] ;
        ocr:hasCartesianPosition [
            ocr:hasVectorComponent [ ocr:hasComponentLabel "x" ; ocr:hasComponentValue ?CartesianPositionX ] ,
                                   [ ocr:hasComponentLabel "y" ; ocr:hasComponentValue ?CartesianPositionY ] ,
                                   [ ocr:hasComponentLabel "z" ; ocr:hasComponentValue ?CartesianPositionZ ] ;
            om:hasUnit/rdfs:label ?CartesianPositionUnitLabel
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
                        '[ ocr:hasVectorComponent [ ocr:hasComponentLabel "x" ; ocr:hasComponentValue ?FractionalPositionX ] , [ ocr:hasComponentLabel "y" ; ocr:hasComponentValue ?FractionalPositionY ] , [ ocr:hasComponentLabel "z" ; ocr:hasComponentValue ?FractionalPositionZ ] ; om:hasUnit/rdfs:label ?FractionalPositionUnitLabel ]',
                    ),
                    (
                        "ocr:hasCartesianPosition",
                        ' [ ocr:hasVectorComponent [ ocr:hasComponentLabel "x" ; ocr:hasComponentValue ?CartesianPositionX ] , [ ocr:hasComponentLabel "y" ; ocr:hasComponentValue ?CartesianPositionY ] , [ ocr:hasComponentLabel "z" ; ocr:hasComponentValue ?CartesianPositionZ ] ; om:hasUnit/rdfs:label ?CartesianPositionUnitLabel ]',
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
            "?FractionalPositionUnitLabel",
            "?CartesianPositionX",
            "?CartesianPositionY",
            "?CartesianPositionZ",
            "?CartesianPositionUnitLabel",
            "?AtomSiteLabel",
        ]
        return vars, patterns

    def _try_convert_hasUnitCell_triple(self, subj: str, pred: str, obj: str):
        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasUnitCell ?UnitCell .
        """
        if not (pred == "ocr:hasCrystalInformation/ocr:hasUnitCell"):
            return None

        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasUnitCell ?UnitCell .
        ?UnitCell 
            ocr:hasUnitCellLengths [
                ocr:hasVectorComponent [ ocr:hasComponentLabel "a" ; ocr:hasComponentValue ?a ] ,
                                       [ ocr:hasComponentLabel "b" ; ocr:hasComponentValue ?b ] ,
                                       [ ocr:hasComponentLabel "c" ; ocr:hasComponentValue ?c ] ;
                om:hasUnit/rdfs:label ?LengthsUnitLabel
            ] ;
            ocr:hasUnitCellAngles [
                ocr:hasVectorComponent [ ocr:hasComponentLabel "alpha" ; ocr:hasComponentValue ?alpha ] ,
                                       [ ocr:hasComponentLabel "beta" ; ocr:hasComponentValue ?beta ] ,
                                       [ ocr:hasComponentLabel "gamma" ; ocr:hasComponentValue ?gamma ] ;
                om:hasUnit/rdfs:label ?AnglesUnitLabel 
            ] ;
            ocr:hasUnitCellVolume [
                om:hasNumericalValue ?VolumeNumericalValue ;
                om:hasUnit/rdfs:label ?VolumeUnitLabel
            ] .
        OPTIONAL {
            ?UnitCell ocr:hasLatticeSystem ?LatticeSystem .
        }
        OPTIONAL {
            ?UnitCell ocr:hasSymmetryNumber ?SymmetryNumber .
        }
        """
        patterns = [
            TriplePattern.from_triple(subj, pred, obj),
            TriplePattern(
                obj,
                [
                    (
                        "ocr:hasUnitCellLengths",
                        '[ ocr:hasVectorComponent [ ocr:hasComponentLabel "a" ; ocr:hasComponentValue ?a ] , [ ocr:hasComponentLabel "b" ; ocr:hasComponentValue ?b ] , [ ocr:hasComponentLabel "c" ; ocr:hasComponentValue ?c ] ; om:hasUnit/rdfs:label ?LengthsUnitLabel ]',
                    ),
                    (
                        "ocr:hasUnitCellAngles",
                        '[ ocr:hasVectorComponent [ ocr:hasComponentLabel "alpha" ; ocr:hasComponentValue ?alpha ] , [ ocr:hasComponentLabel "beta" ; ocr:hasComponentValue ?beta ] , [ ocr:hasComponentLabel "gamma" ; ocr:hasComponentValue ?gamma ] ; om:hasUnit/rdfs:label ?AnglesUnitLabel ]',
                    ),
                    (
                        "ocr:hasUnitCellVolume",
                        "[ om:hasNumericalValue ?VolumeNumericalValue ; om:hasUnit/rdfs:label ?VolumeUnitLabel ]",
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
                        obj, "ocr:hasSymmetryNumber", "?SymmetryNumber"
                    )
                ]
            ),
        ]
        vars = [
            "?a",
            "?b",
            "?c",
            "?LengthsUnitLabel",
            "?alpha",
            "?beta",
            "?gamma",
            "?AnglesUnitLabel",
            "?VolumeNumericalValue",
            "?VolumeUnitLabel",
            "?LatticeSystem",
            "?SymmetryNumber",
        ]
        return vars, patterns

    def _try_convert_hasTiledStructure_triple(self, subj: str, pred: str, obj: str):
        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasTiledStructure ?TiledStructure .
        """
        if not (pred == "ocr:hasCrystalInformation/ocr:hasTiledStructure"):
            return None

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
                        ocr:isNumberOfTileFaces/ocr:hasNumberOfEdges ?TileFaceNumberOfEdges ; 
                    ]
                ] ;
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
                        "[ ocr:hasValue ?NumberOfTiles ; ocr:isNumberOfTiles [ ocr:hasTileCode ?TileCode ; ocr:hasNumberOfEdges ?NumberOfEdges ; ocr:hasNumberOfVertices ?NumberOfVertices ; ocr:hasNumberOfFaces ?NumberOfFaces ; ocr:hasTileFaceNumber [ ocr:hasValue ?NumberOfTileFaces ; ocr:isNumberOfTileFaces/ocr:hasNumberOfEdges ?TileFaceNumberOfEdges ; ] ] ; ]",
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
            "?NumberOfTileFaces",
            "?TileFaceNumberOfEdges",
            "?TransitivityP",
            "?TransitivityQ",
            "?TransitivityR",
            "?TransitivityS",
            "?TileSignature",
        ]
        return vars, patterns

    def _try_convert_hasCoordTrans_triple(self, subj: str, pred: str, obj: str):
        """
        ?Zeolite ocr:hasCrystalInformation/ocr:hasCoordinateTransformation [
            ocr:hasTransformationMatrixTo[Cartesian|Fractional] ?TransformationMatrixTo[Cartesian|Fractional] ;
            ocr:hasTransformationVectorTo[Cartesian|Fractional] ?TransformationVectorTo[Cartesian|Fractional]
        ]
        """
        if not (pred == "ocr:hasCrystalInformation/ocr:hasCoordinateTransformation"):
            return None

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
        return vars, patterns

    def _try_convert_hasZeoTopoScalar_triple(self, subj: str, pred: str, obj: str):
        """
        ?Zeolite zeo:hasFrameworkTopology/zeo:has{key} ?{key}
        """
        if not (pred.startswith("zeo:hasFrameworkTopology/zeo:has")):
            return None

        key = pred[len("zeo:hasFrameworkTopology/zeo:has") :]
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
        ?Zeolite zeo:hasFrameworkTopology/zeo:has{key} ?{key}
        ?{key} om:hasNumericalValue ?{key}NumericalValue ;
               om:hasUnit/rdfs:label ?{key}UnitLabel
        """
        numval = obj + "NumericalValue"
        unitlabel = obj + "UnitLabel"
        patterns = [
            TriplePattern.from_triple(subj, pred, obj),
            TriplePattern(
                obj,
                tails=[
                    ("om:hasNumericalValue", numval),
                    ("om:hasUnit/rdfs:label", unitlabel),
                ],
            ),
        ]
        vars = [numval, unitlabel]
        return vars, patterns

    def _try_convert_hasTopoDensity_triple(self, subj: str, pred: str, obj: str):
        """
        ?Zeolite zeo:hasTopologicalDensity ?TopologicalDensity .
        """
        if not (pred == "zeo:hasTopologicalDensity"):
            return None

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
        return vars, patterns

    def _try_convert_hasRingSizes_triple(self, subj: str, pred: str, obj: str):
        """
        ?Zeolite zeo:hasZeoliteTopology/zeo:hasRingSizes ?RingSizes
        """
        if not (pred == "zeo:hasZeoliteTopology/zeo:hasRingSizes"):
            return None

        """
        ?Zeolite zeo:hasZeoliteTopology/zeo:hasRingSizes ?RingSizes .
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
        return vars, patterns

    def _try_convert_hasCompositeBU_triple(self, subj: str, pred: str, obj: str):
        """
        ?Zeolite zeo:hasCompositeBU ?CompositeBU
        """
        if not (pred == "zeo:hasCompositeBU"):
            return None

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
        return vars, patterns

    def _try_convert_hasSphereDiameter_triple(self, subj: str, pred: str, obj: str):
        """
        ?Zeolite zeo:hasSphereDiameter ?SphereDiameter .
        """
        if not (pred == "zeo:hasSphereDiameter"):
            return None
        """
        ?Zeolite zeo:hasSphereDiameter ?SphereDiameter .
        ?SphereDiameter ocr:hasVectorComponent [ ocr:hasComponentLabel "a" ; ocr:hasComponentValue ?SphereDiameterA ] ... ;
                        om:hasUnit/rdfs:label ?SphereDiameterUnitLabel .
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
                    ("om:hasUnit/rdfs:label", "?SphereDiameterUnitLabel"),
                ],
            ),
        ]
        vars = [
            "?SphereDiameter" + x.capitalize() for x in ["a", "b", "c", "included"]
        ] + ["?SphereDiameterUnitLabel"]
        return vars, patterns

    def _try_convert_hasTAtom_triple(self, subj: str, pred: str, obj: str):
        """
        ?Zeolite zeo:hasTAtom ?TAtom .
        """
        if not (pred == "zeo:hasTAtom"):
            return None

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
        return vars, patterns

    def convert(self, sparql_compact: SparqlQuery):
        select_vars_verbose = list(sparql_compact.select_clause.vars)
        patterns_verbose = []

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
        for pattern in sparql_compact.where_clause.graph_patterns:
            if not (isinstance(pattern, TriplePattern) and len(pattern.tails) == 1):
                patterns_verbose.append(pattern)
                continue

            flag = False
            for func in [
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
            ]:
                optional = func(pattern.subj, *pattern.tails[0])
                if optional is not None:
                    vars, patterns = optional
                    select_vars_verbose.extend(vars)
                    patterns_verbose.extend(patterns)
                    flag = True
                    break
            if not flag:
                patterns_verbose.append(pattern)

        return SparqlQuery(
            select_clause=SelectClause(
                solution_modifier="DISTINCT", vars=select_vars_verbose
            ),
            where_clause=WhereClause(patterns_verbose),
            solution_modifier=sparql_compact.solution_modifier,
        )
