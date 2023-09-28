SPECIES_FROMHEAD_PATTERN_COMPACT_PREFIX = "?SpeciesIRI ?hasIdentifier ?species"
ONTOSPECIES_PATTERN_COMPACT_PREFIX = "?SpeciesIRI os:has"
CHEMCLASS_PATTERN_COMPACT_PREFIX = "?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue"
USE_PATTERN_COMPACT_PREFIX = "?SpeciesIRI os:hasUse ?UseValue"
ALL_PROPERTIES_PATTERN_COMPACT_PREFIX = (
    "?SpeciesIRI ?hasPropertyName ?PropertyNameValue"
)
ALL_IDENTIFIERS_PATTERN_COMPACT_PREFIX = (
    "?SpeciesIRI ?hasIdentifierName ?IdentifierNameValue"
)

SPECIES_FROMTAIL_PATTERN_COMPACT_PREFIX = "?SpeciesIRI os:hasIUPACName ?IUPACNameValue"

SPECIES_FROMHEAD_PATTERNS_VERBOSE = """
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; ?hasIdentifier ?IdentifierIRI .
    ?IdentifierIRI rdf:type ?Identifier ; os:value ?species .
    ?Identifier rdfs:subClassOf os:Identifier .
"""
PROPERTY_PATTERNS_VERBOSE = (
    lambda PropertyName: f"""
    ?SpeciesIRI os:has{PropertyName} ?{PropertyName}IRI .
    ?{PropertyName}IRI os:value ?{PropertyName}Value ; os:unit ?{PropertyName}UnitIRI ; os:hasProvenance ?{PropertyName}ProvenanceIRI . 
    ?{PropertyName}UnitIRI rdfs:label ?{PropertyName}UnitValue .
    OPTIONAL {{
        ?{PropertyName}IRI os:hasReferenceState ?{PropertyName}ReferenceStateIRI .
        ?{PropertyName}ReferenceStateIRI os:value ?{PropertyName}ReferenceStateValue ; os:unit ?{PropertyName}ReferenceStateUnitIRI .
        ?{PropertyName}ReferenceStateUnitIRI rdfs:label ?{PropertyName}ReferenceStateUnitValue .
    }}
"""
)
IDENTIFIER_PATTERNS_VERBOSE = (
    lambda IdentifierName: f"""
    ?SpeciesIRI os:has{IdentifierName} ?{IdentifierName}IRI .
    ?{IdentifierName}IRI os:value ?{IdentifierName}Value .
"""
)

CHEMCLASS_PATTERNS_VERBOSE = (
    lambda i: f"""
    ?SpeciesIRI os:hasChemicalClass* ?x{i} .
    ?x{i} ?y{i} ?z{i} .
    ?z{i} rdfs:subClassOf* ?ChemicalClassIRI{i} .
    ?ChemicalClassIRI{i} rdf:type os:ChemicalClass ; rdfs:label ?ChemicalClassValue{i} .
"""
)
USE_PATTERNS_VERBOSE = (
    lambda i: f"""
    ?SpeciesIRI os:hasUse ?UseIRI{i} .
    ?UseIRI{i} rdfs:label ?UseValue{i} .
"""
)
ALL_PROPERTIES_PATTERNS_VERBOSE = """
    ?SpeciesIRI ?hasPropertyName ?PropertyNameIRI .
    ?PropertyNameIRI rdf:type ?PropertyName .
    ?PropertyName rdfs:subClassOf os:Property .
    ?PropertyNameIRI os:value ?PropertyNameValue ; os:unit ?PropertyNameUnitIRI ; os:hasProvenance ?PropertyNameProvenanceIRI .
    ?PropertyNameUnitIRI rdfs:label ?PropertyNameUnitValue .
    OPTIONAL {
        ?PropertyNameIRI os:hasReferenceState ?PropertyNameReferenceStateIRI .
        ?PropertyNameReferenceStateIRI os:value ?PropertyNameReferenceStateValue ; os:unit ?PropertyNameReferenceStateUnitIRI .
        ?PropertyNameReferenceStateUnitIRI rdfs:label ?PropertyNameReferenceStateUnitValue .
    }

    BIND(strafter(str(?PropertyName),'#') AS ?PropertyLabel)
"""
ALL_IDENTIFIERS_PATTERNS_VERBOSE = """
    ?SpeciesIRI ?hasIdentifierName ?IdentifierNameIRI .
    ?IdentifierNameIRI rdf:type ?IdentifierName .
    ?IdentifierName rdfs:subClassOf os:Identifier .
    ?IdentifierNameIRI os:value ?IdentifierNameValue .

    BIND(strafter(str(?IdentifierName),'#') AS ?IdentifierLabel)
"""

SPECIES_FROMTAIL_PATTERNS_VERBOSE = """
    ?SpeciesIRI rdf:type os:Species ; rdfs:label ?label ; os:hasIUPACName ?IUPACNameIRI .
    ?IUPACNameIRI os:value ?IUPACNameValue .
"""

PROPERTY_NAMES = [
    "AtomChiralCount",
    "AtomChiralDefCount",
    "AtomChiralUndefCount",
    "BondChiralCount",
    "BondChiralDefCount",
    "BondChiralUndefCount",
    "CanonicalizedCompound",
    "Charge",
    "CompoundComplexity",
    "CovalentUnitCount",
    "ExactMass",
    "HeavyAtomCount",
    "HydrogenBondAcceptorCount",
    "HydrogenBondDonorCount",
    "IsotopeAtomCount",
    "MolecularWeight",
    "MonoIsotopicWeight",
    "RotatableBondCount",
    "SubStructureKeysFingerprint",
    "TautomerCount",
    "XLogP3",
    "AutoignitionTemperature",
    "Caco2Permeability",
    "CollisionCrossSection",
    "Hydrophobicity",
    "IonizationPotential",
    "IsoelectricPoint",
    "LogP",
    "LogS",
    "PolarSurfaceArea",
    "BoilingPoint",
    "Density",
    "DissociationConstants",
    "EnthalpyOfSublimation",
    "FlashPoint",
    "StandardEnthalpyOfFormation",
    "HeatOfCombustion",
    "HeatOfVaporization",
    "HenrysLawConstant",
    "MeltingPoint",
    "OpticalRotation",
    "Solubility",
    "SurfaceTension",
    "VaporDensity",
    "VaporPressure",
    "Viscosity",
]
PROPERTY_NAMES_SET = set(PROPERTY_NAMES)

IDENTIFIER_NAMES = [
    "ChebiID",
    "CID",
    "EmpiricalFormula",
    "InChI",
    "InChIKey",
    "IUPACName",
    "MolecularFormula",
    "SMILES",
]
IDENTIFIER_NAMES_SET = set(IDENTIFIER_NAMES)
