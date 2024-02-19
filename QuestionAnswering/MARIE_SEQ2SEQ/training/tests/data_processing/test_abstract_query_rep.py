import pytest

from core.data_processing.abstract_query_rep import AbstractQueryRep


class TestQueryParser:
    @pytest.mark.parametrize(
        "query_string, expected",
        [
            (
                (
                    "SELECT DISTINCT ?MolecularWeightValue\n"
                    "WHERE {\n"
                    "    ?SpeciesIRI ?hasIdentifier ?species .\n"
                    '    FILTER( ?species = "C22H29FO4")\n'
                    "    ?SpeciesIRI os:hasMolecularWeight ?MolecularWeightValue .\n"
                    "}"
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?MolecularWeightValue",
                    where_clause=[
                        "?SpeciesIRI ?hasIdentifier ?species .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasMolecularWeight ?MolecularWeightValue .",
                    ],
                ),
            ),
            (
                "SELECT DISTINCT ?IUPACNameValue\n"
                "WHERE {\n"
                "    ?SpeciesIRI ?hasIdentifier ?species.\n "
                '    FILTER( ?species = "InChI=1S/C6H8O3/c1-6(2)3-9-5(8)4(6)7/h3H2,1-2H3")'
                "    ?SpeciesIRI os:hasIUPACName ?IUPACNameValue."
                "}",
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?IUPACNameValue",
                    where_clause=[
                        "?SpeciesIRI ?hasIdentifier ?species.",
                        'FILTER( ?species = "InChI=1S/C6H8O3/c1-6(2)3-9-5(8)4(6)7/h3H2,1-2H3")',
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameValue.",
                    ],
                ),
            ),
        ],
    )
    def test_fromString(self, query_string, expected):
        assert AbstractQueryRep.from_string(query_string) == expected

    def test_toQueryString(self):
        query = AbstractQueryRep(
            result_clause="SELECT DISTINCT ?label ?IUPACNameValue ?MolecularWeightValue ?MolecularWeightUnitValue ?MolecularWeightReferenceStateValue ?MolecularWeightReferenceStateUnitValue ?ViscosityValue ?ViscosityUnitValue ?ViscosityReferenceStateValue ?ViscosityReferenceStateUnitValue",
            where_clause=[
                "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                "?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .",
                "?IUPACNameIRI os:value ?IUPACNameValue .",
                "?SpeciesIRI os:hasMolecularWeight ?MolecularWeightIRI .",
                "?MolecularWeightIRI os:value ?MolecularWeightValue ; os:unit ?MolecularWeightUnitIRI ; os:hasProvenance ?MolecularWeightProvenanceIRI .",
                "?MolecularWeightUnitIRI rdfs:label ?MolecularWeightUnitValue .",
                (
                    "OPTIONAL{?MolecularWeightIRI os:hasReferenceState ?MolecularWeightReferenceStateIRI .\n"
                    "?MolecularWeightReferenceStateIRI os:value ?MolecularWeightReferenceStateValue ; os:unit ?MolecularWeightReferenceStateUnitIRI .\n"
                    "?MolecularWeightReferenceStateUnitIRI rdfs:label ?MolecularWeightReferenceStateUnitValue .}"
                ),
                "FILTER(?MolecularWeightValue < 252)",
                "?SpeciesIRI os:hasViscosity ?ViscosityIRI .",
                "?ViscosityIRI os:value ?ViscosityValue ; os:unit ?ViscosityUnitIRI ; os:hasProvenance ?ViscosityProvenanceIRI .",
                "?ViscosityUnitIRI rdfs:label ?ViscosityUnitValue .",
                (
                    "OPTIONAL{?ViscosityIRI os:hasReferenceState ?ViscosityReferenceStateIRI .\n"
                    "?ViscosityReferenceStateIRI os:value ?ViscosityReferenceStateValue ; os:unit ?ViscosityReferenceStateUnitIRI .\n"
                    "?ViscosityReferenceStateUnitIRI rdfs:label ?ViscosityReferenceStateUnitValue .}"
                ),
                "FILTER(?ViscosityValue < 54 || ?ViscosityValue > 87)",
            ],
        )
        expected = """SELECT DISTINCT ?label ?IUPACNameValue ?MolecularWeightValue ?MolecularWeightUnitValue ?MolecularWeightReferenceStateValue ?MolecularWeightReferenceStateUnitValue ?ViscosityValue ?ViscosityUnitValue ?ViscosityReferenceStateValue ?ViscosityReferenceStateUnitValue
WHERE {
?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .
?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .
?IUPACNameIRI os:value ?IUPACNameValue .
?SpeciesIRI os:hasMolecularWeight ?MolecularWeightIRI .
?MolecularWeightIRI os:value ?MolecularWeightValue ; os:unit ?MolecularWeightUnitIRI ; os:hasProvenance ?MolecularWeightProvenanceIRI .
?MolecularWeightUnitIRI rdfs:label ?MolecularWeightUnitValue .
OPTIONAL{?MolecularWeightIRI os:hasReferenceState ?MolecularWeightReferenceStateIRI .
?MolecularWeightReferenceStateIRI os:value ?MolecularWeightReferenceStateValue ; os:unit ?MolecularWeightReferenceStateUnitIRI .
?MolecularWeightReferenceStateUnitIRI rdfs:label ?MolecularWeightReferenceStateUnitValue .}
FILTER(?MolecularWeightValue < 252)
?SpeciesIRI os:hasViscosity ?ViscosityIRI .
?ViscosityIRI os:value ?ViscosityValue ; os:unit ?ViscosityUnitIRI ; os:hasProvenance ?ViscosityProvenanceIRI .
?ViscosityUnitIRI rdfs:label ?ViscosityUnitValue .
OPTIONAL{?ViscosityIRI os:hasReferenceState ?ViscosityReferenceStateIRI .
?ViscosityReferenceStateIRI os:value ?ViscosityReferenceStateValue ; os:unit ?ViscosityReferenceStateUnitIRI .
?ViscosityReferenceStateUnitIRI rdfs:label ?ViscosityReferenceStateUnitValue .}
FILTER(?ViscosityValue < 54 || ?ViscosityValue > 87)
}"""
        assert query.to_query_string() == expected

    @pytest.mark.parametrize(
        "compact_query, expected",
        [
            # species -> 1 property
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?MolecularWeightValue",
                    where_clause=[
                        "?SpeciesIRI ?hasIdentifier ?species .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasPropertyMolecularWeight ?MolecularWeightValue .",
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?MolecularWeightValue ?MolecularWeightUnitValue ?MolecularWeightReferenceStateValue ?MolecularWeightReferenceStateUnitValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI ?hasIdentifier ?IdentifierIRI .",
                        "?IdentifierIRI rdf:type ?Identifier ; os:value ?species .",
                        "?Identifier rdfs:subClassOf os:Identifier .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasMolecularWeight ?MolecularWeightIRI .",
                        "?MolecularWeightIRI os:value ?MolecularWeightValue ; os:unit ?MolecularWeightUnitIRI ; os:hasProvenance ?MolecularWeightProvenanceIRI .",
                        "?MolecularWeightUnitIRI rdfs:label ?MolecularWeightUnitValue .",
                        (
                            "OPTIONAL{?MolecularWeightIRI os:hasReferenceState ?MolecularWeightReferenceStateIRI .\n"
                            "?MolecularWeightReferenceStateIRI os:value ?MolecularWeightReferenceStateValue ; os:unit ?MolecularWeightReferenceStateUnitIRI .\n"
                            "?MolecularWeightReferenceStateUnitIRI rdfs:label ?MolecularWeightReferenceStateUnitValue .}"
                        ),
                    ],
                ),
            ),
            # species -> 2 properties
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?MolecularWeightValue ?ViscosityValue",
                    where_clause=[
                        "?SpeciesIRI ?hasIdentifier ?species .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasPropertyMolecularWeight ?MolecularWeightValue .",
                        "?SpeciesIRI os:hasPropertyViscosity ?ViscosityValue .",
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?MolecularWeightValue ?MolecularWeightUnitValue ?MolecularWeightReferenceStateValue ?MolecularWeightReferenceStateUnitValue ?ViscosityValue ?ViscosityUnitValue ?ViscosityReferenceStateValue ?ViscosityReferenceStateUnitValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI ?hasIdentifier ?IdentifierIRI .",
                        "?IdentifierIRI rdf:type ?Identifier ; os:value ?species .",
                        "?Identifier rdfs:subClassOf os:Identifier .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasMolecularWeight ?MolecularWeightIRI .",
                        "?MolecularWeightIRI os:value ?MolecularWeightValue ; os:unit ?MolecularWeightUnitIRI ; os:hasProvenance ?MolecularWeightProvenanceIRI .",
                        "?MolecularWeightUnitIRI rdfs:label ?MolecularWeightUnitValue .",
                        (
                            "OPTIONAL{?MolecularWeightIRI os:hasReferenceState ?MolecularWeightReferenceStateIRI .\n"
                            "?MolecularWeightReferenceStateIRI os:value ?MolecularWeightReferenceStateValue ; os:unit ?MolecularWeightReferenceStateUnitIRI .\n"
                            "?MolecularWeightReferenceStateUnitIRI rdfs:label ?MolecularWeightReferenceStateUnitValue .}"
                        ),
                        "?SpeciesIRI os:hasViscosity ?ViscosityIRI .",
                        "?ViscosityIRI os:value ?ViscosityValue ; os:unit ?ViscosityUnitIRI ; os:hasProvenance ?ViscosityProvenanceIRI .",
                        "?ViscosityUnitIRI rdfs:label ?ViscosityUnitValue .",
                        (
                            "OPTIONAL{?ViscosityIRI os:hasReferenceState ?ViscosityReferenceStateIRI .\n"
                            "?ViscosityReferenceStateIRI os:value ?ViscosityReferenceStateValue ; os:unit ?ViscosityReferenceStateUnitIRI .\n"
                            "?ViscosityReferenceStateUnitIRI rdfs:label ?ViscosityReferenceStateUnitValue .}"
                        ),
                    ],
                ),
            ),
            # species -> identifier
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?IUPACNameValue",
                    where_clause=[
                        "?SpeciesIRI ?hasIdentifier ?species .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasIdentifierIUPACName ?MolecularWeightValue .",
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?IUPACNameValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI ?hasIdentifier ?IdentifierIRI .",
                        "?IdentifierIRI rdf:type ?Identifier ; os:value ?species .",
                        "?Identifier rdfs:subClassOf os:Identifier .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .",
                        "?IUPACNameIRI os:value ?IUPACNameValue .",
                    ],
                ),
            ),
            # species -> chemclass
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?ChemicalClassValue",
                    where_clause=[
                        "?SpeciesIRI ?hasIdentifier ?species .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .",
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?ChemicalClassValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI ?hasIdentifier ?IdentifierIRI .",
                        "?IdentifierIRI rdf:type ?Identifier ; os:value ?species .",
                        "?Identifier rdfs:subClassOf os:Identifier .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasChemicalClass* ?x .",
                        "?x ?y ?z .",
                        "?z rdfs:subClassOf* ?ChemicalClassIRI .",
                        "?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .",
                    ],
                ),
            ),
            # species -> use
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?UseValue",
                    where_clause=[
                        "?SpeciesIRI ?hasIdentifier ?species .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasUse ?UseValue .",
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?UseValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI ?hasIdentifier ?IdentifierIRI .",
                        "?IdentifierIRI rdf:type ?Identifier ; os:value ?species .",
                        "?Identifier rdfs:subClassOf os:Identifier .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasUse ?UseIRI .",
                        "?UseIRI rdfs:label ?UseValue .",
                    ],
                ),
            ),
            # species -> all properties
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?PropertyNameValue",
                    where_clause=[
                        "?SpeciesIRI ?hasIdentifier ?species .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI ?hasPropertyName ?PropertyNameValue .",
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?PropertyLabel ?PropertyNameValue ?PropertyNameUnitValue ?PropertyNameReferenceStateValue ?PropertyNameReferenceStateUnitValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI ?hasIdentifier ?IdentifierIRI .",
                        "?IdentifierIRI rdf:type ?Identifier ; os:value ?species .",
                        "?Identifier rdfs:subClassOf os:Identifier .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI ?hasPropertyName ?PropertyNameIRI .",
                        "?PropertyNameIRI  rdf:type ?PropertyName .",
                        "?PropertyName rdfs:subClassOf os:Property .",
                        "?PropertyNameIRI os:value ?PropertyNameValue ; os:unit ?PropertyNameUnitIRI ; os:hasProvenance ?PropertyNameProvenanceIRI .",
                        "?PropertyNameUnitIRI rdfs:label ?PropertyNameUnitValue .",
                        (
                            "OPTIONAL{?PropertyNameIRI os:hasReferenceState ?PropertyNameReferenceStateIRI .\n"
                            "?PropertyNameReferenceStateIRI os:value ?PropertyNameReferenceStateValue ; os:unit ?PropertyNameReferenceStateUnitIRI .\n"
                            "?PropertyNameReferenceStateUnitIRI rdfs:label ?PropertyNameReferenceStateUnitValue .}\n"
                        ),
                        "BIND(strafter(str(?PropertyName),'#') AS ?PropertyLabel)",
                    ],
                ),
            ),
            # species -> all identifiers
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?IdentifierNameValue",
                    where_clause=[
                        "?SpeciesIRI ?hasIdentifier ?species .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI ?hasIdentifierName ?IdentifierNameValue .",
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?IdentifierLabel ?IdentifierNameValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI ?hasIdentifier ?IdentifierIRI .",
                        "?IdentifierIRI rdf:type ?Identifier ; os:value ?species .",
                        "?Identifier rdfs:subClassOf os:Identifier .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI ?hasIdentifierName ?IdentifierNameIRI .",
                        "?IdentifierNameIRI  rdf:type ?IdentifierName .",
                        "?IdentifierName rdfs:subClassOf os:Identifier .",
                        "?IdentifierNameIRI os:value ?IdentifierNameValue .",
                        "BIND(strafter(str(?IdentifierName),'#') AS ?IdentifierLabel)",
                    ],
                ),
            ),
            # compare properties
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?MolecularWeightValue",
                    where_clause=[
                        "?SpeciesIRI ?hasIdentifier ?species .",
                        'FILTER( ?species = "C22H29FO4" || ?species = "C(C(C(=O)O)N)O")',
                        "?SpeciesIRI os:hasPropertyMolecularWeight ?MolecularWeightValue .",
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?MolecularWeightValue ?MolecularWeightUnitValue ?MolecularWeightReferenceStateValue ?MolecularWeightReferenceStateUnitValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI ?hasIdentifier ?IdentifierIRI .",
                        "?IdentifierIRI rdf:type ?Identifier ; os:value ?species .",
                        "?Identifier rdfs:subClassOf os:Identifier .",
                        'FILTER( ?species = "C22H29FO4" || ?species = "C(C(C(=O)O)N)O")',
                        "?SpeciesIRI os:hasMolecularWeight ?MolecularWeightIRI .",
                        "?MolecularWeightIRI os:value ?MolecularWeightValue ; os:unit ?MolecularWeightUnitIRI ; os:hasProvenance ?MolecularWeightProvenanceIRI .",
                        "?MolecularWeightUnitIRI rdfs:label ?MolecularWeightUnitValue .",
                        (
                            "OPTIONAL{?MolecularWeightIRI os:hasReferenceState ?MolecularWeightReferenceStateIRI .\n"
                            "?MolecularWeightReferenceStateIRI os:value ?MolecularWeightReferenceStateValue ; os:unit ?MolecularWeightReferenceStateUnitIRI .\n"
                            "?MolecularWeightReferenceStateUnitIRI rdfs:label ?MolecularWeightReferenceStateUnitValue .}"
                        ),
                    ],
                ),
            ),
            # two identical properties
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?MolecularWeightValue ?MolecularWeightValue",
                    where_clause=[
                        "?SpeciesIRI ?hasIdentifier ?species .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasPropertyMolecularWeight ?MolecularWeightValue .",
                        "?SpeciesIRI os:hasPropertyMolecularWeight ?MolecularWeightValue .",
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?MolecularWeightValue ?MolecularWeightUnitValue ?MolecularWeightReferenceStateValue ?MolecularWeightReferenceStateUnitValue ?MolecularWeightValue ?MolecularWeightUnitValue ?MolecularWeightReferenceStateValue ?MolecularWeightReferenceStateUnitValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI ?hasIdentifier ?IdentifierIRI .",
                        "?IdentifierIRI rdf:type ?Identifier ; os:value ?species .",
                        "?Identifier rdfs:subClassOf os:Identifier .",
                        'FILTER( ?species = "C22H29FO4")',
                        "?SpeciesIRI os:hasMolecularWeight ?MolecularWeightIRI .",
                        "?MolecularWeightIRI os:value ?MolecularWeightValue ; os:unit ?MolecularWeightUnitIRI ; os:hasProvenance ?MolecularWeightProvenanceIRI .",
                        "?MolecularWeightUnitIRI rdfs:label ?MolecularWeightUnitValue .",
                        (
                            "OPTIONAL{?MolecularWeightIRI os:hasReferenceState ?MolecularWeightReferenceStateIRI .\n"
                            "?MolecularWeightReferenceStateIRI os:value ?MolecularWeightReferenceStateValue ; os:unit ?MolecularWeightReferenceStateUnitIRI .\n"
                            "?MolecularWeightReferenceStateUnitIRI rdfs:label ?MolecularWeightReferenceStateUnitValue .}"
                        ),
                        "?SpeciesIRI os:hasMolecularWeight ?MolecularWeightIRI .",
                        "?MolecularWeightIRI os:value ?MolecularWeightValue ; os:unit ?MolecularWeightUnitIRI ; os:hasProvenance ?MolecularWeightProvenanceIRI .",
                        "?MolecularWeightUnitIRI rdfs:label ?MolecularWeightUnitValue .",
                        (
                            "OPTIONAL{?MolecularWeightIRI os:hasReferenceState ?MolecularWeightReferenceStateIRI .\n"
                            "?MolecularWeightReferenceStateIRI os:value ?MolecularWeightReferenceStateValue ; os:unit ?MolecularWeightReferenceStateUnitIRI .\n"
                            "?MolecularWeightReferenceStateUnitIRI rdfs:label ?MolecularWeightReferenceStateUnitValue .}"
                        ),
                    ],
                ),
            )
        ],
    )
    def test_compact2verbose_fromSpecies(self, compact_query, expected):
        assert compact_query.compact2verbose() == expected

    @pytest.mark.parametrize(
        "compact_query, expected",
        [
            # property -> species
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?IUPACNameValue ?MolecularWeightValue",
                    where_clause=[
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameValue .",
                        "?SpeciesIRI os:hasPropertyMolecularWeight ?MolecularWeightValue .",
                        "FILTER(?MolecularWeightValue < 252)",
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?IUPACNameValue ?MolecularWeightValue ?MolecularWeightUnitValue ?MolecularWeightReferenceStateValue ?MolecularWeightReferenceStateUnitValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .",
                        "?IUPACNameIRI os:value ?IUPACNameValue .",
                        "?SpeciesIRI os:hasMolecularWeight ?MolecularWeightIRI .",
                        "?MolecularWeightIRI os:value ?MolecularWeightValue ; os:unit ?MolecularWeightUnitIRI ; os:hasProvenance ?MolecularWeightProvenanceIRI .",
                        "?MolecularWeightUnitIRI rdfs:label ?MolecularWeightUnitValue .",
                        (
                            "OPTIONAL{?MolecularWeightIRI os:hasReferenceState ?MolecularWeightReferenceStateIRI .\n"
                            "?MolecularWeightReferenceStateIRI os:value ?MolecularWeightReferenceStateValue ; os:unit ?MolecularWeightReferenceStateUnitIRI .\n"
                            "?MolecularWeightReferenceStateUnitIRI rdfs:label ?MolecularWeightReferenceStateUnitValue .}"
                        ),
                        "FILTER(?MolecularWeightValue < 252)",
                    ],
                ),
            ),
            # chemclass -> species
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?IUPACNameValue",
                    where_clause=[
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameValue .",
                        "?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .",
                        'FILTER( ?ChemicalClassValue = "secondary amide")',
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?IUPACNameValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .",
                        "?IUPACNameIRI os:value ?IUPACNameValue .",
                        "?SpeciesIRI os:hasChemicalClass* ?x .",
                        "?x ?y ?z .",
                        "?z rdfs:subClassOf* ?ChemicalClassIRI .",
                        "?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .",
                        'FILTER( ?ChemicalClassValue = "secondary amide")',
                    ],
                ),
            ),
            # use -> species
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?IUPACNameValue",
                    where_clause=[
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameValue .",
                        "?SpeciesIRI os:hasUse ?UseValue .",
                        'FILTER( ?UseValue = "Solvent")',
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?IUPACNameValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .",
                        "?IUPACNameIRI os:value ?IUPACNameValue .",
                        "?SpeciesIRI os:hasUse ?UseIRI .",
                        "?UseIRI rdfs:label ?UseValue .",
                        'FILTER( ?UseValue = "Solvent")',
                    ],
                ),
            ),
            # 2 properties -> species
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?IUPACNameValue ?MolecularWeightValue ?ViscosityValue",
                    where_clause=[
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameValue .",
                        "?SpeciesIRI os:hasPropertyMolecularWeight ?MolecularWeightValue .",
                        "FILTER(?MolecularWeightValue < 252)",
                        "?SpeciesIRI os:hasPropertyViscosity ?ViscosityValue .",
                        "FILTER(?ViscosityValue < 54 || ?ViscosityValue > 87)",
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?IUPACNameValue ?MolecularWeightValue ?MolecularWeightUnitValue ?MolecularWeightReferenceStateValue ?MolecularWeightReferenceStateUnitValue ?ViscosityValue ?ViscosityUnitValue ?ViscosityReferenceStateValue ?ViscosityReferenceStateUnitValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .",
                        "?IUPACNameIRI os:value ?IUPACNameValue .",
                        "?SpeciesIRI os:hasMolecularWeight ?MolecularWeightIRI .",
                        "?MolecularWeightIRI os:value ?MolecularWeightValue ; os:unit ?MolecularWeightUnitIRI ; os:hasProvenance ?MolecularWeightProvenanceIRI .",
                        "?MolecularWeightUnitIRI rdfs:label ?MolecularWeightUnitValue .",
                        (
                            "OPTIONAL{?MolecularWeightIRI os:hasReferenceState ?MolecularWeightReferenceStateIRI .\n"
                            "?MolecularWeightReferenceStateIRI os:value ?MolecularWeightReferenceStateValue ; os:unit ?MolecularWeightReferenceStateUnitIRI .\n"
                            "?MolecularWeightReferenceStateUnitIRI rdfs:label ?MolecularWeightReferenceStateUnitValue .}"
                        ),
                        "FILTER(?MolecularWeightValue < 252)",
                        "?SpeciesIRI os:hasViscosity ?ViscosityIRI .",
                        "?ViscosityIRI os:value ?ViscosityValue ; os:unit ?ViscosityUnitIRI ; os:hasProvenance ?ViscosityProvenanceIRI .",
                        "?ViscosityUnitIRI rdfs:label ?ViscosityUnitValue .",
                        (
                            "OPTIONAL{?ViscosityIRI os:hasReferenceState ?ViscosityReferenceStateIRI .\n"
                            "?ViscosityReferenceStateIRI os:value ?ViscosityReferenceStateValue ; os:unit ?ViscosityReferenceStateUnitIRI .\n"
                            "?ViscosityReferenceStateUnitIRI rdfs:label ?ViscosityReferenceStateUnitValue .}"
                        ),
                        "FILTER(?ViscosityValue < 54 || ?ViscosityValue > 87)",
                    ],
                ),
            ),
            # chemclass + use -> species
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?IUPACNameValue",
                    where_clause=[
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameValue .",
                        "?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .",
                        'FILTER( ?ChemicalClassValue = "secondary amide")',
                        "?SpeciesIRI os:hasUse ?UseValue .",
                        'FILTER( ?UseValue = "Solvent")',
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?IUPACNameValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .",
                        "?IUPACNameIRI os:value ?IUPACNameValue .",
                        "?SpeciesIRI os:hasChemicalClass* ?x .",
                        "?x ?y ?z .",
                        "?z rdfs:subClassOf* ?ChemicalClassIRI .",
                        "?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .",
                        'FILTER( ?ChemicalClassValue = "secondary amide")',
                        "?SpeciesIRI os:hasUse ?UseIRI .",
                        "?UseIRI rdfs:label ?UseValue .",
                        'FILTER( ?UseValue = "Solvent")',
                    ],
                ),
            ),
            # property + chemclas -> species
            (
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?IUPACNameValue ?MolecularWeightValue",
                    where_clause=[
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameValue .",
                        "?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .",
                        'FILTER( ?ChemicalClassValue = "secondary amide")',
                        "?SpeciesIRI os:hasPropertyMolecularWeight ?MolecularWeightValue .",
                        "FILTER(?MolecularWeightValue < 252)",
                    ],
                ),
                AbstractQueryRep(
                    result_clause="SELECT DISTINCT ?label ?IUPACNameValue ?MolecularWeightValue ?MolecularWeightUnitValue ?MolecularWeightReferenceStateValue ?MolecularWeightReferenceStateUnitValue",
                    where_clause=[
                        "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                        "?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .",
                        "?IUPACNameIRI os:value ?IUPACNameValue .",
                        "?SpeciesIRI os:hasChemicalClass* ?x .",
                        "?x ?y ?z .",
                        "?z rdfs:subClassOf* ?ChemicalClassIRI .",
                        "?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .",
                        'FILTER( ?ChemicalClassValue = "secondary amide")',
                        "?SpeciesIRI os:hasMolecularWeight ?MolecularWeightIRI .",
                        "?MolecularWeightIRI os:value ?MolecularWeightValue ; os:unit ?MolecularWeightUnitIRI ; os:hasProvenance ?MolecularWeightProvenanceIRI .",
                        "?MolecularWeightUnitIRI rdfs:label ?MolecularWeightUnitValue .",
                        (
                            "OPTIONAL{?MolecularWeightIRI os:hasReferenceState ?MolecularWeightReferenceStateIRI .\n"
                            "?MolecularWeightReferenceStateIRI os:value ?MolecularWeightReferenceStateValue ; os:unit ?MolecularWeightReferenceStateUnitIRI .\n"
                            "?MolecularWeightReferenceStateUnitIRI rdfs:label ?MolecularWeightReferenceStateUnitValue .}"
                        ),
                        "FILTER(?MolecularWeightValue < 252)",
                    ],
                ),
            ),
        ],
    )
    def test_compact2verbose_fromQualifiers(self, compact_query, expected):
        assert compact_query.compact2verbose() == expected

    def test_compact2verbose_fromChemClass(self):
        compact_query = AbstractQueryRep(
            result_clause="SELECT DISTINCT ?IUPACNameValue ?MolecularWeightValue ?ViscosityValue",
            where_clause=[
                "?SpeciesIRI os:hasIUPACName ?IUPACNameValue .",
                "?SpeciesIRI os:hasChemicalClass ?ChemicalClassValue .",
                'FILTER( ?ChemicalClassValue = "secondary amide")',
                "?SpeciesIRI os:hasPropertyMolecularWeight ?MolecularWeightValue .",
                "?SpeciesIRI os:hasPropertyViscosity ?ViscosityValue .",
            ],
        )
        expected = AbstractQueryRep(
            result_clause="SELECT DISTINCT ?label ?IUPACNameValue ?MolecularWeightValue ?MolecularWeightUnitValue ?MolecularWeightReferenceStateValue ?MolecularWeightReferenceStateUnitValue ?ViscosityValue ?ViscosityUnitValue ?ViscosityReferenceStateValue ?ViscosityReferenceStateUnitValue",
            where_clause=[
                "?SpeciesIRI rdf:type os:Species ; rdfs:label ?label .",
                "?SpeciesIRI os:hasIUPACName ?IUPACNameIRI .",
                "?IUPACNameIRI os:value ?IUPACNameValue .",
                "?SpeciesIRI os:hasChemicalClass* ?x .",
                "?x ?y ?z .",
                "?z rdfs:subClassOf* ?ChemicalClassIRI .",
                "?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue .",
                'FILTER( ?ChemicalClassValue = "secondary amide")',
                "?SpeciesIRI os:hasMolecularWeight ?MolecularWeightIRI .",
                "?MolecularWeightIRI os:value ?MolecularWeightValue ; os:unit ?MolecularWeightUnitIRI ; os:hasProvenance ?MolecularWeightProvenanceIRI .",
                "?MolecularWeightUnitIRI rdfs:label ?MolecularWeightUnitValue .",
                (
                    "OPTIONAL{?MolecularWeightIRI os:hasReferenceState ?MolecularWeightReferenceStateIRI .\n"
                    "?MolecularWeightReferenceStateIRI os:value ?MolecularWeightReferenceStateValue ; os:unit ?MolecularWeightReferenceStateUnitIRI .\n"
                    "?MolecularWeightReferenceStateUnitIRI rdfs:label ?MolecularWeightReferenceStateUnitValue .}"
                ),
                "?SpeciesIRI os:hasViscosity ?ViscosityIRI .",
                "?ViscosityIRI os:value ?ViscosityValue ; os:unit ?ViscosityUnitIRI ; os:hasProvenance ?ViscosityProvenanceIRI .",
                "?ViscosityUnitIRI rdfs:label ?ViscosityUnitValue .",
                (
                    "OPTIONAL{?ViscosityIRI os:hasReferenceState ?ViscosityReferenceStateIRI .\n"
                    "?ViscosityReferenceStateIRI os:value ?ViscosityReferenceStateValue ; os:unit ?ViscosityReferenceStateUnitIRI .\n"
                    "?ViscosityReferenceStateUnitIRI rdfs:label ?ViscosityReferenceStateUnitValue .}"
                ),
            ],
        )
        assert compact_query.compact2verbose() == expected
