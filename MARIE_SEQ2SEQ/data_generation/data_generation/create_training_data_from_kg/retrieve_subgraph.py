import random
from typing import Dict, List, Optional

from SPARQLWrapper import SPARQLWrapper, JSON, POST

from data_generation.constants import IDENTIFIER_NAMES, PROPERTY_NAMES


class SubgraphRetriever:
    PREFIXES = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"""
    IDENTIFIER_NAMES_FOR_HEAD = [
        "InChI",
        "IUPACName",
        "MolecularFormula",
        "SMILES",
    ]

    def __init__(
        self,
        kg_endpoint: str = "http://theworldavatar.com/blazegraph/namespace/copy_ontospecies_pubchem/sparql",
        property_names: List[str] = PROPERTY_NAMES,
        identifier_names: List[str] = IDENTIFIER_NAMES,
    ):
        sparql_client = SPARQLWrapper(kg_endpoint)
        sparql_client.setReturnFormat(JSON)
        sparql_client.setMethod(POST)

        self.sparql_client = sparql_client
        self.property_names = property_names
        self.identifier_names = identifier_names

    def get_subgraph(self, tail_nums: Optional[Dict[str, int]] = None):
        bindings = []

        while len(bindings) == 0:
            if tail_nums is None:
                tail_nums = self.get_rand_tail_nums()
            sparql_query = self.get_subgraph_query(tail_nums)

            self.sparql_client.setQuery(sparql_query)
            bindings = self.sparql_client.queryAndConvert()["results"]["bindings"]

        return self.convert_kg_results_to_subgraph(bindings[0])

    def get_rand_tail_nums(self):
        property_num = random.randint(0, min(3, len(self.property_names)))
        identifier_num = random.randint(property_num, min(3, len(self.property_names) + len(self.identifier_names))) - property_num

        id_prop_num = identifier_num + property_num
        if id_prop_num == 0:
            use_num, chemicalclass_num = random.choice([(0, 1), (1, 0)])
        elif id_prop_num == 1:
            use_num, chemicalclass_num = random.choice([(0, 0), (0, 1), (1, 0), (1, 1)])
        elif id_prop_num == 2:
            use_num, chemicalclass_num = random.choice([(0, 0), (0, 1), (1, 0)])
        else:
            use_num, chemicalclass_num = 0, 0

        return dict(
            property_num=property_num,
            identifier_num=identifier_num,
            use_num=use_num,
            chemicalclass_num=chemicalclass_num,
        )

    def get_where_species(self, identifier_name: str):
        return f"""
    ?SpeciesIRI a os:Species ; os:has{identifier_name} ?IdentifierIRI .
    ?IdentifierIRI os:value ?IdentifierValue ."""

    def get_where_property(self, i: int):
        return f"""
    VALUES (?hasProperty{i}) {{
        {" ".join([f"(os:has{x})" for x in self.property_names])}
    }}
    ?SpeciesIRI ?hasProperty{i} ?PropertyIRI{i} .
    ?PropertyIRI{i} os:value ?PropertyValue{i} ."""

    def get_where_property_filter(self, property_num: int):
        if property_num < 2:
            return ""

        if property_num == 2:
            return """
    FILTER ( ?hasProperty1 != ?hasProperty2 )"""

        if property_num == 3:
            return """
    FILTER ( ?hasProperty1 != ?hasProperty2 )
    FILTER ( ?hasProperty1 != ?hasProperty3 )
    FILTER ( ?hasProperty2 != ?hasProperty3 )"""

        raise ValueError(
            f"`property_num` is expected to be in the range of [0, 3] inclusive, but got {property_num}."
        )

    def get_where_identifier(self, i: int):
        return f"""
    VALUES (?hasIdentifier{i}) {{
        {" ".join([f"(os:has{x})" for x in self.identifier_names])}
    }}
    ?SpeciesIRI ?hasIdentifier{i} ?IdentifierIRI{i} .
    ?IdentifierIRI{i} os:value ?IdentifierValue{i} ."""

    def get_where_identifier_filter(self, identifier_num: int):
        if identifier_num < 2:
            return ""

        if identifier_num == 2:
            return """
    FILTER ( ?hasIdentifier1 != ?hasIdentifier2 )"""

        if identifier_num == 3:
            return """
    FILTER ( ?hasIdentifier1 != ?hasIdentifier2 )
    FILTER ( ?hasIdentifier1 != ?hasIdentifier3 )
    FILTER ( ?hasIdentifier2 != ?hasIdentifier3 )"""

        raise ValueError(
            f"`identifier_num` is expected to be in the range of [0, 3] inclusive, but got {identifier_num}."
        )

    def get_where_use(self):
        return """
    ?SpeciesIRI os:hasUse ?UseIRI .
    ?UseIRI rdfs:label ?UseValue ."""

    def get_where_chemicalclass(self):
        return """
    ?SpeciesIRI os:hasChemicalClass* ?x .
    ?x ?y ?z .
    ?z rdfs:subClassOf* ?ChemicalClassIRI .
    ?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue ."""

    def get_subgraph_query(self, tail_nums: Dict[str, int]):
        identifier_name = random.choice(self.IDENTIFIER_NAMES_FOR_HEAD)

        select_variables = ["?SpeciesIRI", "?IdentifierIRI", "?IdentifierValue"]
        where_clause_blocks = [self.get_where_species(identifier_name)]

        for i in range(1, tail_nums["property_num"] + 1):
            select_variables.extend(
                [
                    f"?hasProperty{i}",
                    f"?PropertyIRI{i}",
                    f"?PropertyValue{i}",
                ]
            )
            where_clause_blocks.append(self.get_where_property(i))
        where_clause_blocks.append(
            self.get_where_property_filter(tail_nums["property_num"])
        )

        for i in range(1, tail_nums["identifier_num"] + 1):
            select_variables.extend(
                [f"?hasIdentifier{i}", f"?IdentifierIRI{i}", f"?IdentifierValue{i}"]
            )
            where_clause_blocks.append(self.get_where_identifier(i))
        where_clause_blocks.append(
            self.get_where_identifier_filter(tail_nums["identifier_num"])
        )

        if tail_nums["use_num"] > 0:
            select_variables.extend(["?UseIRI", "?UseValue"])
            where_clause_blocks.append(self.get_where_use())

        if tail_nums["chemicalclass_num"] > 0:
            select_variables.extend(["?ChemicalClassIRI", "?ChemicalClassValue"])
            where_clause_blocks.append(self.get_where_chemicalclass())

        return f"""{self.PREFIXES}

SELECT {" ".join(select_variables)} 
WHERE {{{"".join(where_clause_blocks)}
}}
LIMIT 1"""

    def convert_kg_results_to_subgraph(self, valueset: Dict[str, Dict[str, str]]):
        values = {k: v["value"] for k, v in valueset.items()}

        head = dict(
            SpeciesIRI=values["SpeciesIRI"],
            IdentifierIRI=values["IdentifierIRI"],
            IdentifierValue=values["IdentifierValue"],
        )

        tails = []

        for i in range(1, 4):
            if f"PropertyIRI{i}" not in values:
                break
            tails.append(
                dict(
                    type="property",
                    PropertyName=values[f"hasProperty{i}"].split("#", maxsplit=1)[-1][
                        len("has") :
                    ],
                    PropertyIRI=values[f"PropertyIRI{i}"],
                    PropertyValue=float(values[f"PropertyValue{i}"]),
                )
            )

        for i in range(1, 4):
            if f"IdentifierIRI{i}" not in values:
                break
            tails.append(
                dict(
                    type="identifier",
                    IdentifierName=values[f"hasIdentifier{i}"].split("#", maxsplit=1)[
                        -1
                    ][len("has") :],
                    IdentifierIRI=values[f"IdentifierIRI{i}"],
                    IdentifierValue=values[f"IdentifierValue{i}"],
                )
            )

        if "UseIRI" in values:
            tails.append(
                dict(type="use", UseIRI=values["UseIRI"], UseValue=values["UseValue"])
            )

        if "ChemicalClassIRI" in values:
            tails.append(
                dict(
                    type="chemicalclass",
                    ChemicalClassIRI=values["ChemicalClassIRI"],
                    ChemicalClassValue=values["ChemicalClassValue"],
                )
            )

        return dict(head=head, tails=tails)
