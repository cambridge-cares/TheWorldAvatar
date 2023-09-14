import random
from typing import Dict

from SPARQLWrapper import SPARQLWrapper, JSON, POST

from data_generation.constants import PROPERTIES


class SubgraphRetriever:
    PREFIXES = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>"""
    IDENTIFIERS_FROM_USER = [
        "InChI",
        "IUPACName",
        "MolecularFormula",
        "SMILES",
    ]
    
    def __init__(
        self,
        kg_endpoint="http://theworldavatar.com/blazegraph/namespace/copy_ontospecies_pubchem/sparql",
    ):
        sparql_client = SPARQLWrapper(kg_endpoint)
        sparql_client.setReturnFormat(JSON)
        sparql_client.setMethod(POST)
        self.sparql_client = sparql_client

    def get_rand_subgraph(self):
        bindings = []

        while len(bindings) == 0:
            sparql_query = self.get_rand_subgraph_query()
            self.sparql_client.setQuery(sparql_query)
            bindings = self.sparql_client.queryAndConvert()["results"]["bindings"]

        return self.convert_kg_results_to_subgraph(bindings[0])

    def get_rand_tail_nums(self):
        property_num = random.randint(0, 3)

        if property_num == 0:
            use_num, chemicalclass_num = random.choice([(0, 1), (1, 0)])
        elif property_num == 1:
            use_num, chemicalclass_num = random.choice([(0, 0), (0, 1), (1, 0), (1, 1)])
        elif property_num == 2:
            use_num, chemicalclass_num = random.choice([(0, 0), (0, 1), (1, 0)])
        else:
            use_num, chemicalclass_num = 0, 0

        return dict(
            property_num=property_num,
            use_num=use_num,
            chemicalclass_num=chemicalclass_num,
        )

    def get_rand_subgraph_query(self):
        tail_nums = self.get_rand_tail_nums()
        identifier = random.choice(self.IDENTIFIERS_FROM_USER)

        select_variables = ["?SpeciesIRI", "?IdentifierIRI", "?IdentifierValue"]

        where_clause = f"""
        ?SpeciesIRI a os:Species ; os:has{identifier} ?IdentifierIRI .
        ?IdentifierIRI os:value ?IdentifierValue ."""

        for i in range(1, tail_nums["property_num"] + 1):
            select_variables += [
                f"?hasProperty{i}",
                f"?PropertyIRI{i}",
                f"?PropertyValue{i}",
            ]
            where_clause += f"""
        ?SpeciesIRI ?hasProperty{i} ?PropertyIRI{i} .
        VALUES (?hasProperty{i}) {{
            {" ".join([f"(os:has{x})" for x in PROPERTIES])}
        }}
        ?PropertyIRI{i} os:value ?PropertyValue{i} ."""

        if tail_nums["property_num"] == 2:
            where_clause += """
        FILTER ( ?hasProperty1 != ?hasProperty2 )"""
        elif tail_nums["property_num"] == 3:
            where_clause += """
        FILTER ( ?hasProperty1 != ?hasProperty2 )
        FILTER ( ?hasProperty1 != ?hasProperty3 )
        FILTER ( ?hasProperty2 != ?hasProperty3 )"""

        if tail_nums["use_num"] > 0:
            select_variables += ["?UseIRI", "?UseValue"]
            where_clause += """
        ?SpeciesIRI os:hasUse ?UseIRI .
        ?UseIRI rdfs:label ?UseValue ."""

        if tail_nums["chemicalclass_num"] > 0:
            select_variables += ["?ChemicalClassIRI", "?ChemicalClassValue"]
            where_clause += """
        ?SpeciesIRI os:hasChemicalClass* ?x .
        ?x ?y ?z .
        ?z rdfs:subClassOf* ?ChemicalClassIRI .
        ?ChemicalClassIRI rdf:type os:ChemicalClass  ; rdfs:label ?ChemicalClassValue ."""

        return f"""{self.PREFIXES}

    SELECT {" ".join(select_variables)} 
    WHERE {{{where_clause}
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
