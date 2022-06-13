import chemaboxwriters.kgoperations.remotestore_client as rsc
import logging
import re
from typing import Optional, List, Dict

logger = logging.getLogger(__name__)


def spec_inchi_query(inchi_string):

    inchi_string_standard = re.sub("^InChI=1(?!S)", "InChI=1S", inchi_string)
    inchi_string_non_standard = re.sub("^InChI=1S", "InChI=1", inchi_string)

    query = """
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?speciesIRI
    WHERE
    {
    OPTIONAL {
           ?speciesIRI rdf:type OntoSpecies:Species .
           ?speciesIRI OntoSpecies:inChI "#inchi_string_standard#"^^xsd:string .}
    OPTIONAL {
           ?speciesIRI rdf:type OntoSpecies:Species .
           ?speciesIRI OntoSpecies:inChI "#inchi_string_non_standard#"^^xsd:string .}
    }
    """  # noqa: E501
    query = query.replace("#inchi_string_standard#", inchi_string_standard).replace(
        "#inchi_string_non_standard#", inchi_string_non_standard
    )
    return query


def get_assemblyModel(gbu_properties: List[Dict], mops_symmetry: str):
    # queries the assembly model of a particular MOP.

    queryStr_part1 = """
    PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    SELECT ?AssemblyModel
    WHERE
    {
    ?mopIRI OntoMOPs:hasAssemblyModel ?AssemblyModel .
    ?AssemblyModel OntoMOPs:hasSymmetryPointGroup "#Symmetry#"^^xsd:string .
    """  # noqa: E501

    queryStr_part1 = queryStr_part1.replace("#Symmetry#", str(mops_symmetry))

    queryStr_part2 = ""

    for i in range(len(gbu_properties)):
        planarity_i = gbu_properties[i]["planarity"]
        modularity_i = gbu_properties[i]["modularity"]
        gbu_number_i = gbu_properties[i]["gbu_number"]

        queryStr_part2 = (
            queryStr_part2
            + """
            ?AssemblyModel OntoMOPs:hasGenericBuildingUnitNumber ?#GBUNumber# .
            ?#GBUNumber# OntoMOPs:isNumberOf ?#GBU# .
            ?#GBU# OntoMOPs:hasPlanarity "#Planarity#"^^xsd:string .
            ?#GBU# OntoMOPs:hasModularity "#Modularity#"^^xsd:string .
            ?#GBUNumber# OntoSpecies:value "#NumberValue#"^^xsd:string .
            """.replace(
                "#GBUNumber#", f"GBUNumber_{i}"
            )
            .replace("#GBU#", f"GBU_{i}")
            .replace("#Planarity#", planarity_i)
            .replace("#Modularity#", modularity_i)
            .replace("#NumberValue#", gbu_number_i)
        )

    queryStr = f"{queryStr_part1}\n{queryStr_part2}}}"
    return queryStr
