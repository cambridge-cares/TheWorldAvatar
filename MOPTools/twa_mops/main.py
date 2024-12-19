from twa.kg_operations import PySparqlClient
from twa.conf import config_generic, Config
from rdflib import Graph, URIRef
import ontomops
import ontospecies
import om
import alg2

class MOPsConfig(Config):
    SPARQL_ENDPOINT: str

endpoint = config_generic(MOPsConfig, env_file='./mops.env').SPARQL_ENDPOINT

if __name__ == "__main__":
    sparql_client = PySparqlClient(endpoint, endpoint)
    alg2_result = sparql_client.perform_query(alg2.alg2)
    # Example result:
    # {
        # 'organic_gbu_label': '2-linear',
        # 'organic_gbu_number': '30',
        # 'organic_charge': '-2.0',
        # 'metal': 'https://www.theworldavatar.com/kg/ontomops/ChemicalBuildingUnit_782a50de-2db7-4910-9547-7200a723d7d3_0',
        # 'organic_formula': '[(C6H4C)2(CO2)2]',
        # 'metal_formula': '[WV5O11]',
        # 'organic_mw': '264.2318',
        # 'am': 'https://www.theworldavatar.com/kg/ontomops/AssemblyModel_4d34c0b4-2a4b-4f16-98dd-97c5ce7349a5',
        # 'metal_mw': '614.54193',
        # 'metal_gbu': 'https://www.theworldavatar.com/kg/ontomops/GenericBuildingUnit_f664df33-ef4a-44f8-a76f-930234465ea5_0',
        # 'am_label': '(5-pyramidal)x12(2-linear)x30',
        # 'am_symmetry': 'Ih',
        # 'metal_gbu_number': '12',
        # 'organic_gbu': 'https://www.theworldavatar.com/kg/ontomops/GenericBuildingUnit_3d71c19a-ab54-4993-8c94-267dcfe41792_1',
        # 'metal_charge': '4.0',
        # 'organic': 'https://www.theworldavatar.com/kg/ontomops/ChemicalBuildingUnit_ea080404-38bb-462e-a885-49f48daca41e_1',
        # 'metal_gbu_label': '5-pyramidal'
    # }
    g = Graph()
    prov = ontomops.Provenance(hasReferenceDOI='PLACEHOLDER DOI')
    for i in range(len(alg2_result)):
        row = alg2_result[i]
        print(f'Instantiating {i + 1}th new MOP: {row}')
        mop_mw = float(row['metal_mw']) * int(row['metal_gbu_number']) + float(row['organic_mw']) * int(row['organic_gbu_number'])
        mop_charge = float(row['metal_charge']) * int(row['metal_gbu_number']) + float(row['organic_charge']) * int(row['organic_gbu_number'])
        new_mop = ontomops.MetalOrganicPolyhedron(
            hasAssemblyModel=row['am'],
            hasChemicalBuildingUnit=[row['metal'], row['organic']],
            hasCharge=ontospecies.Charge(hasValue=om.Measure(hasNumericalValue=mop_charge, hasUnit=om.elementaryCharge)),
            hasMolecularWeight=ontospecies.MolecularWeight(hasValue=om.Measure(hasNumericalValue=mop_mw, hasUnit=om.gramPerMole)),
            hasMOPFormula=f"{row['metal_formula']}{row['metal_gbu_number']}{row['organic_formula']}{row['organic_gbu_number']}",
            hasProvenance=prov,
        )
        g.add((URIRef(row['metal']), URIRef(ontomops.IsFunctioningAs.predicate_iri), URIRef(row['metal_gbu'])))
        g.add((URIRef(row['organic']), URIRef(ontomops.IsFunctioningAs.predicate_iri), URIRef(row['organic_gbu'])))
    ontomops.MetalOrganicPolyhedron.push_all_instances_to_kg(sparql_client, -1)
    sparql_client.upload_graph(g)
