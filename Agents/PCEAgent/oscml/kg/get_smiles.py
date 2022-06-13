from oscml.kg.kgQuery import queryKG
import rdkit
import rdkit.Chem
import rdkit.Chem.inchi


def get_smiles(spec_iri, kg_endpoint):

    query = """
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?Inchi ?smiles
    WHERE
    {
    <#speciesIRI#> rdf:type OntoSpecies:Species .
        OPTIONAL {
            <#speciesIRI#> OntoSpecies:inChI ?Inchi .
            <#speciesIRI#> OntoSpecies:SMILES ?smiles .
        }
    }
    """.replace('#speciesIRI#', spec_iri)

    response = queryKG(sparqlEndPoint=kg_endpoint, queryStr=query)
    if response:
        response = response[0]

    print(response)
    smiles = None
    if 'smiles' in response:
        smiles =  response['smiles']
    elif 'Inchi' in response:
        inchi = response['Inchi']
        mol_from_inchi = rdkit.Chem.inchi.MolFromInchi(inchi)
        smiles =  rdkit.Chem.AllChem.MolToSmiles(mol_from_inchi)

    return smiles



