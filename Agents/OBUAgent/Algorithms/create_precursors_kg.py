import os
from rdkit import Chem
from rdkit import RDLogger
RDLogger.DisableLog('rdApp.*')


def create_mol_for_all_precursors(sparql_client, precursor_mol_dir):
    sparql_client.performUpdate(
        """
        prefix ocof: <https://www.theworldavatar.com/kg/ontocofs/>
        prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        insert { ?precursor ocof:smarts ?precursor_smarts. }
        where {
            ?precursor a ocof:Precursor.
            ?precursor ocof:hasBindingSite/ocof:smarts ?bs_smarts; ocof:hasCore/ocof:smarts ?core_smarts.
            BIND(REPLACE(REPLACE(?core_smarts, "\\\\*", ?bs_smarts), "\\\\[Ge\\\\]#\\\\[Ge\\\\]", ?bs_smarts) AS ?precursor_smarts)
        }
        """
    )

    results_list = sparql_client.performQuery(
        """
        prefix ocof: <https://www.theworldavatar.com/kg/ontocofs/>
        prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        select ?bs_label ?core_label ?precursor_smarts
        where {
            ?precursor a ocof:Precursor.
            ?precursor ocof:hasBindingSite/rdfs:label ?bs_label; ocof:hasCore/rdfs:label ?core_label.
            ?precursor ocof:smarts ?precursor_smarts.
        }
        """
    )

    for res in results_list:
        precursor_mol = Chem.MolFromSmarts(res['precursor_smarts'])
        # Remove V lines from the mol block
        mol_block = Chem.MolToMolBlock(precursor_mol)
        mol_block = '\n'.join([line for line in mol_block.split('\n') if not line.startswith('V')])
        # Replace RDKit
        mol_block = mol_block.replace('RDKit', 'AK-JB-TWA')
        # Save precursor mol file
        new_mol_file = os.path.join(precursor_mol_dir, f"{res['bs_label']}_{res['core_label']}.mol")
        with open(new_mol_file, 'w') as f:
            f.write(mol_block)
