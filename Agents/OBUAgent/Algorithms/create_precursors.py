
import os
import csv
from rdkit import Chem
from rdkit.Chem import AllChem

def read_bs_smarts(filepath):
    bs_smarts_dict = {}
    with open(filepath, 'r') as file:
        reader = csv.reader(file)
        next(reader)  # Skip header
        for row in reader:
            bs_smarts_dict[row[0]] = row[2]
    return bs_smarts_dict


def read_bs_smarts_2(filepath):
    bs_smarts_dict_2 = {}
    with open(filepath, 'r') as file:
        reader = csv.reader(file)
        next(reader)  # Skip header
        for row in reader:
            bs_smarts_dict_2[row[0]] = row[2]
    return bs_smarts_dict_2


def substitute_dummy_atoms(input_mol, bs_smarts):
    smarts = Chem.MolToSmarts(input_mol, isomericSmiles=False)
    if '*' in smarts:
        smarts = smarts.replace('*', bs_smarts)
        output_mol = Chem.MolFromSmarts(smarts)
        return output_mol
    elif '[Ge]#[Ge]' in smarts:
        smarts = smarts.replace('[Ge]#[Ge]', bs_smarts)
        output_mol = Chem.MolFromSmarts(smarts)
        return output_mol
    else:
        return None

def main():
    output_csv = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\UnionAlg1_2_Output.csv'
    bs_smarts_csv = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\InputData\\BS_SMARTS.csv'
    bs_smarts_csv_2 = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\InputData\\BS_SMARTS2.csv'
    cores_mol_dir = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\InputData\\cores_mol\\'
    cores_mol_dir_2 = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\InputData\\cores_mol\\double\\'
    new_precursors_dir = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\new_precursors\\'
    new_precursors_dir_2 = r'C:\\TheWorldAvatar\\Agents\\OBUAgent\\Data\\OutputData\\new_precursors\\double\\'
    bs_smarts_dict = read_bs_smarts(bs_smarts_csv)
    bs_smarts_dict_2 = read_bs_smarts_2(bs_smarts_csv_2)

    if not os.path.exists(new_precursors_dir):
        os.makedirs(new_precursors_dir)

    if not os.path.exists(new_precursors_dir_2):
        os.makedirs(new_precursors_dir_2)

    with open(output_csv, 'r') as file:
        reader = csv.reader(file)
        next(reader)  # Skip header
        for row in reader:
            bs, core = row
            bs_smarts = None
            core_file = None
            if bs == 'OCOCO': 
                bs_smarts = bs_smarts_dict_2.get(bs)
                core_file = os.path.join(cores_mol_dir_2 , f"{core}.mol") 
            else: 
               bs_smarts = bs_smarts_dict.get(bs)
               core_file = os.path.join(cores_mol_dir, f"{core}.mol")
            if not bs_smarts:
                    continue

            if not os.path.exists(core_file):
                continue

            core_mol = Chem.MolFromMolFile(core_file)
            new_mol = substitute_dummy_atoms(core_mol, bs_smarts)
            
            if new_mol is None:
                continue

            # Remove V lines from the mol block
            mol_block = Chem.MolToMolBlock(new_mol)
            mol_block = '\n'.join([line for line in mol_block.split('\n') if not line.startswith('V')])

            # Replace RDKit with AK-CARES in the mol block
            mol_block = mol_block.replace('RDKit', 'AK-CARES')

            if bs_smarts in bs_smarts_dict_2.values():
                new_mol_file = os.path.join(new_precursors_dir_2, f"{bs}_{core}.mol")
            else:
                new_mol_file = os.path.join(new_precursors_dir, f"{bs}_{core}.mol")

            # Save the modified mol block to the new_mol_file
            with open(new_mol_file, 'w') as f:
                f.write(mol_block)
        
if __name__ == '__main__':
    main()