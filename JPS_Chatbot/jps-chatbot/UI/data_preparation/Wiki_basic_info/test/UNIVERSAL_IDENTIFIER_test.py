import json

with open('../NAME_URI_DICT') as f:
    NAME_URI_DICT = json.loads(f.read())
    f.close()

with open('../FORMULA_URI_DICT') as f:
    FORMULA_URI_DICT = json.loads(f.read())
    f.close()

with open('../SMILES_URI_DICT') as f:
    SMILES_URI_DICT = json.loads(f.read())
    f.close()

with open('../FORMULA_NAME_DICT') as f:
    FORMULA_NAME_DICT = json.loads(f.read())
    f.close()


test_names = ['water', 'benzene', 'methane', 'carbon dioxide', 'sulfuric acid']
test_formula = ['H2SO4', 'c8h14', 'c2h2o2', 'C3H6','co2', 'h2o', 'h2o2']
test_SMILES = ['c1=cc=cc=c1', 'c1ccccc1', 'CC', 'C', 'C=C']

print('====================== name ======================')
for name in test_names:
    name = name.upper()
    print(NAME_URI_DICT[name])

print('====================== smiles ======================')
for smiles in test_SMILES:
    smiles = smiles.upper()
    print(SMILES_URI_DICT[smiles])

print('====================== formula ======================')
for formula in test_formula:
    formula = formula.upper()
    print(FORMULA_URI_DICT[formula])
    print(FORMULA_NAME_DICT[formula])


