from Marie.PubChem import PubChemEngine

pubchem_engine = PubChemEngine()
answers, scores = pubchem_engine.find_answers(question='charge', head_entity='CID1')
print(answers)
print(scores)



