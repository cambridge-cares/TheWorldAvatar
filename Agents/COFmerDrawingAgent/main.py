from Algorithms.operations import run_cofmer_pipeline

#Inputs

componentTypeNumber = {"Precursor":1,"Linkage":1}

assemblyModel = {
    "AM": "hcb-[A3L2]n",
    "Component_1": "2-linear",
    "Component_2": "3P-Planar",
    "Component_1 Copies": 3,
    "Component_2 Copies": 2,
    "Component_1 BS": None,
    "Component_2 BS": None,
    "ConstructionSteps": {
        "Product_1": ["Component_1_Copy_1", "Component_2_Copy_1"],
        "Product_2": ["Product_1", "Component_1_Copy_2"],
        "Product_3": ["Product_2", "Component_2_Copy_2"],
        "Product_4": ["Product_3", "Component_1_Copy_3"],
        "COFmer": "Product_4"
    }
}

precursor1 = {
    "Precursor": "2L-BDBOH-10",
    "GBU": "2-linear",
    "ConstructingMol": "2L-XX-1.mol",
    "BS": [
        {
            "UnitFrom": "Precursor",
            "bindingSite": "BDBOH",
            "bsIndex": 2
        }
    ]
}

precursor2 = {
    "Precursor": "3L-BDBOH-10",
    "GBU": "3-linear",
    "ConstructingMol": "3L-XX-10.mol",
    "BS": [
        {
            "UnitFrom": "Precursor",
            "bindingSite": "BDBOH",
            "bsIndex": 3
        }
    ]
}
linkage1 = {
    "Linkage": "LFR-16",
    "GBU": "3P-Planar",
    "ConstructingMol": "LFR-16.mol",
    "BS": [
        {
            "UnitFrom": "Linkage",
            "bindingSite": "BDBOH",
            "bsIndex": 3
        }
    ]
}

input_dir = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\input_dir\\'
output_dir = r'C:\\TheWorldAvatar\\Agents\\COFmerDrawingAgent\\Data\\output_dir\\'

def main():
    # your code here
    run_cofmer_pipeline(assemblyModel, componentTypeNumber, precursor1, linkage1, input_dir, output_dir)

if __name__ == '__main__':
    main()
