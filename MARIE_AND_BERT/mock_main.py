import time
from pprint import pprint

mock_response_mop_1 = [{"node": ["O2 + OH + TC4H9O =] HO2 + TC4H9O2", "O2 + C4H6 [=] HO2 + C4H5-N",
                                 "O2 + CC6H13O =] CC6H13O2", "O2 + C3H6OOH1-2 [=] C3H6OOH2-1O2",
                                 "O2 + C4H72-2 [=] C4H72-2O2", "O2 + CC8H17 =] CC8H17O2", "O2 + CC6H13 =] CC6H13O2",
                                 "O2 + CH3CHO [=] HO2 + CH3CO", "O2 + C7H14OOH4-2 =] C7H14OOH4-2O2",
                                 "O2 + HC6H13 =] HO2 + NEOC6H12", "O2 + XC7H14OOH-X2 =] XC7H14OOH-X2O2",
                                 "O2 + CH3OCH2 [=] CH2OCH2O2H", "O2 + CH3O + DC6H13O =] CH3O2 + DC6H13O2",
                                 "O2 + C3H6OOH1-3 [=] C3H6OOH1-3O2",
                                 "O2 + C5H11O-1 + C5H11O-2 =] C5H11O2-1 + C5H11O2-2", "O2 + HCOH [=] H + OH + CO2",
                                 "O2 + C2H5O + TC4H9O =] C2H5O2 + TC4H9O2", "O2 + C2H5O + SC4H9O =] C2H5O2 + SC4H9O2",
                                 "O2 + C4H71-4 [=] C4H71-4O2", "O2 + C2H5COC2H5 =] HO2 + C2H5COC2H4P",
                                 "O2 + C2H3 [=] OH + CH2CO", "O2 + IC4H8OH-TI =] OH + CH2O + IC3H5OH",
                                 "O2 + NC7H15O + QC7H15O =] NC7H15O2 + QC7H15O2", "O2 + PC4H8OH [=] C4H8OH-1O2",
                                 "O2 + PC7H15 =] HO2 + PC7H14", "O2 + IC4H8OH-IT [=] OH + C2CY(COC)OH",
                                 "O2 + IC8H18 =] HO2 + DC8H17", "O2 + C5H11O2H-2 =] HO2 + C5H11O2-2",
                                 "O2 + XC7H15O + ZC7H15O =] XC7H15O2 + ZC7H15O2", "O2 + CH2COHCHO [=] HO2 + CH2COHCO",
                                 "O2 + C4H10 =] HO2 + PC4H9", "O2 + BC5H11O + CC5H11O =] BC5H11O2 + CC5H11O2",
                                 "O2 + GC6H13O =] GC6H13O2", "O2 + AC8H17O =] AC8H17O2",
                                 "O2 + CH3O + FC6H13O =] CH3O2 + FC6H13O2", "O2 + C7H162-4 =] HO2 + XC7H15",
                                 "O2 + NC3H7 =] NC3H7O2", "H + O2 + AR [=] HO2 + AR",
                                 "O2 + C3H6OOH1-2 [=] C3H51-2,3OOH", "O2 + NAPH- [=] O + NAPHO",
                                 "O2 + C6H5CH2 [=] O + CH2O + C6H5", "O2 + C3H6OOH1-2 [=] HO2 + C3H5-SOOH",
                                 "O2 + AC5H11 =] HO2 + AC5H10", "O2 + C4H61-4OOH3 [=] C4H61-OOH3-OO4",
                                 "O2 + C4H8-1 [=] HO2 + C4H71-2", "O2 + AC5H11O + BC5H11O =] AC5H11O2 + BC5H11O2",
                                 "O2 + C6H13-2 =] HO2 + C6H12-1", "O2 + C3H6OOH2-1 [=] C3H6OOH2-1O2", "O2 + C2 [=] CO",
                                 "O2 + C6H13O-1 + C6H13O-3 =] C6H13O2-1 + C6H13O2-3", "O2 + CH3OCH2 [=] CH3OCH2O2",
                                 "O2 + C5H10OOH1-5 =] C5H10OOH1-5O2", "O2 + DC5H10OOH-B =] DC5H10OOH-BO2",
                                 "O2 + CH3CHCOCH3 =] CH3CHOOCOCH3", "O2 + C2H5OH =] HO2 + SC2H4OH",
                                 "O2 + OH =] O + HO2", "O2 + C4H8OH-2 =] C4H8OH-2O2",
                                 "O2 + PC4H9O + TC4H9O =] TC4H9O2 + PC4H9O2", "O2 + CH2 =] H2O + CO",
                                 "O2 + C5H11-2 =] C5H11O2-2", "O2 + C2H5 =] C2H4O2H", "O2 + NEOC6H14 =] HO2 + GC6H13",
                                 "O2 + PC7H14OOH-Q =] PC7H14OOH-QO2", "O2 + HCO [=] HO2 + CO",
                                 "O2 + PC7H14OOH-O =] PC7H14OOH-OO2", "O2 + IC3H5COCH3 =] HO2 + AC3H4COCH3",
                                 "O2 + C3H6OOH2-1 [=] HO2 + AC3H5OOH", "O2 + C3H6OOH1-3 [=] HO2 + AC3H5OOH",
                                 "O2 + EC6H12OOH-B =] EC6H12OOH-BO2", "O2 + C4H8-2 [=] HO2 + C4H72-2",
                                 "O2 + IC4H8OH-IT [=] TQC4H7OHI", "O2 + C6H12OOH1-5 =] C6H12OOH1-5O2",
                                 "O2 + IC4H8 =] HO2 + IC4H7", "O2 + NC7H15O =] NC7H15O2",
                                 "O2 + C4H61-2OOH34 [=] C4H61-OOH34-OO2", "O2 + C2H [=] CO2 + CH*",
                                 "O2 + SC3H5CHO [=] HO2 + SC3H5CO", "O2 + C4H [=] HCCO + C2O",
                                 "O2 + CH3OCH2 =] OH + CH2O", "O2 + CC6H12OOH-A =] CC6H12OOH-AO2",
                                 "O2 + OHV [=] O2 + OH", "O2 + C3H5-S =] HCO + CH3CHO"],
                        "domain": "ontokin_reaction", "score": 2.0, "target": "reactions"}]
mock_response_mop_2 = [{"node": "AssemblyModel_79cf8eef-6345-4dd2-9809-5eb4ccd4a975",
                        "value": "MetalOrganicPolyhedra_79cf8eef-6345-4dd2-9809-5eb4ccd4a975"},
                       {"node": "AssemblyModel_47213604-4c8b-4365-9f3e-7fcd5faeb704",
                        "value": "MetalOrganicPolyhedra_47213604-4c8b-4365-9f3e-7fcd5faeb704"},
                       {"node": "AssemblyModel_67b36e72-22a7-4677-83cf-bb7296f5d27b",
                        "value": "MetalOrganicPolyhedra_67b36e72-22a7-4677-83cf-bb7296f5d27b"},
                       {"node": "AssemblyModel_d9f72612-3568-4e57-9f0c-5c90465c9fec",
                        "value": "MetalOrganicPolyhedra_d9f72612-3568-4e57-9f0c-5c90465c9fec"},
                       {"node": "AssemblyModel_11bca581-56bb-4bfc-9809-a9589173f914",
                        "value": "MetalOrganicPolyhedra_11bca581-56bb-4bfc-9809-a9589173f914"},
                       {"node": "AssemblyModel_ccce264f-76d0-4457-bc6a-10629fe25404",
                        "value": "MetalOrganicPolyhedra_ccce264f-76d0-4457-bc6a-10629fe25404"},
                       {"node": "AssemblyModel_9d6ea4ba-9372-4a15-9784-abe5ad1c6512",
                        "value": "MetalOrganicPolyhedra_9d6ea4ba-9372-4a15-9784-abe5ad1c6512"},
                       {"node": "AssemblyModel_022e4c36-3bc8-4f2b-8309-b1af4556204f",
                        "value": "MetalOrganicPolyhedra_022e4c36-3bc8-4f2b-8309-b1af4556204f"},
                       {"node": "AssemblyModel_a6865ad0-9f3d-4733-97ca-5d0cea86ec84",
                        "value": "MetalOrganicPolyhedra_a6865ad0-9f3d-4733-97ca-5d0cea86ec84"},
                       {"node": "AssemblyModel_ea804c42-4e78-47a4-8dfe-c65362d2ed85",
                        "value": "MetalOrganicPolyhedra_ea804c42-4e78-47a4-8dfe-c65362d2ed85"},
                       {"node": "AssemblyModel_d07b12ac-bb61-4abf-984f-c7c69dfea499",
                        "value": "MetalOrganicPolyhedra_d07b12ac-bb61-4abf-984f-c7c69dfea499"},
                       {"node": "AssemblyModel_4541a90b-7143-48b8-8c3a-e742a1351940",
                        "value": "MetalOrganicPolyhedra_4541a90b-7143-48b8-8c3a-e742a1351940"},
                       {"node": "AssemblyModel_85e5a959-7ece-4a34-8bac-0a4d7a9aff8c",
                        "value": "MetalOrganicPolyhedra_85e5a959-7ece-4a34-8bac-0a4d7a9aff8c"},
                       {"node": "AssemblyModel_9718b8fa-c44b-4a7c-b24f-8d57ab6f3c8f",
                        "value": "MetalOrganicPolyhedra_9718b8fa-c44b-4a7c-b24f-8d57ab6f3c8f"},
                       {"node": "AssemblyModel_ce490f69-fcc0-40f6-a1ac-75b8312202fa",
                        "value": "MetalOrganicPolyhedra_ce490f69-fcc0-40f6-a1ac-75b8312202fa"},
                       {"node": "AssemblyModel_315f459f-4ea0-4da8-b7a3-e791e669f50f",
                        "value": "MetalOrganicPolyhedra_ce490f69-fcc0-40f6-a1ac-75b8312202fa"}
                       ]


mock_response_mop_3 = [{"node": "AssemblyModel_75b7fbdf-f0cb-4dc0-a260-da7cbce678c5",
                        "value": "MetalOrganicPolyhedra_75b7fbdf-f0cb-4dc0-a260-da7cbce678c5"}]

mock_response_mop_4 = [{"node": "Species_55e8d513-58c9-416a-8eb0-aa8e3ea73f27", "value": 588.15},
                       {"node": "Species_c2f45802-e09e-4ce2-8fbd-36fe85424cc6", "value": 280.53888},
                       {"node": "Species_cba9378c-b272-45b1-a614-52c80d11ba0f", "value": 374.2611},
                       {"node": "Species_f65dc7cf-37ea-4081-a14e-8a43e7c783af", "value": 352.15},
                       {"node": "Species_70f5eb2c-e74c-4147-8bae-41dbeff35255", "value": 20.372223},
                       {"node": "Species_663e4fd4-1fcd-4b07-9521-19eedf78c55d", "value": 423.37222},
                       {"node": "Species_5027d3fa-6d2e-4e93-8207-58b53e63d857", "value": 573.15},
                       {"node": "Species_1c410372-f682-47f0-9c55-01594694c0f1", "value": 345.37222},
                       {"node": "Species_c510ad6b-5fe7-4de3-ae7c-29c2497d70a4", "value": 81.48333},
                       {"node": "Species_1e7d8a91-2ea9-4c75-b9ed-8396f13573bb", "value": 112.15},
                       {"node": "Species_5c09ae8b-a26f-4b61-b706-4ed03f83f162", "value": 462.15},
                       {"node": "Species_b2671f82-7fe9-495a-b17a-f352a5713f29", "value": 188.15},
                       {"node": "Species_bbf861f3-a870-446a-accb-2c284c12b379", "value": 512.15},
                       {"node": "Species_159a54d0-3b24-4367-9a25-e71ef957d6e3", "value": 615.15},
                       {"node": "Species_5668d370-bf13-4626-a76d-a8602700beaf", "value": 212.82222},
                       {"node": "Species_a34ac75f-7a71-4bc5-b2b1-1fe3e449d36b", "value": 470.92776},
                       {"node": "Species_e3b2d2f7-c248-46b3-bbbe-07ce1dd908d4", "value": 239.81667},
                       {"node": "Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b", "value": 452.15},
                       {"node": "Species_a04c658b-5377-4c41-a59c-a810e1885c28", "value": 353.15},
                       {"node": "Species_09bd6732-d24b-4420-9c5e-65153a842a3c", "value": 206.48334},
                       {"node": "Species_9d130682-4c1e-457b-adea-0e6d77591e9c", "value": 390.37222},
                       {"node": "Species_7e752156-2b9d-4fa3-a3cb-7bd501897ac0", "value": 437.15}]


mock_response_mop_5 = [{  "node": "{\"Heat capacity at Constant Pressure\": {\"y\": {\"value\": [\"65.17\", \"65.51\", \"82.54\", \"96.74\", \"108.19\", \"117.53\", \"125.32\", \"131.93\", \"137.61\", \"146.76\", \"156.47\", \"161.08\", \"166.15\", \"171.51\", \"174.72\", \"176.76\", \"178.14\", \"179.10\", \"179.81\"], \"unit\": \"J/mol/K\"}, \"x\": {\"value\": [\"298.15\", \"300.00\", \"400.00\", \"500.00\", \"600.00\", \"700.00\", \"800.00\", \"900.00\", \"1000.00\", \"1200.00\", \"1500.00\", \"1700.00\", \"2000.00\", \"2500.00\", \"3000.00\", \"3500.00\", \"4000.00\", \"4500.00\", \"5000.00\"], \"unit\": \"K\"}}, \"Heat capacity at Constant Volume\": {\"y\": {\"value\": [\"56.86\", \"57.19\", \"74.22\", \"88.43\", \"99.87\", \"109.21\", \"117.00\", \"123.62\", \"129.29\", \"138.44\", \"148.15\", \"152.77\", \"157.84\", \"163.20\", \"166.40\", \"168.45\", \"169.82\", \"170.79\", \"171.49\"], \"unit\": \"J/mol/K\"}, \"x\": {\"value\": [\"298.15\", \"300.00\", \"400.00\", \"500.00\", \"600.00\", \"700.00\", \"800.00\", \"900.00\", \"1000.00\", \"1200.00\", \"1500.00\", \"1700.00\", \"2000.00\", \"2500.00\", \"3000.00\", \"3500.00\", \"4000.00\", \"4500.00\", \"5000.00\"], \"unit\": \"K\"}}}",
                           "domain": "ontoagent", "score": 2.0, "target": "ontothermoagent"}]
mock_response_mop_6 = [
    {
        "node": "5",
        "domain": "pubchem",
        "score": 2,
        "target": "c2h2o3"
    },
    {
        "node": "0",
        "domain": "pubchem",
        "score": 1.36,
        "target": "c2h2o3"
    },
    {
        "node": "1",
        "domain": "pubchem",
        "score": 1.3,
        "target": "c2h2o3"
    },
    {
        "node": "C(=O)C(=O)O",
        "domain": "pubchem",
        "score": 1.26,
        "target": "c2h2o3"
    },
    {
        "node": "3",
        "domain": "pubchem",
        "score": 1.2,
        "target": "c2h2o3"
    }
]

response_list = [mock_response_mop_1, mock_response_mop_2, mock_response_mop_3, mock_response_mop_4,
                 mock_response_mop_5, mock_response_mop_6]

response_list_tmp = response_list
import json
from flask import Flask, request
from flask import render_template, send_from_directory
from Marie.ChatGPTQAEngine import ChatGPTQAEngine

chatbot = ChatGPTQAEngine()
app = Flask(__name__)
response_counter = 0


def result_filter(result):
    if "ontoagent" in str(result):
        print("ontoagent result", result)
        return result

    if type(result) == type([]):
        sample_node = result[0]
        if "domain" in sample_node:
            domain = sample_node["domain"]
            if domain == "ontokin_reaction":
                result_tmp = []
                for row in result[0]["node"]:
                    result_tmp.append({"node": row})
                pprint(result_tmp)
                return result_tmp

    result_tmp = []

    if "score" in str(result):
        for row in result:
            row_tmp = {}
            for row_key in row:
                if row_key.lower() != "score":
                    row_tmp[row_key] = row[row_key]
            result_tmp.append(row_tmp)
        # this is important, this only keeps the core information about a single element answer
        return {"single": result_tmp[0]}
        # return [result_tmp[0]]
    else:
        return result


@app.route('/')
def hello_world():
    return render_template('index_new.html')


@app.route('/ontospecies')
def ontospecies():
    return render_template('ontospecies.html')


@app.route('/static/<path:path>')
def send_js(path):
    return send_from_directory('static', path)


@app.route("/ask_chatgpt", methods=['GET'])
def ask_chatgpt():
    args = request.args
    question = args["question"]
    return json.dumps({"result": chatbot.ask_question(question)})


@app.route("/search", methods=['GET'])
def search():
    return json.dumps(result_filter(response_list.pop()))


if __name__ == "__main__":
    # Only for debugging while developing
    app.run(host='0.0.0.0', debug=True, port=5003)
