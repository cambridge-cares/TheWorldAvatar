import os
import unittest
import torch

from Marie.Util.CommonTools.NLPTools import NLPTools
from Marie.Util.Models.CrossGraphAlignmentModel import CrossGraphAlignmentModel
from Marie.Util.location import DATA_DIR


class TestCrossGraph(unittest.TestCase):

    def test(self):
        label_dict = {"pubchem": 0, "ontocompchem": 1, "ontospecies": 2,
                      "ontokin": 3, "wikidata": 4, "ontospecies_new": 5,
                      "ontoagent": 6, "ontomops": 7, "ontokin_reaction": 8}
        label_list = list(label_dict.keys())
        use_cuda = torch.cuda.is_available()
        self.device = torch.device("cuda" if use_cuda else "cpu")
        print(f'=========== USING {self.device} ===============')
        self.model = CrossGraphAlignmentModel(device=self.device).to(self.device)
        nlp = NLPTools(tokenizer_name="bert-base-uncased")
        my_alignment_model = CrossGraphAlignmentModel(device="cpu")
        dataset_path = os.path.join(DATA_DIR, "CrossGraph/cross_graph_model_with_all_9_updated")
        my_alignment_model.load_state_dict(torch.load(dataset_path, map_location="cpu"))
        global_stop_words = ["g/mol", "dalton", "celsius", "show", "give"]

        questions = ["Give the MOPs which are Anticuboctahedron shaped",
                     "molar mass of benzene",
                     "boiling point of CH4",
                     "geometry",
                     "vapour pressure",
                     "chemical structure",
                     "inventor",
                     "some random bullshit",
                     "flash point",
                     "location discovery",
                     "reactions",
                     "heat capacity of C34CD",
                     "mops with assembly model",
                     "cbu with shape",
                     "logs",
                     "logp",
                     "reactions H2 O2",
                     "mops with cuboctahedron shape",
                     "power conversion efficiency",
                     "pce",
                     "power conversion efficiency",
                     "Whats the number of covalent unit C12H24O2?",
                     "What are the chemical species having molecular weight around 150 g/mol",
                     "Find all species with boiling point above 0 celsius",
                     "Show all the MOPs which have Cuboctahedron shape",
                     "Give the MOPs which are Anticuboctahedron shaped",
                     "Give the assembly model types that can be created with [(C4H2S)(CO2)2] as the CBU",
                     ]

        for q in questions:
            original_question = q.replace("'s", " of ")
            tokens = [t for t in original_question.strip().split(" ") if t.lower() not in global_stop_words]
            original_question = " ".join(tokens)

            predicted_domain_labels = []
            _, tokenized_q = nlp.tokenize_question(original_question, 1)
            pred_domain_list = my_alignment_model.predict_domain([tokenized_q])[0]
            for idx, domain in enumerate(pred_domain_list):
                if domain == 1:
                    predicted_domain_labels.append(label_list[idx])
            print("question", q)
            print("filtered question", original_question)
            print("domain", predicted_domain_labels)

        # print(f"Question: {q} \n Predicted domain: {pred_domain}")
        # print(pred_domain.repeat(5, 1))


if __name__ == "__main__":
    my_test = TestCrossGraph()
    my_test.test()
