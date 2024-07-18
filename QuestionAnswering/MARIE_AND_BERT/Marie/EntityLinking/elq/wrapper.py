import Marie.EntityLinking.elq.main_dense as main_dense
import argparse
#TODO: let's find where the mysterious id2entity is set
class NEL_ELQ():
    def __init__(self, params):
        config = {
            "interactive": False,
            "biencoder_model": params["biencoder_model"],
            "biencoder_config": params["biencoder_config"],
            "cand_token_ids_path": None,
            "entity_catalogue":params["entity_catalogue"],
            "entity_encoding":params["entity_encoding"],
            "output_path": params["output_path"], # logging directory
            "faiss_index": "none",
            "index_path": "none",
            "num_cand_mentions": 10,
            "num_cand_entities": 10,
            "threshold_type": "joint",
            "threshold": -4.5,
            "iddict_path": params["output_path"]
        }
        self.args = argparse.Namespace(**config)
        self.models = main_dense.load_models(self.args, logger=None)
        print('EL loading finished')

    def infer(self,  test_samples):
        predictions = main_dense.run(self.args, None, *self.models, test_data=test_samples)
        return predictions