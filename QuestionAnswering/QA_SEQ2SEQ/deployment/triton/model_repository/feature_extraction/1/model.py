import os

import triton_python_backend_utils as pb_utils
from sentence_transformers import SentenceTransformer

class TritonPythonModel:
    def initialize(self, args):
        model_path = os.getenv("FEATURE_EXTRACTION_MODEL_PATH")
        print("Loading weights for feature extraction model from: " + model_path, flush=True)
        self.model = SentenceTransformer(model_path)
        
    def execute(self, requests):
        responses = []

        for request in requests:
            input_texts = [x.decode("UTF-8") for x in pb_utils.get_input_tensor_by_name(
                request, "INPUT"
            ).as_numpy().tolist()]

            output_embeds = self.model.encode(input_texts)
            
            inference_response = pb_utils.InferenceResponse(
                output_tensors=[
                    pb_utils.Tensor("OUTPUT", output_embeds),
                ]
            )
            responses.append(inference_response)

        return responses
