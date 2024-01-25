import os

import numpy as np
import triton_python_backend_utils as pb_utils
from transformers import AutoTokenizer
from optimum.onnxruntime import ORTModelForSeq2SeqLM


class TritonPythonModel:
    def initialize(self, args):
        model_path = os.getenv("SEQ2SEQ_MODEL_PATH")
        print("Loading weights for seq2seq model from: " + model_path, flush=True)

        self.tokenizer = AutoTokenizer.from_pretrained(model_path)
        self.model = ORTModelForSeq2SeqLM.from_pretrained(model_path)
        self.max_new_tokens = 512

    def forward(self, text: str):
        encoded_input = self.tokenizer(text, return_tensors="pt").to(self.model.device)
        output_ids = self.model.generate(
            **encoded_input, max_new_tokens=self.max_new_tokens
        )
        return self.tokenizer.decode(output_ids[0], skip_special_tokens=True)
        
    def execute(self, requests):
        responses = []

        for request in requests:
            input_text = pb_utils.get_input_tensor_by_name(
                request, "INPUT"
            ).as_numpy().tolist()[0].decode("UTF-8")

            output_text = self.forward(input_text)
            
            inference_response = pb_utils.InferenceResponse(
                output_tensors=[
                    pb_utils.Tensor("OUTPUT", np.array([output_text], dtype=object)),
                ]
            )
            responses.append(inference_response)

        return responses
