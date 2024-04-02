import json
import os

import numpy as np
import triton_python_backend_utils as pb_utils
from transformers import AutoTokenizer
from optimum.onnxruntime import ORTModelForSeq2SeqLM


_ENGINE_ARGS_FILENAME = "model.json"

class TritonPythonModel:
    @staticmethod
    def auto_complete_config(auto_complete_model_config):
        # input [
        #   {
        #     name: "INPUT"
        #     data_type: TYPE_STRING
        #     dims: [-1]
        #   }
        # ]
        # output [
        #   {
        #     name: "OUTPUT"
        #     data_type: TYPE_STRING
        #     dims: [-1]
        #   }
        # ]

        # instance_group [
        #   {
        #     kind: KIND_CPU
        #   }
        # ]
        inputs = [
            {"name": "INPUT", "data_type": "TYPE_STRING", "dims": [-1]},
        ]
        outputs = [{"name": "OUTPUT", "data_type": "TYPE_STRING", "dims": [-1]}]

        # Store the model configuration as a dictionary.
        config = auto_complete_model_config.as_dict()
        input_names = [input["name"] for input in config["input"]]
        output_names = [output["name"] for output in config["output"]]

        # Add only missing inputs and output to the model configuration.
        for input in inputs:
            if input["name"] not in input_names:
                auto_complete_model_config.add_input(input)
        for output in outputs:
            if output["name"] not in output_names:
                auto_complete_model_config.add_output(output)

        auto_complete_model_config.set_max_batch_size(0)
        
        return auto_complete_model_config

    def initialize(self, args):
        engine_args_filepath = os.path.join(
            pb_utils.get_model_dir(), _ENGINE_ARGS_FILENAME
        )

        assert os.path.isfile(
            engine_args_filepath
        ), f"'{_ENGINE_ARGS_FILENAME}' containing engine args must be provided in '{pb_utils.get_model_dir()}'"
        with open(engine_args_filepath) as file:
            engine_config = json.load(file)

        model_path = engine_config["model_path"]
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
            input_text = (
                pb_utils.get_input_tensor_by_name(request, "INPUT")
                .as_numpy()
                .tolist()[0]
                .decode("UTF-8")
            )

            output_text = self.forward(input_text)

            inference_response = pb_utils.InferenceResponse(
                output_tensors=[
                    pb_utils.Tensor("OUTPUT", np.array([output_text], dtype=object)),
                ]
            )
            responses.append(inference_response)

        return responses
