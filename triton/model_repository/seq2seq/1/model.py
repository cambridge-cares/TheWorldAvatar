import os

import numpy as np
import triton_python_backend_utils as pb_utils
from transformers import AutoTokenizer
from transformers.generation.configuration_utils import GenerationConfig
from transformers.utils.hub import cached_file
from optimum.onnxruntime import ORTModelForSeq2SeqLM
from optimum.utils.save_utils import maybe_load_preprocessors


class TritonPythonModel:
    MODEL_PATH = os.getenv("MODEL_PATH")

    def _load_model(self):
        encoder_path = cached_file(self.MODEL_PATH, "encoder_model_quantized.onnx")
        decoder_path = cached_file(self.MODEL_PATH, "decoder_model_quantized.onnx")
        decoder_wp_path = cached_file(self.MODEL_PATH, "decoder_with_past_model_quantized.onnx")

        (
            encoder_session,
            decoder_session,
            decoder_wp_session,
        ) = ORTModelForSeq2SeqLM.load_model(
            encoder_path=encoder_path,
            decoder_path=decoder_path,
            decoder_with_past_path=decoder_wp_path,
        )
        config = ORTModelForSeq2SeqLM._load_config(self.MODEL_PATH)
        generation_config = GenerationConfig.from_pretrained(self.MODEL_PATH)
        preprocessors = maybe_load_preprocessors(self.MODEL_PATH)

        return ORTModelForSeq2SeqLM(
            encoder_session=encoder_session,
            decoder_session=decoder_session,
            config=config,
            onnx_paths=[
                encoder_path,
                decoder_path,
                decoder_wp_path,
            ],
            decoder_with_past_session=decoder_wp_session,
            model_save_dir=self.MODEL_PATH,
            preprocessors=preprocessors,
            generation_config=generation_config,
        )

    def initialize(self, args):
        self.tokenizer = AutoTokenizer.from_pretrained(self.MODEL_PATH)
        self.model = self._load_model()
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
