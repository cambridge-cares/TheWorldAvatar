import os

import numpy as np
import triton_python_backend_utils as pb_utils
from transformers import AutoTokenizer
from transformers.generation.configuration_utils import GenerationConfig
from transformers.utils.hub import cached_file
from optimum.onnxruntime import ORTModelForSeq2SeqLM
from optimum.utils.save_utils import maybe_load_preprocessors


class TritonPythonModel:
    def _load_model(self, model_path: str):
        encoder_path = cached_file(model_path, "encoder_model_quantized.onnx")
        decoder_path = cached_file(model_path, "decoder_model_quantized.onnx")
        decoder_wp_path = cached_file(model_path, "decoder_with_past_model_quantized.onnx")

        (
            encoder_session,
            decoder_session,
            decoder_wp_session,
        ) = ORTModelForSeq2SeqLM.load_model(
            encoder_path=encoder_path,
            decoder_path=decoder_path,
            decoder_with_past_path=decoder_wp_path,
        )
        config = ORTModelForSeq2SeqLM._load_config(model_path)
        generation_config = GenerationConfig.from_pretrained(model_path)
        preprocessors = maybe_load_preprocessors(model_path)

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
            model_save_dir=model_path,
            preprocessors=preprocessors,
            generation_config=generation_config,
        )

    def initialize(self, args):
        model_path = os.getenv("MODEL_PATH", "picas9dan/20231115_6_onnx_8bit")
        print("Loading weights from: " + model_path, flush=True)

        self.tokenizer = AutoTokenizer.from_pretrained(model_path)
        self.model = self._load_model(model_path)
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
