import os

import numpy as np
import triton_python_backend_utils as pb_utils
from transformers import AutoTokenizer
from transformers.generation.configuration_utils import GenerationConfig
from transformers.utils.hub import cached_file
from optimum.onnxruntime import ORTModelForSeq2SeqLM
from optimum.utils.save_utils import maybe_load_preprocessors

from compact_query.compact_query_rep import CompactQueryRep
from compact_query.correct_relations import RelationCorrector
from compact_query.correct_spans import SpanCorrector


def replace_multi(text: str, mapper: dict):
    for k, v in mapper.items():
        text = text.replace(k, v)
    return text


class TritonPythonModel:
    MODEL_PATH = os.getenv("TRANSLATION_MODELPATH")
    T5_INPUT_PREFIX = "translate to SPARQL: "
    T5_INPUT_ENCODINGS = {
        "<": "&lt;",
        ">": "&gt;",
    }
    T5_OUTPUT_DECODINGS = {
        "var_": "?",
        "&lcub;": "{",
        "&rcub;": "}",
        "&lt;": "<",
        "&gt;": ">",
    }

    def _load_model(self):
        encoder_path = cached_file(self.MODEL_PATH, "encoder_model_quantized.onnx")
        decoder_path = cached_file(self.MODEL_PATH, "decoder_model_quantized.onnx")
        decoder_wp_path = cached_file(
            self.MODEL_PATH, "decoder_with_past_model_quantized.onnx"
        )

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
        self.max_new_tokens = 256

        self.span_corrector = SpanCorrector()
        self.relation_corrector = RelationCorrector(os.getenv("EMBEDDING_MODELPATH"))

    def preprocess(self, text):
        text = replace_multi(text, self.T5_INPUT_ENCODINGS)
        text = self.T5_INPUT_PREFIX + text
        return text

    def postprocess(self, text):
        text = replace_multi(text, self.T5_OUTPUT_DECODINGS)
        return text

    def translate(self, text):
        text = self.preprocess(text)

        input_ids = self.tokenizer(text, return_tensors="pt").input_ids
        output_ids = self.model.generate(
            input_ids=input_ids, max_new_tokens=self.max_new_tokens
        )
        output_text = self.tokenizer.decode(output_ids[0], skip_special_tokens=True)

        output_text = self.postprocess(output_text)
        return output_text

    def execute(self, requests):
        responses = []

        for request in requests:
            input_text = (
                pb_utils.get_input_tensor_by_name(request, "text")
                .as_numpy()
                .tolist()[0]
                .decode("UTF-8")
            )
            output_text = self.translate(input_text)

            try:
                query = CompactQueryRep.from_string(output_text)
                query = self.span_corrector.correct(query, nlq=input_text)
                query = self.relation_corrector.correct(query)
                verbose_output = query.to_verbose()
            except:
                verbose_output = ""

            inference_response = pb_utils.InferenceResponse(
                output_tensors=[
                    pb_utils.Tensor(
                        "output_compact", np.array([output_text], dtype=object)
                    ),
                    pb_utils.Tensor(
                        "output_verbose", np.array([verbose_output], dtype=object)
                    ),
                ]
            )
            responses.append(inference_response)

        return responses
