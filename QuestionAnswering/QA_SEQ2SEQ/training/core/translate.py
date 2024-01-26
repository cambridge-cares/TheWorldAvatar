import traceback
from typing import Dict, Optional

from core.args_schema import ModelArguments
from core.data_processing.constants import T5_PREFIX_DOMAINCLS, T5_PREFIX_NL2SPARQL
from core.data_processing.nl import preprocess_nl
from core.data_processing.ontobuiltenv.postprocess import OBEPostProcessor
from core.data_processing.ontocompchem.postprocess import OCCPostProcessor
from core.data_processing.ontokin.postprocess import OKPostProcessor
from core.data_processing.ontospecies.postprocess import OSPostProcessor
from core.data_processing.postprocess import PostProcessor
from core.data_processing.sparql import postprocess_sparql
from core.sparql import SparqlQuery


from core.args_schema import ModelArguments
from core.model_utils import get_hf_model_and_tokenizer, get_ort_model_and_tokenizer


class ModelWrapper:
    def __init__(
        self,
        model_args: ModelArguments,
        max_new_tokens: int = 256,
    ):
        if model_args.model_format == "hf":
            model, tokenizer = get_hf_model_and_tokenizer(model_args)
        elif model_args.model_format == "onnx":
            model, tokenizer = get_ort_model_and_tokenizer(model_args)
        else:
            raise ValueError("Unrecognized model_format: " + model_args.model_format)
            
        self.model = model
        self.tokenizer = tokenizer
        self.max_new_tokens = max_new_tokens

    def forward(self, text: str):
        encoded_input = self.tokenizer(text, return_tensors="pt").to(self.model.device)
        output_ids = self.model.generate(
            **encoded_input, max_new_tokens=self.max_new_tokens
        )
        return self.tokenizer.decode(output_ids[0], skip_special_tokens=True)


class Translator:
    def __init__(
        self,
        model_args: ModelArguments,
        max_new_tokens: int = 256,
        domain: Optional[str] = None
    ):
        self.model = ModelWrapper(model_args, max_new_tokens=max_new_tokens)
        self.domain = domain
        self.domain2postprocessor: Dict[str, PostProcessor] = dict(
            ontospecies=OSPostProcessor(),
            ontokin=OKPostProcessor(),
            ontocompchem=OCCPostProcessor(),
            ontobuiltenv=OBEPostProcessor()
        )

    def get_domain(self, question: str):
        if self.domain is not None:
            return self.domain
        return self.model.forward(T5_PREFIX_DOMAINCLS + question)

    def nl2sparql(self, question: str):
        question_encoded = preprocess_nl(question)

        domain = self.get_domain(question_encoded)
        pred_raw = self.model.forward(T5_PREFIX_NL2SPARQL + question_encoded)
        pred_decoded = postprocess_sparql(pred_raw)

        try:
            pred_decoded_parsed = SparqlQuery.fromstring(pred_decoded)
            pred_verbose = self.domain2postprocessor[domain].postprocess(
                query=pred_decoded_parsed, nlq=question
            )
            pred_verbose_str = str(pred_verbose)
        except Exception as e:
            print(e)
            traceback.print_tb(e.__traceback__)
            pred_verbose_str = None

        return dict(
            domain=domain,
            sparql=dict(
                raw=pred_raw,
                decoded=pred_decoded,
                verbose=pred_verbose_str,
            ),
        )
