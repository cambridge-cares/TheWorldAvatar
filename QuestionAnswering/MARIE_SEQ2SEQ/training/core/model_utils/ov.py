from core.arguments_schema import ModelArguments
from core.constants import FAMILY_SEQ2SEQ
from core.model_utils.hf import get_hf_tokenizer


def get_ov_model_and_tokenizer(model_args: ModelArguments, max_input_tokens: int = 256):
    from optimum.intel import OVModelForSeq2SeqLM, OVModelForCausalLM

    model_cls = (
        OVModelForSeq2SeqLM if model_args.model_family in FAMILY_SEQ2SEQ else OVModelForCausalLM
    )
    model = model_cls.from_pretrained(model_args.model_path)

    model.reshape(1, max_input_tokens)
    model.compile()

    tokenizer = get_hf_tokenizer(model_args)
    return model, tokenizer
