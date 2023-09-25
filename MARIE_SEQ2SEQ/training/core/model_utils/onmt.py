import os

from core.arguments_schema import ModelArguments


def get_onmt_tokenizer(model_args: ModelArguments):
    import pyonmttok

    if model_args.model_family != "t5":
        raise ValueError(
            f"Only T5 tokenizer has been tested and supported, while the model family is {model_args.model_family}."
        )
    return pyonmttok.SentencePieceTokenizer(
        os.path.join(model_args.model_path, "spiece.model")
    )


def get_onmt_model_and_tokenizer(model_args: ModelArguments):
    import ctranslate2

    tokenizer = get_onmt_tokenizer(model_args)
    model = ctranslate2.Translator(
        model_args.model_path, device=model_args.device_map or "auto"
    )
    return model, tokenizer
