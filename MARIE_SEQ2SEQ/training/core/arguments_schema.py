from dataclasses import dataclass, field
from typing import Optional


@dataclass
class ModelArguments:
    model_path: str = field(default="google/flan-t5-base")
    model_format: str = field(
        default="hf", metadata={"help": "Either `hf` (Hugging Face) or `ctranslate2`."}
    )
    cpu: bool = field(default=False)
    lora_path: Optional[str] = field(default=None)
    # quantization hyperparams
    bits: Optional[int] = field(
        default=None, metadata={"help": "How many bits to use."}
    )
    # lora hyperparams
    lora_r: Optional[int] = field(default=None)
    lora_alpha: Optional[float] = field(default=None)
    lora_dropout: Optional[float] = field(default=None)


@dataclass
class DatasetArguments:
    train_data_path: Optional[str] = field(default=None)
    eval_data_path: Optional[str] = field(default=None)
    source_max_len: int = field(
        default=512,
        metadata={
            "help": "Maximum source sequence length. Sequences will be right padded (and possibly truncated)."
        },
    )
    target_max_len: int = field(
        default=512,
        metadata={
            "help": "Maximum target sequence length. Sequences will be right padded (and possibly truncated)."
        },
    )


@dataclass
class InferenceArguments:
    out_file: str = field(metadata={"help": "File to write predictions to."})
    max_new_tokens: int = field(
        default=512, metadata={"help": "Maximum number of tokens to be generated."}
    )
