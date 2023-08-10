from dataclasses import dataclass, field
from typing import Optional


@dataclass
class ModelArguments:
    model_path: str
    # quantization hyperparams
    bits: Optional[int] = field(
        default=None,
        metadata={"help": "How many bits to use."}
    )
    # lora hyperparams
    lora_r: Optional[int] = field(default=None)
    lora_alpha: Optional[float] = field(default=None)
    lora_dropout: Optional[float] = field(default=None)


@dataclass
class DatasetArguments:
    data_path: str
    source_max_len: int = field(
        default=512,
        metadata={"help": "Maximum source sequence length. Sequences will be right padded (and possibly truncated)."},
    )
    target_max_len: int = field(
        default=512,
        metadata={"help": "Maximum target sequence length. Sequences will be right padded (and possibly truncated)."},
    )

@dataclass
class InferenceArguments:
    out_file: str = field(
        metadata={"help": "File to write predictions to."}
    )
    postprocess: bool = field(
        default=True,
        metadata={"help": "Whether to post-process the output of the machine translation model."}
    )
    max_new_tokens: int = field(
        default=512,
        metadata={"help": "Maximum number of tokens to be generated."}
    )
