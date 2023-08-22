import os

from datasets import Dataset
import transformers
from transformers import (
    DataCollatorForSeq2Seq,
    Seq2SeqTrainingArguments,
    Seq2SeqTrainer,
    PreTrainedModel,
    PreTrainedTokenizer,
    TrainingArguments,
)
from trl import SFTTrainer, DataCollatorForCompletionOnlyLM

from marie.data_processing.qn_processing import (
    LLAMA_COMPLETION_PREFIX,
    LLAMA_PROMPT_PREFIX,
    t5_preprocess_qn,
)
from marie.data_processing.query_processing import t5_preprocess_query

from marie.arguments_schema import DatasetArguments, ModelArguments
from marie.model_utils import (
    get_model_and_tokenizer,
    get_model_family_from_model_args,
)


def get_t5_trainer(
    model: PreTrainedModel,
    tokenizer: PreTrainedTokenizer,
    data_args: DatasetArguments,
    train_args: Seq2SeqTrainingArguments,
):
    def _tokenize(examples):
        model_inputs = tokenizer(
            examples["source"], max_length=data_args.source_max_len, truncation=True
        )
        labels = tokenizer(
            examples["target"], max_length=data_args.target_max_len, truncation=True
        )
        model_inputs["labels"] = labels["input_ids"]
        return model_inputs

    def _preprocess_examples(examples):
        sources = [t5_preprocess_qn(qn) for qn in examples["question"]]
        targets = [
            t5_preprocess_query(query) for query in examples["sparql_query_compact"]
        ]
        return dict(source=sources, target=targets)

    def _get_tokenized_dataset(data_path: str):
        dataset = Dataset.from_json(data_path)
        dataset = dataset.map(
            _preprocess_examples,
            batched=True,
            remove_columns=[
                x for x in dataset.column_names if x not in ["source", "target"]
            ],
        )
        return dataset.map(_tokenize, batched=True, remove_columns=["source", "target"])

    train_dataset = _get_tokenized_dataset(data_args.train_data_path)
    eval_dataset = _get_tokenized_dataset(data_args.eval_data_path)

    data_collator = DataCollatorForSeq2Seq(tokenizer=tokenizer, model=model)

    return Seq2SeqTrainer(
        model=model,
        args=train_args,
        train_dataset=train_dataset,
        eval_dataset=eval_dataset,
        tokenizer=tokenizer,
        data_collator=data_collator,
    )


def get_llama_trainer(
    model: PreTrainedModel,
    tokenizer: PreTrainedTokenizer,
    data_args: DatasetArguments,
    train_args: TrainingArguments,
):
    train_dataset = Dataset.from_json(data_args.train_data_path)
    eval_dataset = Dataset.from_json(data_args.eval_data_path)

    def formatting_func(examples):
        output_texts = []
        for i in range(len(examples["question"])):
            text = (
                LLAMA_PROMPT_PREFIX
                + examples[i]["question"]
                + LLAMA_COMPLETION_PREFIX
                + examples[i]["sparql_query_compact"]
            )
            output_texts.append(text)
        return output_texts

    collator = DataCollatorForCompletionOnlyLM(
        LLAMA_COMPLETION_PREFIX, tokenizer=tokenizer
    )

    return SFTTrainer(
        model=model,
        tokenizer=tokenizer,
        train_dataset=train_dataset,
        eval_dataset=eval_dataset,
        formatting_func=formatting_func,
        data_collator=collator,
        args=train_args,
    )


def train():
    hfparser = transformers.HfArgumentParser(
        (ModelArguments, DatasetArguments, Seq2SeqTrainingArguments)
    )
    model_args, data_args, train_args = hfparser.parse_args_into_dataclasses()

    model, tokenizer = get_model_and_tokenizer(model_args, is_trainable=True)
    model_family = get_model_family_from_model_args(model_args)

    if model_family == "t5":
        trainer_getter = get_t5_trainer
    else:
        trainer_getter = get_llama_trainer

    trainer = trainer_getter(
        model=model, tokenizer=tokenizer, data_args=data_args, train_args=train_args
    )

    trainer.train()

    model_output_dir = os.path.join(train_args.output_dir, "model")
    model.save_pretrained(model_output_dir)
    tokenizer.save_pretrained(model_output_dir)


if __name__ == "__main__":
    train()
