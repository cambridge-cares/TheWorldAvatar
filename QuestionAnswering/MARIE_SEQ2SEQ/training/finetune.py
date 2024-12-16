import os

from datasets import Dataset
import transformers
from transformers import (
    DataCollatorForSeq2Seq,
    Seq2SeqTrainingArguments,
    Seq2SeqTrainer,
    TrainingArguments,
)
from trl import SFTTrainer
from core.constants import FAMILY_CAUSAL, FAMILY_SEQ2SEQ

from core.data_processing.input_processing import preprocess_input
from core.data_processing.output_processing import preprocess_output
from core.arguments_schema import DatasetArguments, ModelArguments
from core.model_utils.hf import get_hf_model_and_tokenizer


def get_seq2seq_trainer(
    model_args: ModelArguments,
    data_args: DatasetArguments,
    train_args: Seq2SeqTrainingArguments,
):
    model, tokenizer = get_hf_model_and_tokenizer(model_args, is_trainable=True)

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
        sources = [
            preprocess_input(qn, model_family=model_args.model_family)
            for qn in examples["question"]
        ]
        targets = [
            preprocess_output(query, model_family=model_args.model_family)
            for query in examples["sparql_query_compact"]
        ]
        return dict(source=sources, target=targets)

    def _get_tokenized_dataset(data_path: str):
        dataset = Dataset.from_json(data_path).shuffle(seed=42)
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


def get_sft_trainer(
    model_args: ModelArguments,
    data_args: DatasetArguments,
    train_args: TrainingArguments,
):
    model, tokenizer = get_hf_model_and_tokenizer(model_args, is_trainable=True)

    model.config.use_cache = False
    model.config.pretraining_tp = 1

    train_dataset = Dataset.from_json(data_args.train_data_path)
    eval_dataset = Dataset.from_json(data_args.eval_data_path)

    def formatting_func(examples):
        output_texts = []
        for i in range(len(examples["question"])):
            input_preprocessed = preprocess_input(
                examples["question"][i], model_family=model_args.model_family
            )
            output_preprocessed = preprocess_output(
                examples["sparql_query_compact"][i],
                model_family=model_args.model_family,
            )
            text = input_preprocessed + output_preprocessed
            output_texts.append(text)
        return output_texts

    return SFTTrainer(
        model=model,
        tokenizer=tokenizer,
        train_dataset=train_dataset,
        eval_dataset=eval_dataset,
        formatting_func=formatting_func,
        args=train_args,
    )


def train():
    hfparser = transformers.HfArgumentParser(
        (ModelArguments, DatasetArguments, Seq2SeqTrainingArguments)
    )
    model_args, data_args, train_args = hfparser.parse_args_into_dataclasses()

    if model_args.model_family in FAMILY_SEQ2SEQ:
        trainer = get_seq2seq_trainer(
            model_args=model_args,
            data_args=data_args,
            train_args=train_args,
        )
    elif model_args.model_family in FAMILY_CAUSAL:
        trainer = get_sft_trainer(
            model_args=model_args,
            data_args=data_args,
            train_args=train_args,
        )
    else:
        raise ValueError(model_args.model_family + " is not supported.")

    trainer.train()

    model_output_dir = os.path.join(train_args.output_dir, "model")
    trainer.model.save_pretrained(model_output_dir)
    trainer.tokenizer.save_pretrained(model_output_dir)


if __name__ == "__main__":
    train()
