from functools import cache
import json
import logging
from typing import Annotated
from fastapi import Depends
from openai import OpenAI
from pydantic import TypeAdapter, ValidationError

from config import AppSettings, get_app_settings
from model.nlq2datareq import DataRequest
from model.structured_answer import TranslationContext
from utils.rdf import try_make_prefixed_iri


logger = logging.getLogger(__name__)


class Nlq2DataReqLLMCaller:
    PROMPT_TEMPLATE = """### Instruction: 
- Your task is to translate the input question to an executable data request based on the provided properties and translation examples. 
- Do not use FILTER clauses for literal matching in SPARQL queries.
- If no reasonable translation can be performed, return "null". 
- Please respond with a single JSON object exactly."""

    def __init__(
        self,
        openai_base_url: str | None,
        openai_api_key: str | None,
        openai_model: str,
    ):
        self.openai_client = OpenAI(base_url=openai_base_url, api_key=openai_api_key)
        self.openai_model = openai_model
        self.datareq_adapter = TypeAdapter(DataRequest)

    def forward(self, nlq: str, translation_context: TranslationContext):

        prompt = self.PROMPT_TEMPLATE
        if len(translation_context.examples) > 0:
            prompt += "\n" + self._get_examples(translation_context)
        if len(translation_context.properties) > 0:
            prompt += "\n" + self._get_properties(translation_context)
        prompt += self._get_questions(nlq)
        logger.info("PROMPT:\n" + prompt)

        res = self.openai_client.chat.completions.create(
            model=self.openai_model,
            response_format={"type": "json_object"},
            messages=[
                {"role": "user", "content": prompt},
            ],
            temperature=0,
        )
        logger.info("LLM's response: " + str(res))

        content = res.choices[0].message.content
        if not content:
            # TODO: handle empty response from OpenAI
            raise Exception()

        if content == "null":
            return None
        try:
            return self.datareq_adapter.validate_json(content)
        except ValidationError as err:
            logger.error(err)
            logger.info("Treat semantic parsing result as None")
            return None
        
    def _get_examples(self, translation_context: TranslationContext):
        return "### Input-output examples:" + "\n".join(
                '"{input}" => {output}'.format(
                    input=example.nlq,
                    output=example.data_req.model_dump_json(
                        exclude_unset=True,
                        exclude_none=True,
                    ),
                )
                for example, _ in sorted(
                    translation_context.examples,
                    key=lambda x: x[1],
                    reverse=True,
                )  # place most relevant example towards the end of the prompt
            )
    
    def _get_properties(self, translation_context: TranslationContext):
        return "### Properties:" + json.dumps(
                [
                    {
                        "IRI": try_make_prefixed_iri(prop.iri),
                        "label": prop.label,
                        "comment": prop.comment,
                    }
                    for prop, _ in sorted(
                        translation_context.properties,
                        key=lambda x: x[1],
                        reverse=True,
                    )  # place most relevant relation towards the end of the prompt
                ],
                indent=2,
            )
    
    def _get_questions(self, nlq: str):
        return "### Input:\n" + nlq



@cache
def get_nlq2datareq_llmCaller(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return Nlq2DataReqLLMCaller(
        openai_base_url=settings.translator.base_url,
        openai_api_key=settings.translator.api_key,
        openai_model=settings.translator.model,
    )
