from functools import cache
import logging
from typing import Annotated
from fastapi import Depends
from openai import OpenAI
from pydantic import TypeAdapter

from config import AppSettings, get_app_settings
from model.nlq2datareq import DataRequest
from model.qa import TranslationContext
from utils.rdf import try_make_prefixed_iri


logger = logging.getLogger(__name__)


class Nlq2DataReqTranslator:
    SYSTEM_MSG = "You are a SPARQL expert designed to output JSON."
    PROMPT_TEMPLATE = """### RDF schema:
{schema}

### Examples of translating natural language questions to executable data requests:
{examples}

### Instruction: 
Your task is to translate the following question to an executable data request. Please do not provide any explanation and respond with a single JSON object exactly.

### Question:
{question}"""

    def __init__(
        self,
        openai_base_url: str | None,
        openai_api_key: str | None,
        openai_model: str,
    ):
        self.openai_client = OpenAI(base_url=openai_base_url, api_key=openai_api_key)
        self.model = openai_model
        self.datareq_adapter = TypeAdapter(DataRequest)

    def translate(
        self, nlq: str, translation_context: TranslationContext
    ) -> DataRequest:
        prompt = self.PROMPT_TEMPLATE.format(
            examples="\n".join(
                '"{input}" => {output}'.format(
                    input=example.nlq,
                    output=example.data_req.model_dump_json(),
                )
                for example in translation_context.examples
            ),
            schema="\n".join(
                "({s})-[{p}]->({o})".format(
                    s=try_make_prefixed_iri(rel.s),
                    p=try_make_prefixed_iri(rel.p),
                    o=try_make_prefixed_iri(rel.o),
                )
                for rel in translation_context.schema_relations
            ),
            question=nlq,
        )
        logger.info("PROMPT:\n" + prompt)

        res = self.openai_client.chat.completions.create(
            model=self.model,
            response_format={"type": "json_object"},
            messages=[
                {"role": "system", "content": self.SYSTEM_MSG},
                {"role": "user", "content": prompt},
            ],
            temperature=0,
        )
        logger.info("LLM's response: " + str(res))

        return self.datareq_adapter.validate_json(res.choices[0].message.content)


@cache
def get_nlq2datareq_translator(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return Nlq2DataReqTranslator(
        openai_base_url=settings.translator.base_url,
        openai_api_key=settings.translator.api_key,
        openai_model=settings.translator.model,
    )
