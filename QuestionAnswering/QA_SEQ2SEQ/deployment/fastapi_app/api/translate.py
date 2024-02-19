from functools import cache
import logging
import os
import time
from typing import Annotated, Optional

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from services.translate.triton_client.feature_extraction_client import (
    FeatureExtractionClient,
    IFeatureExtractionClient,
)
from services.translate.triton_client.seq2seq_client import (
    ISeq2SeqClient,
    Seq2SeqClient,
)
from services.preprocess import IPreprocessor
from services.preprocess.identity import IdentityPreprocessor
from services.preprocess.chemistry import ChemistryPreprocessor
from services.translate import Translator, Translator


class TranslateRequest(BaseModel):
    question: str
    domain: Optional[str]


class TranslateResponseSparql(BaseModel):
    predicted: str
    postprocessed: Optional[str]


class TranslateResponse(BaseModel):
    question: str
    preprocessed_question: str
    domain: str
    sparql: TranslateResponseSparql
    latency: float


logger = logging.getLogger(__name__)

router = APIRouter()


def get_seq2seq_client():
    return Seq2SeqClient()


def get_feature_extraction_client():
    return FeatureExtractionClient()


@cache
def get_translator(
    seq2seq_client: Annotated[ISeq2SeqClient, Depends(get_seq2seq_client)],
    feature_extraction_client: Annotated[
        IFeatureExtractionClient, Depends(get_feature_extraction_client)
    ],
):
    return Translator(seq2seq_client, feature_extraction_client)


def get_preprocessor() -> IPreprocessor:
    if os.getenv("QA_SUPERDOMAIN") == "chemistry":
        return ChemistryPreprocessor()
    else:
        return IdentityPreprocessor()


@router.post("")
def translate(
    req: TranslateRequest,
    translator: Annotated[Translator, Depends(get_translator)],
    preprocessor: Annotated[IPreprocessor, Depends(get_preprocessor)],
):
    logger.info(
        "Received request to translation endpoint with the following request body"
    )
    logger.info(req)

    preprocessed_text = preprocessor.preprocess(req.question)
    logger.info("Preprocessed text: " + str(preprocessed_text))

    logger.info("Sending translation request to triton server")
    start = time.time()
    translation_result = translator.nl2sparql(
        preprocessed_text.for_trans, domain=req.domain
    )
    end = time.time()

    logger.info("Translation result: " + str(translation_result))

    return TranslateResponse(
        question=req.question,
        preprocessed_question=preprocessed_text.for_user,
        domain=translation_result.domain,
        sparql=TranslateResponseSparql(
            predicted=translation_result.sparql.decoded,
            postprocessed=translation_result.sparql.verbose,
        ),
        latency=end - start,
    )
