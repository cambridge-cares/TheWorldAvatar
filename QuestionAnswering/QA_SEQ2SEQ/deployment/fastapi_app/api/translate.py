import logging
import os
import time
from typing import Annotated, Optional

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from services.preprocess import IPreprocessor
from services.preprocess.identity import IdentityPreprocessor
from services.preprocess.chemistry import ChemistryPreprocessor
from services.translate import ITranslator, Translator


class TranslateRequest(BaseModel):
    question: str


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


def get_translator() -> ITranslator:
    return Translator()


def get_preprocessor() -> IPreprocessor:
    if os.getenv("QA_SUPERDOMAIN") == "chemistry":
        return ChemistryPreprocessor()
    else:
        return IdentityPreprocessor()


@router.post("")
def translate(
    req: TranslateRequest,
    translator: Annotated[ITranslator, Depends(get_translator)],
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
    translation_result = translator.nl2sparql(preprocessed_text.for_trans)
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
