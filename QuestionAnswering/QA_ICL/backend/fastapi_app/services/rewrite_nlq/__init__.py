from functools import cache
import logging
from typing import Annotated

from fastapi import Depends

from services.rewrite_nlq.normalise_qt import QtNormaliser, get_qt_normaliser


logger = logging.getLogger(__name__)


class NlqRewriter:

    def __init__(self, qt_normaliser: QtNormaliser):
        self.qt_normaliser = qt_normaliser

    def rewrite(self, question: str):
        try:
            return self.qt_normaliser.normalise(question)
        except Exception as e:
            logger.error(
                "Error encountered while normalising physical quantities: " + str(e)
            )
            logger.info("Error is ignored and NLQ is passed on unmodified.")
            return question


@cache
def get_nlq_rewriter(
    qt_normaliser: Annotated[QtNormaliser, Depends(get_qt_normaliser)]
):
    return NlqRewriter(qt_normaliser=qt_normaliser)
