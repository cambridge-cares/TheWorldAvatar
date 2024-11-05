from functools import cache
import logging
from typing import Annotated

from fastapi import Depends

from .normalise_qt import QtNormaliser, get_qt_normaliser


logger = logging.getLogger(__name__)


class NlqRewriter:
    def __init__(self, qt_normaliser: QtNormaliser):
        self.qt_normaliser = qt_normaliser

    def rewrite(self, question: str):
        try:
            rewritten = self.qt_normaliser.normalise(question)
            logger.info(f'"{question}" has been rewritten to "{rewritten}".')
            return rewritten
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
