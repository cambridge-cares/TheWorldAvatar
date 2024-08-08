from functools import cache
from importlib import resources

from pydantic import TypeAdapter

from model.qt_recog import QtRecogExample


class QtRecogExampleStore:
    QTRECOG_EXAMPLES_DIRNAME = "qtRecog_examples"

    def __init__(self):
        files = [
            f
            for f in resources.files("data")
            .joinpath(self.QTRECOG_EXAMPLES_DIRNAME)
            .iterdir()
            if f.is_file() and f.name.endswith(".json")
        ]
        adapter = TypeAdapter(list[QtRecogExample])

        self.examples = [
            example for f in files for example in adapter.validate_json(f.read_text())
        ]

    def retrieve(self):
        return self.examples


@cache
def get_qtRecog_exampleStore():
    return QtRecogExampleStore()
