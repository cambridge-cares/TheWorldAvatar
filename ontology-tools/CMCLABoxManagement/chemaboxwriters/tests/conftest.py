import pytest
import os
from typing import List, Callable


def pytest_addoption(parser):
    parser.addoption("--clean-tests", action="store", default=True)


@pytest.fixture
def clean_tests(request):
    return request.config.getoption("--clean-tests")


@pytest.fixture
def cleanup_test_data() -> Callable[[List[str]], None]:
    def _cleanup_test_data(files: List[str]) -> None:
        for file in files:
            os.remove(file)

    return _cleanup_test_data
