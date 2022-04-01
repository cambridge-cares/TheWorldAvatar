import pytest
import pathlib
import os
from typing import List, Callable


def pytest_addoption(parser):
    parser.addoption("--clean-tests", action="store", default=True)
    parser.addoption(
        "--upload-tests", action="store", default=True, help="Run uploads tests"
    )


@pytest.fixture
def clean_tests(request):
    return request.config.getoption("--clean-tests")


@pytest.fixture
def cleanup_test_data() -> Callable[[List[str]], None]:
    def _cleanup_test_data(files: List[str]) -> None:
        for file in files:
            os.remove(file)

    return _cleanup_test_data


def pytest_configure(config):
    upload_tests = config.getoption("--upload-tests")
    if not upload_tests:
        print("Upload tests skipped by default. Enable via --upload-tests flag.")
        config.args = [
            p
            for p in pathlib.Path().rglob("test_abox_*")
            if p.is_dir() and "uploads" not in str(p)
        ]
