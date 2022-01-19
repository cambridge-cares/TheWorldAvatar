import pytest

def pytest_addoption(parser):
    parser.addoption("--clean-tests", action="store", default=True)

@pytest.fixture
def clean_tests(request):
	return request.config.getoption('--clean-tests')