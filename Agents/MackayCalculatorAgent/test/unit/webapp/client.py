import pytest
from app import app

@pytest.fixture
def client():
    """Configures the app for testing

    Sets app config variable ``TESTING`` to ``True``

    :return: App for testing
    """

    #app.config['TESTING'] = True
    client = app.test_client()

    yield client