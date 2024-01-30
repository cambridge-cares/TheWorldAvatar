import pytest
from app import create_app


@pytest.fixture
def app():
    app = create_app(testing=True)
    yield app

@pytest.fixture
def client(app):
    return app.test_client()


#test update request
def test_update(client):
    assert client.get('/update').status_code == 200


def test_get_data(client):
    re = client.get('data')
    print(re.json)
    assert re.status_code == 200
    assert 'dwellingunit' in re.json
    assert 'installed_pv' in re.json
    assert 'temperature_mean' in re.json