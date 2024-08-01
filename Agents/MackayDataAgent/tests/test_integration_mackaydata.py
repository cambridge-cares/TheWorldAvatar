import pytest
from app import create_app
from testconf import clear_database, initialise_triples, clear_kg

@pytest.fixture
def app():
    initialise_triples()
    app = create_app(testing=True)
    yield app

@pytest.fixture
def client(app):
    return app.test_client()


#test update request
def test_update(client):
    assert client.get('/update').status_code == 200
    print('update finished')
    re = client.get('data')
    print(re.json)
    assert re.status_code == 200
    assert 'dwelling_unit' in re.json
    assert 'installed_pv' in re.json
    assert 'monthly_temperature' in re.json
    clear_database()
    clear_kg()

