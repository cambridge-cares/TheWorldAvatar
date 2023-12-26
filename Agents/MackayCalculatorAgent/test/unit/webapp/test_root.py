# test home
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


def test_landing(client):
    landing = client.get("/")
    assert landing.status_code == 200


def test_post_lever(client):
    response = client.post("/data", json={'levers':[1 for _ in range(45)]})
    assert response.status_code !=500
    assert response.json["status"] == "Success"
    assert type(response.json['values']) == list

def test_post_lever_err_range(client):
    response = client.post("/data", json={'levers':[5 for _ in range(45)]})
    assert response.status_code == 500

