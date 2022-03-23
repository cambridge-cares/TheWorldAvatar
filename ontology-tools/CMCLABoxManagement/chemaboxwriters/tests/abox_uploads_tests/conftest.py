import pytest
import os
import time
import requests

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
SECRETS_PATH = os.path.join(THIS_DIR, "docker_settings", "dummy_services_secrets")
SERVICE_ROUTES = {
    "blazegraph-geo": "blazegraph/namespace/kb/sparql",
    "fileserver": "FileServer/",
}
NAMESPACES_TO_INSERT = ["ospecies", "omops", "ocompchem", "opsscan"]

def pytest_sessionstart(session):
    """This will run before all the tests"""

    # split secrets file into user and passwd files needed for the docker compose
    for service in SERVICE_ROUTES:
        service_secrets_file = os.path.join(SECRETS_PATH, f"{service}_secrets.txt")
        service_user_file = os.path.join(SECRETS_PATH, f"{service}_user.txt")
        service_passwd_file = os.path.join(SECRETS_PATH, f"{service}_passwd.txt")

        with open(service_secrets_file, "r") as secrets_file:
            user, passwd = secrets_file.read().strip().split(":")

        with open(service_user_file, "w") as user_file:
            user_file.write(user)

        with open(service_passwd_file, "w") as passwd_file:
            passwd_file.write(passwd)


def pytest_sessionfinish(session):
    """This will run after all the tests"""
    for service in SERVICE_ROUTES:
        service_user_file = os.path.join(SECRETS_PATH, f"{service}_user.txt")
        service_passwd_file = os.path.join(SECRETS_PATH, f"{service}_passwd.txt")
        if os.path.exists(service_user_file):
            os.remove(service_user_file)
        if os.path.exists(service_passwd_file):
            os.remove(service_passwd_file)


# ----------------------------------------------------------------------------------
# Session-scoped test fixtures
# ----------------------------------------------------------------------------------


@pytest.fixture(scope="session", autouse=True)
def start_services(session_scoped_container_getter):
    time.sleep(8)
    insert_namespaces()

    # this will run only once per entire test session and ensures that all the services
    # in docker containers are ready. Increase the sleep value in case services need
    #  bit more time to run on your machine.
    time.sleep(8)

def insert_namespaces():
    insert_namespace = """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
    <!DOCTYPE properties SYSTEM "http://java.sun.com/dtd/properties.dtd">
    <properties>
    <entry key="com.bigdata.rdf.sail.truthMaintenance">false</entry>
    <entry key="com.bigdata.namespace.#ns_to_insert#.spo.com.bigdata.btree.BTree.branchingFactor">1024</entry>
    <entry key="com.bigdata.rdf.store.AbstractTripleStore.textIndex">false</entry>
    <entry key="com.bigdata.rdf.store.AbstractTripleStore.justify">false</entry>
    <entry key="com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers">false</entry>
    <entry key="com.bigdata.rdf.store.AbstractTripleStore.axiomsClass">com.bigdata.rdf.axioms.NoAxioms</entry>
    <entry key="com.bigdata.rdf.sail.namespace">#ns_to_insert#</entry>
    <entry key="com.bigdata.rdf.store.AbstractTripleStore.quads">false</entry>
    <entry key="com.bigdata.namespace.#ns_to_insert#.lex.com.bigdata.btree.BTree.branchingFactor">400</entry>
    <entry key="com.bigdata.rdf.store.AbstractTripleStore.geoSpatial">false</entry>
    <entry key="com.bigdata.rdf.sail.isolatableIndices">false</entry>
    </properties>
    """

    for namespace in NAMESPACES_TO_INSERT:
        response = requests.post(
            url = "http://localhost:48083/blazegraph/namespace",
            data= insert_namespace.replace("#ns_to_insert#", namespace),
            headers = {"Content-Type":"application/xml"}
        )