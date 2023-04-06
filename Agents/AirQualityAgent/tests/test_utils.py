################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 01 Apr 2022                            #
################################################

import os
import pytest

# Import module under test from airquality package
import airquality.utils.properties as utils


def test_read_properties_file(tmp_path):
    # Create empty test properties file
    p = os.path.join(tmp_path, "test_airquality.properties")

    # Test for TimeSeriesClient (PostgreSQL DB) 'db.url'
    # 1) Exception for missing key
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'Key "db.url" is missing in properties file: '
    assert expected in str(excinfo.value)
    # 2) Exception for available key, but missing value
    with open(p, 'a') as f:
        f.write("db.url = ")
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'No "db.url" value has been provided in properties file: '
    assert expected in str(excinfo.value)

    # Test for TimeSeriesClient (PostgreSQL DB) 'db.user'
    with open(p, 'w') as f:
        f.write("db.url = test_url\n")
    with pytest.raises(KeyError) as excinfo:
        utils.read_properties_file(p)
    expected = 'Key "db.user" is missing in properties file: '
    assert expected in str(excinfo.value)
    with open(p, 'a') as f:
        f.write("db.user = ")
    with pytest.raises(KeyError) as excinfo:
        utils.read_properties_file(p)
    expected = 'No "db.user" value has been provided in properties file: '
    assert expected in str(excinfo.value)

    # Test for TimeSeriesClient (PostgreSQL DB) 'db.password'
    with open(p, 'w') as f:
        f.write("db.url = test_url\n\
                db.user = test_user\n")
    with pytest.raises(KeyError) as excinfo:
        utils.read_properties_file(p)
    expected = 'Key "db.password" is missing in properties file: '
    assert expected in str(excinfo.value)
    with open(p, 'a') as f:
        f.write("db.password = ")
    with pytest.raises(KeyError) as excinfo:
        utils.read_properties_file(p)
    expected = 'No "db.password" value has been provided in properties file: '
    assert expected in str(excinfo.value)

    # Test for 'sparql.query.endpoint'
    with open(p, 'w') as f:
        f.write("db.url = test_url\n\
                db.user = test_user\n\
                db.password = test_password\n")
    with pytest.raises(KeyError) as excinfo:
        utils.read_properties_file(p)
    expected = 'Key "sparql.query.endpoint" is missing in properties file: '
    assert expected in str(excinfo.value)
    with open(p, 'a') as f:
        f.write("sparql.query.endpoint = ")
    with pytest.raises(KeyError) as excinfo:
        utils.read_properties_file(p)
    expected = 'No "sparql.query.endpoint" value has been provided in properties file: '
    assert expected in str(excinfo.value)

    # Test for 'sparql.update.endpoint'
    with open(p, 'w') as f:
        f.write("db.url = test_url\n\
                db.user = test_user\n\
                db.password = test_password\n\
                sparql.query.endpoint = test_query_endpoint\n")
    with pytest.raises(KeyError) as excinfo:
        utils.read_properties_file(p)
    expected = 'Key "sparql.update.endpoint" is missing in properties file: '
    assert expected in str(excinfo.value)
    with open(p, 'a') as f:
        f.write("sparql.update.endpoint = ")
    with pytest.raises(KeyError) as excinfo:
        utils.read_properties_file(p)
    expected = 'No "sparql.update.endpoint" value has been provided in properties file: '
    assert expected in str(excinfo.value)

    # Test correct reading of timeseries.properties file
    with open(p, 'w') as f:
        f.write("db.url = test_url\n\
                db.user = test_user\n\
                db.password = test_password\n\
                sparql.query.endpoint = test_query_endpoint\n\
                sparql.update.endpoint = test_update_endpoint")
    utils.read_properties_file(p)
    assert utils.DB_URL == 'test_url'
    assert utils.DB_USER == 'test_user'
    assert utils.DB_PASSWORD == 'test_password'
    assert utils.QUERY_ENDPOINT == 'test_query_endpoint'
    assert utils.UPDATE_ENDPOINT == 'test_update_endpoint'
