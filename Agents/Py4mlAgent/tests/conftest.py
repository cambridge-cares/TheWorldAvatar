import pytest
import math
import csv
import os


THIS_DIR = os.path.dirname(os.path.abspath(__file__))

@pytest.fixture(autouse=True, scope="session")
def fake_csv_data():
    def _add_data_point(x, fake_data):
        x2 = x*x
        x3 = x*x*x
        y = 3.0*x3 + 2*x2 + 8*x

        fake_data.append(
            [
                x, x2, x3, y
            ]
        )

    fake_data = []
    fake_data.append(['feature 1','feature 2','feature 3','y(x)'])
    for x in range(300):
        _add_data_point(-0.1* x, fake_data)
        _add_data_point(0.1* x, fake_data)



    with open(os.path.join(THIS_DIR,'fake_data.csv'), 'w') as outcsv:
        writer = csv.writer(outcsv, delimiter=',', lineterminator='\n')
        for item in fake_data:
            writer.writerow(item)