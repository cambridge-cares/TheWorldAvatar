import pytest
import math
import csv
import os
from typing import List

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

@pytest.fixture(autouse=True, scope="session")
def fake_csv_data_one_y():
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

    save_fake_data(
        data = fake_data,
        save_path= os.path.join(THIS_DIR,'fake_data.csv'))

@pytest.fixture(autouse=True, scope="session")
def fake_csv_data_mult_y():
    def _add_data_point(x, fake_data):
        x2 = x*x
        x3 = x*x*x
        y1 = 3.0*x3 + 2*x2 + 8*x
        y2 = 6.0*x2 + x3 - 5*x

        fake_data.append(
            [
                x, x2, x3, y1, y2
            ]
        )

    fake_data = []
    fake_data.append(['feature 1','feature 2','feature 3','y1(x)','y2(x)'])
    for x in range(300):
        _add_data_point(-0.1* x, fake_data)
        _add_data_point(0.1* x, fake_data)

    save_fake_data(
        data = fake_data,
        save_path= os.path.join(THIS_DIR,'fake_data_mult_y.csv'))


def save_fake_data(data: List, save_path: str):
    with open(save_path, 'w') as outcsv:
        writer = csv.writer(outcsv, delimiter=',', lineterminator='\n')
        for item in data:
            writer.writerow(item)