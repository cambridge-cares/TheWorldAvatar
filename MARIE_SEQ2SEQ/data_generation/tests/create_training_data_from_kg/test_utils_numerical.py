import pytest
from data_generation.create_training_data_from_kg.utils.numerical import get_value_higher, get_value_lower


class TestUtilsNumerical:
    @pytest.mark.parametrize(
        "value",
        [
            "0",
            "1",
            "4",
            "123",
            "0.5",
            "80.32",
            "200.677",
            "1000.5",
            "-1",
            "-2",
            "-100",
            "-202.41",
            "-50.5",
            "-9.99",
        ],
    )
    def test_getValueLower(self, value):
        assert float(get_value_lower(value)) < float(value)

    @pytest.mark.parametrize(
        "value",
        [
            "0",
            "1",
            "4",
            "123",
            "0.5",
            "80.32",
            "200.677",
            "1000.5",
            "-1",
            "-2",
            "-100",
            "-202.41",
            "-50.5",
            "-9.99",
        ],
    )
    def test_getValueHigher(self, value):
        assert float(get_value_higher(value)) > float(value)