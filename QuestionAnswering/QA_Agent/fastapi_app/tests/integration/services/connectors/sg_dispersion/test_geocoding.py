from services.connectors.sg_dispersion.geocoding import NominatimGeocoder


class TestNominatimGeocoder:
    def test_getCoords(self):
        # Arrange
        geocoder = NominatimGeocoder()
        location = "NUS UTown"

        # Act
        actual = geocoder.get_coords(location)

        # Assert
        expected = ("1.30589515", "103.77319830315889")
        assert actual == expected
