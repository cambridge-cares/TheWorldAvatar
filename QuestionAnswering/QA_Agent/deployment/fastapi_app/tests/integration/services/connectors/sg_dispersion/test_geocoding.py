from services.connectors.sg_dispersion.geocoding import NominatimGeocoder, Place


class TestNominatimGeocoder:
    def test_getCoords(self):
        # Arrange
        geocoder = NominatimGeocoder()
        location = "NUS UTown"
        expected = Place(
            lat="1.30589515",
            lot="103.77319830315889",
            display_name="University Town, Dover Road, Queenstown, Southwest, Singapore, 139657, Singapore",
        )

        # Act
        actual = geocoder.search(location)

        # Assert
        assert actual == expected
