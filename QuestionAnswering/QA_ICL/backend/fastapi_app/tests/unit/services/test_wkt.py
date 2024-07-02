import pytest
from services.wkt import WKTTextSRS


class TestWktDataItem:
    @pytest.mark.parametrize(
        "wkt_literal,expected",
        [
            (
                "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> POLYGON Z ((103.72325444924806 1.285012624123125 0,103.72322777707456 1.285098327039333 0,103.7233563637899 1.285138865713388 0,103.72338303596409 1.28505316279727 0,103.72325444924806 1.285012624123125 0))",
                WKTTextSRS(
                    wkt_text="POLYGON Z ((103.72325444924806 1.285012624123125 0,103.72322777707456 1.285098327039333 0,103.7233563637899 1.285138865713388 0,103.72338303596409 1.28505316279727 0,103.72325444924806 1.285012624123125 0))",
                    srs_uri="http://www.opengis.net/def/crs/OGC/1.3/CRS84",
                ),
            ),
            (
                "POLYGON Z ((103.72627841509309 1.27642417395936 0,103.72613741509231 1.276550273961379 0,103.72624241509563 1.276667573960144 0,103.72638341509649 1.276541373958117 0,103.72627841509309 1.27642417395936 0))",
                WKTTextSRS(
                    wkt_text="POLYGON Z ((103.72627841509309 1.27642417395936 0,103.72613741509231 1.276550273961379 0,103.72624241509563 1.276667573960144 0,103.72638341509649 1.276541373958117 0,103.72627841509309 1.27642417395936 0))",
                    srs_uri="http://www.opengis.net/def/crs/OGC/1.3/CRS84",
                ),
            ),
        ],
    )
    def test_fromLiteral(self, wkt_literal: str, expected: WKTTextSRS):
        # Act
        actual = WKTTextSRS.from_literal(wkt_literal)

        # Assert
        assert actual == expected
