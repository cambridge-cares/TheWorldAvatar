from pydantic import BaseModel


CRS84_URI = "http://www.opengis.net/def/crs/OGC/1.3/CRS84"


class WKTTextSRS(BaseModel):
    wkt_text: str
    srs_uri: str = CRS84_URI

    @classmethod
    def from_literal(cls, wkt_literal: str):
        start_srs = wkt_literal.find("<")
        end_srs = wkt_literal.find(">", start_srs + 1)

        if start_srs >= 0 and end_srs >= 0:
            return cls(
                srs_uri=wkt_literal[start_srs + 1 : end_srs],
                wkt_text=wkt_literal[end_srs + 1 :].strip(),
            )
        else:
            return cls(wkt_text=wkt_literal.strip())
