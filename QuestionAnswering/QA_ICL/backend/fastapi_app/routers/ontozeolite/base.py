from fastapi import Response


class CIFResponse(Response):
    media_type = "chemical/x-cif"
