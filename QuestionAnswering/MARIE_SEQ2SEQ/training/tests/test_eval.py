from core.eval import convert_kg_results_to_hashable


class TestEval:
    def test_convertKgResultsToHashable(self):
        kg_results = [
            {
                "label": {"type": "literal", "value": "C15H17FN4O3"},
                "IUPACNameValue": {
                    "type": "literal",
                    "value": "1-ethyl-6-fluoro-4-oxo-7-piperazin-1-yl-1,8-naphthyridine-3-carboxylic acid",
                },
                "LogPValue": {
                    "datatype": "http://www.w3.org/2001/XMLSchema#float",
                    "type": "literal",
                    "value": "2.3",
                },
                "UnitValue": {"type": "literal", "value": ""},
            },
            {
                "label": {"type": "literal", "value": "C15H17FN4O3"},
                "IUPACNameValue": {
                    "type": "literal",
                    "value": "1-ethyl-6-fluoro-4-oxo-7-(1-piperazinyl)-1,8-naphthyridine-3-carboxylic acid",
                },
                "LogPValue": {
                    "datatype": "http://www.w3.org/2001/XMLSchema#float",
                    "type": "literal",
                    "value": "2.3",
                },
                "UnitValue": {"type": "literal", "value": ""},
            },
            {
                "label": {"type": "literal", "value": "C15H17FN4O3"},
                "IUPACNameValue": {
                    "type": "literal",
                    "value": "1-ethyl-6-fluoranyl-4-oxidanylidene-7-piperazin-1-yl-1,8-naphthyridine-3-carboxylic acid",
                },
                "LogPValue": {
                    "datatype": "http://www.w3.org/2001/XMLSchema#float",
                    "type": "literal",
                    "value": "2.3",
                },
                "UnitValue": {"type": "literal", "value": ""},
            },
        ]
        expected = (
            (
                (
                    "IUPACNameValue",
                    "1-ethyl-6-fluoranyl-4-oxidanylidene-7-piperazin-1-yl-1,8-naphthyridine-3-carboxylic acid",
                ),
                ("LogPValue", "2.3"),
                ("UnitValue", ""),
                ("label", "C15H17FN4O3"),
            ),
            (
                (
                    "IUPACNameValue",
                    "1-ethyl-6-fluoro-4-oxo-7-(1-piperazinyl)-1,8-naphthyridine-3-carboxylic acid",
                ),
                ("LogPValue", "2.3"),
                ("UnitValue", ""),
                ("label", "C15H17FN4O3"),
            ),
            (
                (
                    "IUPACNameValue",
                    "1-ethyl-6-fluoro-4-oxo-7-piperazin-1-yl-1,8-naphthyridine-3-carboxylic acid",
                ),
                ("LogPValue", "2.3"),
                ("UnitValue", ""),
                ("label", "C15H17FN4O3"),
            ),
        )
        assert convert_kg_results_to_hashable(kg_results) == expected
