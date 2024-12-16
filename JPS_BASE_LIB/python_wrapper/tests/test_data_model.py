import pytest

from twa.data_model import utils

@pytest.mark.parametrize(
    "iri,expected_after_trim",
    [
        (' <> ', ''),
        (' <<<http://a>>> ', 'http://a'),
        ('http://a', 'http://a'),
        ('<http://a>', 'http://a'),
        (['<http://a>', ' <<<http://a>>> '], ['http://a', 'http://a']),
    ],
)
def test_trim_iri(iri,expected_after_trim):
    assert utils.trim_iri(iri) == expected_after_trim
