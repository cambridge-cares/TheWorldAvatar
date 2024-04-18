import numpy as np
import pytest

from services.utils.numerical import cosine_similarity


class TestNumerical:
    @pytest.mark.parametrize("a,b,expected", [
        ([0, 1, 2], [0, 1, 2], 1),
        ([[3, 4, 5], [6, 7, 8], [9, 10, 11]], [[-3, -4, -5], [6, 6, 6]], [[-1, 0.979796], [-0.996369, 0.993266], [-0.992822 , 0.996683]])
    ])
    def test_cosineSimilarity(self, a, b, expected):
        assert np.allclose(cosine_similarity(a, b), expected)