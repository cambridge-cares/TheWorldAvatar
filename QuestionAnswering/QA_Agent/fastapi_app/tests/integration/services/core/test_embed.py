import numpy as np

from services.utils.numerical import cosine_similarity

class TestEmbedder:
    def test_tritonMpnetEmbedder_semantic(self, embedder):
        # Arrange
        documents = ["pH regulator", "pH regulation", "pesticide"]
        
        # Act
        embeddings = embedder(documents)

        # Assert
        assert isinstance(embeddings, np.ndarray)
        assert embeddings.shape[0] == 3
        assert cosine_similarity(embeddings[0], embeddings[1]) > cosine_similarity(embeddings[0], embeddings[2])