from typing import List
import numpy as np
import tritonclient.http as httpclient

from .constants import TRITON_ENDPOINT


class FeatureExtractionClient:
    def __init__(self):
        self.client = httpclient.InferenceServerClient(url=TRITON_ENDPOINT)

    def forward(self, texts: List[str]):
        texts = np.array(texts, dtype=object)

        # Set Inputs
        input_tensors = [httpclient.InferInput("INPUT", texts.shape, datatype="BYTES")]
        input_tensors[0].set_data_from_numpy(texts)

        # Get Predictions
        pred_response = self.client.infer(
            model_name="feature_extraction", inputs=input_tensors
        )

        # Output
        return pred_response.as_numpy("OUTPUT")
