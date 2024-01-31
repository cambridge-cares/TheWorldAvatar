import numpy as np
import tritonclient.http as httpclient

from .constants import TRITON_ENDPOINT


class Seq2SeqClient:
    def __init__(self):
        self.client = httpclient.InferenceServerClient(url=TRITON_ENDPOINT)

    def forward(self, text: str):
        text = np.array(
            [text],
            dtype=object,
        )

        # Set Inputs
        input_tensors = [httpclient.InferInput("INPUT", text.shape, datatype="BYTES")]
        input_tensors[0].set_data_from_numpy(text)

        try:
            # Get Predictions
            pred_response = self.client.infer(
                model_name="seq2seq", inputs=input_tensors
            )
        except ConnectionRefusedError:
            raise ConnectionRefusedError("Unable to connect to triton server at the endpoint: " + str(self.client._parsed_url))

        # Output
        return pred_response.as_numpy("OUTPUT").astype(str)[0]
