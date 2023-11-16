import numpy as np
import tritonclient.http as httpclient


class Seq2SeqClient:
    def __init__(self, triton_endpoint: str = "localhost:8000"):
        self.client = httpclient.InferenceServerClient(url=triton_endpoint)

    def forward(self, text: str):
        text = np.array(
            [text],
            dtype=object,
        )

        # Set Inputs
        input_tensors = [httpclient.InferInput("INPUT", text.shape, datatype="BYTES")]
        input_tensors[0].set_data_from_numpy(text)

        # Get Predictions
        pred_response = self.client.infer(
            model_name="seq2seq", inputs=input_tensors
        )

        # Output
        return pred_response.as_numpy("OUTPUT").astype(str)[0]
