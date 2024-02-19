from abc import abstractmethod

import numpy as np
import tritonclient.http as httpclient

from .constants import TRITON_ENDPOINT


class ISeq2SeqClient:
    @abstractmethod
    def forward(self, text: str, model: str) -> str:
        pass


class Seq2SeqClient(ISeq2SeqClient):
    def __init__(self):
        self.client = httpclient.InferenceServerClient(url=TRITON_ENDPOINT)

    def forward(self, text: str, model: str):
        if model not in ["seq2seq_chemistry", "seq2seq_singapore", "seq2seq_kingslynn"]:
            raise ValueError(
                "Expects model to be either `seq2seq_chemistry`, `seq2seq_kingslynn`, or `seq2seq_singapore`; found "
                + model
            )

        text = np.array(
            [text],
            dtype=object,
        )

        # Set Inputs
        input_tensors = [httpclient.InferInput("INPUT", text.shape, datatype="BYTES")]
        input_tensors[0].set_data_from_numpy(text)

        try:
            # Get Predictions
            pred_response = self.client.infer(model_name=model, inputs=input_tensors)
        except ConnectionRefusedError:
            raise ConnectionRefusedError(
                "Unable to connect to triton server at the endpoint: "
                + str(self.client._parsed_url)
            )

        # Output
        return pred_response.as_numpy("OUTPUT").astype(str)[0]
