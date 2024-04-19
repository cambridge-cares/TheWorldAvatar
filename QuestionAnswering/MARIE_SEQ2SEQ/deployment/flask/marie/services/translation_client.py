import time
import numpy as np
import tritonclient.http as httpclient


class TranslationClient:
    def __init__(self, triton_endpoint: str = "localhost:8000"):
        print("Connecting to triton server at " + triton_endpoint)
        self.client = httpclient.InferenceServerClient(url=triton_endpoint)

        try_num = 0
        try_limit = 3
        is_ready = False
        while not is_ready and try_num < try_limit:
            try:
                if self.client.is_model_ready("translation"):
                    print("Translation server is ready.")
                    is_ready = True
                    break
            except ConnectionRefusedError:
                pass
            try_num += 1
            print(f"Attempt number {try_num} to connect to the translation server fails.")
            if try_num < try_limit:
                print("Sleep for 5 seconds")
                time.sleep(5)
        if not is_ready:
            raise Exception(
                "There are issues connecting to translation server. Please ensure that the translation server is running and restart the app."
            )

    def translate(self, text: str):
        text = np.array(
            [text],
            dtype=object,
        )

        # Set Inputs
        input_tensors = [httpclient.InferInput("text", text.shape, datatype="BYTES")]
        input_tensors[0].set_data_from_numpy(text)

        # Query
        query_response = self.client.infer(
            model_name="translation", inputs=input_tensors
        )

        # Output
        output_compact = query_response.as_numpy("output_compact").astype(str)[0]
        output_verbose = query_response.as_numpy("output_verbose").astype(str)[0]

        return dict(sparql_query=output_verbose, sparql_query_compact=output_compact)
