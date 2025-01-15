import numpy as np
import tritonclient.http as httpclient


def main():
    client = httpclient.InferenceServerClient(url="localhost:8000")

    input_ids = [101, 2054, 1005]

    # Inputs
    text = np.array([input_ids])

    # Set Inputs
    input_tensors = [
        httpclient.InferInput(
            "input_ids", text.shape, datatype="INT64"
        ).set_data_from_numpy(text),
        httpclient.InferInput(
            "attention_mask", text.shape, datatype="INT64"
        ).set_data_from_numpy(np.ones_like(text)),
    ]

    # Query
    query_response = client.infer(model_name="mpnet", inputs=input_tensors)

    # Output
    output = query_response.as_numpy("sentence_embedding")[0]
    print(output)


if __name__ == "__main__":
    main()
