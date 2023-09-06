import numpy as np
import tritonclient.http as httpclient

def main():
    client = httpclient.InferenceServerClient(url="localhost:8000")

    # Inputs
    text = np.array(["What's the covalent unit count of chemical entities belonging to aminoacridines?"], dtype=object)

    # Set Inputs
    input_tensors = [
        httpclient.InferInput("text", text.shape, datatype="BYTES")
    ]
    input_tensors[0].set_data_from_numpy(text)

    # Query
    query_response = client.infer(model_name="translation",
                                  inputs=input_tensors)

    # Output
    output_compact = query_response.as_numpy("output_compact").astype(str)[0]
    output_verbose = query_response.as_numpy("output_verbose").astype(str)[0]
    print(output_compact, output_verbose)


if __name__ == "__main__":
    main()