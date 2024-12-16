import numpy as np
import tritonclient.http as httpclient


def main():
    client = httpclient.InferenceServerClient(url="localhost:8000")

    questions = [
        "What's the covalent unit count of chemical entities belonging to aminoacridines?",
        "What are the compound complexity and bond chiral def count of molecules categorized under sulfite ester?",
    ]
    for question in questions:
        for prompt in ["classify query domain: ", "translate to SPARQL: "]:
            # Inputs
            text = np.array(
                [prompt + question],
                dtype=object,
            )

            # Set Inputs
            input_tensors = [
                httpclient.InferInput("INPUT", text.shape, datatype="BYTES")
            ]
            input_tensors[0].set_data_from_numpy(text)

            # Query
            query_response = client.infer(model_name="seq2seq", inputs=input_tensors)

            # Output
            output = query_response.as_numpy("OUTPUT").astype(str)[0]
            print(output)


if __name__ == "__main__":
    main()
