# Chatbot

## Run OpenAI API-compatible web server

Browse for a suitable GGUF-formatted weights for Llama 2 Chat (e.g. [TheBloke/Llama-2-7B-Chat-GGUF](https://huggingface.co/TheBloke/Llama-2-7B-Chat-GGUF)) and download it to the local machine. 

```{shell}
pip install llama-cpp-python[server]
python3 -m llama_cpp.server --model <path_to_GGUF_weights>
```

Visit localhost:8000/docs for API documentation.

More details on llama-ccp-python can be found [here](https://llama-cpp-python.readthedocs.io/en/latest/).

## Client-side testing

```{python}
from openai import OpenAI

# base_url should include /v1 and api_key can be any string but must not be empty
chatbot_client = OpenAI(base_url="http://localhost:8000/v1", api_key="placeholder")

question = "what are the reactions in which O2 reacts to form OH"
data = "[{'SampledReactionEquation': 'H + O2 <=> O + OH'},\n{'SampledReactionEquation': 'H2 + O2 <=> OH + OH'}]"

prompt_template = "## Query:\n{query}\n\n### Data:\n{data}"
stream = chatbot_client.chat.completions.create(
    model="llama-2-7b-chat.Q4_K_M.gguf",
    messages=[
        {"role": "system", "content": "You are a chatbot that concisely responds to user queries based on provided data."},
        {
            "role": "user",
            "content": prompt_template.format(query=question, data=str(data)),
        },
    ],
    temperature=0,
    stream=True
)

for chunk in stream:
    if chunk.choices[0].delta.content is not None:
        print(chunk.choices[0].delta.content, end="")
```