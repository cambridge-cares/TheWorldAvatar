import json
import logging
import os
import time
from importlib.resources import files

from flask import Flask, Response, render_template, request, stream_with_context
from openai import OpenAI

from marie.services import KgExecutor, MultiDomainTranslator, sanitize_quantities


SAMPLE_QUESTIONS = json.loads(
    files("marie.resources").joinpath("sample_questions.json").read_text()
)
app = Flask(__name__)

translator = MultiDomainTranslator()
kg_executor = KgExecutor()

CHATBOT_ENDPOINT = os.getenv("CHATBOT_ENDPOINT", "http://localhost:8001/v1")
print("Connecting to chatbot at endpoint:", CHATBOT_ENDPOINT)
chatbot_client = OpenAI(base_url=CHATBOT_ENDPOINT, api_key="placeholder")


if __name__ != "__main__":
    app.logger.setLevel(logging.DEBUG)
    # gunicorn_logger = logging.getLogger("gunicorn.error")
    # app.logger.handlers = gunicorn_logger.handlers
    # app.logger.setLevel(gunicorn_logger.level)


@app.route("/", methods=["GET"])
def home():
    return render_template("index.html", sample_questions=SAMPLE_QUESTIONS)


@app.route("/", methods=["POST"])
def ask():
    app.logger.info("Request received to translation endpoint")

    data = request.get_json()
    question = data.get("question")
    app.logger.info("Question received: " + str(question))

    sanitized_inputs = sanitize_quantities(question)
    app.logger.info("Santized inputs: " + str(sanitized_inputs))

    app.logger.info("Sending translation request to triton server...")
    start_trans = time.time()
    translation_result = translator.nl2sparql(
        sanitized_inputs["preprocessed_text_for_trans"]
    )
    end_trans = time.time()

    app.logger.info("Translation result: " + str(translation_result))

    domain = translation_result["domain"]
    sparql_query_verbose = translation_result["sparql"]["verbose"]

    app.logger.info("Sending sparql query to KG server...")
    start_kg = time.time()
    if sparql_query_verbose:
        try:
            sparql_query_verbose = sparql_query_verbose.strip()
            data = kg_executor.query(domain=domain, query=sparql_query_verbose)
        except Exception as e:
            app.logger.exception(e)
            data = None
    else:
        sparql_query_verbose = None
        data = None
    end_kg = time.time()

    return dict(
        question=question,
        preprocessed_question=sanitized_inputs["preprocessed_text_for_user"],
        domain=translation_result["domain"],
        sparql=dict(
            predicted=translation_result["sparql"]["decoded"],
            postprocessed=sparql_query_verbose,
        ),
        data=data,
        translation_latency=end_trans - start_trans,
        kg_latency=end_kg - start_kg,
    )


@app.route("/chatbot", methods=["POST"])
def stream_chatbot_response():
    app.logger.info("Request received to chatbot endpoint")

    request_data = request.get_json()
    question = request_data.get("question")
    data = request_data.get("data")
    app.logger.info("Question: " + question)
    app.logger.info("Data: " + data)

    prompt_template = "## Query:\n{query}\n\n### Data:\n{data}"
    response_stream = chatbot_client.chat.completions.create(
        model="llama-2-7b-chat.Q4_K_M.gguf",
        messages=[
            {
                "role": "system",
                "content": "You are a chatbot that concisely responds to user queries.",
            },
            {
                "role": "user",
                "content": prompt_template.format(query=question, data=data),
            },
        ],
        temperature=0,
        stream=True,
    )

    def generate():
        for chunk in response_stream:
            content = chunk.choices[0].delta.content
            if content is not None:
                yield 'data: {data}\n\n'.format(data=json.dumps({"content": content}))

    return generate(), {"Content-Type": "text/event-stream"}


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000)
