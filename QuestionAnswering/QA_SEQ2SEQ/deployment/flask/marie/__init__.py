import json
import logging
import os
import time
from importlib.resources import files
import traceback

from flask import Flask, jsonify, render_template, request
from openai import OpenAI

from marie.exceptions import BaseError
from marie.services.kg_execute import get_kg
from marie.services import MultiDomainTranslator, sanitize_quantities


SAMPLE_QUESTIONS = json.loads(
    files("marie.resources").joinpath("sample_questions.json").read_text()
)
app = Flask(__name__)


translator = MultiDomainTranslator()

CHATBOT_ENDPOINT = os.getenv("CHATBOT_ENDPOINT", "http://localhost:8001/v1")
print("Connecting to chatbot at endpoint: " + CHATBOT_ENDPOINT)
chatbot_client = OpenAI(base_url=CHATBOT_ENDPOINT, api_key="placeholder")


if __name__ != "__main__":
    app.logger.setLevel(logging.DEBUG)


@app.route("/", methods=["GET"])
def home():
    return render_template("index.html", sample_questions=SAMPLE_QUESTIONS)


@app.route("/translate", methods=["POST"])
def ask():
    app.logger.info("Received request to translation endpoint")

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

    return dict(
        question=question,
        preprocessed_question=sanitized_inputs["preprocessed_text_for_user"],
        domain=translation_result["domain"],
        sparql=dict(
            predicted=translation_result["sparql"]["decoded"],
            postprocessed=translation_result["sparql"]["verbose"],
        ),
        latency=end_trans - start_trans,
    )


@app.route("/kg", methods=["POST"])
def query_kg():
    app.logger.info("Received request to KG executation endpoint")

    data = request.get_json()
    query = data.get("sparql_query")
    domain = data.get("domain")
    app.logger.info("SPARQL query: " + query)
    app.logger.info("Domain: " + domain)

    start_kg = time.time()
    data = get_kg().query(domain=domain, query=query)

    end_kg = time.time()

    return dict(
        data=data,
        latency=end_kg - start_kg,
    )


@app.errorhandler(BaseError)
def handle_kg_connection_error(e: BaseError):
    app.logger.error(traceback.format_exc())
    return jsonify(e.to_dict()), e.code


def make_chatbot_response_stream(question: str, data: str):
    prompt_template = "## Query:\n{query}\n\n### Data:\n{data}"
    return chatbot_client.chat.completions.create(
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
        stream=True,
    )


@app.route("/chatbot", methods=["POST"])
def stream_chatbot_response():
    app.logger.info("Request received to chatbot endpoint")

    request_data = request.get_json()
    question = request_data.get("question")
    data = request_data.get("data")
    app.logger.info("Question: " + question)
    app.logger.info("Data: " + data)

    def generate():
        start = time.time()
        for chunk in make_chatbot_response_stream(question, data):
            content = chunk.choices[0].delta.content
            if content is not None:
                yield "data: {data}\n\n".format(
                    data=json.dumps(
                        {"content": content, "latency": time.time() - start}
                    )
                )

    return generate(), {"Content-Type": "text/event-stream"}


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000)
