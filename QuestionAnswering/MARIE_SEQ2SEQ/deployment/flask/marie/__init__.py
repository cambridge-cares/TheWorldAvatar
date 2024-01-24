import json
import logging
import os
import time
from importlib.resources import files
from flask import Flask, render_template, request

from marie.services import KgClient, TranslationClient, sanitize_quantities


SAMPLE_QUESTIONS = json.loads(
    files("marie.resources").joinpath("sample_questions.json").read_text()
)
app = Flask(__name__)

translation_client = TranslationClient(
    triton_endpoint=os.environ.get("TRITON_ENDPOINT", "localhost:8000")
)
kg_client = KgClient(os.environ.get("KG_ENDPOINT"))


if __name__ != '__main__':
    gunicorn_logger = logging.getLogger('gunicorn.error')
    app.logger.handlers = gunicorn_logger.handlers
    app.logger.setLevel(gunicorn_logger.level)


@app.route("/", methods=["GET"])
def home():
    return render_template("index.html", sample_questions=SAMPLE_QUESTIONS)


@app.route("/", methods=["POST"])
def ask():
    data = request.get_json()
    question = data.get("question")
    app.logger.info("Question received: " + str(question))

    sanitized_inputs = sanitize_quantities(question)
    app.logger.info("Santized inputs: " + str(sanitized_inputs))

    app.logger.info("Sending translation request to triton server...")
    start_trans = time.time()
    translation_result = translation_client.translate(
        sanitized_inputs["preprocessed_text_for_trans"]
    )
    end_trans = time.time()
    app.logger.info("Translation result: " + str(translation_result))
    
    sparql_query = translation_result.get("sparql_query").strip()
    app.logger.info("Sending sparql query to KG server...")
    start_kg = time.time()
    if sparql_query:
        data = kg_client.query(sparql_query)
    else:
        sparql_query = None
        data = None
    end_kg = time.time()

    return dict(
        question=question,
        preprocessed_question=sanitized_inputs["preprocessed_text_for_user"],
        sparql_query=sparql_query,
        data=data,
        translation_latency=end_trans - start_trans,
        kg_latency=end_kg - start_kg,
    )


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000)
