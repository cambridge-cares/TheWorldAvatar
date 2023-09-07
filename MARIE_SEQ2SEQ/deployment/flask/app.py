import json
import os
import time
from flask import Flask, render_template, request

from services import KgClient, TranslationClient


with open("./resources/sample_questions.json", "r") as f:
    SAMPLE_QUESTIONS = json.load(f)

app = Flask(__name__)

translation_client = TranslationClient(
    triton_endpoint=os.environ.get("TRITON_ENDPOINT", "localhost:8000")
)
kg_client = KgClient()


@app.route("/", methods=["GET"])
def home():
    return render_template("index.html", sample_questions=SAMPLE_QUESTIONS)


@app.route("/", methods=["POST"])
def ask():
    data = request.get_json()
    question = data.get("question")
    app.logger.info("Question received: " + str(question))

    start_trans = time.time()
    translation_result = translation_client.translate(question)
    end_trans = time.time()
    app.logger.info("Translation result: " + str(translation_result))

    sparql_query = translation_result.get("sparql_query").strip()
    start_kg = time.time()
    if sparql_query:
        data = kg_client.query(sparql_query)
    else:
        sparql_query = None
        data = None
    end_kg = time.time()

    return dict(
        question=question,
        sparql_query=sparql_query,
        data=data,
        translation_latency=end_trans - start_trans,
        kg_latency=end_kg - start_kg,
    )


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000)
