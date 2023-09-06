import os
from flask import Flask, render_template, request

from services import KgClient, TranslationClient

app = Flask(__name__)

translation_client = TranslationClient(triton_endpoint=os.environ.get("TRITON_ENDPOINT", "localhost:8000"))
kg_client = KgClient()


@app.route("/", methods=["GET"])
def home():
    return render_template("index.html")


@app.route("/", methods=["POST"])
def ask():
	data = request.get_json()
	question = data.get("question")
	app.logger.info("Question received: " + str(question))

	translation_result = translation_client.translate(question)
	app.logger.info("Translation result: " + str(translation_result))

	sparql_query = translation_result.get("sparql_query")
	results = kg_client.query(sparql_query)

	return results


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000)
