nohup python -u -m vllm.entrypoints.openai.api_server \
       --host 0.0.0.0 \
       --model mistralai/Mistral-7B-Instruct-v0.2 &

sleep 60

python -m paraphrase \
    data/examples.json \
    --endpoint http://0.0.0.0:8000/v1 \
    --api_key placeholder \
    --model mistralai/Mistral-7B-Instruct-v0.2