nohup python -u -m vllm.entrypoints.openai.api_server \
    --host 0.0.0.0 \
    --model mistralai/Mixtral-8X7B-Instruct-v0.1 \
    --tensor-parallel-size 2 \
    --load-format pt &

sleep 60

python make_examples \
    data/examples.json \
    --object_properties data/object_properties.json \
    --categories data/categories.json \
    --openai_base_url http://0.0.0.0:8000/v1 \
    --openai_model mistralai/Mixtral-8X7B-Instruct-v0.1