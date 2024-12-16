if [ -n "$HF_ACCESS_TOKEN" ]
then
       huggingface-cli login --token $HF_ACCESS_TOKEN --add-to-git-credential
fi

nohup python -u -m vllm.entrypoints.openai.api_server \
       --host 0.0.0.0 \
       --model mistralai/Mistral-7B-Instruct-v0.2 &

sleep 60

python run.py \
    --endpoint http://0.0.0.0:8000/v1 \
    --model mistralai/Mistral-7B-Instruct-v0.2 \
    --prompt_template data/prompt_template.json \
    --input_data data/input_data.csv \
    --output_file data/results.csv
