data_path=$1
output_path=$2

command1="python trainbi.py  --data_path ${data_path} --output_path ${output_path} --learning_rate 1e-05 --num_train_epochs 1 --max_context_length 256 --max_cand_length 256 --train_batch_size 16 --eval_batch_size 8 --bert_model bert-base-uncased --type_optimization all_encoder_layers --data_parallel"
${command1}
command2="python evalbi.py --path_to_model ${output_path}/pytorch_model.bin --data_path ${data_path} --output_path models/pretrain --encode_batch_size 8 --eval_batch_size 1 --top_k 8 --save_topk_result --bert_model bert-base-uncased --mode test --zeshel False --data_parallel  --cand_encode_path ${output_path}/embeddingbase.pt"
${command2}
