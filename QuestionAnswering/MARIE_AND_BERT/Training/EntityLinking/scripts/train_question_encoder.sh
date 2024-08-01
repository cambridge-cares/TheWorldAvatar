firststep_output = $1
data_path=$2
output_path=$3


command="python elq/biencoder/train_biencoder.py --output_path ${output_path} --path_to_model ${firststep_output}/pytorch_model.bin --data_path ${data_path} --num_train_epochs 2 --learning_rate 0.00001 --max_context_length 128 --max_cand_length 256 --train_batch_size 16 --eval_batch_size 8 --bert_model bert-base-uncased --mention_scoring_method qa_linear --eval_interval 500 --last_epoch -1 --no_mention_bounds --mention_aggregation_type all_avg --data_parallel --get_losses --dont_distribute_train_samples"

${command}