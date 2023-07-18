valid_file_name=$1
train_file_name=$2
output_path=$3
command="python translator/train.py --valid_file_name ${valid_file_name} --train_file_name ${train_file_name} --model_name ${output_path}/SMILES_NER.bin"
${command}