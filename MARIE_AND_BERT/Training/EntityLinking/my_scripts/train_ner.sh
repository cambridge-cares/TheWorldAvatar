valid_file_name=$1
train_file_name=$2
command="python translator/train.py --valid_file_name ${valid_file_name} --train_file_name ${train_file_name}"
${command}