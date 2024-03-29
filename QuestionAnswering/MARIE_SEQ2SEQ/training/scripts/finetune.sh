torchrun \
    --nproc_per_node 4 finetune.py \
    --model_path google/flan-t5-base \
    --train_data_path ./data/train.json \
    --eval_data_path ./data/dev.json \
    --output_dir /rds/user/nmdt2/hpc-work/outputs \
    --do_train \
    --do_eval \
    --per_device_train_batch_size 8 \
    --per_device_eval_batch_size 8 \
    --num_train_epochs 3 \
    --optim paged_adamw_32bit \
    --learning_rate 0.0002 \
    --logging_steps 1 \
    --evaluation_strategy steps \
    --eval_steps 10 \
    --load_best_model_at_end \
    --save_total_limit 2 \
    --report_to wandb \
    --run_name flan-t5-base