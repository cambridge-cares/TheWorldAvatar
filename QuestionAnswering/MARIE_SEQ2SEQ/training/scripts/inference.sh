python inference.py \
    --model_path models/20230924_0 \
    --model_format ort \
    --device_map cpu\
    --bits 8 \
    --do_correct \
    --eval_data_path data/test_2023-09-21_16.12.12.json \
    --out_file predictions/predictions_20230924_0_ort_quantized.json \
    --do_profile