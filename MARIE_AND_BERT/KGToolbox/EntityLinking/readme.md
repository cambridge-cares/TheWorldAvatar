# Entity Linking 
Training set creation scripts.
###Overview

Training set contains an entity dictionary json and/or train/val/test.jsonl question files. Refer to Entity Linking training document for more details: Training/EntityLinking/EL_training.md.

###Setup
Install Python 3.8. No other library is required. 
Go through the following steps to set up required files for this data creation:
1. Download `DATA/Dictionaries`, which contain entity name lists source files. Refer to [main readme](MARIE_AND_BERT/readme.md) on how to download and set up.
2. Download `DATA/EntityLinking.zip`; downloading instructions same as above.
3. Copy `pubchemwithSMILE.jsonl`, which is contained in `DATA/EntityLinking/training_files_generation`, into `DATA/KGToolbox/EntityLinking`.
3. Copy all files from `DATA/EntityLinking/training_files_generation/templates` into `DATA/KGToolbox/EntityLinking/templates`.
4. `DATA/EntityLinking/training_files_generation/examples` contains output example files that can be referred to for checking file formats.

###Entity dict
To generate entity dictionaries:
```
python generateEntityDictJPS.py
```
(See `DATA/EntityLinking/training_files_generation/examples/ontokin.json` for an example output.)

### Question files
####1. To generate train/val/test.jsonl for <b>SMILES-NER training step</b>:   

To generate SMILES question files, supply the outfile name:
```
python generateFromTemplate.py --outfile train.jsonl --question_type smiles --seed 0
```
It is recommended to change random seed when generate train/test/valid files.

####2. To generate train/test/valid files for <b>Entity Extraction-pretrain step</b>:    
For general question files, need to specify the path of respective domain entity dictionaries (generated from Entity dict step).
An example is:
```
python generateFromTemplate.py --infile ontokin.jsonl --outfile test_raw.jsonl --question_type general --seed 0
```

Run convertQuestionBlinkFormat.py to convert it to the right format. Parameter `convert_type` set to `blink`.
```
python convertQuestionBlinkFormat.py --infile test_raw.jsonl --outfile test.jsonl --convert_type blink --seed 0
```
(See `DATA/EntityLinking/training_files_generation/examples/ontokin_blink_format_example.jsonl` for an example output.)

#####3. To generate train/test/valid files for <b>Entity Extraction-finetune step</b>:   
First generate question files.
```
python generateFromTemplate.py --infile ontokin.jsonl --outfile test_raw.jsonl --question_type general --seed 0
```
Run convertQuestionBlinkFormat to convert it to the right format. Parameter `convert_type` set to `elq`.
```
python convertQuestionBlinkFormat.py --infile test_raw.jsonl --outfile test.jsonl --convert_type elq --seed 0
```
(See `DATA/EntityLinking/training_files_generation/examples/ontokin_elq_format_example.jsonl` for example reference.)