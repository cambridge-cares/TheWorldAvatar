# Entity Linking 
Training set creation scripts.
###Overview

Training set contains an entity dictionary json and/or train/val/test.jsonl question files. Refer to Entity Linking training document for more details: Training/EntityLinking/EL_training.md.

###Setup
Install Python 3.8. No other library is required. I/O paths should be supplied in the parameters.
Following files are required for this data creation:
1. DATA/Dictionaries, which contain entity name lists source files. Refer to MARIE_AND_BERT/readme.md on how to download.
2. SMILES entity source file pubchemwithSMILE.jsonl, which is contained in DATA/EntityLinking/training_files_generation; downloading instructions same as above. Copy into DATA/KGToolbox/EntityLinking.
3. Question templates in DATA/EntityLinking/training_files_generation/templates. Copy into DATA/KGToolbox/EntityLinking/templates.

###Entity dict
To generate entity dictionaries:
```
python generateEntityDictJPS.py
```
(See DATA/EntityLinking/training_files_generation/examples/ontokin.json for an example output.)

### Question files
####For NER:   

To generate SMILES question files, supply the outfile name:
```
python generateFromTemplate.py --outfile train.jsonl --question_type smiles --seed 0
```
It is recommended to change random seed when generate train/test/valid files.
####For ER/EL-pretrain:    
For general question files, need to specify the path of respective domain entity dictionaries (generated from Entity dict step).
An example is:
```
python generateFromTemplate.py --infile ontokin.jsonl --outfile test_raw.jsonl --question_type general --seed 0
```

Run convertQuestionBlinkFormat.py to convert it to the right format. Parameter 'convert_type' set to 'blink'.
```
python convertQuestionBlinkFormat.py --infile test_raw.jsonl --outfile test.jsonl --convert_type blink --seed 0
```
(See DATA/EntityLinking/training_files_generation/examples/ontokin_blink_format_example.jsonl for an example output.)

#####For ER/EL-finetune:   
Use generateFromTemplate.generate_questions to generate question files.
```
python generateFromTemplate.py --infile ontokin.jsonl --outfile test_raw.jsonl --question_type general --seed 0
```
Run convertQuestionBlinkFormat to convert it to the right format. Parameter 'convert_type' set to 'elq'.
```
python convertQuestionBlinkFormat.py --infile test_raw.jsonl --outfile test.jsonl --convert_type elq --seed 0
```
(See DATA/EntityLinking/training_files_generation/examples/ontokin_elq_format_example.jsonl)