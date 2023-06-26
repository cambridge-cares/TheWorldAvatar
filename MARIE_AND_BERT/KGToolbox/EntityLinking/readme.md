# Entity Linking 
Training set creation scripts.
###Overview

Training set contains an entity dictionary json and/or train/val/test.jsonl question files. Refer to Entity Linking training document for more details: Training/EntityLinking/EL_training.md.

###Entity dict
Use generateEntityDictJPS to generate entity dictionaries. (See DATA/EntityLinking/training_files_generation/examples/ontokin.json)

### Question files
####For NER:   
Use generateFromTemplate.generate_questions_SMILES to generate question files.(See DATA/EntityLinking/training_files_generation/examples/smiles_question_file_example.jsonl)

####For ER/EL-pretrain:    
Use generateFromTemplate.generate_questions to generate question files.

Run convertQuestionBlinkFormat to convert it to the right format. Parameter 'convert_type' set to 'blink'. (See DATA/EntityLinking/training_files_generation/examples/ontokin_blink_format_example.jsonl)

#####For ER/EL-finetune:   
Use generateFromTemplate.generate_questions to generate question files.

Run convertQuestionBlinkFormat to convert it to the right format. Parameter 'convert_type' set to 'elq'. (See DATA/EntityLinking/training_files_generation/examples/ontokin_elq_format_example.jsonl)