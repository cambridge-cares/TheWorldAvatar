# Entity Linking 
Training set creation scripts 
Training set contains a entity dictionary json and train/val/test.jsonl question files.  
Use generateEntityDictJPS to generate entity dictionaries. (See DATA/EntityLinking/training_files_genration/examples/ontokin.json)
Use generateFromTemplate to generate question files.   
Then use convertQuestionBlinkFormat to convert it to the right format. (See DATA/EntityLinking/training_files_generation/examples/train_ontokin_blink_format.jsonl)
