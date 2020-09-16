import json
properties = ['chemistry_properties', 'physical_properties','process_properties']
classes = ['chemical_substance_class']

for cls in classes:
    with open('%s_altLabel.json' % cls) as f:
        content = json.loads(f.read())
        for entity in content:
            label = entity['label']
            alt_label = entity['altLabel_list']
            print(label)
            print(alt_label)
