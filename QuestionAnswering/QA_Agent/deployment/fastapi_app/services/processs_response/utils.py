def iri_slot_template_to_func(template: str):
    assert "{IRI}" in template

    def func(IRI: str):
        return template.format(IRI=IRI)

    return func
