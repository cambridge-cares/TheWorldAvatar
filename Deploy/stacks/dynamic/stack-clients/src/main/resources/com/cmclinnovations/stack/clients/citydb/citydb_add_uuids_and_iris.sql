CREATE UNIQUE INDEX IF NOT EXISTS iri_index ON cityobject_genericattrib (attrname, cityobject_id);

WITH uuids AS (
    INSERT INTO
        cityobject_genericattrib (
            attrname,
            datatype,
            strval,
            cityobject_id
        )
    SELECT
        'uuid',
        1,
        gen_random_uuid(),
        id
    FROM
        cityobject ON CONFLICT (attrname, cityobject_id) DO NOTHING RETURNING id,
        strval,
        cityobject_id
)
INSERT INTO
    cityobject_genericattrib (
        attrname,
        datatype,
        urival,
        cityobject_id
    )
SELECT
    'iri',
    4,
    '{baseIRI}' || objectclass.classname || '/' || strval,
    cityobject_id
FROM
    uuids,
    cityobject,
    objectclass
WHERE
    cityobject.id = uuids.cityobject_id
    AND cityobject.objectclass_id = objectclass.id ON CONFLICT (attrname, cityobject_id) DO NOTHING