CREATE UNIQUE INDEX IF NOT EXISTS iri_index ON cityobject_genericattrib (attrname, cityobject_id);
WITH uuids AS (
INSERT INTO cityobject_genericattrib (parent_genattrib_id, root_genattrib_id, attrname, datatype, strval, intval, realval, urival, dateval, unit, genattribset_codespace, blobval, geomval, surface_geometry_id, cityobject_id)
SELECT NULL, NULL, 'uuid', 1, gen_random_uuid(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, id
FROM cityobject
ON CONFLICT (attrname, cityobject_id) DO NOTHING
RETURNING id, strval, cityobject_id)
INSERT INTO cityobject_genericattrib (parent_genattrib_id, root_genattrib_id, attrname, datatype, strval, intval, realval, urival, dateval, unit, genattribset_codespace, blobval, geomval, surface_geometry_id, cityobject_id)
SELECT uuids.id, NULL, 'iri', 4, NULL, NULL, NULL, '{baseIRI}' || objectclass.classname || '/' || strval, NULL, NULL, NULL, NULL, NULL, NULL, cityobject_id
FROM uuids, cityobject, objectclass
WHERE cityobject.id = uuids.cityobject_id AND cityobject.objectclass_id = objectclass.id
ON CONFLICT (attrname, cityobject_id) DO NOTHING