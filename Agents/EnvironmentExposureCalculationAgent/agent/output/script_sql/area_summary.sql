ALTER TABLE "env_exposure"."point_table"
ADD COLUMN IF NOT EXISTS greenspace_area FLOAT,
ADD COLUMN IF NOT EXISTS greenspace FLOAT;

UPDATE "env_exposure"."point_table"
SET greenspace = agg.count,
    greenspace_area = agg.area
FROM (
    SELECT COUNT(iri) as count, SUM(area) as area, point_id, query_id
    FROM "env_exposure"."results"
    WHERE area IS NOT null and type_label = 'greenspace' and query_id = %(query_id)s
    GROUP BY (point_id, query_id)
) AS agg
WHERE agg.point_id = "env_exposure"."point_table".id and agg.query_id = "env_exposure"."point_table".query_id;