INSERT INTO slr_population (slr_uuid, populationatrisk)
SELECT subquery.uuid, subquery.sum
FROM (
SELECT sealevelprojections.uuid, SUM((ST_SummaryStats(ST_Clip(population.rast, sealevelprojections.geom, TRUE))).sum) AS sum
FROM sealevelprojections, population
WHERE sealevelprojections.uuid = '2198a569-a95a-4d46-9577-b1ffa1b7a2ce'
GROUP BY sealevelprojections.uuid
) AS subquery;