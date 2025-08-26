UPDATE
    isochrone_aggregated
SET
    populationTable = subquery.sum
FROM
    (
        SELECT
            isochrone_aggregated.geom,
            SUM(
                (
                    ST_SummaryStats(
                        ST_Clip(
                            populationTable.rast,
                            isochrone_aggregated.geom,
                            TRUE
                        )
                    )
                ).sum
            )
        FROM
            populationTable,
            isochrone_aggregated
        GROUP BY
            isochrone_aggregated.geom
    ) AS subquery
WHERE
    subquery.geom = isochrone_aggregated.geom;