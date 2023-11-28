SELECT fc.id, fc.source, fc.target, fc.cost_s as cost, fc.reverse_cost_s as reverse_cost, rw.tag_id
FROM flood_cost_30cm fc
JOIN routing_ways rw ON fc.id = rw.gid WHERE rw.tag_id IN (100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 121, 123, 124, 125, 401)