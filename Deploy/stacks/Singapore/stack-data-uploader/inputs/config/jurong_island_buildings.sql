CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
alter table jurong_island_buildings 
drop column if exists company_uuid;
alter table jurong_island_buildings 
add column company_uuid UUID;
UPDATE jurong_island_buildings
SET company_uuid = uuid_generate_v5('a085eb7a-56e3-48e3-a4ea-2bf1399c9999', company);