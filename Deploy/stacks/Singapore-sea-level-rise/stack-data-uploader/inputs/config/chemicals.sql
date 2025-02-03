CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
alter table chemicals 
drop column if exists company_uuid,
drop column if exists industry_uuid,
drop column if exists product_uuid,
drop column if exists facility_uuid;
alter table chemicals 
add column company_uuid TEXT,
add column industry_uuid TEXT,
add column facility_uuid TEXT;
UPDATE chemicals
SET company_uuid = uuid_generate_v5('3fadc915-c914-4197-9991-19c93f941067', company),
   industry_uuid = uuid_generate_v5('e39bcbb3-5e48-4c5a-890d-bae247e05b29', infrastructure_type),
   facility_uuid = uuid_generate_v5('52f30a7c-ab7e-43bb-8a63-c40509c402bd',ogc_fid::VARCHAR);
