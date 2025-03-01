CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
alter table semiconductors 
drop column if exists company_uuid,
drop column if exists industry_uuid,
drop column if exists product_uuid,
drop column if exists facility_uuid;
alter table semiconductors 
add column company_uuid TEXT,
add column industry_uuid TEXT,
add column facility_uuid TEXT;
UPDATE semiconductors
SET company_uuid = uuid_generate_v5('19a7288a-dbfd-48a4-8726-de1b6e30fce4', company),
   industry_uuid = uuid_generate_v5('ee95784f-22cc-4389-82a5-244736859dc3', infrastructure_type),
   facility_uuid = uuid_generate_v5('1e3f6693-9d9d-402c-be1f-30a3ef3ea138',ogc_fid::TEXT);
