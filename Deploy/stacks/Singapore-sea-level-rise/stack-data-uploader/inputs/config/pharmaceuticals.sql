CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
alter table pharmaceuticals 
drop column if exists company_uuid,
drop column if exists industry_uuid,
drop column if exists product_uuid,
drop column if exists facility_uuid;
alter table pharmaceuticals 
add column company_uuid TEXT,
add column industry_uuid TEXT,
add column facility_uuid TEXT;
UPDATE pharmaceuticals
SET company_uuid = uuid_generate_v5('b6f1ef16-6312-4bd8-b078-ee86fded99b6', company),
   industry_uuid = uuid_generate_v5('382ff8b0-e665-487d-89d9-c65b5bb3c541', infrastructure_type),
   facility_uuid = uuid_generate_v5('b6336a92-6f12-4ae7-94f4-89b741a8fc0f',ogc_fid::VARCHAR);
