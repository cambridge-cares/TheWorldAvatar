CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
alter table food_beverages 
drop column if exists company_uuid,
drop column if exists industry_uuid,
drop column if exists product_uuid,
drop column if exists facility_uuid;
alter table food_beverages 
add column company_uuid TEXT,
add column industry_uuid TEXT,
add column facility_uuid TEXT;
UPDATE food_beverages
SET company_uuid = uuid_generate_v5('eac6af5a-0089-4102-8d88-2b0419cc4043', company),
   industry_uuid = uuid_generate_v5('7f97a6c5-f290-4fc0-bdd0-c167aae1a7ac', infrastructure_type),
   facility_uuid = uuid_generate_v5('9babc2b3-5d3e-45e7-948d-1b54ac2d0360',ogc_fid::VARCHAR);
