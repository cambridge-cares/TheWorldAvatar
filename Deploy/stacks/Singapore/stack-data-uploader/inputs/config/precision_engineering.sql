CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
alter table precision_engineering 
drop column if exists company_uuid,
drop column if exists industry_uuid,
drop column if exists facility_uuid;
alter table precision_engineering 
add column company_uuid UUID,
add column industry_uuid UUID,
add column facility_uuid UUID;
UPDATE precision_engineering
SET company_uuid = uuid_generate_v5('d91dfcb0-f54f-4700-b04d-5119acdbab54', company),
   industry_uuid = uuid_generate_v5('7bc0dc70-4a9b-4ece-9bd5-96dc53fc5313', infrastructure_type),
   facility_uuid = uuid_generate_v5('c7c22b7b-a4b2-4182-9c62-89b8e758bd6e',ogc_fid::VARCHAR);
