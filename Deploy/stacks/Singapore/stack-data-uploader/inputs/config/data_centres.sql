CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
alter table data_centres 
drop column if exists company_uuid,
drop column if exists industry_uuid,
drop column if exists facility_uuid;
alter table data_centres 
add column company_uuid UUID,
add column industry_uuid UUID,
add column facility_uuid UUID;
UPDATE data_centres
SET company_uuid = uuid_generate_v5('6ba7b810-9dad-11d1-80b4-00c04fd430c8', company),
   industry_uuid = uuid_generate_v5('9a791b56-6c62-4ebf-87bd-e4e87cbedd61', infrastructure_type),
   facility_uuid = uuid_generate_v5('0f849316-1751-461a-8462-e3b0c5df3b85',ogc_fid::VARCHAR);

