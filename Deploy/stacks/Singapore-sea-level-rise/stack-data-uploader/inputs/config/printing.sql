CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
alter table printing 
drop column if exists company_uuid,
drop column if exists industry_uuid,
drop column if exists facility_uuid;
alter table printing 
add column company_uuid UUID,
add column industry_uuid UUID,
add column facility_uuid UUID;
UPDATE printing
SET company_uuid = uuid_generate_v5('a93df160-4427-4c31-b936-2d96114ef0d0', company),
   industry_uuid = uuid_generate_v5('721d04c7-5af9-4ffa-b90e-7ec27d8f5776', infrastructure_type),
   facility_uuid = uuid_generate_v5('f329ad5e-8fb5-40d9-bbea-3ed43c1d4310',ogc_fid::VARCHAR);
