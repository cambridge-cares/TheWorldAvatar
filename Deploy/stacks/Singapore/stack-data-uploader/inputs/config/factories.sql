alter table factories 
drop column if exists company_uuid,
drop column if exists facility_uuid;
alter table factories 
add column company_uuid UUID,
add column facility_uuid UUID;
UPDATE factories
SET company_uuid = uuid_generate_v5('19a7288a-dbfd-48a4-8726-de1b6e30fce4', company),
   facility_uuid = uuid_generate_v5('1e3f6693-9d9d-402c-be1f-30a3ef3ea138',ogc_fid::VARCHAR);
