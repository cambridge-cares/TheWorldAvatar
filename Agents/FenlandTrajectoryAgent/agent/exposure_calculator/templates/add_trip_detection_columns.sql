DO $$ BEGIN
    IF NOT EXISTS (
        SELECT FROM information_schema.columns
        WHERE table_name = '{table_name}' AND column_name = '{column_name}'
    ) THEN
        EXECUTE 'ALTER TABLE "{table_name}" ADD COLUMN "{column_name}" {col_type}';
    END IF;
END $$;
