SELECT column_name
FROM information_schema.columns
WHERE table_name = %s AND column_name = 'Trip index';
