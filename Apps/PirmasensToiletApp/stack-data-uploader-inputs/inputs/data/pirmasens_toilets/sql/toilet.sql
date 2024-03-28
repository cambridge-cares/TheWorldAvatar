SELECT
  id,
  amenity,
  operator,
  male::boolean,
  female::boolean,
  price,
  price_currency,
  wheelchair::varchar,
  street_address,
  locality,
  postal_code::varchar,
  image,
  media_type,
  wkb_geometry as "geometryProperty"
FROM ps_data