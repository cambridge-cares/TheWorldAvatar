select
    "UNITNO" as name,
    wkb_geometry as geometry_kl,
    round(random()) as status

from
    street_lighting_kl_table
