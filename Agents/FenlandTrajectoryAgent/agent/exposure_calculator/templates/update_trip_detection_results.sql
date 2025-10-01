UPDATE "{table_name}"
SET "Points in segment" = %s,
    "x_utm_sd" = %s,
    "y_utm_sd" = %s,
    "kernel" = %s,
    "zone" = %s,
    "norm_kernel" = %s,
    "modfied_kernel" = %s,
    "norm_modified_kernel" = %s,
    "snap_to_hs" = %s,
    "Trip index" = %s,
    "Visit index" = %s,
    "Gap" = %s
WHERE "time" = %s;