SELECT
    fid,
    prob,
    county,
    cromeid,
    cropmap.lucode,
    geom,
    label,
    colour
FROM
    cropmap,
    cropmap_labels,
    cropmap_colours
WHERE
    cropmap.lucode = cropmap_labels.lucode
    AND cropmap.lucode = cropmap_colours.lucode