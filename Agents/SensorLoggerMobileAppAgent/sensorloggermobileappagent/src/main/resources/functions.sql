-- ANY CHANGES IN THE FILE SHOULD BE CROSS CHECKED WITH THE PREFIXES IN ontop.obda
CREATE OR REPLACE FUNCTION get_point_iri(device_id VARCHAR)
RETURNS VARCHAR AS $$
BEGIN
    RETURN CONCAT('https://www.theworldavatar.com/kg/sensorloggerapp/point_', device_id);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_altitude_iri(device_id VARCHAR)
RETURNS VARCHAR AS $$
BEGIN
    RETURN CONCAT('https://www.theworldavatar.com/kg/sensorloggerapp/altitude_measure_', device_id);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_bearing_iri(device_id VARCHAR)
RETURNS VARCHAR AS $$
BEGIN
    RETURN CONCAT('https://www.theworldavatar.com/kg/sensorloggerapp/bearing_measure_', device_id);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_speed_iri(device_id VARCHAR)
RETURNS VARCHAR AS $$
BEGIN
    RETURN CONCAT('https://www.theworldavatar.com/kg/sensorloggerapp/speed_measure_', device_id);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_device_iri(device_id VARCHAR)
RETURNS VARCHAR AS $$
BEGIN
    RETURN CONCAT('https://www.theworldavatar.com/kg/sensorloggerapp/smartphone_', device_id);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_session_iri(device_id VARCHAR)
RETURNS VARCHAR AS $$
BEGIN
    RETURN CONCAT('https://www.theworldavatar.com/kg/sensorloggerapp/sessionID_', device_id);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_activity_type_iri(device_id VARCHAR)
RETURNS VARCHAR AS $$
BEGIN
    RETURN CONCAT('https://www.theworldavatar.com/kg/sensorloggerapp/activity_type_', device_id);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_confidence_level_iri(device_id VARCHAR)
RETURNS VARCHAR AS $$
BEGIN
    RETURN CONCAT('https://www.theworldavatar.com/kg/sensorloggerapp/confidence_level_', device_id);
END;
$$ LANGUAGE plpgsql;