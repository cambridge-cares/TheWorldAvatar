package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.locationtech.jts.io.WKTWriter;

import java.io.IOException;

// NOTE: You must ensure JTS dependencies are available in your project.
public class GeometryTypeAdapter extends TypeAdapter<Geometry> {
    
    // WKTWriter and WKTReader are reusable and thread-safe.
    private static final WKTWriter WKT_WRITER = new WKTWriter();
    private static final WKTReader WKT_READER = new WKTReader();

    @Override
    public void write(JsonWriter out, Geometry value) throws IOException {
        if (value == null) {
            out.nullValue();
            return;
        }
        
        // Convert Geometry to its Well-Known Text (WKT) string
        String wktString = WKT_WRITER.write(value);
        out.value(wktString);
    }

    @Override
    public Geometry read(JsonReader in) throws IOException {
        if (in.peek() == com.google.gson.stream.JsonToken.NULL) {
            in.nextNull();
            return null;
        }

        String wktString = in.nextString();
        try {
            // Convert WKT string back to a Geometry object
            return WKT_READER.read(wktString);
        } catch (ParseException e) {
            // Wrap the JTS ParseException in an IOException
            throw new IOException("Failed to parse WKT string into Geometry: " + wktString, e);
        }
    }
}