package uk.ac.cam.cares.jps.routing;

import static org.junit.Assert.assertEquals;

import android.content.Context;
import android.graphics.Color;

import androidx.test.ext.junit.runners.AndroidJUnit4;
import androidx.test.platform.app.InstrumentationRegistry;

import com.mapbox.maps.extension.style.expressions.generated.Expression;

import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * Instrumented test, which will execute on an Android device.
 *
 * @see <a href="http://d.android.com/tools/testing">Testing documentation</a>
 */
@RunWith(AndroidJUnit4.class)
public class ExampleInstrumentedTest {
    @Test
    public void useAppContext() {
        // Context of the app under test.
        Context appContext = InstrumentationRegistry.getInstrumentation().getTargetContext();
        assertEquals("uk.ac.cam.cares.jps.locationtracking.test", appContext.getPackageName());
    }

    @Test
    public void testInterpolate() {
        // Context of the app under test.
        Expression.ExpressionBuilder builder = new Expression.ExpressionBuilder("interpolate");
        builder.stop(expressionBuilder -> {
            expressionBuilder.literal(0.0);
            expressionBuilder.literal(0.6);
            return null;
        });
        builder.stop(expressionBuilder -> {
            expressionBuilder.literal(20.0);
            expressionBuilder.literal(1.0);
            return null;
        });
        builder.zoom();
        builder.interpolate(interpolatorBuilder -> {
            interpolatorBuilder.linear();
            return null;
        });

        System.out.println(builder.build().toJson());
    }
}