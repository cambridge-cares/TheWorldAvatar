package uk.ac.cam.cares.jps.routing

import androidx.test.ext.junit.runners.AndroidJUnit4
import com.mapbox.maps.extension.style.expressions.dsl.generated.interpolate
import com.mapbox.maps.extension.style.expressions.generated.Expression
import org.junit.Test
import org.junit.runner.RunWith

@RunWith(AndroidJUnit4::class)
class Test {
    @Test
    fun testInterpolation() {
        val json = interpolate {
            linear()
            zoom()
            stop {
                literal(0.0)
                literal(0.6)
            }
            stop {
                literal(20.0)
                literal(1.0)
            }
        }.toJson()
        val builder = Expression.ExpressionBuilder("interpolate")
        builder.build()
        println(json)
    }
}