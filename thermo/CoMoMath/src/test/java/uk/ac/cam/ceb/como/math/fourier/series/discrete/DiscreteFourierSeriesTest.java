/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.fourier.series.discrete;

/**
 *
 * @author pb556
 */
public class DiscreteFourierSeriesTest {

    private double f(double x) {
        return (1 - Math.cos(3*x));
    }

//    @Test
//    public void realTest() throws Exception {
//        double[] anglesDegree = new double[31];
//        for (int i = 0; i < anglesDegree.length; i++) {
//            anglesDegree[i] = i * 12;
//        }
//
//        double[] x = new double[anglesDegree.length];
//        double[] y = new double[]{0.0,
//            1.228734,
//            4.5079835,
//            8.695656,
//            12.2006985,
//            13.5712095,
//            12.2269535,
//            8.7507915,
//            4.5919995,
//            1.302248,
//            0,
//            1.1420925,
//            4.402963501,
//            8.648397,
//            12.245332,
//            13.5659585,
//            12.0221645,
//            8.4042255,
//            4.300569,
//            1.170973,
//            0,
//            1.118463,
//            4.211302,
//            8.333337,
//            11.9801565,
//            13.563333,
//            12.276838,
//            8.7140345,
//            4.4712265,
//            1.202479,
//            0};
//        
//        double lower = 0;
//        double upper = 2 * Math.PI;
//        int order = 5;
//
//        XYSeries seriesDisc = new XYSeries("Discrete");
//        for (int i = 0; i < x.length; i++) {
//            seriesDisc.add(anglesDegree[i], y[i]);
//        }
//
//        DiscreteFourierSeries fourierSeries = new DiscreteFourierSeries();
//        DiscreteFourierCoefficientFitting coeffFitting = new DiscreteFourierCoefficientFitting();
//
//        coeffFitting.setValues(x, y);
//        coeffFitting.setLimits(lower, upper);
//        coeffFitting.setOrder(order);
//        coeffFitting.fitCoefficients();
//
//        fourierSeries.setLimit(lower, upper);
//        fourierSeries.setCoefficients(coeffFitting.getCoefficients());
//
//        // Visualisation of the output
//        XYSeries seriesFS = new XYSeries("Fourier Series");
//
//        for (int i = 0; i < 360; i++) {
//            double val = fourierSeries.calculate(AngleConversion.toRadians(i));
//            seriesFS.add(i, val);
//        }
//
//        XYSeriesCollection dataset = new XYSeriesCollection();
//        dataset.addSeries(seriesFS);
//        dataset.addSeries(seriesDisc);
//
//        ApplicationFrame frame = new ApplicationFrame("Fitting");
//
//        NumberAxis xax = new NumberAxis("Angle");
//        NumberAxis yax = new NumberAxis("Energy");
//        XYSplineRenderer a = new XYSplineRenderer();
//        a.setPrecision(10);
//        XYPlot xyplot = new XYPlot(dataset, xax, yax, a);
//
//        JFreeChart chart = new JFreeChart(xyplot);
//
//        ChartPanel chartPanel = new ChartPanel(chart);
//        frame.setContentPane(chartPanel);
//        frame.pack();
//        frame.setVisible(true);
//
//        double tolerance = 0.00001;
//        for (int i = 0; i < x.length; i++) {
//            double val = fourierSeries.calculate(x[i]);
//            assert (val > y[i] - tolerance);
//            assert (val < y[i] + tolerance);
//        }
//
//    }
//
//    @Test
//    public void cosTest() throws Exception {
//        double[] anglesDegree = new double[10];
//        for (int i = 0; i < anglesDegree.length; i++) {
//            anglesDegree[i] = i * 36;
//        }
//
//        double[] x = new double[anglesDegree.length];
//        double[] y = new double[x.length];
//
//        double lower = 0;
//        double upper = 2 * Math.PI;
//        int order = 4;
//
//        XYSeries seriesDisc = new XYSeries("Discrete");
//        for (int i = 0; i < x.length; i++) {
//            x[i] = AngleConversion.toRadians(anglesDegree[i]);
//            y[i] = f(x[i]);
//            seriesDisc.add(anglesDegree[i], y[i]);
//        }
//
//        DiscreteFourierSeries fourierSeries = new DiscreteFourierSeries();
//        DiscreteFourierCoefficientFitting coeffFitting = new DiscreteFourierCoefficientFitting();
//
//        coeffFitting.setValues(x, y);
//        coeffFitting.setLimits(lower, upper);
//        coeffFitting.setOrder(order);
//        coeffFitting.fitCoefficients();
//
//        fourierSeries.setLimit(lower, upper);
//        fourierSeries.setCoefficients(coeffFitting.getCoefficients());
//
//        // Visualisation of the output
//        XYSeries seriesFS = new XYSeries("Fourier Series");
//
//        for (int i = 0; i < 360; i++) {
//            double val = fourierSeries.calculate(AngleConversion.toRadians(i));
//            seriesFS.add(i, val);
//        }
//
//        XYSeriesCollection dataset = new XYSeriesCollection();
//        dataset.addSeries(seriesFS);
//        dataset.addSeries(seriesDisc);
//
//        ApplicationFrame frame = new ApplicationFrame("Fitting");
//
//        NumberAxis xax = new NumberAxis("Angle");
//        NumberAxis yax = new NumberAxis("Energy");
//        XYSplineRenderer a = new XYSplineRenderer();
//        a.setPrecision(10);
//        XYPlot xyplot = new XYPlot(dataset, xax, yax, a);
//
//        JFreeChart chart = new JFreeChart(xyplot);
//
//        ChartPanel chartPanel = new ChartPanel(chart);
//        frame.setContentPane(chartPanel);
//        frame.pack();
//        frame.setVisible(true);
//
//        double tolerance = 0.00001;
//        for (int i = 0; i < x.length; i++) {
//            double val = fourierSeries.calculate(x[i]);
//            assert (val > y[i] - tolerance);
//            assert (val < y[i] + tolerance);
//        }
//    }
//    
//    @Test
//    public void simpleTest() throws Exception {
//        double[] x = new double[]{-3.14, -2.44, -1.75, -1.05, -0.35, 0.35, 1.05, 1.75, 2.44, 3.14};
//        double[] y = new double[]{-8.87, -4.97, -2.05, -0.1, 0.88, 0.88, -0.1, -2.05, -4.97, -8.87};
//        
//        DiscreteFourierSeries fourierSeries = new DiscreteFourierSeries();
//        DiscreteFourierCoefficientFitting coeffFitting = new DiscreteFourierCoefficientFitting();
//
//        coeffFitting.setValues(x, y);
//        coeffFitting.setLimits(0, 2*Math.PI);
//        coeffFitting.setOrder(5);
//        coeffFitting.fitCoefficients();
//
//        fourierSeries.setLimit(0, 2*Math.PI);
//        fourierSeries.setCoefficients(coeffFitting.getCoefficients());
//        
//        XYSeries seriesDisc = new XYSeries("Discrete");
//        for (int i = 0; i < x.length; i++) {
//            seriesDisc.add(x[i], y[i]);
//        }
//        
//        XYSeries seriesFS = new XYSeries("Fourier Series");
//
//        for (int i = -180; i < 180; i++) {
//            double val = fourierSeries.calculate(AngleConversion.toRadians(i));
//            seriesFS.add(AngleConversion.toRadians(i), val);
//        }
//
//        XYSeriesCollection dataset = new XYSeriesCollection();
//        dataset.addSeries(seriesFS);
//        dataset.addSeries(seriesDisc);
//
//        ApplicationFrame frame = new ApplicationFrame("Fitting");
//
//        NumberAxis xax = new NumberAxis("Angle");
//        NumberAxis yax = new NumberAxis("Energy");
//        XYSplineRenderer a = new XYSplineRenderer();
//        a.setPrecision(10);
//        XYPlot xyplot = new XYPlot(dataset, xax, yax, a);
//
//        JFreeChart chart = new JFreeChart(xyplot);
//
//        ChartPanel chartPanel = new ChartPanel(chart);
//        frame.setContentPane(chartPanel);
//        frame.pack();
//        frame.setVisible(true);
//    }
}
