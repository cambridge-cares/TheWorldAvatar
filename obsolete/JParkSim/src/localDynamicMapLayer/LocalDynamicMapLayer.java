package localDynamicMapLayer;

import java.awt.BorderLayout;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.JOptionPane;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;

import com.esri.client.local.ArcGISLocalDynamicMapServiceLayer;
import com.esri.map.JMap;
import com.esri.map.LayerInitializeCompleteEvent;
import com.esri.map.LayerInitializeCompleteListener;
import com.esri.runtime.ArcGISRuntime;

/**
 * This application shows how to create a local dynamic layer from a map package (.mpk).
 * An <code>ArcGISLocalDynamicMapServiceLayer</code> is created from the map package,
 * then the layer is added to the map.
 */
public class LocalDynamicMapLayer {

  private JComponent contentPane;
  private JMap map;
  private JProgressBar progressBar;
  private static String FSP = System.getProperty("file.separator");

  // ------------------------------------------------------------------------
  // Constructors
  // ------------------------------------------------------------------------
  /**
   * Creates an instance of this sample.
   */
  public LocalDynamicMapLayer() {
  }

  // ------------------------------------------------------------------------
  // Static methods
  // ------------------------------------------------------------------------
  /**
   * Starting point of this application.
   * @param args arguments to this application.
   */
  public static void main(String[] args) {
    SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {
        try {
          // instance of this application
          LocalDynamicMapLayer localDynamicMapLayerApp = new LocalDynamicMapLayer();

          // create the UI, including the map, for the application.
          JFrame appWindow = localDynamicMapLayerApp.createWindow();
          appWindow.add(localDynamicMapLayerApp.createUI());
          appWindow.setVisible(true);
        } catch (Exception e) {
          // on any error, display the stack trace.
          e.printStackTrace();
        }
      }
    });
  }

  // ------------------------------------------------------------------------
  // Public methods
  // ------------------------------------------------------------------------
  /**
   * Creates and displays the UI, including the map, for this application.
   * @return the UI for this sample.
   */
  public JComponent createUI() {

    contentPane = createContentPane();

    // progress bar
    progressBar = createProgressBar(contentPane);

    // create map
    map = createMap();

    contentPane.add(progressBar);
    contentPane.add(map);

    return contentPane;
  }

  /**
   * Creates the map.
   * @return a map.
   */
  private JMap createMap() {

    final JMap jMap = new JMap();

    updateProgresBarUI("Starting local dynamic map service...", true);

    // create the local dynamic layer from the mpk and to the map
    final ArcGISLocalDynamicMapServiceLayer dynamicLayer =
        new ArcGISLocalDynamicMapServiceLayer(getPathSampleData() +
//            "mpks" + FSP + "USCitiesStates_Lambert_Conformal_Conic.mpk");
              "mpks" + FSP + "template.mpk");
    dynamicLayer
        .addLayerInitializeCompleteListener(new LayerInitializeCompleteListener() {

          @Override
          public void layerInitializeComplete(LayerInitializeCompleteEvent e) {
            synchronized (progressBar) {
              if (e.getID() == LayerInitializeCompleteEvent.LOCALLAYERCREATE_ERROR) {
                String errMsg = "Failed to initialize due to "
                    + dynamicLayer.getInitializationError();
                JOptionPane.showMessageDialog(jMap, wrap(errMsg), "",
                    JOptionPane.ERROR_MESSAGE);
              }
              updateProgresBarUI(null, false);
            }
          }
        });
    jMap.getLayers().add(dynamicLayer);

    return jMap;
  }

  /**
   * Creates a content pane.
   * @return a content pane.
   */
  private static JLayeredPane createContentPane() {
    JLayeredPane contentPane = new JLayeredPane();
    contentPane.setBounds(100, 100, 1000, 700);
    contentPane.setLayout(new BorderLayout(0, 0));
    contentPane.setVisible(true);
    return contentPane;
  }

  /**
   * Creates a progress bar.
   * @param parent progress bar's parent. The horizontal axis of the progress bar will be
   * center-aligned to the parent.
   * @return a progress bar.
   */
  private static JProgressBar createProgressBar(final JComponent parent) {
    final JProgressBar progressBar = new JProgressBar();
    progressBar.setSize(320, 20);
    parent.addComponentListener(new ComponentAdapter() {
      @Override
      public void componentResized(ComponentEvent e) {
        progressBar.setLocation(
            parent.getWidth()/2 - progressBar.getWidth()/2,
            parent.getHeight() - progressBar.getHeight() - 20);
      }
    });
    progressBar.setStringPainted(true);
    progressBar.setIndeterminate(true);
    progressBar.setVisible(false);
    return progressBar;
  }

  /**
   * Updates progress bar UI from the Swing's Event Dispatch Thread.
   * @param str string to be set.
   * @param visible flag to indicate visibility of the progress bar.
   */
  private void updateProgresBarUI(final String str, final boolean visible) {
    SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {
        if (str != null) {
          progressBar.setString(str);
        }
        progressBar.setVisible(visible);
      }
    }); 
  }

  /**
   * Creates a window.
   * @return a window.
   */
  private JFrame createWindow() {
    JFrame window = new JFrame("Local Dynamic Map Layer Application");
    window.setBounds(100, 100, 1000, 700);
    window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    window.getContentPane().setLayout(new BorderLayout(0, 0));
    window.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent windowEvent) {
        super.windowClosing(windowEvent);
        map.dispose();
      }
    });
    return window;
  }

  private String getPathSampleData() {
    String dataPath = null;
    String javaPath = ArcGISRuntime.getInstallDirectory();
    if (javaPath != null) {
      if (!(javaPath.endsWith("/") || javaPath.endsWith("\\"))){
        javaPath += FSP;
      }
      dataPath = javaPath + "sdk" + FSP + "samples" + FSP + "data" + FSP;
    }
    File dataFile = new File(dataPath);
    if (!dataFile.exists()) { 
      dataPath = ".." + FSP + "data" + FSP;
    }
    return dataPath;
  }

  private String wrap(String str) {
    // create a HTML string that wraps text when longer
    return "<html><p style='width:200px;'>" + str + "</html>";
  }
}
