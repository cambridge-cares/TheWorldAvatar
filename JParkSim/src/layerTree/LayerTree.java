/* Copyright 2014 Esri
All rights reserved under the copyright laws of the United States
and applicable international laws, treaties, and conventions.

You may freely redistribute and use this sample code, with or
without modification, provided you include the original copyright
notice and use restrictions.

See the use restrictions.*/
package layerTree;

import com.esri.client.samples.toolkit.*;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import com.esri.toolkit.JLayerTree;
import com.esri.core.geometry.Envelope;
import com.esri.core.geometry.GeometryEngine;
import com.esri.core.geometry.Point;
import com.esri.map.ArcGISDynamicMapServiceLayer;
import com.esri.map.ArcGISTiledMapServiceLayer;
import com.esri.map.JMap;
import com.esri.map.LayerInitializeCompleteEvent;
import com.esri.map.LayerInitializeCompleteListener;
import com.esri.map.LayerList;
import com.esri.map.MapEvent;
import com.esri.map.MapEventListenerAdapter;
import com.esri.map.Layer;

/**
 * Uses a {@link JLayerTree} to control layer display.
 */
public class LayerTree {

  private JMap jMap;
  private JLayerTree jLayerTree;
  private JPanel jPanel;

  public static void main(String[] args) {
    SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {
        try {
          LayerTree layerTreeApp = new LayerTree();
          JFrame window = layerTreeApp.createWindow();
          window.setContentPane(layerTreeApp.createUI());
          window.setVisible(true);
        } catch (Exception e) {
          e.printStackTrace();
        }
      }
    });
  }

  public LayerTree() {
    // add each layer, give it a name and refresh the layer tree
    ArcGISTiledMapServiceLayer worldLayer = new ArcGISTiledMapServiceLayer(
        "http://services.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer");
    worldLayer.addLayerInitializeCompleteListener(new EditLayerName());
    LayerList layers = getJMap().getLayers();
    layers.add(worldLayer);

    ArcGISDynamicMapServiceLayer highwayLayer = new ArcGISDynamicMapServiceLayer(
        "http://sampleserver1.arcgisonline.com/ArcGIS/rest/services/Specialty/ESRI_StateCityHighway_USA/MapServer");
    highwayLayer.addLayerInitializeCompleteListener(new EditLayerName());
    layers.add(highwayLayer);

    ArcGISDynamicMapServiceLayer californiaLayer = new ArcGISDynamicMapServiceLayer(
        "http://maverick.arcgis.com/ArcGIS/rest/services/California/MapServer");
    californiaLayer.addLayerInitializeCompleteListener(new EditLayerName());
    // zoom to the full extent of the California layer
    californiaLayer.addLayerInitializeCompleteListener(new ZoomToLayer());
    layers.add(californiaLayer);
  }

  private void setLayerName(Layer layer) {
    // get the last part of the URL as a name for the layer e.g. get
    // "ESRI_StateCityHighway_USA" from
    // .../ESRI_StateCityHighway_USA/MapServer
    String[] words = layer.getUrl().split("/");
    layer.setName(words[words.length - 2]);
  }

  private JMap getJMap() {
    if (jMap == null) {
      jMap = new JMap();
      jMap.setPreferredSize(new Dimension(768, 768));
      jMap.setMinimumSize(new Dimension(512, 512));
    }
    return jMap;
  }

  public JPanel createUI() {
    if (jPanel == null) {
      jPanel = new JPanel();
      jPanel.setOpaque(true);
      jPanel.setLayout(new BorderLayout());
      jPanel.add(getJLayerTree(), BorderLayout.WEST);
      jPanel.add(getJMap(), BorderLayout.CENTER);
    }
    return jPanel;
  }

  protected JLayerTree getJLayerTree() {
    if (jLayerTree == null) {
      jLayerTree = new JLayerTree(getJMap());
    }
    return jLayerTree;
  }

  /**
   * Creates a window.
   * @return a window.
   */
   private JFrame createWindow() {
     JFrame window = new JFrame("Layer Tree Application");
     window.setBounds(100, 100, 1000, 700);
     window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
     window.getContentPane().setLayout(new BorderLayout(0, 0));
     window.addWindowListener(new WindowAdapter() {
       @Override
       public void windowClosing(WindowEvent windowEvent) {
         super.windowClosing(windowEvent);
         jMap.dispose();
       }
     });
     return window;
   }

   // when each layer is initialized give it a name and refresh the layer tree
   class EditLayerName implements LayerInitializeCompleteListener {
     @Override
     public void layerInitializeComplete(
         final LayerInitializeCompleteEvent arg0) {
       SwingUtilities.invokeLater(new Runnable() {
         @Override
         public void run() {
           setLayerName(arg0.getLayer());
           jLayerTree.invalidate();
           jLayerTree.validate();
           jLayerTree.refresh();
         }
       });
     }
   }

   class ZoomToLayer implements LayerInitializeCompleteListener {
     @Override
     public void layerInitializeComplete(
         final LayerInitializeCompleteEvent arg) {
       // layer is initialized
       jMap.addMapEventListener(new MapEventListenerAdapter() {
         @Override
         public void mapReady(MapEvent arg0) {
           // map is ready to zoom
           SwingUtilities.invokeLater(new Runnable() {
             @Override
             public void run() {
               Point lowerLeft = (Point) GeometryEngine.project(
                   arg.getLayer().getFullExtent()
                   .getLowerLeft(), arg.getLayer()
                   .getDefaultSpatialReference(), jMap
                   .getSpatialReference());
               Point upperRight = (Point) GeometryEngine.project(
                   arg.getLayer().getFullExtent()
                   .getUpperRight(), arg.getLayer()
                   .getDefaultSpatialReference(), jMap
                   .getSpatialReference());
               jMap.zoomTo(new Envelope(lowerLeft.getX(),
                   lowerLeft.getY(), upperRight.getX(),
                   upperRight.getY()));
             }
           });
         }
       });
     }
   }
}

