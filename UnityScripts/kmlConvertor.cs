using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DotSpatial.Symbology;
using DotSpatial.Topology;
using DotSpatial.Controls;
using DotSpatial.Data;
using System.Data;
using System.Xml;
using DotSpatial.Projections;
using System.Collections;
using System.Data.OleDb;
using System.IO;
namespace ConsoleApplication7
{
    class Program
    {

        static void Main(string[] args)
        {
            int counter = 0;
            int iterator = 0;
            int counter_2 = 0;
            Shapefile shapefile = Shapefile.OpenFile("Buildings.shp");
            int[] marker = new int[shapefile.Features.Count];
            
            ArrayList points = new ArrayList();

            foreach (Feature f in shapefile.Features)
            {
                foreach (Coordinate c in f.Coordinates)
                {
                    points.Add(c.X);
                    points.Add(c.Y);
                    iterator++;
                    marker[counter_2] = iterator;
                }

                counter_2++;
            }
            
            foreach (int num in marker)
            {
                Console.WriteLine("Number " + num);
            }
            
            Console.WriteLine("count is " + points.Count);
            double[] xy = new double[points.Count];
            
            foreach (double point in points)
            {
                xy[counter] = point;
                counter++;
            }


            //An array for the z coordinate
            double[] z = new double[counter];
            for (int j = 0; j < counter; j++)
            {
                z[j] = 1;
            }


            ProjectionInfo pStart = shapefile.Projection;
            //   ProjectionInfo pEnd = KnownCoordinateSystems.Projected.UtmWgs1984.WGS1984UTMZone48N;
            ProjectionInfo pEnd = KnownCoordinateSystems.Geographic.World.WGS1984;
            Console.WriteLine("Counter here" + counter);

            int final_count = counter;
            Reproject.ReprojectPoints(xy, z, pStart, pEnd, 0, counter / 2);
            counter = 0;
            foreach (double location in xy)
            {

                Console.WriteLine("Location is " + xy[counter]);
                counter++;
            }


            counter = 0;
            

            int counter_feature = 0;
            int loop_counter = 0;
            IFeatureList set = shapefile.Features;

            string[] kml_coordiantes = new string[set.Count];

            foreach (Feature feature in set)
            {


                for (int i = 0; i < feature.Coordinates.Count; i++)
                {

                    kml_coordiantes[counter_feature] = kml_coordiantes[counter_feature] + xy[loop_counter * 2].ToString() + "," + xy[loop_counter * 2 + 1] + ",100" + "\n";
                    loop_counter++;
                }

                counter_feature++;
            }
            
            Console.WriteLine("Counter_feature" + counter_feature);
            Shapefile shapefile_2 = new Shapefile();
             foreach (Feature feature in set)
            {
                Feature feature_2 = new Feature();
                feature_2.Coordinates = feature.Coordinates;
            }

            foreach (Feature feature in set)
            {
                Feature feature_2 = new Feature(feature.FeatureType, feature.Coordinates);
                Console.WriteLine("Feature type is " + feature.FeatureType);
                shapefile_2.Features.Add(feature_2);
            }



            using (XmlWriter writer = XmlWriter.Create("SHP/NEW/test.kml"))
            {
                writer.WriteStartDocument();
                writer.WriteStartElement("kml", "http://www.opengis.net/kml/2.2");
                writer.WriteString("\n");
                writer.WriteStartElement("Placemark");
                writer.WriteString("\n");
                writer.WriteStartElement("MultiGeometry");
                writer.WriteString("\n");
                writer.WriteElementString("name", "AirLine");
                writer.WriteString("\n");



                foreach (string kml in kml_coordiantes)
                {
                    writer.WriteString("\n");
                    writer.WriteStartElement("LineString");
                    writer.WriteString("\n");
                    writer.WriteElementString("tessellate", "0");
                    writer.WriteString("\n");
                    writer.WriteStartElement("coordinates");
                    writer.WriteString("\n");
                    writer.WriteString(kml);
                    writer.WriteString("\n");
                    writer.WriteEndElement();
                    writer.WriteString("\n");
                    writer.WriteEndElement();
                    writer.WriteString("\n");
                }
                writer.WriteString("\n");
                writer.WriteEndElement();
                writer.WriteString("\n");
                writer.WriteEndElement();
                writer.WriteString("\n");
            }
      //  shapefile_2.DataTable = shapefile.DataTable;
      //    shapefile_2.SaveAs("SHP/NEW/airLine_shape_new.shp", true);
     //     Console.WriteLine("Table is populated " + shapefile_2.AttributesPopulated);
      //      Console.WriteLine("Counter is " + counter);
            DataTable table = new DataTable();
          
            table = shapefile.DataTable;
            table.TableName = "test";
            for (int i = 0; i < table.Columns.Count; i++)
            {
                if (table.Columns[i].ColumnName.Equals("Long")||table.Columns[i].ColumnName.Equals("long"))
                {
                    table.Columns[i].ColumnName = "Longitude";
                }


                if (table.Columns[i].DataType.ToString().Contains("Int16"))
                {
                    Console.WriteLine("Type changed " + table.Columns[i].ColumnName);
                    ChangeColumnDataType(table, table.Columns[i].ColumnName, typeof(Int32));
                }

                if (table.Columns[i].DataType.ToString().Contains("Int64"))
                {
                    Console.WriteLine("Type changed " + table.Columns[i].ColumnName);
                    ChangeColumnDataType(table, table.Columns[i].ColumnName, typeof(Int32));
                }

            }

            table.WriteXml("SHP/NEW/test.xml", XmlWriteMode.WriteSchema);
            AttributeTable attribute_table = new AttributeTable();
            

           

           
             

            Shapefile shape_3 = new Shapefile();
            Console.ReadLine();
            //  PrintValues(attribute_table.Table,"Look at this");




            ///   Console.WriteLine(shapefile.Attributes);


            DataTable new_data = new DataTable();
            new_data.ReadXml("SHP/NEW/test.xml");

            foreach (Feature feature in set)
            {
                Feature feature_2 = new Feature(feature.FeatureType, feature.Coordinates);
                 shape_3.Features.Add(feature_2);
            }

              

            shape_3.SaveAs("SHP/NEW/test.shp", true);

           
            PrintValues(new_data,"data");

            Console.ReadLine();



            WriteToDbf(new_data);
            Console.ReadLine();
            
            //============================================================================================================


        }

        public static void PrintValues(DataTable table, string label)
        {
            Console.WriteLine(label);
            foreach (DataRow row in table.Rows)
            {
                foreach (DataColumn column in table.Columns)
                {
                    Console.Write("\t{0}", row[column]);
                }
                Console.WriteLine();
            }
        }

   
        /// <param name="dt"></param>
        public  static void WriteToDbf(DataTable dt)
        {
            Console.WriteLine("Writing to: " + dt.TableName + ".dbf ...");

            string sConn =
                "Provider=Microsoft.Jet.OLEDB.4.0; " +
      //         "Data Source=" + System.IO.Directory.GetCurrentDirectory() + "; " +
      "Data Source=" + System.IO.Directory.GetCurrentDirectory() + "/SHP/NEW" + "; " +
                "Extended Properties=dBASE IV;";
            OleDbConnection conn = new OleDbConnection(sConn);
            conn.Open();

            try
            {
                string path = System.IO.Directory.GetCurrentDirectory() + "/SHP/NEW/" + dt.TableName;
                Console.WriteLine(path);
                if (File.Exists(path + ".dbf"))
                {
                    
                    Console.WriteLine("Delete file: " + dt.TableName + ".dbf ...");
                    File.Delete(path + ".dbf");
                }

                OleDbCommand cmd;

                StringBuilder sbCreate = new StringBuilder();
                sbCreate.Append("CREATE TABLE " + dt.TableName + ".dbf (");
                for (int i = 0; i < dt.Columns.Count; i++)
                {
                    sbCreate.Append(dt.Columns[i].ColumnName);
                    Console.WriteLine("Very Important ---->" + dt.Columns[i].DataType + "------- " + dt.Columns[i].ToString());

                    if (dt.Columns[i].DataType.ToString().Contains("Int"))
                    {
                        if (dt.Columns[i].DataType.ToString().Contains("16"))
                        {
                            sbCreate.Append(" shortint");
                        }
                        else
                        {
                            sbCreate.Append(" int");
                        }
                    }
                    else
                    {


                        sbCreate.Append(" char(50)");

                    }












                    if (i != dt.Columns.Count - 1)
                    {
                        sbCreate.Append(", ");
                    }
                    else
                    {
                        sbCreate.Append(')');
                    }
                }

                Console.WriteLine("\nCreating Table ...");
                Console.WriteLine(sbCreate.ToString());
                cmd = new OleDbCommand(sbCreate.ToString(), conn);
                cmd.ExecuteNonQuery();

                StringBuilder sbInsert = new StringBuilder();
                foreach (DataRow dr in dt.Rows)
                {
                    sbInsert.Clear();
                    sbInsert.Append("INSERT INTO " + dt.TableName + ".dbf (");
                    for (int i = 0; i < dt.Columns.Count; i++)
                    {
                        sbInsert.Append(dt.Columns[i].ColumnName);
                        if (i != dt.Columns.Count - 1)
                        {
                            sbInsert.Append(", ");
                        }
                    }
                    sbInsert.Append(") VALUES (");
                    for (int i = 0; i < dt.Columns.Count; i++)
                    {
                        sbInsert.Append("'" + dr[i].ToString() + "'");
                       
                        if (i != dt.Columns.Count - 1)
                        {
                            sbInsert.Append(", ");
                        }
                    }
                    sbInsert.Append(')');

                    Console.WriteLine("\nInserting lines ...");
                    Console.WriteLine(sbInsert.ToString());
                    cmd = new OleDbCommand(sbInsert.ToString(), conn);
                    cmd.ExecuteNonQuery();
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }

            conn.Close();
        }

        public static bool ChangeColumnDataType(DataTable table, string columnname, Type newtype)
        {
            if (table.Columns.Contains(columnname) == false)
                return false;

            DataColumn column = table.Columns[columnname];
            if (column.DataType == newtype)
                return true;

            try
            {
                DataColumn newcolumn = new DataColumn("temporary", newtype);
                table.Columns.Add(newcolumn);

                foreach (DataRow row in table.Rows)
                {
                    try
                    {
                        row["temporary"] = Convert.ChangeType(row[columnname], newtype);
                    }
                    catch { }
                }

                newcolumn.SetOrdinal(0);
                table.Columns.Remove(columnname);

                if (columnname.Equals("Long") || columnname.Equals("long"))
                {
                    newcolumn.ColumnName = "Longitude";
                }
                else { newcolumn.ColumnName = columnname; }
            }
            catch (Exception)
            {
                return false;
            }

            return true;
        }
    }



}
