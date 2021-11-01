using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Linq;
using System.Windows;
using System.IO;
using System.Windows.Media.Imaging;

namespace WpfApp1
{
    
    //References:
    //https://www.codeguru.com/dotnet/creating-images-from-scratch-in-c/
    //
    
    public partial class GraphPopUp : Window
    {
        private const int ImageWidth = 750;
        private const int ImageHeight = 400;
        private const int BytesPerPixel = 4;
        
        private byte[] imageBuffer = new byte[1200000];
        
        public GraphPopUp(string value)
        {

            //Show window
            InitializeComponent();

            //Set graph background to white and opacity to max
            //Add basic x and y axis
            for (int i = 0; i < imageBuffer.Length; i+=4)
            {
                if (i % (ImageWidth * BytesPerPixel) != 0 && i > (ImageWidth * BytesPerPixel))
                {
                    imageBuffer[i] = imageBuffer[i+1] = imageBuffer[i+2] = imageBuffer[i+3] = 255;
                }
                else
                {
                    imageBuffer[i + 3] = 255;
                }
                
            }
            
            //Create test data
            double[] xArray = generateX();
            double[] yArray = generateY();
            
            //Generate image of graph
            GenerateGraph(xArray, yArray);

            //Display graph
            string path = Path.GetTempPath();
            ImageGraph.Source = new BitmapImage(new Uri(path + "graph.png"));
        }
        
        public GraphPopUp(int[] x, int[] y)
        {
            InitializeComponent();
        }

        private void InvertGraph()
        {
            for (int i = 0; i < ImageHeight / 2; i++)
            {
                byte[] temp = new byte[ImageWidth * BytesPerPixel];
                Array.Copy(imageBuffer, i * ImageWidth * BytesPerPixel, temp, 0, ImageWidth * BytesPerPixel);
                Array.Copy(imageBuffer, ImageHeight*ImageWidth*BytesPerPixel - i*ImageWidth*BytesPerPixel - ImageWidth*BytesPerPixel, imageBuffer, i * ImageWidth * BytesPerPixel, ImageWidth * BytesPerPixel);
                Array.Copy(temp, 0, imageBuffer, ImageHeight*ImageWidth*BytesPerPixel - i*ImageWidth*BytesPerPixel - ImageWidth*BytesPerPixel, ImageWidth * BytesPerPixel);
            }
        }
        
        private void PlotPixel(int x, int y)
        {
            //Calculate starting byte of pixel
            int offset = ((ImageWidth * BytesPerPixel) * y) + (x * BytesPerPixel);
            Console.WriteLine(offset + "");
            //Set BGR to black
            imageBuffer[offset] = imageBuffer[offset + 1] = imageBuffer[offset + 2] = 0;
        }

        private void GenerateGraph(double[] xArray, double[] yArray)
        {
            //Set axis labels
            double yMax = yArray.Max();
            double yMin = yArray.Min();
            double xMax = xArray.Max();
            double xMin = xArray.Min();

            LabelYMax.Content = Math.Round(yMax);
            LabelYMin.Content = Math.Round(yMin);
            LabelXMax.Content = Math.Round(xMax);
            LabelXMin.Content = Math.Round(xMin);
            
            //Scale y values to size of graph
            for (int i = 0; i < ImageWidth; i++)
            {
                yArray[i] -= yMin;
            }
            
            yMax = yArray.Max();

            double scale = (ImageHeight - 1) / yMax;

            for (int i = 0; i < ImageWidth; i++)
            {
                yArray[i] *= scale;
            }
            
            //Plot graph
            for (int i = 0; i < ImageWidth; i++)
            {
                PlotPixel(i, (int)yArray[i]);
            }
            
            //Invert graph due to coordinate system
            InvertGraph();
            
            //Convert to image
            unsafe
            {
                fixed (byte* ptr = imageBuffer)
                {
                    using (Bitmap image = new Bitmap(ImageWidth, ImageHeight, ImageWidth*BytesPerPixel, PixelFormat.Format32bppRgb, new IntPtr(ptr)))
                    {
                        string path = Path.GetTempPath();
                        image.Save(path + "graph.png");
                    }
                }
            }
            
        }

        private double[] generateX()
        {
            double[] xArray = new double[ImageWidth];

            for (int i = 0; i < ImageWidth; i++)
            {
                xArray[i] = i;
            }

            return xArray;
        }
        
        private double[] generateY()
        {
            double[] yArray = new double[ImageWidth];

            for (int i = 0; i < ImageWidth; i++)
            {
                yArray[i] = Math.Sin((double)i/100);
            }

            return yArray;
        }
        
    }
    
    //Array of x values from min to max with step ((max - min) / 750)
    //Array of y values with whatever range is calculated (length 750)
    //Find min and max of y values
    //Subtract y min from all y values
    //Scale all y values such that y max = 400
    //For (i=0;i<750;i++) set correct y pixel to black
    //Save image
    //Display image
    
}