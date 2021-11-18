using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.Linq;
using System.Windows;
using System.IO;
using System.Windows.Media.Imaging;
using JetBrains.Annotations;

namespace WpfApp1
{
    
    // References:
    // Create image from array of pixel data: https://www.codeguru.com/dotnet/creating-images-from-scratch-in-c/
    // Custom action on window close: https://docs.microsoft.com/en-us/dotnet/api/system.windows.window.closing?view=windowsdesktop-5.0
    // Free BitmapImage for deletion: https://stackoverflow.com/questions/8352787/how-to-free-the-memory-after-the-bitmapimage-is-no-longer-needed
    
    public partial class GraphPopUp
    {
        private const int ImageWidth = 750;
        private const int ImageHeight = 400;
        private const int BytesPerPixel = 4;
        private static int _imageId;
        private readonly int _thisImageId;
        private bool isDataDirty = true;

        private readonly byte[] _imageBuffer = new byte[1200000];
        
        public GraphPopUp(double[] x, double[] y)
        {
            _thisImageId = _imageId;
            _imageId++;
            //Show window
            InitializeComponent();

            //Set graph background to white and opacity to max
            for (var i = 0; i < _imageBuffer.Length; i++)
            {
                _imageBuffer[i] = 255;
            }

            //Generate image of graph
            GenerateGraph(x, y);
        
            //Display graph
            var path = Path.GetTempPath();
            
            var stream = File.OpenRead(path + "MyMathsPal\\graph" + _thisImageId + ".png");

            var graph = new BitmapImage();
            graph.BeginInit();
            graph.CacheOption = BitmapCacheOption.OnLoad;
            graph.StreamSource = stream;
            graph.EndInit();
            ImageGraph.Source = graph;
            stream.Close();
            stream.Dispose();
        }

        private void InvertGraph()
        {
            for (var i = 0; i < ImageHeight / 2; i++)
            {
                var temp = new byte[ImageWidth * BytesPerPixel];
                Array.Copy(_imageBuffer, i * ImageWidth * BytesPerPixel, temp, 0, ImageWidth * BytesPerPixel);
                Array.Copy(_imageBuffer, ImageHeight*ImageWidth*BytesPerPixel - i*ImageWidth*BytesPerPixel - ImageWidth*BytesPerPixel, _imageBuffer, i * ImageWidth * BytesPerPixel, ImageWidth * BytesPerPixel);
                Array.Copy(temp, 0, _imageBuffer, ImageHeight*ImageWidth*BytesPerPixel - i*ImageWidth*BytesPerPixel - ImageWidth*BytesPerPixel, ImageWidth * BytesPerPixel);
            }
        }
        
        private void PlotPixel(int x, int y)
        {
            //Calculate starting byte of pixel
            Console.WriteLine(ImageWidth + " * " + BytesPerPixel + " * " + y + " + " + x + " * " + BytesPerPixel);
            var offset = ((ImageWidth * BytesPerPixel) * y) + (x * BytesPerPixel);
            //Set BGR to black
            Console.WriteLine(offset);
            _imageBuffer[offset] = _imageBuffer[offset + 1] = _imageBuffer[offset + 2] = 0;
        }

        private void GenerateGraph(double[] xArray, double[] yArray)
        {
            //Set axis labels
            var yMax = yArray.Max();
            var yMin = yArray.Min();
            var xMax = xArray.Max();
            var xMin = xArray.Min();

            LabelYMax.Content = Math.Round(yMax);
            LabelYMin.Content = Math.Round(yMin);
            LabelXMax.Content = Math.Round(xMax);
            LabelXMin.Content = Math.Round(xMin);

            //Draw axis
            DrawAxis(xArray, yArray);
            
            //Scale y values to size of graph
            for (var i = 0; i < ImageWidth; i++)
            {
                yArray[i] -= yMin;
            }
            
            yMax = yArray.Max();

            var scale = (ImageHeight - 1) / yMax;

            for (var i = 0; i < ImageWidth; i++)
            {
                yArray[i] *= scale;
            }
            
            //Plot graph
            for (var i = 0; i < ImageWidth; i++)
            {
                PlotPixel(i, (int)yArray[i]);
            }
            
            //Invert graph due to coordinate system
            InvertGraph();
            
            //Convert to image
            var path = Path.GetTempPath();
            unsafe
            {
                fixed (byte* ptr = _imageBuffer)
                {
                    using var image = new Bitmap(ImageWidth, ImageHeight, ImageWidth*BytesPerPixel, PixelFormat.Format32bppRgb, new IntPtr(ptr));
                    Directory.CreateDirectory(path + "MyMathsPal");
                    image.Save(path + "MyMathsPal\\graph" + _thisImageId + ".png" );
                }
            }
            
        }

        private void DrawAxis([NotNull] double[] xArray, [NotNull] double[] yArray)
        {
            //Find yArray index of y=0, default to below graph
            var yZero = 0;
            //y=0 is above graph
            if (yArray[749] < 0.0)
            {
                yZero = 479;
            }
            //y=0 is within graph
            else
            {
                for (var i = 0; i < 750; i++)
                {
                    Console.WriteLine(yArray[i]);
                    //if y=0 exists exactly
                    if (yArray[i] == 0.0)
                    {
                        Console.WriteLine("0 found");
                        yZero = i;
                        break;
                    }
                    //if y=0 is skipped, set to line below
                    if (!(yArray[i] > 0.0)) continue;
                    //Console.WriteLine("0 skipped");
                    yZero = i - 1;
                    break;
                }
            }
            
            Console.WriteLine("yZero: " + yZero);
            
            //Scale y=0 line to image size
            var temp = yZero / 750.0;
            temp *= ImageHeight;
            yZero = (int)temp;

            //Draw y=0 line
            var offset = yZero * ImageWidth * BytesPerPixel;
            for (var i = 0; i < ImageWidth * BytesPerPixel; i += 4)
            {
                _imageBuffer[i + offset] = _imageBuffer[i + 1 + offset] = _imageBuffer[i + 2 + offset] = 0;
            }
            
            //Find xArray index of x=0, default to left of graph
            var xZero = 0;
            
            if (xArray[0] > 0.0)
            {
                //nothing
            }
            //x=0 is to right of graph
            else if (xArray[749] < 0.0)
            {
                xZero = 479;
            }
            //x=0 is within graph
            else
            {
                for (var i = 0; i < 750; i++)
                {
                    //if x=0 exists exactly
                    if (xArray[i] == 0.0)
                    {
                        xZero = i;
                        break;
                    }
                    //if x=0 is skipped, set to line to left
                    if (!(xArray[i] > 0.0)) continue;
                    xZero = i - 1;
                    break;
                }
            }
            
            Console.WriteLine("xZero: " + xZero);
            
            //Scale x=0 line to image size
            temp = xZero / 750.0;
            temp *= ImageWidth;
            xZero = (int)temp;

            //Draw x=0 line
            for (var i = 0; i < ImageHeight; i ++)
            {
                PlotPixel(xZero, i);
            }
        }

        private void GraphPopUp_Closing(object sender, CancelEventArgs e)
        {
            // If data is dirty, notify user and ask for a response
            if (!isDataDirty) return;
            const string msg = "Close graph without saving?";
            var result = 
                MessageBox.Show(
                    msg, 
                    "MyMathsPal", 
                    MessageBoxButton.YesNo, 
                    MessageBoxImage.Warning);
            if (result == MessageBoxResult.No)
            {
                // If user doesn't want to close, cancel closure
                e.Cancel = true;
            }
            else
            {
                //Delete temp image of graph before closing
                ImageGraph.Source = new BitmapImage(new Uri("Images/graph.png", UriKind.Relative));
                var path = Path.GetTempPath();
                File.Delete(path + "MyMathsPal\\graph" + _thisImageId + ".png");
            }
        }
        
    }

}