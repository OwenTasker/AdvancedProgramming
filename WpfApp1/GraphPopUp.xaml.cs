using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.Linq;
using System.Windows;
using System.IO;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media.Imaging;
using Interpreter;
using Microsoft.FSharp.Collections;

namespace WpfApp1
{
    /// <summary>
    /// Interaction and plotting logic for GraphPopUp.xaml
    /// References:
    /// Create image from array of pixel data: https://www.codeguru.com/dotnet/creating-images-from-scratch-in-c/
    /// Custom action on window close: https://docs.microsoft.com/en-us/dotnet/api/system.windows.window.closing?view=windowsdesktop-5.0
    /// Free BitmapImage for deletion: https://stackoverflow.com/questions/8352787/how-to-free-the-memory-after-the-bitmapimage-is-no-longer-needed
    /// Copy image file to save it: https://stackoverflow.com/questions/7462997/copy-file-to-a-different-directory
    /// </summary>
    
    public partial class GraphPopUp
    {
        //Constants for ease of readability
        private const int ImageWidth = 750;
        private const int ImageHeight = 400;
        private const int BytesPerPixel = 4;
        
        //Image ID tracking for temp saving
        private static int _imageId;
        private readonly int _thisImageId;
        
        //Has this graph been saved? true = no
        private bool _isDataDirty = true;

        //Array containing all pixels of graph
        private readonly byte[] _imageBuffer = new byte[ImageWidth * ImageHeight * BytesPerPixel];

        //List of all plotted functions (starts as just one)
        private List<string> _functions;
        
        //Copy of main execution environment for this session.
        private readonly IDictionary<string, FSharpList<Util.terminal>> _environment;
        
        /// <summary>
        /// Entry point, initializes the graph window, generates and displays graph.
        /// </summary>
        public GraphPopUp(string function, double[] xArray, double[] yArray, 
            IDictionary<string, FSharpList<Util.terminal>> environment)
        {
            //Image temp save tracking
            _thisImageId = _imageId;
            _imageId++;

            //Add initial function to list
            _functions = new List<string> {function};

            //Set up copy of execution environment
            _environment = environment;

            //Show window
            InitializeComponent();

            //Generate image of graph
            GenerateGraph(xArray, yArray);
        }
        
        /// <summary>
        /// Do everything required to plot/re-plot a graph.
        /// </summary>
        private void GenerateGraph(double[] xArray, double[] yArray)
        {
            //Set graph background to white and opacity to max
            for (var i = 0; i < _imageBuffer.Length; i++)
            {
                _imageBuffer[i] = 255;
            }
            
            //Set axis labels
            var yMax = yArray.Max();
            var yMin = yArray.Min();
            var xMax = xArray.Max();
            var xMin = xArray.Min();
            LabelYMax.Content = Math.Ceiling(yMax);
            LabelYMin.Content = Math.Floor(yMin);
            LabelXMax.Content = Math.Ceiling(xMax);
            LabelXMin.Content = Math.Floor(xMin);

            //Draw axis
            DrawAxis(xArray, yArray);
            
            //Generate line of a function
            GenerateLine(yArray);
            
            //Invert graph due to coordinate system
            InvertGraph();
            
            //Convert to image
            var path = Path.GetTempPath();
            unsafe //Required to use pointers
            {
                fixed (byte* ptr = _imageBuffer)
                {
                    using var image = new Bitmap(ImageWidth, ImageHeight, ImageWidth*BytesPerPixel, PixelFormat.Format32bppRgb, new IntPtr(ptr));
                    Directory.CreateDirectory(path + "MyMathsPal");
                    image.Save(path + "MyMathsPal\\graph" + _thisImageId + ".png" );
                }
            }
            
            //Display graph
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

        /// <summary>
        /// Method to mirror graph along x axis to correct for different coordinate systems.
        /// </summary>
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
        
        /// <summary>
        /// Method to plot a black pixel at (x,y)
        /// </summary>
        private void PlotPixel(int x, int y)
        {
            //Calculate starting byte of pixel
            var offset = ((ImageWidth * BytesPerPixel) * y) + (x * BytesPerPixel);
            
            //Set BGR to black
            _imageBuffer[offset] = _imageBuffer[offset + 1] = _imageBuffer[offset + 2] = 0;
        }

        /// <summary>
        /// Method to plot a graph from arrays of x and y values
        /// </summary>
        private void GenerateLine(double[] yArray)
        {
            //Scale y values to size of graph
            var yMin = yArray.Min();
            for (var i = 0; i < ImageWidth; i++)
            {
                yArray[i] -= yMin;
            }
            var yMax = yArray.Max();
            var scale = (ImageHeight - 1) / yMax;
            for (var i = 0; i < ImageWidth; i++)
            {
                yArray[i] *= scale;
            }
            
            //Plot line
            for (var i = 0; i < ImageWidth; i++)
            {
                PlotPixel(i, (int)yArray[i]);
            }
        }

        /// <summary>
        /// Method to draw x and y axis in correct locations
        /// </summary>
        private void DrawAxis(IReadOnlyCollection<double> xArray, IReadOnlyCollection<double> yArray)
        {
            //Find yArray index of y=0, default to below graph
            var yZero = 0;
            if (yArray.Min() > 0.0)
            {
                //Do nothing: yZero defaults to correct value
            }
            //y=0 is above graph
            else if (yArray.Max() < 0.0)
            {
                yZero = yArray.Count - 1;
            }
            //y=0 is within graph
            else
            {
                var yMin = yArray.Min();
                var yMax = yArray.Max();
                var range = yMax - yMin;
                var step = range / yArray.Count;
                for (var i = 0; i < yArray.Count; i++)
                {
                    //if y=0 exists exactly
                    if (yMin + (i * step) == 0)
                    {
                        yZero = i;
                        break;
                    }
                    //if y=0 is skipped, set to line to left
                    if (yMin + (i * step) > 0.0)
                    {
                        yZero = i - 1;
                        break;
                    }                                                                                                  
                }
            }

            //Scale y=0 line to image size
            var temp = yZero / (double)yArray.Count;
            temp *= ImageHeight;
            yZero = (int)temp;

            //Draw y=0 line
            for (var i = 0; i < ImageWidth; i++)
            {
                PlotPixel(i, yZero);
            }
            
            //Find xArray index of x=0, default to left of graph
            var xZero = 0;
            if (xArray.Min() > 0.0)
            {
                //Do nothing: xZero defaults to correct value
            }
            //x=0 is to right of graph
            else if (xArray.Max() < 0.0)
            {
                xZero = xArray.Count - 1;
            }
            //x=0 is within graph
            else
            {
                var xMin = xArray.Min();
                var xMax = xArray.Max();
                var range = xMax - xMin;
                var step = range / xArray.Count;
                for (var i = 0; i < xArray.Count; i++)
                {
                    //if x=0 exists exactly
                    if (xMin + (i * step) == 0)
                    {
                        xZero = i;
                        break;
                    }
                    //if x=0 is skipped, set to line to left
                    if (xMin + (i * step) > 0.0)
                    {
                        xZero = i - 1;
                        break;
                    }
                }
            }
            
            //Scale x=0 line to image size
            temp = xZero / (double)xArray.Count;
            temp *= ImageWidth;
            xZero = (int)temp;

            //Draw x=0 line
            for (var i = 0; i < ImageHeight; i ++)
            {
                PlotPixel(xZero, i);
            }
        }

        /// <summary>
        /// Method to check if user wants to save before closing if they haven't already
        /// </summary>
        private void GraphPopUp_Closing(object sender, CancelEventArgs e)
        {
            // If data is dirty, notify user and ask for a response
            if (!_isDataDirty) return;
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

        /// <summary>
        /// Save graph to location of user's choice
        /// </summary>
        private void SaveButton_OnClick(object sender, RoutedEventArgs e)
        {
            var fileToSaveTo = SaverLoader.DetermineFileToSaveTo("PNG Image (*.png)|*.png", "graph" + _thisImageId + ".png");
            
            //fileToSaveTo is null if user chooses cancel above
            if (fileToSaveTo != null)
            {
                //Delete file if one already exists at chosen location
                if (File.Exists(fileToSaveTo.FileName))
                {
                    File.Delete(fileToSaveTo.FileName);
                }
                
                var path = Path.GetTempPath();
                File.Copy(path + "MyMathsPal\\graph" + _thisImageId + ".png", fileToSaveTo.FileName);
                _isDataDirty = false;
            }
        }

        /// <summary>
        /// Re-plot graph with new x axis range.
        /// </summary>
        private void PlotButton_OnClick(object sender, RoutedEventArgs e)
        {
            //If xMin is invalid
            try
            {
                _ = Convert.ToDouble(TextBoxXMin.Text);
            }
            catch (Exception)
            {
                TextBoxXMin.Text = "";
                return;
            }
            
            //If xMax is invalid
            try
            {
                _ = Convert.ToDouble(TextBoxXMax.Text);
            }
            catch (Exception)
            {
                TextBoxXMax.Text = "";
                return;
            }
            
            //Check that xMin is smaller than xMax, swap if not
            if (Convert.ToDouble(TextBoxXMin.Text) > Convert.ToDouble(TextBoxXMax.Text))
            {
                (TextBoxXMin.Text, TextBoxXMax.Text) = (TextBoxXMax.Text, TextBoxXMin.Text);
            }
            
            //Check if xMin is the same as xMax. If true, do not re-plot
            if (TextBoxXMin.Text == TextBoxXMax.Text)
            {
                return;
            }
            
            try
            {
                var argsArray = new[]{_functions[0], TextBoxXMin.Text, TextBoxXMax.Text};
                
                var xArray = GraphDataCalculator.ComputeXArray(argsArray);
                var yArray = GraphDataCalculator.ComputeYArray(argsArray, xArray, _environment);
                    
                GenerateGraph(xArray, yArray);
            }
            catch (Exception plottingException)
            {
                Console.WriteLine("Plotting Exception: " + plottingException.Message + "\n" +
                                       plottingException.StackTrace + "\n>>");
            }
        }

        /// <summary>
        /// Ensure only numbers entered in xMin and xMax.
        /// </summary>
        private void xRange_TextChanged(object sender, TextChangedEventArgs e)
        {
            string input;
            
            //If xMin is modified
            if (sender.Equals(TextBoxXMin))
            {
                input = TextBoxXMin.Text;
            }
            //If xMax is modified
            else if (sender.Equals(TextBoxXMax))
            {
                input = TextBoxXMax.Text;
            }
            //This should never happen:
            else
            {
                Console.WriteLine("How did we get here?");
                return;
            }

            //Check if entry is a valid double, do not allow key-press if not
            if (input.Length > 0 && input != "-" && input != "-.")
            {
                try
                {
                    _ = Convert.ToDouble(input);
                }
                catch (Exception)
                {
                    //If xMin is invalid
                    if (sender.Equals(TextBoxXMin))
                    {
                        TextBoxXMin.Text = TextBoxXMin.Text[..^1];
                        TextBoxXMin.CaretIndex = int.MaxValue;
                    }
                    //If xMax is invalid
                    else if (sender.Equals(TextBoxXMax))
                    {
                        TextBoxXMax.Text = TextBoxXMax.Text[..^1];
                        TextBoxXMax.CaretIndex = int.MaxValue;
                    }
                    //This should never happen:
                    else
                    {
                        Console.WriteLine("How did we get here?");
                    }
                }
            }
        }

        /// <summary>
        /// Re-plot graph if enter key is pressed on xRange TextBoxes.
        /// </summary>
        private void xRange_EnterKeyClick(object sender, KeyEventArgs e)
        {
            if (e.Key != Key.Enter)
            {
                return;
            }
            
            PlotButton_OnClick(this, new RoutedEventArgs());
        }
    }
}