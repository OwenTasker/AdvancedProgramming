using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Globalization;
using System.Linq;
using System.Windows;
using System.IO;
using System.Windows.Controls;
using System.Windows.Ink;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using Microsoft.Win32;
using Brushes = System.Drawing.Brushes;
using FontFamily = System.Windows.Media.FontFamily;
using Image = System.Drawing.Image;
using PixelFormat = System.Drawing.Imaging.PixelFormat;
using Rectangle = System.Windows.Shapes.Rectangle;

namespace WpfApp1
{
    /// <summary>
    /// Interaction and plotting logic for GraphPopUp.xaml
    /// References:
    /// Create image from array of pixel data: https://www.codeguru.com/dotnet/creating-images-from-scratch-in-c/
    /// Custom action on window close: https://docs.microsoft.com/en-us/dotnet/api/system.windows.window.closing?view=windowsdesktop-5.0
    /// Free BitmapImage for deletion: https://stackoverflow.com/questions/8352787/how-to-free-the-memory-after-the-bitmapimage-is-no-longer-needed
    /// Copy image file to save it: https://stackoverflow.com/questions/7462997/copy-file-to-a-different-directory
    /// Overlay text on image: https://stackoverflow.com/questions/6826921/write-text-on-an-image-in-c-sharp
    /// Get mouse coordinates: https://getandplay.github.io/2019/05/13/How-does-WPF-application-get-mouse-position-when-mouse-stay-outside-window/
    /// </summary>
    public partial class GraphPopUp : IGraphPopUp
    {
        //Constants for ease of readability
        private const int ImageWidth = 750;
        private const int ImageHeight = 400;
        private const int BytesPerPixel = 4;

        //Image ID tracking for temp saving
        private static int _imageId;
        private readonly int _thisImageId = _imageId++;

        //Has this graph been saved? true = no
        private bool _isDataDirty;

        //Array containing all pixels of graph
        private readonly byte[] _imageBuffer = new byte[ImageWidth * ImageHeight * BytesPerPixel];

        //Lists of all plotted functions, their arrays, and key axis locations (starts as just one)
        private readonly List<(string, (double[], double[]))> _functions = new();
        private readonly List<(int, string)> _yProcessed = new();

        //Create cursor for hovering
        private readonly Label _cursor = new();

        //Int to store location of click for zooming
        private int _mouseDownXCoord;
        private int _mouseDownYCoord;

        //Rectangle to be used for selection of zoom area
        private readonly Rectangle _selection = new();

        //boolean of whether mouse is currently held
        private bool _mouseDown;

        //Save time graph was last generated - prevents ghost double mouse releases causing graph to rapidly zoom in
        private long _timeLastGenerated;

        private readonly IGraphDataCalculator _graphDataCalculator;

        /// <summary>
        /// Entry point, initializes the graph window, generates and displays graph.
        /// </summary>
        public GraphPopUp(IGraphDataCalculator graphDataCalculator)
        {
            //Set data to unchanged as none is generated yet
            _isDataDirty = false;

            _graphDataCalculator = graphDataCalculator;

            //Show window
            InitializeComponent();

            Show();
        }

        /// <summary>
        /// Do everything required to plot/re-plot a graph.
        /// </summary>
        public void GenerateGraph(string input)
        {
            //Save time of generation
            _timeLastGenerated = DateTimeOffset.Now.ToUnixTimeSeconds();

            var (xArray, yArray, command) = _graphDataCalculator.GetXyArrays(input);

            if (xArray.Max() - xArray.Min() > 100000000000)
            {
                throw new ArgumentException(
                    "Graphing Error: The range given by the x bounds is too large to plot.");
            }
            if (yArray.Max() - yArray.Min() > 100000000000)
            {
                throw new ArgumentException(
                    "Graphing Error: Computing this function within the given x bounds has resulted in a y axis range too large to plot.");
            }

            //Add initial function and its number arrays to list
            _functions.Add((command,((double[]) xArray.Clone(), (double[]) yArray.Clone())));

            //Pre-fill x range boxes
            TextBoxXMin.Text = "" + Math.Round(xArray.Min(), 2);
            TextBoxXMax.Text = "" + Math.Round(xArray.Max(), 2);

            //Set graph background to white and opacity to max
            for (var i = 0; i < _imageBuffer.Length; i++)
            {
                _imageBuffer[i] = 255;
            }

            //Add some padding to top and bottom of graph
            var yMin = yArray[0];
            var yMax = yArray[^1];
            var yMinIndex = 0;
            var yMaxIndex = yArray.Length - 1;
            for (var i = 0; i < yArray.Length; i++)
            {
                if (yArray[i] < yMin)
                {
                    yMin = yArray[i];
                    yMinIndex = i;
                }
                else if (yArray[i] > yMax)
                {
                    yMax = yArray[i];
                    yMaxIndex = i;
                }
            }
            yArray[yMinIndex] -= 2;
            yArray[yMaxIndex] += 2;

            //Draw axis and get axis positions
            int yZero;
            int xZero;
            int yZeroUnPadded;
            ((yZero, xZero), yZeroUnPadded) = DrawAxis(xArray, yArray);
            _yProcessed.Add((yZeroUnPadded, "functionRight"));
            
            double xGridStep;
            double yGridStep;

            //Get steps for grid lines
            (xGridStep, yGridStep) = CalculateGridStep(xArray, yArray);

            //Generate grid lines and get labels - y grid lines are parallel with y axis, xZero
            var labels = GenerateGridLines(xZero, yGridStep, xArray, yZero, yZeroUnPadded, xGridStep, yArray);

            //Generate line of a function
            //.Clone() to avoid accidental pass-by-reference
            GenerateLine((double[]) yArray.Clone(), yZeroUnPadded);

            //Invert graph due to coordinate system
            InvertGraph();

            //Convert to image
            var path = Path.GetTempPath();
            unsafe //Required to use pointers
            {
                fixed (byte* ptr = _imageBuffer)
                {
                    using var image = new Bitmap(ImageWidth, ImageHeight, ImageWidth * BytesPerPixel,
                        PixelFormat.Format32bppRgb, new IntPtr(ptr));
                    Directory.CreateDirectory(path + "MyMathsPal");
                    image.Save(path + "MyMathsPal\\graph" + _thisImageId + ".png");
                }
            }

            //Add axis labels to graph
            AddLabels(yZero, xZero, labels);

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

            //Add mouse tracking for displaying coordinates
            CompositionTarget.Rendering += OnRendering;

            //Set up cursor for hovering
            _cursor.Content = "x";
            _cursor.FontFamily = new FontFamily("Courier New");
            _cursor.FontSize = 14;
            _cursor.Foreground = System.Windows.Media.Brushes.Red;
            _cursor.Opacity = 0;
            //Add mouse event handlers to label as is appears in front of and blocks the graph image
            _cursor.MouseLeftButtonDown += ImageGraph_OnMouseLeftButtonDown;
            _cursor.MouseLeftButtonUp += ImageGraph_OnMouseLeftButtonUp;
            _cursor.MouseRightButtonDown += ImageGraph_OnMouseRightButtonDown;
            mainGrid.Children.Add(_cursor);

            //Set up selection rectangle for zooming
            var colour = new System.Windows.Media.Color
            {
                R = 0,
                G = 118,
                B = 215,
                A = 127
            };
            var brush = new SolidColorBrush(colour);
            _selection.Fill = brush;
            _selection.Opacity = 0;
            colour = new System.Windows.Media.Color
            {
                R = 0,
                G = 84,
                B = 153,
                A = 127
            };
            _selection.Stroke = new SolidColorBrush(colour);
            _selection.StrokeThickness = 2;
            _selection.VerticalAlignment = VerticalAlignment.Top;
            _selection.HorizontalAlignment = HorizontalAlignment.Left;
            _selection.MouseLeftButtonDown += ImageGraph_OnMouseLeftButtonDown;
            _selection.MouseLeftButtonUp += ImageGraph_OnMouseLeftButtonUp;
            _selection.Visibility = Visibility.Visible;

            // mark graph as unsaved
            _isDataDirty = true;

            // set graph title to function plotted
            var (function, _) = _functions.Last();
            functionLabel.Content = function;
        }

        /// <summary>
        /// Method to plot grid lines on the graph.
        /// </summary>
        private List<(PointF, string)> GenerateGridLines(int yAxisXCoord, double yGridStep, double[] xArray, int xAxisYCoord,
            int xAxisYCoordUnPadded, double xGridStep, double[] yArray)
        {
            var labels = new List<(PointF, string)>();

            //Get number of pixels per number along the x axis
            var xRange = xArray.Max() - xArray.Min();
            var pixelsPerXNumber = ImageWidth / xRange;

            //Adjust coordinates of x axis labels up/down depending on axis location
            var labelAbove = xAxisYCoord < ImageHeight - xAxisYCoord;
            var xTop = ImageHeight - xAxisYCoord + 30;
            if (labelAbove)
            {
                xTop -= 17;
            }

            //Space out grid lines if they would be too close together
            var multiplier = 1;
            if (pixelsPerXNumber*yGridStep < 25)
            {
                multiplier = 4;
            }

            //Plot vertical grid lines to right of y axis
            for (double i = yAxisXCoord; i < ImageWidth; i += yGridStep * pixelsPerXNumber*multiplier)
            {
                if ((int)Math.Round(i) != yAxisXCoord)
                {
                    for (var j = 0; j < ImageHeight; j++)
                    {
                        if (j != xAxisYCoord)
                        {
                            PlotPixel((int)Math.Round(i), j, 127, 127, 127);
                        }
                    }
                }
            }

            //Generate x axis labels to right of y axis
            var value = yGridStep*multiplier;
            if (xArray.Min() > 0)
            {
                value += xArray.Min();
            }
            var valueFormat = "{0:0.#}";
            if (value < 1)
            {
                valueFormat = "{0:0.0}";
            }
            for (double i = yAxisXCoord; i <= ImageWidth; i += yGridStep * pixelsPerXNumber * multiplier)
            {
                if ((int)Math.Round(i) != yAxisXCoord)
                {
                    //Calculate location and content
                    var offset = string.Format(valueFormat, value).Length * 9;
                    labels.Add((new PointF((int) i - 2 - offset, xTop - 30), string.Format(valueFormat, value)));

                    value += yGridStep*multiplier;
                }

            }

            //Plot vertical grid lines to left of y axis
            for (double i = yAxisXCoord; i > 0; i -= yGridStep * pixelsPerXNumber*multiplier)
            {
                if ((int)Math.Round(i) != yAxisXCoord)
                {
                    for (var j = 0; j < ImageHeight; j++)
                    {
                        if (j != xAxisYCoord)
                        {
                            PlotPixel((int)Math.Round(i), j, 127, 127, 127);
                        }
                    }
                }
            }

            //Generate x axis labels to left of y axis
            value = -yGridStep*multiplier;
            if (xArray.Max() < 0)
            {
                value += xArray.Max();
            }
            for (double i = yAxisXCoord; i >= 0; i -= yGridStep * pixelsPerXNumber*multiplier)
            {
                if ((int)Math.Round(i) != yAxisXCoord)
                {
                    //Calculate location and content
                    labels.Add((new PointF((int) i - 1, xTop - 30), string.Format(valueFormat, value)));
                    
                    value -= yGridStep*multiplier;
                }
            }

            //Get number of pixels per number along the y axis
            var yRange = yArray.Max() - yArray.Min();
            //Account for padding that is sometimes added to top and bottom of y axis
            var dataRangeHeight = ImageHeight;
            if (xAxisYCoord != xAxisYCoordUnPadded)
            {
                dataRangeHeight -= 40;
            }
            var pixelsPerYNumber = dataRangeHeight / yRange;

            //Adjust coordinates of y axis labels left/right depending on axis location
            var labelRight = yAxisXCoord < (ImageWidth - yAxisXCoord);
            var yLeft = yAxisXCoord - 50 - 1;
            if (labelRight)
            {
                yLeft += 15;
            }

            //Space out grid lines if they would be too close together
            multiplier = 1;
            if (pixelsPerYNumber*xGridStep < 25)
            {
                multiplier = 4;
            }

            //Plot horizontal grid lines above y axis
            for (double i = xAxisYCoord; i < ImageHeight; i += xGridStep * pixelsPerYNumber* multiplier)
            {
                if ((int)Math.Round(i) != xAxisYCoord)
                {
                    for (var j = 0; j < ImageWidth; j++)
                    {
                        if (j != yAxisXCoord)
                        {
                            PlotPixel(j, (int)Math.Round(i), 127, 127, 127);
                        }
                    }
                }
            }

            //Generate y axis labels above x axis
            value = xGridStep * multiplier;
            if (yArray.Min() > 0)
            {
                value += yArray.Min();
            }
            else if (yArray.Max() < 0)
            {
                value = yArray.Max();
            }
            valueFormat = "{0:0.#}";
            if (Math.Abs(value) < 1)
            {
                valueFormat = "{0:0.0}";
            }
            for (double i = xAxisYCoord; i < ImageHeight; i += xGridStep * pixelsPerYNumber*multiplier)
            {
                if ((int)Math.Round(i) != xAxisYCoord)
                {
                    //Calculate location and content
                    var offset = 9;
                    if (!labelRight)
                    {
                        offset = string.Format(valueFormat, value).Length * 9;
                    }
                    labels.Add((new PointF(yLeft + 47 - offset, ImageHeight - (int) i), string.Format(valueFormat, value)));
                    
                    value += xGridStep*multiplier;
                }
            }

            //Plot horizontal grid lines below y axis
            for (double i = xAxisYCoord; i > 0; i -= xGridStep * pixelsPerYNumber*multiplier)
            {
                if ((int)Math.Round(i) != xAxisYCoord)
                {
                    for (var j = 0; j < ImageWidth; j++)
                    {
                        if (j != yAxisXCoord)
                        {
                            PlotPixel(j, (int)Math.Round(i), 127, 127, 127);
                        }
                    }
                }
            }
            
            //Generate y axis labels below x axis
            value = -xGridStep*multiplier;
            if (yArray.Max() < 0)
            {
                value += yArray.Max();
            }
            else if (yArray.Min() > 0)
            {
                value += yArray.Min() + xGridStep*multiplier;
            }
            for (double i = xAxisYCoord; i > 0; i -= xGridStep * pixelsPerYNumber*multiplier)
            {
                if ((int)Math.Round(i) != xAxisYCoord)
                {
                    //Calculate location and content
                    var offset = 9;
                    if (!labelRight)
                    {
                        offset = string.Format(valueFormat, value).Length * 9;
                    }
                    labels.Add((new PointF(yLeft + 47 - offset, ImageHeight - (int) i - 19), string.Format(valueFormat, value)));
                    
                    value -= xGridStep*multiplier;
                }
            }

            return labels;
        }

        /// <summary>
        /// Method to calculate step of grid lines for both x and y axis.
        /// </summary>
        private (double xGridStep, double yGridStep) CalculateGridStep(double[] xArray, double[] yArray)
        {
            double xGridStep;
            double yGridStep;

            //X grid line step
            var yRange = yArray.Max() - yArray.Min();
            switch (yRange)
            {
                //Special case for range <= 1
                case <= 1:
                    xGridStep = 0.1;
                    break;
                //Special case for range <= 10
                case <= 10:
                    xGridStep = 1;
                    break;
                //Default step is a magnitude smaller than range
                default:
                {
                    var yRangeString = ((int) Math.Ceiling(yRange)).ToString();
                    var yRangeStringLength = yRangeString.Length - 1;
                    xGridStep = Math.Pow(10, yRangeStringLength) / 5;
                    break;
                }
            }

            //Y grid line step
            var xRange = xArray.Max() - xArray.Min();
            switch (xRange)
            {
                //Special case for range <= 1
                case <= 1:
                    yGridStep = 0.1;
                    break;
                //Special case for range <= 10
                case <= 10:
                    yGridStep = 1;
                    break;
                //Default step is a magnitude smaller than range
                default:
                {
                    var xRangeString = ((int) Math.Ceiling(xRange)).ToString();
                    var xRangeStringLength = xRangeString.Length - 1;
                    yGridStep = Math.Pow(10, xRangeStringLength) / 5;
                    break;
                }
            }

            return (xGridStep, yGridStep);
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
                Array.Copy(_imageBuffer,
                    ImageHeight * ImageWidth * BytesPerPixel - i * ImageWidth * BytesPerPixel -
                    ImageWidth * BytesPerPixel, _imageBuffer, i * ImageWidth * BytesPerPixel,
                    ImageWidth * BytesPerPixel);
                Array.Copy(temp, 0, _imageBuffer,
                    ImageHeight * ImageWidth * BytesPerPixel - i * ImageWidth * BytesPerPixel -
                    ImageWidth * BytesPerPixel, ImageWidth * BytesPerPixel);
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
        /// Method to plot a coloured pixel at (x,y)
        /// </summary>
        private void PlotPixel(int x, int y, int red, int green, int blue)
        {
            if (y == 400)
            {
                y--;
            }
            if (y == -1)
            {
                y++;
            }
            if (x == 750)
            {
                x--;
            }
            if (x == -1)
            {
                x++;
            }
            //Calculate starting byte of pixel
            var offset = ((ImageWidth * BytesPerPixel) * y) + (x * BytesPerPixel);

            //Set BGR to black
            _imageBuffer[offset] = (byte) blue;
            _imageBuffer[offset + 1] = (byte) green;
            _imageBuffer[offset + 2] = (byte) red;
        }

        /// <summary>
        /// Method to add labels to image
        /// </summary>
        private void AddLabels(int yZero, int xZero, List<(PointF, string)> labels)
        {
            //This method uses inverted y axis

            //Create zero label
            var zeroLabel = "0";

            //Find zero label location:
            var zeroPointX = xZero;
            var zeroPointY = ImageHeight - yZero;
            if (xZero > ImageWidth / 2 - 1)
            {
                zeroPointX -= zeroLabel.Length * 9 + 5;
            }
            if (yZero < ImageHeight / 2)
            {
                zeroPointY -= 18;
            }
            var zeroPoint = new PointF(zeroPointX, zeroPointY);

            //Don't draw zero label if it will overlap
            if (yZero is < 10 or > ImageHeight - 10 || xZero is < 10 or > ImageWidth - 10)
            {
                zeroLabel = "";
            }

            //Draw labels in graph
            Bitmap newBitmap;
            var path = Path.GetTempPath();
            using (var bitmap = (Bitmap) Image.FromFile(path + "MyMathsPal\\graph" + _thisImageId + ".png"))
            {
                using (var graphics = Graphics.FromImage(bitmap))
                {
                    using (var font = new Font("Courier New", 14, GraphicsUnit.Pixel))
                    {
                        //Draw zero label
                        graphics.DrawString(zeroLabel, font, Brushes.Black, zeroPoint);
                        
                        //Draw all other axis labels
                        foreach (var label in labels)
                        {
                            var (point, labelText) = label;
                            graphics.DrawString(labelText, font, Brushes.Black, point);
                        }
                    }
                }

                newBitmap = new Bitmap(bitmap);
            }

            newBitmap.Save(path + "MyMathsPal\\graph" + _thisImageId + ".png");
            newBitmap.Dispose();
        }

        /// <summary>
        /// Method to plot a graph from arrays of x and y values
        /// </summary>
        private void GenerateLine(IList<double> yArray, int yZero)
        {
            //Scale y values to size of graph
            var yMin = yArray.Min();
            for (var i = 0; i < ImageWidth; i++)
            {
                yArray[i] -= yMin;
            }
            var yMax = yArray.Max();
            double scale;
            //Add some padding if x axis is near edge of screen
            if (yZero is < 20 or > ImageHeight - 20)
            {
                scale = (ImageHeight - 41) / yMax;
            }
            else
            {
                scale = (ImageHeight - 1) / yMax;
            }
            for (var i = 0; i < ImageWidth; i++)
            {
                yArray[i] *= scale;
                if (yZero is < 20 or > ImageHeight - 20)
                {
                    yArray[i] += 20;
                }
            }

            //Plot line
            for (var i = 0; i < ImageWidth; i++)
            {
                //Plot line
                PlotPixel(i, (int) yArray[i], 53, 179, 242);

                //Plot pixel above line to make it thicker
                if ((int) yArray[i] < ImageHeight - 2)
                {
                    PlotPixel(i, (int) yArray[i] + 1, 53, 179, 242);
                }
            }
        }

        /// <summary>
        /// Method to draw x and y axis in correct locations
        /// </summary>
        private ((int yZero, int xZero), int yZeroUnPadded) DrawAxis(IReadOnlyCollection<double> xArray, IReadOnlyCollection<double> yArray)
        {
            //Find yArray index of y=0, x axis, default to below graph
            var yZero = 0;

            //y=0 is above graph
            if (yArray.Max() <= 0.0)
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
            var temp = yZero / (double) yArray.Count;
            temp *= ImageHeight;
            yZero = (int) temp;

            //Pad yZero if it would be too close to edge of screen
            var yZeroUnPadded = yZero;
            if (yZero < 20)
            {
                yZero += 20;
            }
            else if (yZero > ImageHeight - 20)
            {
                yZero -= 20;
            }

            //Draw y=0 line
            for (var i = 0; i < ImageWidth; i++)
            {
                PlotPixel(i, yZero);
            }

            //Find xArray index of x=0, y axis, default to left of graph
            var xZero = 0;
            if (xArray.Min() > 0.0)
            {
                //Do nothing: xZero defaults to correct value
            }
            //x=0 is to right of graph
            else if (xArray.Max() <= 0.0)
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
                    if (xMin + i * step == 0)
                    {
                        xZero = i;
                        break;
                    }

                    //if x=0 is skipped, set to line to left
                    if (xMin + i * step > 0.0)
                    {
                        xZero = i - 1;
                        break;
                    }
                }
            }

            //Scale x=0 line to image size
            temp = xZero / (double) xArray.Count;
            temp *= ImageWidth;
            xZero = (int) temp;

            //Draw x=0 line
            for (var i = 0; i < ImageHeight; i++)
            {
                PlotPixel(xZero, i);
            }

            return ((yZero, xZero), yZeroUnPadded);
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
                //Remove mouse tracking for displaying coordinates
                CompositionTarget.Rendering -= OnRendering;

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
            var (function, _) = _functions.Last();

            SaveFileDialog fileToSaveTo = null; // Initialise

            // Check for forbidden characters in Windows filenames
            if (function.Contains("*"))
            {
                function = function.Replace("*", "_times_");
            }
            if (function.Contains("/"))
            {
                function = function.Replace("/", "_divided_by_");

            }

            // Prepare file to save to
            fileToSaveTo =
                SaverLoader.DetermineFileToSaveTo("PNG Image (*.png)|*.png", "graph_" + function[3..] + ".png");


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

            //Check if new x range is less than 0.5. If true, do not re-plot
            if (Convert.ToDouble(TextBoxXMax.Text) - Convert.ToDouble(TextBoxXMin.Text) < 0.5)
            {
                return;
            }

            //Remove all WPF elements except title and graph before redrawing
            mainGrid.Children.RemoveRange(2,mainGrid.Children.Count-1);

            var (function, (_, _)) = _functions.Last();
            GenerateGraph("plot(" + function + "," + TextBoxXMin.Text + "," + TextBoxXMax.Text + ")");

            //Set data as having been modified since last save
            _isDataDirty = true;
        }

        /// <summary>
        /// Ensure only numbers entered in xMin and xMax.
        /// </summary>
        private void xRange_TextChanged(object sender, TextChangedEventArgs e)
        {
            //If xMin is modified
            var input = sender.Equals(TextBoxXMin) ? TextBoxXMin.Text : TextBoxXMax.Text;

            //Check if entry is a valid double, do not allow key-press if not
            if (input.Length <= 0 || input is "-" or "." or "-.") return;
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
                else
                {
                    TextBoxXMax.Text = TextBoxXMax.Text[..^1];
                    TextBoxXMax.CaretIndex = int.MaxValue;
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

        /// <summary>
        /// Display mouse coordinates on graph in boxes at top of screen.
        /// </summary>
        private void OnRendering(object sender, EventArgs e)
        {
            //Remove cursor added in previous frame
            mainGrid.Children.Remove(_cursor);

            //Get mouse coordinates relative to graph
            var coords = Mouse.GetPosition(ImageGraph);
            var xCoord = Math.Floor(coords.X);
            var yCoord = Math.Floor(coords.Y);

            //Calculate coordinates and display cursor if mouse over graph
            if (xCoord is >= 0 and <= 749 && yCoord is >= 0 and <= 399)
            {
                //Get data for line cursor will be on
                var (_, (xArray, yArray)) = _functions.Last();
                var (yZero, _) = _yProcessed.Last();

                //Set coordinate labels
                var xCoordText = Math.Round(xArray[(int) xCoord], 2).ToString(CultureInfo.InvariantCulture);
                var yCoordText = Math.Round(yArray[(int) xCoord], 2).ToString(CultureInfo.InvariantCulture);
                TextBoxXCoord.Text = xCoordText;
                TextBoxYCoord.Text = yCoordText;

                //Create clone of y array to manipulate to calculate cursor position
                var yArrayClone = (double[]) yArray.Clone();

                //Save minimum value of y before any further processing
                var yMin = yArrayClone.Min() - 2;

                //Start of y array scaling
                for (var i = 0; i < ImageWidth; i++)
                {
                    yArrayClone[i] -= yMin;
                }
                var yMax = yArrayClone.Max() + 2;

                //Continuation of scaling y values to size of graph
                double scale;
                //Add some padding if x axis is near edge of screen
                if (yZero is < 20 or > ImageHeight - 20)
                {
                    scale = (ImageHeight - 41) / yMax;
                }
                else
                {
                    scale = (ImageHeight - 1) / yMax;
                }
                for (var i = 0; i < ImageWidth; i++)
                {
                    yArrayClone[i] *= scale;
                    if (yZero is < 20 or > ImageHeight - 20)
                    {
                        yArrayClone[i] += 20;
                    }
                }

                //Set cursor location and display it
                _cursor.Opacity = 1;
                _cursor.Margin = new Thickness(xCoord + 20, ImageHeight - yArrayClone[(int) xCoord] + 17, 0, 0);
                mainGrid.Children.Add(_cursor);
            }

            if (_mouseDown)
            {
                // Remove zoom selection rectangle so it can be redrawn
                mainGrid.Children.Remove(_selection);

                _selection.Opacity = 1;

                if (xCoord is >= 0 and <= 749 && yCoord is >= 0 and <= 399)
                {
                    // Mouse moving from top left to bottom right
                    if (_mouseDownXCoord - xCoord < 0 && _mouseDownYCoord - yCoord < 0)
                    {
                        _selection.Width = Math.Abs(_mouseDownXCoord - xCoord);
                        _selection.Height = Math.Abs(_mouseDownYCoord - yCoord);
                    }
                    // Mouse moving from bottom left to top right
                    else if (_mouseDownXCoord - xCoord < 0 && _mouseDownYCoord - yCoord > 0)
                    {
                        // Change co-ordinate of top left of rectangle
                        var selectionMargin = _selection.Margin;
                        selectionMargin.Top = yCoord + 31;
                        _selection.Margin = selectionMargin;

                        _selection.Width = Math.Abs(_mouseDownXCoord - xCoord);
                        _selection.Height = Math.Abs(_mouseDownYCoord - yCoord);
                    }
                    // Mouse moving from top right to bottom left
                    else if (_mouseDownXCoord - xCoord > 0 && _mouseDownYCoord - yCoord < 0)
                    {
                        // Change co-ordinate of top left of rectangle
                        var selectionMargin = _selection.Margin;
                        selectionMargin.Left = xCoord + 30;
                        _selection.Margin = selectionMargin;

                        _selection.Width = Math.Abs(_mouseDownXCoord - xCoord);
                        _selection.Height = Math.Abs(_mouseDownYCoord - yCoord);
                    }
                    // Mouse moving from bottom right to top left
                    else if (_mouseDownXCoord - xCoord > 0 && _mouseDownYCoord - yCoord > 0)
                    {
                        // Change co-ordinate of top left of rectangle
                        var selectionMargin = _selection.Margin;
                        selectionMargin.Left = xCoord + 30;
                        selectionMargin.Top = yCoord + 31;
                        _selection.Margin = selectionMargin;

                        _selection.Width = Math.Abs(_mouseDownXCoord - xCoord);
                        _selection.Height = Math.Abs(_mouseDownYCoord - yCoord);
                    }

                    mainGrid.Children.Add(_selection);
                }
                else
                {
                    _selection.Opacity = 0;
                }
            }
        }

        /// <summary>
        /// Public wrapper for .Close().
        /// </summary>
        public void ClosePopUp()
        {
            Close();
        }

        /// <summary>
        /// Detect when user presses mouse on graph as part of zooming.
        /// </summary>
        private void ImageGraph_OnMouseLeftButtonDown(object sender, MouseButtonEventArgs e)
        {
            //Get mouse coords
            var coords = Mouse.GetPosition(ImageGraph);

            //Store mouse location when pressed
            _mouseDownXCoord = (int) Math.Floor(coords.X);
            _mouseDownYCoord = (int) Math.Floor(coords.Y);
            _mouseDown = true;

            //Start drawing selection rectangle
            _selection.Width = 0;
            _selection.Height = 0;
            _selection.Margin = new Thickness(coords.X + 30, coords.Y + 31, 0, 0);
            _selection.Opacity = 1;
            mainGrid.Children.Remove(_selection);
            mainGrid.Children.Add(_selection);
        }

        /// <summary>
        /// Detect when user releases mouse on graph, re-plot using new x range.
        /// </summary>
        private void ImageGraph_OnMouseLeftButtonUp(object sender, MouseButtonEventArgs e)
        {
            //If it has been less than 1 second since last zoom, this is probably the result of mouse release ghosting
            //Therefore the zoom should be cancelled
            var currentTime = DateTimeOffset.Now.ToUnixTimeSeconds();
            if (currentTime == _timeLastGenerated)
            {
                return;
            }

            //Remove selection rectangle on mouse release
            mainGrid.Children.Remove(_selection);
            _selection.Opacity = 0;
            _selection.Width = 0;
            _selection.Height = 0;
            _mouseDown = false;

            //Get mouse location when mouse released
            var mouseUpXCoord = (int) Math.Floor(Mouse.GetPosition(ImageGraph).X);

            //Get x array of line
            var (_, (xArray, _)) = _functions.Last();

            //Ensure new min and max values are correct way around
            var newXMinCoord = Math.Min(_mouseDownXCoord, mouseUpXCoord);
            var newXMaxCoord = Math.Max(_mouseDownXCoord, mouseUpXCoord);

            //Pad selection slightly if not at edge of screen to ensure rounding doesn't case part of what the user
            //wanted to see to get cut off
            if (newXMinCoord > 0)
            {
                newXMinCoord--;
            }
            if (newXMaxCoord < ImageWidth - 1)
            {
                newXMaxCoord++;
            }

            //Convert x coordinates to values
            var newXMin = xArray[newXMinCoord];
            var newXMax = xArray[newXMaxCoord];

            //Do not zoom in if new range would be below 0.5
            if (newXMax - newXMin < 0.5)
            {
                return;
            }

            //Emulate user re-plotting graph using top tool bar
            TextBoxXMin.Text = newXMin.ToString(CultureInfo.InvariantCulture);
            TextBoxXMax.Text = newXMax.ToString(CultureInfo.InvariantCulture);
            PlotButton_OnClick(this, new RoutedEventArgs());
        }

        /// <summary>
        /// Reset graph to original state after zooming when right click pressed.
        /// </summary>
        private void ImageGraph_OnMouseRightButtonDown(object sender, MouseButtonEventArgs e)
        {
            //If it has been less than 1 second since last zoom, this is probably the result of mouse release ghosting
            //Therefore the zoom should be cancelled
            var currentTime = DateTimeOffset.Now.ToUnixTimeSeconds();
            if (currentTime == _timeLastGenerated)
            {
                return;
            }

            var (function, (xArray, _)) = _functions.First();
            var xMin = xArray.Min();
            var xMax = xArray.Max();

            TextBoxXMin.Text = xMin.ToString(CultureInfo.InvariantCulture);
            TextBoxXMax.Text = xMax.ToString(CultureInfo.InvariantCulture);
            PlotButton_OnClick(this, new RoutedEventArgs());
        }

        /// <summary>
        /// Ensure mouse button release is detected from anywhere on screen.
        /// </summary>
        private void GraphPopUp_OnMouseLeftButtonUp(object sender, MouseButtonEventArgs e)
        {
            _mouseDown = false;
        }
    }
}