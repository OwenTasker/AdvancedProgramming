using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Globalization;
using System.Linq;
using System.Windows;
using System.IO;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using Brushes = System.Drawing.Brushes;
using FontFamily = System.Windows.Media.FontFamily;
using Image = System.Drawing.Image;
using PixelFormat = System.Drawing.Imaging.PixelFormat;

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
        private bool _isDataDirty = true;

        //Array containing all pixels of graph
        private readonly byte[] _imageBuffer = new byte[ImageWidth * ImageHeight * BytesPerPixel];

        //Lists of all plotted functions, their arrays, and key axis locations (starts as just one)
        private readonly List<(string, (double[], double[]))> _functions = new();
        private readonly List<(int, string)> _yProcessed = new();
        
        //Create cursor for hovering
        private readonly Label _cursor = new();

        private readonly IInterpreter _interpreter;
        private readonly IGraphDataCalculator _graphDataCalculator;

        /// <summary>
        /// Entry point, initializes the graph window, generates and displays graph.
        /// </summary>
        public GraphPopUp(IInterpreter interpreter, IGraphDataCalculator graphDataCalculator)
        {
            _interpreter = interpreter;
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
            //Split command into arguments
            var trimmedArgsArray = _graphDataCalculator.TrimmedArgsArray(input);

            //Compute evenly spaced values for x axis between given bounds
            var xArray =
                _graphDataCalculator.ComputeXArray(double.Parse(trimmedArgsArray[1]),
                    double.Parse(trimmedArgsArray[2]));

            //Compute values for y axis based on given function and calculated x array 
            var yArray = _graphDataCalculator.ComputeYArray(trimmedArgsArray, xArray, _interpreter);

            //Add initial function and its number arrays to list
            _functions.Add((trimmedArgsArray[0],((double[]) xArray.Clone(), (double[]) yArray.Clone())));

            //Pre-fill x range boxes
            TextBoxXMin.Text = "" + xArray.Min();
            TextBoxXMax.Text = "" + xArray.Max();

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

                if (yArray[i] > yMax)
                {
                    yMax = yArray[i];
                    yMaxIndex = i;
                }
            }
            yArray[yMinIndex] -= 2;
            yArray[yMaxIndex] += 2;

            //If y is constant, modify axis range
            var functionRight = trimmedArgsArray[0].Split(">")[^1];
            var isNumber = false;
            var isPositive = false;
            try
            {
                var number = Convert.ToDouble(functionRight);
                isNumber = true;
                if (number >= 0)
                {
                    isPositive = true;
                }
            }
            catch (Exception)
            {
                // ignored
            }
            if (isNumber && isPositive)
            {
                yArray[yMinIndex] = 0;
            }
            if (isNumber && !isPositive)
            {
                yArray[yMaxIndex] = 0;
            }
                
            //Draw axis and get axis positions
            int yZero;
            int xZero;
            int yZeroUnPadded;
            ((yZero, xZero), yZeroUnPadded) = DrawAxis(xArray, yArray);
            _yProcessed.Add((yZeroUnPadded, functionRight));

            //Get steps for grid lines
            var (xGridStep, yGridStep) = CalculateGridStep(xArray, yArray);
            
            //Generate grid lines - y grid lines are parallel with y axis, xZero
            GenerateGridLines(xZero, yGridStep, xArray, yZeroUnPadded, xGridStep, yArray);

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
            AddLabels(yZero, xZero, xArray, yArray);

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
            mainGrid.Children.Add(_cursor);
        }

        private void GenerateGridLines(int yAxisXCoord, double yGridStep, double[] xArray, int xAxisYCoord, double xGridStep, double[] yArray)
        {
            var xRange = xArray.Max() - xArray.Min();
            var pixelsPerNumber = ImageWidth / xRange;
            
            for (var i = yAxisXCoord; i < ImageWidth; i += (int) Math.Floor(yGridStep * pixelsPerNumber))
            {
                if (i != yAxisXCoord)
                {
                    for (var j = 0; j < ImageHeight; j++)
                    {
                        if (j != xAxisYCoord)
                        {
                            PlotPixel(i, j, 127, 127, 127);
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Method to calculate step of grid lines for both x and y axis.
        /// </summary>
        private (double xGridStep, double yGridStep) CalculateGridStep(double[] xArray, double[] yArray)
        {
            double xGridStep;
            double yGridStep;
            
            //X grid line step
            var xRange = xArray.Max() - xArray.Min();
            switch (xRange)
            {
                //Special case for range < 1
                case < 1:
                    xGridStep = 0.1;
                    break;
                //Special case for range < 10
                case < 10:
                    xGridStep = 1;
                    break;
                //Default step is a magnitude smaller than range
                default:
                {
                    var xRangeString = ((int) Math.Ceiling(xRange)).ToString();
                    var xRangeStringLength = xRangeString.Length - 1;
                    xGridStep = Math.Pow(10, xRangeStringLength) / 5;
                    break;
                }
            }
            
            //Y grid line step
            var yRange = yArray.Max() - yArray.Min();
            switch (yRange)
            {
                //Special case for range < 1
                case < 1:
                    yGridStep = 0.1;
                    break;
                //Special case for range < 10
                case < 10:
                    yGridStep = 1;
                    break;
                //Default step is a magnitude smaller than range
                default:
                {
                    var yRangeString = ((int) Math.Ceiling(yRange)).ToString();
                    var yRangeStringLength = yRangeString.Length - 1;
                    yGridStep = Math.Pow(10, yRangeStringLength) / 5;
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
        private void AddLabels(int yZero, int xZero, double[] xArray, double[] yArray)
        {
            //This method uses inverted y axis

            //Create axis labels
            var yMaxLabel = "" + Math.Round(yArray.Max(), 2);
            var yMinLabel = "" + Math.Round(yArray.Min(), 2);
            var xMaxLabel = "" + Math.Round(xArray.Max(), 2);
            var xMinLabel = "" + Math.Round(xArray.Min(), 2);
            var zeroLabel = "0";

            //Find axis label locations
            //Find y axis label locations:
            var yMaxPointX = xZero;
            var yMinPointX = xZero;
            if (xZero > ImageWidth / 2 - 1)
            {
                yMaxPointX -= yMaxLabel.Length  * 9 + 5;
                yMinPointX -= yMinLabel.Length * 9 + 5;
            }

            var yMaxPoint = new PointF(yMaxPointX, 0);
            var yMinPoint = new PointF(yMinPointX, ImageHeight - 16);

            //Find x axis label locations:
            var xMaxPointY = yZero;
            var xMinPointY = yZero;
            if (yZero < ImageHeight / 2)
            {
                xMaxPointY += 18;
                xMinPointY += 18;
            }

            var xMaxPoint = new PointF(ImageWidth - (xMaxLabel.Length * 9 + 5), ImageHeight - xMaxPointY);
            var xMinPoint = new PointF(0, ImageHeight - xMinPointY);

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
            
            //Don't draw yMin label if it would be a positive or 0 below the x axis 
            if (yZero < ImageHeight - 16 && yArray.Min() >= 0)
            {
                yMinLabel = "";
            }
            
            //Don't draw yMax label if it would be a negative or 0 above the x axis
            if (yZero > 0 && yArray.Max() <= 0)
            {
                yMaxLabel = "";
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
                        graphics.DrawString(yMaxLabel, font, Brushes.Black, yMaxPoint);
                        graphics.DrawString(yMinLabel, font, Brushes.Black, yMinPoint);
                        graphics.DrawString(xMaxLabel, font, Brushes.Black, xMaxPoint);
                        graphics.DrawString(xMinLabel, font, Brushes.Black, xMinPoint);
                        graphics.DrawString(zeroLabel, font, Brushes.Black, zeroPoint);
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
                PlotPixel(i, (int) yArray[i]);
            }
        }

        /// <summary>
        /// Method to draw x and y axis in correct locations
        /// </summary>
        private ((int yZero, int xZero), int yZeroUnPadded) DrawAxis(IReadOnlyCollection<double> xArray, IReadOnlyCollection<double> yArray)
        {
            //Find yArray index of y=0, x axis, default to below graph
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
            var fileToSaveTo =
                SaverLoader.DetermineFileToSaveTo("PNG Image (*.png)|*.png", "graph" + _thisImageId + ".png");

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

            //Remove cursor before redrawing
            mainGrid.Children.Remove(_cursor);
            
            try
            {
                var (function, (_, _)) = _functions.Last();
                GenerateGraph("plot(" + function + "," + TextBoxXMin.Text + "," + TextBoxXMax.Text + ")");
            }
            catch (Exception plottingException)
            {
                Console.WriteLine("Plotting Exception: " + plottingException.Message + "\n" +
                                  plottingException.StackTrace + "\n>>");
            }

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
                var (yZero, functionRight) = _yProcessed.Last();

                //Set coordinate labels
                var xCoordText = Math.Round(xArray[(int) xCoord], 2).ToString(CultureInfo.InvariantCulture);
                var yCoordText = Math.Round(yArray[(int) xCoord], 2).ToString(CultureInfo.InvariantCulture);
                TextBoxXCoord.Text = xCoordText;
                TextBoxYCoord.Text = yCoordText;

                //Create clone of y array to manipulate to calculate cursor position
                var yArrayClone = (double[]) yArray.Clone();
                
                //Save minimum value of y before any further processing
                var yMin = yArrayClone.Min() - 2;
                
                //Detect if y is a constant
                var isNumber = false;
                var isPositive = false;
                try
                {
                    var number = Convert.ToDouble(functionRight);
                    isNumber = true;
                    if (number >= 0)
                    {
                        isPositive = true;
                    }
                }
                catch (Exception)
                {
                    // ignored
                }
                if (isNumber && isPositive)
                {
                    yArrayClone[0] = 0;
                    yMin = 0;
                }
                
                //Start of y array scaling
                for (var i = 0; i < ImageWidth; i++)
                {
                    yArrayClone[i] -= yMin;
                }
                var yMax = yArrayClone.Max() + 2;
                
                //Continuation of checking if y is constant
                if (isNumber && !isPositive)
                {
                    yArrayClone[0] = 0;
                    yMax = 0;
                }

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
        }
    }
}