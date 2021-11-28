using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using Interpreter;
using Ninject;

namespace WpfApp1
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// References:
    /// https://stackoverflow.com/questions/11873378/adding-placeholder-text-to-textbox
    /// https://stackoverflow.com/questions/34298857/check-whether-a-textbox-is-empty-or-not
    /// https://stackoverflow.com/questions/19975617/press-enter-in-textbox-to-and-execute-button-command
    /// https://stackoverflow.com/questions/14598024/make-textbox-uneditable
    /// https://stackoverflow.com/questions/18260702/textbox-appendtext-not-autoscrolling
    /// https://www.c-sharpcorner.com/uploadfile/dpatra/autocomplete-textbox-in-wpf/
    /// https://stackoverflow.com/questions/13180486/convert-list-of-tuples-to-dictionary
    /// https://stackoverflow.com/questions/141088/what-is-the-best-way-to-iterate-over-a-dictionary
    /// </summary>
    public partial class MainWindow
    {
        /// <summary>
        /// Execution environment for this session.
        /// </summary>
        private readonly IInterpreter _interpreter;

        private readonly ISaverLoader _saverLoader;

        private readonly StandardKernel _kernel;

        /// <summary>
        /// Collection of mathematical functions. 
        /// </summary>
        private Trie _functions;

        /// <summary>
        /// Entry point, initializes the app window.
        /// </summary>
        public MainWindow()
        {
            InitializeComponent();

            _kernel = new StandardKernel();
            _kernel.Load(Assembly.GetExecutingAssembly());
            
            _interpreter = _kernel.Get<IInterpreter>();
            _saverLoader = _kernel.Get<ISaverLoader>();

            InitialiseSuggestionTrie();

            inputText.Text = "Enter query here..."; // initialise prompt text for input terminal
        }

        /// <summary>
        /// Method to initialise trie used for suggestion dropdown.
        /// </summary>
        private void InitialiseSuggestionTrie()
        {
            _functions = new Trie();

            foreach (var (function, description) in Util.functions)
            {
                _functions.Add(function + " : " + description);
            }
        }

        /// <summary>
        /// Method to refresh the variable pane, to be called whenever an action might cause the environment to update.
        /// </summary>
        private void UpdateVariableWindow()
        {
            varDisplay.ItemsSource = _interpreter.GetVariables().ToList();
        }

        /// <summary>
        /// Method to control user submission of a statement to the application.
        /// </summary>
        private void EnterButtonPress(object sender, RoutedEventArgs e)
        {
            if (inputText.Text == "Enter query here..." || string.IsNullOrEmpty(inputText.Text) ||
                string.IsNullOrWhiteSpace(inputText.Text)) return;

            if (inputText.Text.Length >= 4 && inputText.Text.ToUpper()[..4] == "PLOT")
            {
                ShowGraph();
                consoleText.AppendText(inputText.Text + "\n" + ">>");
            }
            else if (inputText.Text.ToLower().Equals("clear"))
            {
                ClearButton_OnClick(null, null);
            }
            else
            {
                Calculate();
            }
        }

        /// <summary>
        /// Method to control user submission of a statement to the application.
        /// </summary>
        private void EnterKeyClick(object sender, KeyEventArgs e)
        {
            if (e.Key != Key.Enter)
                return;

            EnterButtonPress(this, new RoutedEventArgs());
            inputText.Clear();
        }

        /// <summary>
        /// Method to control removal of dummy text from input box.
        /// </summary>
        private void InputTextBoxRemovePrompt(object sender, RoutedEventArgs e)
        {
            if (inputText.Text == "Enter query here...")
            {
                inputText.Text = "";
            }
        }

        /// <summary>
        /// Method to control addition of dummy text to input box.
        /// </summary>
        private void InputTextBoxAddPrompt(object sender, RoutedEventArgs e)
        {
            if (string.IsNullOrEmpty(inputText.Text) || string.IsNullOrWhiteSpace(inputText.Text))
            {
                inputText.Text = "Enter query here...";
            }
        }

        /// <summary>
        /// Method to control addition of text replacement for a defined list of special characters' in input box.
        /// </summary>
        private void SpecialCharacterButton_OnClick(object sender, RoutedEventArgs routedEventArgs)
        {
            if (inputText.Text == "Enter query here...")
            {
                inputText.Text = "";
            }

            var caretPos = inputText.CaretIndex;

            var (text, caretMod) = (sender as Button)?.Name switch
            {
                "sqrtButton" => ("sqrt()", -1),
                "cbrtButton" => ("cbrt()", -1),
                "xrtButton" => ("xrt()", -1),
                "differentiateButton" => ("differentiate()", -1),
                "integralButton" => ("integral()", -1),
                "absButton" => ("abs()", -1),
                "moduloButton" => ("modulo()", -1),
                "factorialButton" => ("factorial()", -1),
                "piButton" => ("pi()", 0),
                "eulerButton" => ("euler()", 0),
                _ => throw new InvalidOperationException()
            };


            inputText.Text = inputText.Text.Insert(caretPos, text);
            inputText.Focus();
            inputText.CaretIndex = caretPos + text.Length + caretMod;
        }

        /// <summary>
        /// Method to control action of Save button.
        /// </summary>
        private void SaveButton_OnClick(object sender, RoutedEventArgs routedEventArgs)
        {
            try
            {
                _saverLoader.Save(consoleText.Text, _interpreter.Environment);
            }
            catch (Exception e)
            {
                MessageBox.Show(e.Message);
            }
        }

        /// <summary>
        /// Method to control action of Load button.
        /// </summary>
        private void LoadButton_OnClick(object sender, RoutedEventArgs routedEventArgs)
        {
            try
            {
                var (item1, item2, dictionary) = _saverLoader.Load();
                if (!item1) return;
                _interpreter.Environment = dictionary;
                consoleText.Text = item2;
                UpdateVariableWindow();
                UpdateSuggestionTrie();
            }
            catch (Exception e)
            {
                MessageBox.Show(e.Message + "\nPlease Check The File Then Try Again");
            }
        }

        /// <summary>
        /// Method to control action of Clear button.
        /// </summary>
        private void ClearButton_OnClick(object sender, RoutedEventArgs e)
        {
            consoleText.Text = ">>";
            _interpreter.ClearEnvironment();
            UpdateVariableWindow();
            UpdateSuggestionTrie();
        }

        /// <summary>
        /// Method to update function suggestion dropdown in response to user input.
        /// </summary>
        private void inputText_TextChanged(object sender, TextChangedEventArgs e)
        {
            var input = inputText.Text;
            var split = Regex.Split(input, @"[^a-zA-Z]");

            var partialMatch = "";

            for (var i = split.Length - 1; i >= 0; i--)
            {
                if (!split[i].Equals(""))
                {
                    partialMatch = split[i];
                    break;
                }
            }

            var matches = _functions.Contains(partialMatch);

            if (input.Equals("Enter query here...") || input.Equals(""))
            {
                suggestionDropDown.Visibility = Visibility.Collapsed;
                suggestionDropDown.ItemsSource = new List<string>();
            }
            else if (matches != null)
            {
                suggestionDropDown.SelectedIndex = -1;
                suggestionDropDown.SelectedItem = null;

                suggestionDropDown.ItemsSource = matches.ToList();
                suggestionDropDown.Visibility = Visibility.Visible;
            }
            else
            {
                suggestionDropDown.Visibility = Visibility.Collapsed;
                suggestionDropDown.ItemsSource = new List<string>();
            }
        }

        /// <summary>
        /// Method to update input terminal with selection from function suggestion dropdown.
        /// </summary>
        private void suggestionDropDown_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            if (suggestionDropDown.ItemsSource != null)
            {
                suggestionDropDown.Visibility = Visibility.Collapsed;
                // remove event handler for updating suggestions
                inputText.TextChanged -= inputText_TextChanged;

                var input = inputText.Text;
                if (suggestionDropDown.SelectedIndex != -1)
                {
                    var split = Regex.Split(input, @"[^a-zA-Z]");

                    var partialMatch = "";

                    for (var i = split.Length - 1; i >= 0; i--)
                    {
                        if (!split[i].Equals(""))
                        {
                            partialMatch = split[i];
                            break;
                        }
                    }

                    var s = suggestionDropDown.SelectedItem.ToString()?[partialMatch.Length..];

                    var caretPos = inputText.CaretIndex;

                    var selectedString = suggestionDropDown.SelectedItem.ToString();

                    if (selectedString != null && selectedString.Contains(":") && !selectedString.Contains("clear"))
                    {
                        if (s != null)
                        {
                            inputText.Text = inputText.Text.Insert(caretPos, s.Split(" ")[0] + "()");
                            inputText.CaretIndex = caretPos + s.Split(" ")[0].Length + 1;
                        }

                        inputText.Focus();
                    }
                    else
                    {
                        if (s != null)
                        {
                            inputText.Text = inputText.Text.Insert(inputText.CaretIndex, s.Split(" ")[0]);
                            inputText.CaretIndex = caretPos + s.Split(" ")[0].Length;
                        }

                        inputText.Focus();
                    }
                }

                // re-add event handler for updating suggestions
                inputText.TextChanged += inputText_TextChanged;
            }
        }

        /// <summary>
        /// Method to refresh suggestion trie.
        /// </summary>
        private void UpdateSuggestionTrie()
        {
            // reinitialise trie
            InitialiseSuggestionTrie();

            // add var list to trie
            foreach (var (key, _) in _interpreter.GetVariables().ToList())
            {
                _functions.Add(key);
            }
        }

        private void ShowGraph()
        {
            try
            {
                var graphPopUp = _kernel.Get<IGraphPopUp>();
                graphPopUp.GenerateGraph(inputText.Text);
            }
            catch (Exception plottingException)
            {
                consoleText.AppendText("Plotting Exception: " + plottingException.Message + "\n" +
                                       plottingException.StackTrace + "\n>>");
            }
        }

        private void Calculate()
        {
            var input = inputText.Text;

            try
            {
                consoleText.AppendText(" " + input + "\n");
                consoleText.ScrollToEnd();
                inputText.Clear();

                consoleText.AppendText(_interpreter.Interpret(input) + "\n>>");

                UpdateVariableWindow();
                UpdateSuggestionTrie();

                inputText.Text = "Enter query here...";
            }
            catch (Util.TokenizeError ex1)
            {
                consoleText.AppendText(input + "\n\"" + input + "\"\n" + ex1.Data0 + "\n>>");
            }
            catch (Util.ScanError ex1)
            {
                consoleText.AppendText(input + "\n\"" + input + "\"\n" + ex1.Data0 + "\n>>");
            }
            catch (Util.ParseError ex1)
            {
                consoleText.AppendText(input + "\n\"" + input + "\"\n" + ex1.Data0 + "\n>>");
            }
            catch (Util.ExecError ex1)
            {
                consoleText.AppendText(input + "\n\"" + input + "\"\n" + ex1.Data0 + "\n>>");
            }
            catch (Util.InvalidArgumentError ex1)
            {
                consoleText.AppendText(input + "\n\"" + input + "\"\n" + ex1.Data0 + "\n>>");
            }
            catch (Exception ex)
            {
                consoleText.AppendText("Expression cannot be executed\n" + ex.Message);
            }
        }
    }
}