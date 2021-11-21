using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using Interpreter;
using Microsoft.FSharp.Collections;
using JetBrains.Annotations;

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
        private IDictionary<string, FSharpList<Util.terminal>> _environment =
            new Dictionary<string, FSharpList<Util.terminal>>();

        /// <summary>
        /// Collection of mathematical functions.
        /// </summary>
        private Trie _functions;
        
        /// <summary>
        /// Class to represent a user defined variable.
        /// </summary>
        private class Variable
        {
            /// <summary>
            /// Identifier of the variable.
            /// </summary>
            public string Name { [UsedImplicitly] get; init; }
            /// <summary>
            /// Value held by the variable.
            /// </summary>
            public string Value { [UsedImplicitly] get; init; }
        }

        /// <summary>
        /// Entry point, initializes the app window.
        /// </summary>
        public MainWindow()
        {
            InitialiseSuggestionTrie();
            
            InitializeComponent();
            
            inputText.Text = "Enter query here..."; // initialise prompt text for input terminal
        }

        /// <summary>
        /// Method to initialise trie used for suggestion dropdown.
        /// </summary>
        private void InitialiseSuggestionTrie()
        {
            _functions = new Trie();

            Dictionary<string, string> fsfunctions = Util.functions.ToDictionary(t => t.Item1, t => t.Item2);

            foreach (var p in fsfunctions)
            {
                var s = p.Key + " : " + p.Value;
                _functions.Add(s);
            }
        }

        /// <summary>
        /// Method to refresh the variable pane, to be called whenever an action might cause the environment to update.
        /// </summary>
        private void UpdateVariableWindow()
        {
            var keyValuePairs = _environment.ToList();
            var variablesList = keyValuePairs.Select(pair => new Variable
                {Name = pair.Key, Value = Util.terminalListToString("", pair.Value)});
            varDisplay.ItemsSource = variablesList;
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
                consoleText.AppendText(inputText.Text+"\n" + ">>");
                try
                {
                    var trimmedArgsArray = Grapher.TrimmedArgsArray(inputText.Text);

                    var xArray = Grapher.ComputeXArray(trimmedArgsArray);
                    var yArray = Grapher.ComputeYArray(trimmedArgsArray, xArray, _environment);
                    
                    var graphPopUp = new GraphPopUp(xArray, yArray);
                    graphPopUp.Show();
                }
                catch (Exception plottingException)
                {
                    consoleText.AppendText("Plotting Exception: " + plottingException.Message + "\n" +
                                           plottingException.StackTrace + "\n>>");
                }
            }
            else
            {
                // 1. send query to lexer/parser/executor
                // 2a. if valid, put received answer on new line
                // 2b. if invalid, put out error
                var input = inputText.Text;

                try
                {
                    var inputList = input.Select(c => c.ToString()).ToList();
                    var inputFSharpList = ListModule.OfSeq(inputList);
                    var lexerOutput = Lexer.lexer(inputFSharpList);
                    Parser.expression(lexerOutput);
                    consoleText.AppendText(" " + input + "\n");
                    consoleText.ScrollToEnd();
                    inputText.Clear();
                    var (item1, item2) = Exec.exec(lexerOutput, Util.toMap(_environment));
                    consoleText.AppendText(Util.terminalListToString("", item1) + "\n>>");
                    _environment = item2;
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
                catch (Exception ex)
                {
                    consoleText.AppendText("Expression cannot be executed\n" + ex.Message);
                }
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
        /// Method to control action of Save button.
        /// </summary>
        private void SaveButton_OnClick(object sender, RoutedEventArgs routedEventArgs)
        {
            try
            {
                SaverLoader.ConstructSaveContents(consoleText.Text, _environment);
            }
            catch (SaverLoader.SaveLoadException e)
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
                var (item1, item2, dictionary) = SaverLoader.Load();
                if (!item1) return;
                _environment = dictionary;
                consoleText.Text = item2;
                UpdateVariableWindow();
                UpdateSuggestionTrie();
            }
            catch (SaverLoader.SaveLoadException e)
            {
                MessageBox.Show(e.Message  + "\nPlease Check The File Then Try Again");
            }
        }

        /// <summary>
        /// Method to control action of Clear button.
        /// </summary>
        private void ClearButton_OnClick(object sender, RoutedEventArgs e)
        {
            consoleText.Text = ">>";
            _environment = new Dictionary<string, FSharpList<Util.terminal>>();
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
            else if(matches != null)
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
                    
                    var s = suggestionDropDown.SelectedItem.ToString().Substring(partialMatch.Length);

                    var caretPos = inputText.CaretIndex;

                    if (suggestionDropDown.SelectedItem.ToString().Contains(":"))
                    {
                        inputText.Text = inputText.Text.Insert(caretPos, s.Split(" ")[0] + "()");
                        inputText.CaretIndex = caretPos + s.Split(" ")[0].Length + 1;
                        inputText.Focus();
                    }
                    else
                    {
                        inputText.Text = inputText.Text.Insert(inputText.CaretIndex, s.Split(" ")[0]);
                        inputText.CaretIndex = caretPos + s.Split(" ")[0].Length;
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
            var keyValuePairs = _environment.ToList();
            var variablesList = keyValuePairs.Select(pair => new Variable
                {Name = pair.Key, Value = Util.terminalListToString("", pair.Value)});
            
            // reinitialise trie
            InitialiseSuggestionTrie();
            
            // add varlist to trie
            foreach (var v in variablesList)
            {
                _functions.Add(v.Name);
            }
        }
    }
}