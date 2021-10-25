using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Input;
using Interpreter;
using Microsoft.FSharp.Collections;
using Microsoft.Win32;

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
    /// </summary>
    public partial class MainWindow : Window
    {
        private IDictionary<string, string> _environment = new Dictionary<string, string>();

        public class Variable
        {
            public string Name { get; set; }
            public string Value { get; set; }
        }

        public MainWindow()
        {
            InitializeComponent();
        }
        
        public void UpdateVariableWindow()
        {
            var keyValuePairs = _environment.ToList();
            var variablesList = keyValuePairs.Select(pair => new Variable {Name = pair.Key, Value = pair.Value});
            varDisplay.ItemsSource = variablesList;
        }

        private void EnterButtonPress(object sender, RoutedEventArgs e)
        {
            if (!(inputText.Text == "Enter query here..." || String.IsNullOrEmpty(inputText.Text) ||
                  String.IsNullOrWhiteSpace(inputText.Text)))
            {
                // 1. send query to lexer/parser/executor
                // 2a. if valid, put received answer on new line
                // 2b. if invalid, put out error
                var input = inputText.Text;

                try
                {
                    var inputList = input.Select(c => c.ToString()).ToList();
                    var inputfSharpList = ListModule.OfSeq(inputList);
                    var lexerOutput = Lexer.lexer(inputfSharpList);
                    Parser.expression(lexerOutput);
                    consoleText.AppendText(" " + input + "\n");
                    consoleText.ScrollToEnd();
                    inputText.Clear();
                    var execOutput = Exec.exec(lexerOutput, Util.toMap(_environment));
                    consoleText.AppendText(execOutput.Item1 + "\n>>");
                    _environment = execOutput.Item2;
                    UpdateVariableWindow();
                    inputText.Text = "Enter query here...";
                }
                catch (Util.TokenizeError exception)
                {
                    consoleText.AppendText(input + "\n" + exception.Data0 + "\n>>");
                }
                catch (Util.ScanError exception)
                {
                    consoleText.AppendText(input + "\n" + exception.Data0 + "\n>>");
                }
            }
        }

        private void EnterKeyClick(object sender, KeyEventArgs e)
        {
            if (e.Key == Key.Enter)
            {
                EnterButtonPress(this, new RoutedEventArgs());
                inputText.Clear();
            }
        }

        private void InputTextBoxRemovePrompt(object sender, RoutedEventArgs e)
        {
            if (inputText.Text == "Enter query here...")
            {
                inputText.Text = "";
            }
        }

        private void InputTextBoxAddPrompt(object sender, RoutedEventArgs e)
        {
            if (String.IsNullOrEmpty(inputText.Text) || String.IsNullOrWhiteSpace(inputText.Text))
            {
                inputText.Text = "Enter query here...";
            }
        }

        private void SaveButton_OnClick(object sender, RoutedEventArgs routedEventArgs)
        {
            var variableArray = new string[_environment.Count + 1];
            var idx = 0;
            foreach (var variable in _environment)
            {
                var text = $"[{variable.Key}, {variable.Value}]";
                variableArray[idx] = text;
                idx += 1;
            }
            
            var internalText = consoleText.Text.Substring(0, consoleText.Text.Length - 3);
            variableArray[idx] = internalText;
            
            File.WriteAllLines("../NeedToAddSaveAsOption.mmp", variableArray);
        }

        private void LoadButton_OnClick(object sender, RoutedEventArgs routedEventArgs)
        {
            var dir = Path.Combine(Directory.GetCurrentDirectory(), "..\\");
            OpenFileDialog fileDialog = new OpenFileDialog
            {
                Filter = "MyMathsPal Files (*.mmp)|*.mmp;",
                FilterIndex = 1,
                Multiselect = false,
                InitialDirectory = Path.GetFullPath(dir)
            };

            if (fileDialog.ShowDialog() == true)
            {
                consoleText.Clear();
                _environment.Clear();
                UpdateVariableWindow();

                //Get the path of specified file
                var filePath = fileDialog.FileName;

                foreach (var VARIABLE in File.ReadLines(filePath))
                {
                    if (VARIABLE.StartsWith('[') && VARIABLE[VARIABLE.Length - 2].Equals(']'))
                    {

                        var line = VARIABLE.Substring(1);
                        line = line.Substring(0, line.Length - 1);
                        var dictArr = line.Split(",");
                        _environment.Add(dictArr[0], dictArr[1]);
                        UpdateVariableWindow();
                    }
                    else
                    {
                        consoleText.Text += VARIABLE + "\n";
                    }
                }
                consoleText.Text += ">>";
            }
        }
    }
}   