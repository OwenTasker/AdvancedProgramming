using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
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
        private IDictionary<string, FSharpList<Util.terminal>> _environment = new Dictionary<string, FSharpList<Util.terminal>>();

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
            var variablesList = keyValuePairs.Select(pair => new Variable {Name = pair.Key, Value = pair.Value.ToString()});
            varDisplay.ItemsSource = variablesList;
        }

        private void EnterButtonPress(object sender, RoutedEventArgs e)
        {
            if (inputText.Text == "Enter query here..." || string.IsNullOrEmpty(inputText.Text) ||
                string.IsNullOrWhiteSpace(inputText.Text)) return;
            
            if (inputText.Text.Length >= 4 && inputText.Text.ToUpper()[..4] == "PLOT")
            {
                try
                {
                    var graphPopUp = new GraphPopUp(inputText.Text);
                    graphPopUp.Show();
                }
                catch(Exception plottingException)
                {
                    consoleText.AppendText("Plotting Exception: " + plottingException.Message + "\n" + plottingException.StackTrace + "\n>>");
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
            var newText = consoleText.Text;
            newText = newText.Replace("\n", "");
            var isValidToSave = Regex.Match(newText, "(>>.*){2}");

            //If not enough characters present to save, return, else continue
            if (!isValidToSave.Success)
            {
                MessageBox.Show("Insufficient Console Text To Save. Please execute at least one line");
                return;
            } 
            
            var savableInfo = new string[_environment.Count + 1];
            var idx = 0;

            //Collect each variable and add them to savableInfo
            foreach (var (key, value) in _environment)
            {
                var text = $"[{key}, {value}]";
                savableInfo[idx] = text;
                idx += 1;
            }

            //Add console text to savableInfo
            savableInfo[idx] = consoleText.Text[..^3];

            var dialog = new SaveFileDialog
            {
                InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments),
                Filter = "MyMathsPal File (*.mmp)|*.mmp"
            };

            if (dialog.ShowDialog() != true)
            {
                MessageBox.Show("Cancelled Save Operation");
                return;
            }

            File.WriteAllLines(dialog.FileName, savableInfo);
        }

        private void LoadButton_OnClick(object sender, RoutedEventArgs routedEventArgs)
        {
            
            var fileDialog = new OpenFileDialog
            {
                InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments),
                Filter = "MyMathsPal Files (*.mmp)|*.mmp;",
                FilterIndex = 1,
                Multiselect = false
            };

            //If no file is selected return, else load that file
            if (fileDialog.ShowDialog() != true)
            {
                MessageBox.Show("Load Cancelled/Not Successful");
                return;
            }

            consoleText.Clear();
            _environment.Clear();
            UpdateVariableWindow();

            //Get the path of specified file
            var filePath = fileDialog.FileName;
                
            foreach (var loadedLine in File.ReadLines(filePath))
            {
                //Make sure line is an assigned variable
                if (loadedLine.Contains(','))
                {
                    var line = loadedLine[1..];
                    line = line[..^1];
                    var dictArr = line.Split(",");
                    _environment.Add(dictArr[0], dictArr[1]);
                }
                else
                {
                    consoleText.Text += loadedLine + "\n";
                }
            }
            UpdateVariableWindow();
            consoleText.Text += ">>";
        }
    }
}   