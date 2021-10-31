using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Windows;
using System.Windows.Input;
using Interpreter;
using Microsoft.FSharp.Collections;

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
                if (inputText.Text.Length >= 4 && inputText.Text.ToUpper()[..4] == "PLOT")
                {
                    try
                    {
                        GraphPopUp graphPopUp = new GraphPopUp(inputText.Text);
                        graphPopUp.Show();
                    }
                    catch(Exception its_fucked)
                    {
                        consoleText.AppendText(its_fucked.Message + "\n" + its_fucked.StackTrace + "\n>>");
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
    }
}