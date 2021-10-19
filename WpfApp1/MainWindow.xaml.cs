using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
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
        public class Variable
        {
            public string Name { get; set; }
            public float Value { get; set; }

            public Variable(string n, float v)
            {
                Name = n;
                Value = v;
            }
        }
        public MainWindow()
        {
            InitializeComponent();

            // variable list to populate variables column in GUI
            List<Variable> variablesList = new List<Variable>();
            variablesList.Add(new Variable("x", (float)5.0));
            variablesList.Add(new Variable("y", (float)2.0));
            variablesList.Add(new Variable("z", (float)-4.0));

            varDisplay.ItemsSource = variablesList;
        }

        private void EnterButtonPress(object sender, RoutedEventArgs e)
        {
            if (!(inputText.Text == "Enter query here..." || String.IsNullOrEmpty(inputText.Text) || String.IsNullOrWhiteSpace(inputText.Text)))
            {
                var input = inputText.Text;
                var inputList = input.Select(c => c.ToString()).ToList();
                var inputfSharpList = ListModule.OfSeq(inputList);
                consoleText.AppendText(" " + input + "\n");
                consoleText.ScrollToEnd();
                inputText.Clear();

                // 1. send query to lexer/parser/executor
                // 2a. if valid, put received answer on new line
                // 2b. if invalid, put out error
                var lexerOutput = Lexer.lexer(inputfSharpList);
                var execOutput = Exec.reduce(lexerOutput.Item2);
                consoleText.AppendText(execOutput.ToString(CultureInfo.InvariantCulture) + "\n");
                consoleText.AppendText(">>");
                inputText.Text = "Enter query here...";
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
