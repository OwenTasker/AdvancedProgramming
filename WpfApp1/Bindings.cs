using JetBrains.Annotations;
using Ninject.Modules;

namespace WpfApp1
{

    /// <summary>
    /// Class to control the bindings for the dependency injection kernel in MyMathsPal. Used implicitly
    /// </summary>
    [UsedImplicitly] public class Bindings : NinjectModule
    {
        /// <summary>
        /// Method to load interfaces with their concrete implementations.
        /// </summary>
        public override void Load()
        {
            //Singletons
            Bind<IInterpreter>().To<Interpreter>().InSingletonScope();
            Bind<IGraphDataCalculator>().To<GraphDataCalculator>().InSingletonScope();
            Bind<ISaverLoader>().To<SaverLoader>().InSingletonScope();
            Bind<IAutoCompleter>().To<AutoCompleter>().InSingletonScope();
            Bind<IPrefixTree>().To<Trie>().InSingletonScope();

            //Transients
            Bind<IGraphPopUp>().To<GraphPopUp>().InTransientScope();
        }
    }
}