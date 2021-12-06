using Ninject.Modules;

namespace WpfApp1
{
    public class Bindings : NinjectModule
    {
        public override void Load()
        {
            Bind<IInterpreter>().To<Interpreter>().InSingletonScope();
            Bind<IGraphPopUp>().To<GraphPopUp>().InTransientScope();
            Bind<IGraphDataCalculator>().To<GraphDataCalculator>().InSingletonScope();
            Bind<ISaverLoader>().To<SaverLoader>().InSingletonScope();
            Bind<IAutoCompleter>().To<AutoCompleter>().InSingletonScope();
            Bind<IPrefixTree>().To<Trie>().InSingletonScope();
        }

    }
}