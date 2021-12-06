using System.Collections.Generic;

namespace WpfApp1
{
    public interface IPrefixTree
    {
        public void Clear();

        public void Add(string item);

        public IEnumerable<string> Contains(string partialMatch);
    }
}