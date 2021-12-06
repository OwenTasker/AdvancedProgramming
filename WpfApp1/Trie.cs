using System.Collections.Generic;
using System.Linq;

namespace WpfApp1
{
    public class Trie : IPrefixTree
    {
        private TrieNode _root = new() {Data = '/', IsKey = false};

        public void Clear()
        {
            _root = new TrieNode {Data = '/', IsKey = false};
        }

        public void Add(string s)
        {
            var t = _root;

            for (var i = 0; i < s.Length; i++)
            {
                var nextNode = t.GetOffspring(s[i]);

                switch (nextNode.IsEmpty)
                {
                    case false when i == s.Length - 1:
                        nextNode.IsKey = true;
                        break;
                    case true when i == s.Length - 1:
                        nextNode = new TrieNode {Data = s[i], IsKey = true};
                        t.Offspring.Add(nextNode);
                        break;
                    case true:
                        nextNode = new TrieNode {Data = s[i], IsKey = false};
                        t.Offspring.Add(nextNode);
                        break;
                }

                t = nextNode;
            }
        }

        public IEnumerable<string> Contains(string s)
        {
            var t = _root;

            foreach (var nextNode in s.Select(t1 => t.GetOffspring(t1)))
            {
                if (nextNode.IsEmpty)
                {
                    return null;
                }

                t = nextNode;
            }

            var subTrie = new Trie {_root = t};

            var matches = new HashSet<string>();

            if (s.Length != 0)
            {
                matches = DfsTraversal(s[..^1], subTrie._root, new HashSet<string>());
            }

            return matches;
        }

        private HashSet<string> DfsTraversal(string search, TrieNode t, HashSet<string> matches)
        {
            if(t == null)
            {
                return null;
            }

            // PREORDER
            search += t.Data;

            if (t.IsKey)
            {
                matches.Add(search);
            }

            var offspring = t.Offspring;

            foreach (var t1 in offspring)
            {
                matches.UnionWith(DfsTraversal(search, t1, matches)); // https://stackoverflow.com/questions/15267034/is-there-an-addrange-equivalent-for-a-hashset-in-c-sharp
            }
            // POSTORDER c.wl(t)
            return matches;
        }

        private class TrieNode
        {
            public char Data { get; init; }
            public bool IsKey { get; set; }
            public bool IsEmpty { get; private init; }
            public List<TrieNode> Offspring { get; } = new();

            public TrieNode GetOffspring(char c)
            {
                var t = new TrieNode {IsEmpty = true};

                foreach (var t1 in Offspring.Where(t1 => t1.Data == c))
                {
                    return t1;
                }

                return t;
            }

            public override string ToString()
            {
                return Data.ToString();
            }
        }
    }
}