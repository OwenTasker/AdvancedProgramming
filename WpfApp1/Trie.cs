using System.Collections.Generic;
using System.Linq;

namespace WpfApp1
{
    /// <summary>
    /// Class to represent a prefix tree in MyMathsPal. Contains methods for adding new words with order maintained and
    /// checking membership based on a prefix string.
    /// </summary>
    public class Trie : IPrefixTree
    {
        /// <summary>
        /// Root node of the tree.
        /// </summary>
        private TrieNode _root = new() {Data = '/', IsKey = false};

        /// <summary>
        /// Method to reset the tree.
        /// </summary>
        public void Clear()
        {
            _root = new TrieNode {Data = '/', IsKey = false};
        }

        /// <summary>
        /// Method to add a string to the tree, maintains the tree order.
        /// </summary>
        /// <param name="s">The string to be added</param>
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

        /// <summary>
        /// Method to check for membership of a given prefix string. Searches depth first.
        /// </summary>
        /// <param name="s">The string for which to check membership.</param>
        /// <returns>A list of strings from the tree beginning with s.</returns>
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

        /// <summary>
        /// Method to perform a depth first search of the tree.
        /// </summary>
        /// <param name="search">A string to use for the search.</param>
        /// <param name="t">A trienode at which to start the search.</param>
        /// <param name="matches">The number of matches as yet found for the search string.</param>
        /// <returns></returns>
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

        /// <summary>
        /// A private class representing a node in the Trie.
        /// </summary>
        private class TrieNode
        {
            /// <summary>
            /// The character stored at this node.
            /// </summary>
            public char Data { get; init; }
            /// <summary>
            /// Bool representing whether this node is the start of a string contained within.
            /// </summary>
            public bool IsKey { get; set; }
            /// <summary>
            /// Bool representing whether this node is empty.
            /// </summary>
            public bool IsEmpty { get; private init; }
            /// <summary>
            /// A list of this node's offspring.
            /// </summary>
            public List<TrieNode> Offspring { get; } = new();

            /// <summary>
            /// Method to check whether this node has an offspring with data c and return it.
            /// </summary>
            /// <param name="c">The data to use for the search.</param>
            /// <returns>The offspring with data c, or an empty node.</returns>
            public TrieNode GetOffspring(char c)
            {
                var t = new TrieNode {IsEmpty = true};

                foreach (var t1 in Offspring.Where(t1 => t1.Data == c))
                {
                    return t1;
                }

                return t;
            }

            /// <summary>
            /// Method to retrieve this node's data as a string.
            /// </summary>
            /// <returns>A string representation of this node's data.</returns>
            public override string ToString()
            {
                return Data.ToString();
            }
        }
    }
}