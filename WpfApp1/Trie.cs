using System;
using System.Collections.Generic;

namespace WpfApp1
{
    public class Trie
    {
        private TrieNode _root = new TrieNode('/', false);

        public Trie()
        { }
        
        public Trie(TrieNode root)
        {
            _root = root;
        }

        public TrieNode GetRoot()
        {
            return _root;
        }
        public void Add(string s)
        {
            var t = _root;

            for (var i = 0; i < s.Length; i++)
            {
                var nextNode = t.GetOffspring(s[i]);

                if (nextNode.GetIsEmpty() == false && i == s.Length - 1) // probably works?
                {
                    nextNode.SetIsKey(true);
                }
                else if (nextNode.GetIsEmpty())
                {
                    if (i == s.Length - 1)
                    {
                        nextNode = new TrieNode(s[i], true);
                        t.SetOffspring(nextNode);
                    }
                    else
                    {
                        nextNode = new TrieNode(s[i], false);
                        t.SetOffspring(nextNode);
                    }
                }

                t = nextNode;
            }
        }

        public HashSet<string> Contains(string s)
        {
            var t = _root;

            for (var i = 0; i < s.Length; i++)
            {
                var nextNode = t.GetOffspring(s[i]);

                if (nextNode.GetIsEmpty())
                {
                    return null;
                }
                
                t = nextNode;
            }
            
            var subtrie = new Trie(t);

            var matches = new HashSet<string>();
            
            if (s.Length != 0)
            {
                matches = DfsTraversal(s[..^1], subtrie.GetRoot(), new HashSet<string>());
            }

            return matches;
        }

        public TrieNode DfsTraversal(TrieNode t)
        {
            if(t == null)
            {
                return null;
            }
            else
            {
                // PREORDER
                Console.Write(t);
                var offspring = t.GetAllOffspring();

                for(int i = 0; i < offspring.Count; i++)
                {
                    DfsTraversal(offspring[i]);
                }
            }
            // POSTORDER c.wl(t)
            return t;
        }
        
        private HashSet<string> DfsTraversal(string search, TrieNode t, HashSet<string> matches)
        {
            if(t == null)
            {
                return null;
            }
            else
            {
                // PREORDER
                search = search + t.GetC();

                if (t.GetIsKey())
                {
                    matches.Add(search);
                }

                var offspring = t.GetAllOffspring();

                for (int i = 0; i < offspring.Count; i++)
                {
                    matches.UnionWith(DfsTraversal(search, offspring[i], matches)); // https://stackoverflow.com/questions/15267034/is-there-an-addrange-equivalent-for-a-hashset-in-c-sharp
                }
            }
            // POSTORDER c.wl(t)
            return matches;
        }
    }
}