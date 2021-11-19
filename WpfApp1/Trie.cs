using System;
using System.Collections.Generic;

namespace WpfApp1
{
    public class Trie
    {
        private TrieNode Root = new TrieNode('/', false);

        public Trie()
        { }
        
        public Trie(TrieNode root)
        {
            Root = root;
        }

        public TrieNode GetRoot()
        {
            return Root;
        }
        public void Add(string s)
        {
            TrieNode t = Root;

            for (int i = 0; i < s.Length; i++)
            {
                TrieNode nextNode = t.GetOffspring(s[i]);

                if (nextNode.GetIsEmpty() == false && i == s.Length - 1) // probably works?
                {
                    nextNode.SetIsKey(true);
                }
                else if (nextNode.GetIsEmpty() == true)
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
            TrieNode t = Root;

            for (int i = 0; i < s.Length; i++)
            {
                TrieNode nextNode = t.GetOffspring(s[i]);

                if (nextNode.GetIsEmpty() == true)
                {
                    return null;
                }
                
                t = nextNode;
            }
            
            Trie subtrie = new Trie(t);

            HashSet<string> matches = new HashSet<string>();
            
            if (s.Length != 0)
            {
                matches = dfsTraversalRecursive(s[..(s.Length-1)], subtrie.GetRoot(), new HashSet<string>());
            }

            return matches;
        }

        public TrieNode dfsTraversalRecursive(TrieNode t)
        {
            if(t == null)
            {
                //Console.WriteLine("if");
                return null;
            }
            else
            {
                // PREORDER
                Console.Write(t);
                List<TrieNode> offspring = t.GetAllOffspring();

                for(int i = 0; i < offspring.Count; i++)
                {
                    dfsTraversalRecursive(offspring[i]);
                }
            }
            // POSTORDER c.wl(t)
            return t;
        }
        
        public HashSet<string> dfsTraversalRecursive(string search, TrieNode t, HashSet<string> matches)
        {
            if(t == null)
            {
                //Console.WriteLine("if");
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

                List<TrieNode> offspring = t.GetAllOffspring();

                for (int i = 0; i < offspring.Count; i++)
                {
                    matches.UnionWith(dfsTraversalRecursive(search, offspring[i], matches)); // https://stackoverflow.com/questions/15267034/is-there-an-addrange-equivalent-for-a-hashset-in-c-sharp
                }
            }
            // POSTORDER c.wl(t)
            return matches;
        }
    }
}