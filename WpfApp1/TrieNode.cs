using System.Collections.Generic;

namespace WpfApp1
{
    public class TrieNode
    {
        private char C;
        private bool IsKey;
        private bool IsEmpty;
        private List<TrieNode> Offspring;

        public TrieNode()
        {
            IsEmpty = true;
        }
        
        public TrieNode(char c, bool isKey)
        {
            C = c;
            IsKey = isKey;
            IsEmpty = false;
            Offspring = new List<TrieNode>();
        }
        
        public TrieNode(char c, bool isKey, List<TrieNode> offspring)
        {
            C = c;
            IsKey = isKey;
            IsEmpty = false;
            Offspring = offspring;
        }

        public char GetC()
        {
            return C;
        }
        
        public bool GetIsKey()
        {
            return IsKey;
        }
        
        public void SetIsKey(bool k)
        {
            IsKey = k;
        }
        
        public bool GetIsEmpty()
        {
            return IsEmpty;
        }
        
        public List<TrieNode> GetAllOffspring()
        {
            return Offspring;
        }
        
        public TrieNode GetOffspring(char c)
        {
            TrieNode t = new TrieNode();
            
            for (int i = 0; i < Offspring.Count; i++)
            {
                if (Offspring[i].GetC() == c)
                {
                    t = Offspring[i];
                    return t;
                }
            }

            return t;
        }
        
        public void SetOffspring(TrieNode t)
        {
            Offspring.Add(t);
        }

        public override string ToString()
        {
            return C + "";
        }
    }
}