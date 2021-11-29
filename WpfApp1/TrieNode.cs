using System.Collections.Generic;

namespace WpfApp1
{
    public class TrieNode
    {
        private char _c;
        private bool _isKey;
        private bool _isEmpty;
        private List<TrieNode> _offspring;

        public TrieNode()
        {
            _isEmpty = true;
        }
        
        public TrieNode(char c, bool isKey)
        {
            _c = c;
            _isKey = isKey;
            _isEmpty = false;
            _offspring = new List<TrieNode>();
        }
        
        public TrieNode(char c, bool isKey, List<TrieNode> offspring)
        {
            _c = c;
            _isKey = isKey;
            _isEmpty = false;
            _offspring = offspring;
        }

        public char GetC()
        {
            return _c;
        }
        
        public bool GetIsKey()
        {
            return _isKey;
        }
        
        public void SetIsKey(bool k)
        {
            _isKey = k;
        }
        
        public bool GetIsEmpty()
        {
            return _isEmpty;
        }
        
        public List<TrieNode> GetAllOffspring()
        {
            return _offspring;
        }
        
        public TrieNode GetOffspring(char c)
        {
            TrieNode t = new TrieNode();
            
            for (int i = 0; i < _offspring.Count; i++)
            {
                if (_offspring[i].GetC() == c)
                {
                    t = _offspring[i];
                    return t;
                }
            }

            return t;
        }
        
        public void SetOffspring(TrieNode t)
        {
            _offspring.Add(t);
        }

        public override string ToString()
        {
            return _c + "";
        }
    }
}