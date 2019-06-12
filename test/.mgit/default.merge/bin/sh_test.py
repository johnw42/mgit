import unittest
import sh

class TestSh(unittest.TestCase):

  def test_Tokenizer(self):
    tokens = list(sh._CMD_TOKENIZER.Tokenize('  foo '))
    self.assertEquals()

unittest.main()
