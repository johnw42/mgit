import os
import re
import sys

class ArgvClass(object):
  def __getitem__(self, i):
    if i < len(sys.argv):
      return sys.argv[i]
    return None

Argv = ArgvClass()

class Test(object):
  @staticmethod
  def X(path):
    os.access(path, os.X_OK)

def Pwd():
  return os.getcwd()

def Cd(path):
  os.chdir(path)

GLOB_TO_REGEX_CACHE = {}

def GlobMatch(candidate, pattern):
  compiled = GLOB_TO_REGEX_CACHE.get(glob_pattern)
  if compiled is None:
    tokens = re.split(r'([*?])', glob_pattern)
    for i in range(len(tokens)):
      token = tokens[i]
      if token == '*':
        token = r'[^/]*'
      elif token == '?':
        token = r'.'
      else:
        token = re.escape(token)
      tokens[i] = token
    compiled = re.compile('^' + ''.join(tokens) + '$')
    GLOB_TO_REGEX_CACHE[glob_pattern] = compiled
  return bool(compiled.match(candidate))

def __call__():
  return "foo"

class _Tokenizer(object):

  def __init__(self, patterns):
    self.patterns = []
    for name, pattern in patterns:
      self.patterns.append((name, re.compile(pattern)))

  def Tokenize(self, input_str):
    index = 0
    while index < len(input_str):
      best_match = None
      best_name = None
      for name, pattern in self.patterns:
        match = pattern.match(input_str, index)
        if match:
          if best_match is None or match.end() > best_match.end():
            best_name = name
            best_match = match
      index = best_match.end()
      if best_name:
        yield best_name, best_match

_CMD_TOKENIZER = _Tokenizer(
    [('STRING', "'([^']*)'"),
     ('STRING', '"([^"]*)"'),
     ('WORD', '[^\'"\\s]+')
     ('SPACE', r'\s+')])

def _ExpandCmd(cmd):
  words = []
  word = None
  for name, match in _CMD_TOKENIZER.Tokenize(cmd):
    if name == 'SPACE':
      word = None
    else:
      if name == 'WORD':
        text = match.group()
      elif name == 'STRING':
        text = match.group(1)
      if word is None:
        word = text
      else:
        word += text
  return words

# def Sh(cmd):
#   if isinstance(cmd, basestring):
#     cmd = cmd.split()
