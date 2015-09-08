#! /usr/bin/env python3

import os.path
import shutil
import subprocess
import tempfile
import unittest

class MgitTest(unittest.TestCase):

  def setUp(self):
    self.temp_dir = tempfile.mkdtemp()

  def tearDown(self):
    shutil.rmtree(self.temp_dir)

  def test_Init(self):
    self.CheckMgit('init')
    self.AssertExists('.mgit')
    self.AssertExists('.mgit/default.git')
    self.AssertExists('.mgit/default.git/config')

  def test_CommitRevert(self):
    self.CheckMgit('init')
    self.PutFile('foo', 'original content')
    self.CheckMgit('add', 'foo')
    self.CheckMgit('commit', '-am.')
    self.PutFile('foo', 'new content')
    self.CheckMgit('add', 'foo')
    self.CheckMgit('commit', '-am.')
    self.CheckMgit('checkout', 'HEAD^')
    self.AssertFileContains('foo', 'original content')
    self.CheckMgit('checkout', 'master')
    self.AssertFileContains('foo', 'new content')

  def test_MergeWithConflict(self):
    self.CheckMgit('init')
    self.PutFile('foo', 'original content')
    self.CheckMgit('add', 'foo')
    self.CheckMgit('commit', '-am.')
    self.CheckMgit('checkout', '-bbranch')
    self.PutFile('foo', 'branch content')
    self.CheckMgit('add', 'foo')
    self.CheckMgit('commit', '-am.')
    self.CheckMgit('checkout', 'master')
    self.PutFile('foo', 'master content')
    self.CheckMgit('add', 'foo')
    self.CheckMgit('commit', '-am.')
    code = self.Mgit('merge', 'branch')
    self.assertEqual(code, 1)
    self.AssertFileContains('.mgit/default.merge/foo', '''\
<<<<<<< HEAD
master content
=======
branch content
>>>>>>> branch
''')

  def Mgit(self, *args):
    mgit_path = os.path.abspath('mgit')
    return subprocess.call([mgit_path] + list(args), cwd=self.temp_dir)

  def CheckMgit(self, *args):
    code = self.Mgit(*args)
    self.assertEqual(code, 0)

  def AssertExists(self, path):
    self.assertTrue(
        os.path.exists(os.path.join(self.temp_dir, path)),
        'expected file/directory missing: {}'.format(path))

  def PutFile(self, path, content):
    with open(os.path.join(self.temp_dir, path), 'w') as io:
      io.write(content)

  def AssertFileContains(self, path, expected_content):
    with open(os.path.join(self.temp_dir, path)) as io:
      actual_content = io.read()
    self.assertEqual(actual_content, expected_content)

unittest.main()
