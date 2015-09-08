import argparse
import logging
import os, os.path
import subprocess
import sys


MGIT_DIR_NAME = '.mgit'
DEFAULT_REPO_NAME = 'default'


def Die(message):
  sys.exit(sys.argv[0] + ': ' + message)


class App:

  def Run(self):
    logging.basicConfig(level=logging.INFO)

    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--repo', default=DEFAULT_REPO_NAME, help='repository to use')
    parser.add_argument(
        '-C', action=ChdirAction,
        help='change current directory before executing')
    parser.add_argument('command', help='git command to run')
    parser.add_argument(
        'command_args', nargs=argparse.REMAINDER)

    args = parser.parse_args()
    self._repo = args.repo

    if args.command == 'init':
      self._DoInit(args)
    else:
      self._FindMgitDir()
      self._FindMgitRelativeDirs()

      if args.command == 'merge':
        self._DoMerge(args)
      else:
        self._git_runner = ProcessRunner()
        self._git_runner.env['GIT_DIR'] = self._git_dir
        self._git_runner.env['GIT_WORK_TREE'] = self._work_dir
        code = self._git_runner.Call(
            ['git', args.command] + args.command_args)
        sys.exit(code)

  def _FindMgitDir(self):
    self._git_dir = None
    here = os.getcwd()
    while True:
      mgit_dir = os.path.join(here, MGIT_DIR_NAME)
      if os.path.isdir(mgit_dir):
        self._mgit_dir = mgit_dir
        break
      parent = os.path.dirname(here)
      if parent == here:
        break
      here = parent

  def _FindMgitRelativeDirs(self):
    if self._mgit_dir is None:
      Die('cannot find .mgit directory')
    self._git_dir = os.path.join(self._mgit_dir, self._repo + '.git')
    self._merge_dir = os.path.join(self._mgit_dir, self._repo + '.merge')
    self._work_dir = os.path.dirname(self._mgit_dir)

  def _DoInit(self, args):
    self._mgit_dir = os.path.abspath(MGIT_DIR_NAME)
    if not os.path.isdir(self._mgit_dir):
      os.makedirs(self._mgit_dir)
    self._FindMgitRelativeDirs()
    subprocess.check_call(
        ['git', '--git-dir=' + self._git_dir, 'init'],
        cwd=self._mgit_dir)

  def _DoMerge(self, args):
    if os.path.exists(self._merge_dir):
      Die('merge already in progress')
    print('making dir: ' + self._merge_dir)
    os.mkdir(self._merge_dir)
    subprocess.check_call(
        ['git',
         '--git-dir=' + self._git_dir,
         '--work-tree=' + self._merge_dir,
         'checkout', '-f', 'HEAD'],
        cwd=self._merge_dir)
    code = subprocess.call(
        ['git',
         '--git-dir=' + self._git_dir,
         '--work-tree=' + self._merge_dir,
         'merge'] + args.command_args,
        cwd=self._merge_dir)
    sys.exit(code)


class ChdirAction(argparse.Action):
  def __call__(self, parser, namespace, value, option_string):
    os.chdir(value)


class ProcessRunner:

  def __init__(self, **popen_defaults):
    self.env = popen_defaults.pop('env', None)
    self.cwd = popen_defaults.pop('cwd', None)
    for key in popen_defaults:
      raise TypeError('Unexpected keyword argument: ' + key)
    if self.env is None:
      self.env = {}
      self.env.update(os.environ)

  def CheckCall(self, *args, **kwargs):
    return self._StartProcess('check_call', *args, **kwargs)

  def Call(self, *args, **kwargs):
    return self._StartProcess('call', *args, **kwargs)

  def _StartProcess(self, method_name, *args, **kwargs):
    method = getattr(subprocess, method_name)
    kwargs.setdefault('cwd', self.cwd)
    kwargs.setdefault('env', self.env)
    return method(*args, **kwargs)
