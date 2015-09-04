#!/usr/bin/python3

import argparse
import logging
import os, os.path
import subprocess
import sys


MGIT_DIR_NAME = '.mgit'
DEFAULT_REPO_NAME = 'default'


def FindMgitDir():
  here = os.getcwd()
  while True:
    mgit_dir = os.path.join(here, MGIT_DIR_NAME)
    if os.path.isdir(mgit_dir):
      return mgit_dir
    parent = os.path.dirname(here)
    if parent == here:
      return None
    here = parent


def RepoDir(mgit_dir, repo_name):
  return os.path.join(mgit_dir, repo_name + '.git')


def Die(message):
  sys.exit(sys.argv[0] + ': ' + message)


class App:

  def Run(self):
    logging.basicConfig()

    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--repo', default=DEFAULT_REPO_NAME, help='repository to use')
    parser.add_argument('command', help='git command to run')
    parser.add_argument(
        'command_args', nargs=argparse.REMAINDER)

    args = parser.parse_args()

    if args.command == 'init':
      self._DoInit(args)
    else:
      mgit_dir = FindMgitDir()
      if mgit_dir is None:
        Die('cannot find .mgit directory')
      repo_dir = RepoDir(mgit_dir, args.repo)
      work_dir = os.path.dirname(mgit_dir)

      code = subprocess.call(
          ['git',
           '--git-dir=' + repo_dir,
           '--work-tree=' + work_dir,
           args.command] +
          args.command_args)
      sys.exit(code)

  def _DoInit(self, args):
    mgit_dir = os.path.abspath(MGIT_DIR_NAME)
    if not os.path.isdir(mgit_dir):
      os.makedirs(mgit_dir)
    repo_dir = RepoDir(mgit_dir, args.repo)
    subprocess.check_call(
        ['git', '--git-dir=' + repo_dir, 'init'],
        cwd=mgit_dir)

App().Run()
