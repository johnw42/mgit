#! /usr/bin/env python
import argparse
import errno
import logging
import os, os.path
import subprocess
import sys

class AppError(Exception): pass

class App:

  def __init__(self):
    logging.basicConfig(level=logging.INFO)

    self.parser = p = argparse.ArgumentParser()
    #p.add_argument('--force', dest='file', help='force overwrite of FILE')

    self.name = os.path.basename(sys.argv[0])
    self.home_dir = os.getenv('HOME')
    self.src_dir = os.path.abspath(os.path.dirname(sys.argv[0]))

  def Run(self):
    opts = self.parser.parse_args()
    try:
      os.chdir(os.path.dirname(sys.argv[0]))
      self.UpdateFiles()
    except AppError, e:
      print '{}: {}'.format(self.name, str(e))
      sys.exit(1)

  def UpdateFiles(self):
    go_pkgs = os.listdir('go/src')
    self.BuildGoDirs(go_pkgs)
    self.EnsureLinkContents('bin')
    self.EnsureLinkContents('go/bin', 'bin')
    #self.EnsureLink('bin')
    #self.EnsureLink('.bashrc.d')
    #self.EnsureLink('.bash_profile.d')
    #self.EnsureLink('.gitignore.d')
    self.EnsureLink('.emacs.d')
    self.EnsureLink('.bashrc')
    self.EnsureLink('.bash_logout')
    self.EnsureLink('.bash_profile')
    self.EnsureLink('.emacs')
    self.EnsureLink('.gitconfig')
    self.EnsureLink('.inputrc')
    self.EnsureLink('.logout')
    self.EnsureLink('.profile')
    self.EnsureLink('.pythonrc.py')

  def BuildGoDirs(self, go_pkgs):
    logging.info('Building go targets')
    env = {}
    env.update(os.environ)
    env['GOPATH'] = os.path.abspath('go')
    subprocess.call(['go', 'install'] + go_pkgs, env=env)

  def JoinHome(self, path):
    """Builds an absolute path to a file in $HOME."""
    assert not os.path.isabs(path)
    return os.path.join(self.home_dir, path)

  def JoinSrc(self, path):
    """Builds an absolute path to a file in this directory."""
    assert not os.path.isabs(path)
    return os.path.join(self.src_dir, path)

  def EnsureLinkContents(self, from_path, to_path=None):
    if to_path is None:
      to_path = from_path
    logging.info('EnsureLinkContents: %s => ~/%s', from_path, to_path)
    to_path = self.JoinHome(to_path)
    from_path = self.JoinSrc(from_path)
    if os.path.islink(to_path):
      os.unlink(to_path)
    if not os.path.isdir(to_path):
      os.mkdir(to_path)
    for filename in os.listdir(from_path):
      self._DoEnsureLink(
          from_path + '/' + filename,
          to_path + '/' + filename)

  def EnsureLink(self, from_path, to_path=None):
    if to_path is None:
      to_path = from_path
    logging.info('EnsureLink: %s => ~/%s', from_path, to_path)
    from_path = self.JoinSrc(from_path)
    to_path = self.JoinHome(to_path)
    self._DoEnsureLink(from_path, to_path)

  def _DoEnsureLink(self, from_path, to_path):
    """Tries to ensure that |from_path| in $HOME is a symlink to |to_path|
    in this directory.  Existing files are replaced by symlinks, but
    only the the content of the file matches the content of the link
    target."""

    logging.debug('_DoEnsureLink: %s => %s', from_path, to_path)
    link_target = os.path.relpath(from_path, os.path.dirname(to_path))
    logging.debug('link target = %s', link_target)
    if os.path.lexists(to_path):
      logging.debug('lexists: %s', to_path)
      if os.path.islink(to_path):
        if os.readlink(to_path) == link_target:
          logging.debug('correct link already exists')
          return
        else:
          logging.debug('removing incorrect symlink: %r', to_path)
          os.unlink(to_path)
      elif self.SameContent(from_path, to_path):
        logging.debug('removing copy of existing content')
        subprocess.check_call(['rm', '-rf', to_path])
    assert not os.path.exists(to_path)
    try:
      logging.debug('creating new symlink: %s', to_path)
      os.symlink(link_target, to_path)
    except OSError, e:
      if e.errno == errno.EEXIST:
        raise AppError('{}: {}'.format(e, to_path))
      else:
        raise

  def SameContent(self, path1, path2):
    """Tests whether two files/directories have the same content.
    TODO: Maybe consider file metadata.

    Returns:
      True iff the two paths point to the same content.
    """
    if os.path.isfile(path1) and os.path.isfile(path2):
      code = subprocess.call(['diff', '-u', path1, path2])
      if code == 0:
        return True
    if os.path.isdir(path1) and os.path.isdir(path2):
      names1 = set(os.listdir(path1))
      names2 = set(os.listdir(path2))
      if names1 == names2:
        for name in names1:
          if not self.SameContent(
              os.path.join(path1, name),
              os.path.join(path2, name)):
            return False
        return True
      else:
        for name in sorted(names1 - names2):
          print 'In {} but not {}: {}'.format(path1, path2, name)
        for name in sorted(names2 - names1):
          print 'In {} but not {}: {}'.format(path2, path1, name)
        return False
    print 'Found differing content for', path1, 'and', path2
    return False

App().Run()
