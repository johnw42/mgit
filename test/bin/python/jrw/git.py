import os
import subprocess
import re

import jrw.path

# class HashCode(object):

#   def __init__(self, value):
#     self.value = value

#   def __str__(self):
#     return self.value

#   def __repr__(self):
#     return 'HashCode(%r)' % self.value

#   def __hash__(self):
#     return hash(self.value)

#   def __eq__(self, other):
#     return self.value == other.value


_HASH_PATTERN = re.compile(r'[0-9a-f]{40}')


class Ref(object):

  def __init__(self, string_or_ref=None, hash_code=None, branch_name=None, path=None):
    """Create or copy a ref."""

    if string_or_ref:
      if hash_code or branch_name or path:
        raise TypeError('Cannot pass string_or_ref with other arguments.')
      if isinstance(string_or_ref, Ref):
        self.hash_code = string_or_ref.hash_code
        self.branch_name = string_or_ref.branch_name
        self.path = string_or_ref.path
      elif re.match(r'[0-9a-f]+', string_or_ref):
        self.hash_code = string_or_ref
      elif string_or_ref.startswith('refs/heads/'):
        self.path = string_or_ref
      else:
        self.branch_name = string_or_ref
    else:
      self.hash_code = hash_code
      self.branch_name = branch_name
      self.path = path

    if self.hash_code and not _HASH_PATTERN.match(self.hash_code):
      raise ValueError('Invalid hash code %r' % self.hash_code)

    if self.path and self.branch_name:
      if self.path != 'refs/heads/' + self.branch_name:
        raise ValueError('Inconsistent path and branch_name')
    elif self.branch_name:
      assert not self.path
      self.path = 'refs/heads/' + self.branch_name
    elif self.path:
      assert not self.branch_name
      junk, sep, name = self.path.partition('refs/heads/')
      if sep and not junk:
        self.branch_name = name

  def __str__(self):
    return self.hash_code or self.path

  def __repr__(self):
    parts = []
    if self.hash_code:
      parts.append('hash_code=%r' % self.hash_code)
    if self.path:
      parts.append('path=%r' % self.path)
    return 'Ref(%s)' % ','.join(parts)

  def __hash__(self):
    if not self.hash_code:
      raise ValueError('Cannot hash a Ref with no hash_code')
    return hash(self.hash_code)

  def __eq__(self, other):
    if self is other:
      return True
    if self.hash_code and other.hash_code:
      return self.hash_code == other.hash_code
    return False


class Git(object):

  # def NameRev(self, *args, **kwargs):
  #   cmd = ['git', 'name-rev', '--stdin']
  #   if kwargs.pop('all', False): cmd.append('--all')
  #   if kwargs.pop('tags', False): cmd.append('--tags')
  #   refs = kwargs.pop('refs', None)
  #   if refs: cmd.append('--refs=' + refs)

  #   if kwargs:
  #     raise TypeError('Invalid keyword argument %r' % kwargs.popitem()[0])

  #   input = '\n'.join(args)
  #   proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
  #   try:
  #     output, _ = proc.communicate(input)
  #   finally:
  #     proc.wait()

  #   result = []
  #   for line in output.split('\n'):
  #     m = re.match(r'[0-9a-f]{40} \(([^()]+)\)', line)
  #     if m:
  #       result.append(m.group(1))
  #     else:
  #       result.append(None)
  #   return result

  def __init__(self):
    self._env = os.environ.copy()
    #self._env['GIT_DIR'] = git_dir

  def _CallPopenFunc(self, func, git_args, kwargs):
    kwargs['env'] = self._env
    return func(['git'] + git_args, **kwargs)

  def _CheckOutput(self, cmd, **kwargs):
    return self._CallPopenFunc(subprocess.check_output, cmd, kwargs)

  def _CheckCall(self, cmd, **kwargs):
    return self._CallPopenFunc(subprocess.check_call, cmd, kwargs)

  def _Popen(self, cmd, **kwargs):
    return self._CallPopenFunc(subprocess.Popen, cmd, kwargs)

  def Head(self):
    """Finds the current head.

    Returns:
      A Ref object with a valid hash_code attribute.
    """
    return Ref(hash_code=self._CheckOutput(['rev-parse', 'HEAD']).rstrip('\n'))

  def GetRoot(self):
    """Get the root of the working tree."""
    return self._CheckOutput(['rev-parse', '--show-toplevel']).rstrip('\n')

  def Heads(self):
    """Finds the branch heads in the repository.

    Returns:
      A list of Ref objects with valid hash_code and branch_name
      attributes.
    """
    result = []
    proc = self._Popen(['show-ref', '--heads'], stdout=subprocess.PIPE)
    try:
      for line in proc.stdout:
        code, path = line.rstrip('\n').split(' ', 1)
        ref = Ref(hash_code=code, path=path)
        assert ref.branch_name
        result.append(ref)
    finally:
      proc.wait()
    return result

  def RefLog(self):
    """Finds the entries in the reflog in reverse chronological order.

    Returns:
      A list of Ref objects with value hash_code attributes.
    """
    result = []
    proc = self._Popen(['rev-list', '-g', 'HEAD'], stdout=subprocess.PIPE)
    try:
      for line in proc.stdout:
        result.append(Ref(hash_code=line.rstrip('\n')))
    finally:
      proc.wait()
    return result

  def RecentBranches(self, include_head):
    """Finds branches that have been checked out recently.

    Args:
      include_head: (bool) If False and branch is checked out, omit
          that branch from the results.

    Returns:
      A list of unique Ref objects with valid hash_code and
      branch_name attributes, sorted from most to least recently
      checked out.
    """
    result = []
    heads_dict = {}
    for ref in self.Heads():
      heads_dict[ref] = ref
    if include_head:
      seen = set()
    else:
      seen = set([self.Head()])
    for ref in self.RefLog():
      head_ref = heads_dict.get(ref)
      if head_ref and head_ref not in seen:
        result.append(head_ref)
        seen.add(head_ref)
    return result

  def Checkout(self, ref):
    self._CheckCall(['checkout', Ref(ref).branch_name])

  def LsFiles(self, full_name=False):
    """Gets a list of all files in the repository.

    Returns:
      A list of strings.
    """
    git_args = ['ls-files', '-z']
    if full_name:
      git_args.append('--full-name')
    output = self._CheckOutput(git_args)
    return output.split('\0')

  def LsDirs(self, full_name=False, prune=False):
    result_set = set()
    for path in self.LsFiles(full_name=full_name):
      index = path.rfind('/')
      if index >= 0:
        result_set.add(path[:index])
    if prune:
      return jrw.path.PruneSubdirectories(result_set)
    else:
      return sorted(result_set)



_DEFAULT_INSTANCE = Git()
Head = _DEFAULT_INSTANCE.Head
GetRoot = _DEFAULT_INSTANCE.GetRoot
Heads = _DEFAULT_INSTANCE.Heads
RefLog = _DEFAULT_INSTANCE.RefLog
RecentBranches = _DEFAULT_INSTANCE.RecentBranches
Checkout = _DEFAULT_INSTANCE.Checkout
LsFiles = _DEFAULT_INSTANCE.LsFiles
LsDirs = _DEFAULT_INSTANCE.LsDirs
