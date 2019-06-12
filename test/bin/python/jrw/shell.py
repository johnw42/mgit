# Copyright 2012 Google Inc. All Rights Reserved.

"""One-line documentation for shell module.

A detailed description of shell.
"""

__author__ = 'jrw@google.com (John Williams)'

import threading

__all__ = ["sh", "Pipe"]

class ShellCommand(object):
  def __init__(self, cmd, **kw):
    self.stdout = kw.pop("stdout", None)
    self.stderr = kw.pop("stderr", None)
    self.stdin = kw.pop("stdin", None)
    self.ignore_failure = kw.pop("ignore_failure", None)
    if kw:
      raise ArgumentError("Invalid argument %r" % kw.popitem()[0])
    self.cmd = cmd

  def Run(self):
    self.Start()
    self.Wait()

  def Start(self):
    self.p = subprocess.Popen(cmd,
                              stdin=self.stdin,
                              stdout=self.stdout,
                              stderr=self.seterr)

  def Wait(self):
    exit_code = self.p.wait()
    if exit_code != 0 and not self.ignore_failure:
      raise ShellCommandError(exit_code)

class ShellCommandError(Exception):
  def __init__(self, code):
    Exception.__init__(self, "shell command returned %d" % code)
    self.exit_code = code

class Pipe(object):
  def __call__(*args, **kw):
    cmd = ShellCommand(args, kw)

  def __enter__():
    return self

  def __exit__(*args):
    pass

def Run(*args, **kw):
  return ShellCommand(args).Run()

def Lines(*args, **kw):
  pass
