#! /usr/bin/env python

# This script extracts the contents of the archive files or URLs given
# as arguments.  The contents are placed in a directory in the current
# directory whose name is the basename of the archive.

# This script currently supports the following file types and variant
# spellings of each: .tar .tar.bz2 .tar.gz .tar.Z .zip

# TODO: Prompt user to merge contents of multiple archives into a
# single directory.
import os
import optparse
import shutil
import subprocess
import sys



HERE = "(here)\0"
THERE = "(there)\0"



def main():
  p = optparse.OptionParser("xx [options] <file-or-url>...")
  p.add_option("-o", "--output", action="store",
               help="set output directory")
  p.add_option("--here",
               action="store_const", dest="output", const=HERE,
               help="put output directory in the currect directory")
  p.add_option("--there",
               action="store_const", dest="output", const=THERE,
               help="put output directory in the directory where the original archive resides")
  opts, args = p.parse_args()

  if opts.output is None:
    opts.output = HERE

  code = 0

  extractors = []
  for arg in args:
    try:
      extractors.append(Extractor(opts, arg))
    except AppError, x:
      x.log()
      code = 1
  if code != 0:
    sys.exit(code)

  for extractor in extractors:
    try:
      extractor.run()
    except AppError, x:
      x.log()
      code = 1

  sys.exit(code)



class AppError(Exception):
  "application-level error"

  def log(self):
    sys.stderr.write(os.path.basename(sys.argv[0]) + ": " + str(self) + "\n")



class Extractor(object):
  def __init__(self, opts, arg):
    self.arg = arg
    self.opts = opts
    self.parseArg(arg)
    self.chooseExtractor()
    self.chooseOutputDir()

  def run(self):
    self.tempFile = None
    try:
      self.download()
      self.extract()
    finally:
      self.cleanUp()

  def chooseExtractor(self):
    """Set self.suffix and self.extractor, and strip the suffix off of
    self.basename."""
    for suffixes, extractor in \
        [(".tar", TarExtractor("")),
         (".tar.gz .tgz", TarExtractor("z")),
         (".tar.bz2 .tbz2 .tbz", TarExtractor("j")),
         (".tar.Z .tz", TarExtractor("Z")),
         (".zip .jar .war .ear .apk .crx", ZipExtractor())]:
      for suffix in suffixes.split():
        if self.basename.endswith(suffix):
          self.basename = self.basename[:-len(suffix)]
          self.suffix = suffix
          self.extractor = extractor
          return
    raise AppError("could not choose extractor for " + self.arg)

  def parseArg(self, arg):
    """Parse arg and set self.file and self.url.  Exactly one of
    self.file or self.url will be None."""
    
    self.url = None
    self.file = None
    
    if os.path.exists(arg):
      self.parseFileName(arg)
    elif arg.startswith("file:"):
      path = arg[5:]
      if path.startswith("//"):
        path = path[2:]
      import urllib
      self.parseFileName(urllib.unquote_plus(path))
    elif arg.startswith("http:") or arg.startswith("ftp:"):
      self.parseUrl(arg)
    else:
      self.parseFileName(arg)

  def parseFileName(self, fileName):
    if not os.path.exists(fileName):
      raise AppError("no such file: " + fileName)
    elif not os.path.isfile(fileName):
      raise AppError("not a regular file: " + fileName)
    elif not os.access(fileName, os.R_OK):
      raise AppErrpr("file not readable: " + fileName)
    self.file = fileName
    self.dirname, self.basename = os.path.split(fileName)

  def parseUrl(self, url):
    self.url = url
    self.dirname = None
    if "?" in url:
      url = url[:url.index("?")]
    self.basename = url[1+url.rindex("/"):]

  def chooseOutputDir(self):
    if self.opts.output == HERE:
      outputDir = self.basename
    elif self.opts.output == THERE:
      if self.dirname is None:
        raise AppError("can't determine where to put output")
      outputDir = os.path.join(self.dirname, self.basename)
    else:
      outputDir = self.opts.output
    if os.path.exists(outputDir):
      raise AppError("output directory already exists: " + outputDir)
    self.outputDir = outputDir

  def download(self):
    if self.url is None: return
    self.file = os.path.join(os.path.dirname(self.outputDir), self.basename + self.suffix)
    code = subprocess.call(["wget", "-c", "-O", self.file, self.url])
    if code != 0:
      raise AppError("wget returned " + str(code))

  def cleanUp(self):
    if self.url is not None and os.path.isfile(self.file):
      os.remove(self.file)

  def extract(self):
    outputDir = self.outputDir
    os.makedirs(outputDir)
    self.extractor.extract(self.file, outputDir)
    entries = os.listdir(outputDir)
    subdir = os.path.join(outputDir, entries[0])
    if len(entries) == 1 and os.path.isdir(subdir):
      tempDir = outputDir + ".tmp"
      os.rename(subdir, tempDir)
      os.rmdir(outputDir)
      os.rename(tempDir, outputDir)
      oldName = os.path.basename(subdir)
      finalName = os.path.basename(outputDir)
      if oldName != finalName:
        print oldName, "=>", finalName



class TarExtractor(object):
  def __init__(self, flag):
    self.flag = flag

  def extract(self, inputFile, outputDir):
    print inputFile
    proc = subprocess.Popen(
      ["tar", "-" + self.flag + "xvf",
       inputFile, "-C", outputDir])
    code = proc.wait()
    if code != 0:
      raise AppError("tar returned " + str(code))



class ZipExtractor(object):
  def extract(self, inputFile, outputDir):
    proc = subprocess.Popen(
      ["unzip", "-d", outputDir, inputFile])
    code = proc.wait()
    if code != 0:
      raise AppError("unzip returned " + str(code))



main()
