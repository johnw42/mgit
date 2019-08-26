from __future__ import absolute_import
import logging
import logging.config
import logging.handlers
import sys

from logging import (
    Logger, debug, info, warning, error, critical, exception)

LOG_FILENAME_BASE = '/export/hda3/tmp/jrw.logging'

def _DefaultHandler(level_name):
  return {
      'class': 'logging.handlers.RotatingFileHandler',
      'filename': '/export/hda3/tmp/jrw.logging.' + level_name,
      'level': level_name,
      'maxBytes': 1024 ** 2,
      'formatter': 'formatter',
      }

logging.config.dictConfig({
    'version': 1,
    'formatters': {
      'formatter': {
          'format': '%(asctime)s:%(levelname)s:%(filename)s:%(funcName)s: %(message)s',
          'datefmt': '%Y-%m-%d %H:%M:%S',
        },
      },
    'handlers': {
      'info': _DefaultHandler('INFO'),
      'warning': _DefaultHandler('WARNING'),
      'error': _DefaultHandler('ERROR'),
      },
    'loggers': {
      '': {
          'level': 'INFO',
          'handlers': ['info', 'warning', 'error'],
        },
      },
    })
