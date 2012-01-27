from __future__ import with_statement

import csv
import sys, getopt

from utils.unicodecsv import UnicodeReader
from utils.zeromq import ZmqSocket


def usage():
    print 'usage: python quotes [option]'
    print '\nOptions and arguments:'
    print '--file     : feeds the server historic data from a csv file.'

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "ho:v", ["help", "file="])
    except getopt.GetoptError, err:
        # print help information and exit:
        print str(err) # will print something like "option -a not recognized"
        usage()
        sys.exit(2)        

    kwargs = {}
    for key, value in opts:
        kwargs[key.replace('--', '')] = value

    # Output help information if requested.
    if 'help' in [ arg.replace('--', '') for arg in args] or \
        'help' in kwargs.keys():
        usage()

    
    socket = ZmqSocket('tcp://127.0.0.1:6000', 'XPUB')

    

    if 'file' in kwargs.keys():
        load_file(kwargs['file'], socket)


def load_file(file_name, socket):
    """ Loads a file and sends each line of the file through a socket. """    

    reader = UnicodeReader(open(file_name, 'rb'), delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)

    for line in reader:
        print line


if __name__ == "__main__":
    main()
