from __future__ import with_statement

import csv
from datetime import datetime
from decimal import Decimal
import sys, getopt

from utils.unicodecsv import UnicodeReader
from publisher import QuotePublisher


def usage():
    print 'usage: python quotes [option]'
    print '\nOptions and arguments:'
    print '--file     : feeds the server historic data from a csv file.'

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "ho:v", ["help", "file=", "limit=", "symbol="])
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

    if 'limit' in kwargs.keys():
        kwargs['limit'] = int(kwargs['limit'])

    server = QuotePublisher()

    if 'file' in kwargs.keys():

        if 'symbol' not in kwargs.keys():
            print 'You need to specify the ticker symbol with the symbol flag in order to process this file.'
            sys.exit(2)

        for quote in get_quotes(kwargs['file'], kwargs.get('symbol').upper(), kwargs.get('limit', None), ):
            server.send(quote)


def get_quotes(file_name, symbol, limit=None):
    """ Loads a file and sends each line of the file through a socket. """    

    reader = UnicodeReader(open(file_name, 'rb'), delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)

    n = 0

    for line in reader:

        if limit and  n > limit:
            print "Retrieved {0} quotes.".format(limit)
            return

        n += 1

        # First line is CSV header
        if n == 1:
            continue


        quote = {}
        quote['symbol'] = symbol
        quote['date'] = datetime.strptime(line[0], "%Y.%m.%d %H:%M:%S")
        quote['ask'] = Decimal(line[1])
        quote['bid'] = Decimal(line[2])
        quote['ask_volume'] = Decimal(line[3])
        quote['bid_volume'] = Decimal(line[4])
        print quote
        yield quote


if __name__ == "__main__":
    main()
