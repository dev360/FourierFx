import sys, getopt

from trendanalyzer import main_analyzer
from publisher import main_publisher


def usage():
    print 'usage: python quotes [mode] [options]'
    print '\nModes:'
    print ' publisher'
    print ' analyzer'


def main():

    if len(sys.argv) < 2:
        usage()
        sys.exit(2)
    
    mode = sys.argv[1].lower()

    if mode not in ('publisher', 'analyzer'):
        usage()
        sys.exit(2)

    if mode == 'publisher':
        main_publisher()

    if mode == 'analyzer':
        main_analyzer()

if __name__ == "__main__":
    main()
