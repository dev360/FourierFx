from utils.zeromq import ZmqSocket

import settings


class BandAnalyzer(object):

    def __init__(self, *args, **kwargs):
        self.socket = ZmqSocket(settings.QUOTES_BIND_ADDRESS, 'XSUB')
        super(BandAnalyzer, self).__init__(*args, **kwargs)

    def process_quotes(self):

        data = self.socket.receive()

        if data:
            print data
